pub use self::error::Error;
use heck::{CamelCase, SnakeCase};
use pest_meta::{
    ast::{Expr, Rule, RuleType},
    parser::{self, consume_rules, parse},
};
use proc_macro2::TokenStream;
use quote::quote;
use snafu::ResultExt;

mod ast_helpers;

pub fn grammar_to_rust(grammar: &str) -> Result<TokenStream, Error> {
    let tokens = parse(parser::Rule::grammar_rules, grammar).context(error::ParseGrammar)?;
    let ast = consume_rules(tokens).map_err(|errors| Error::ReadGrammar { errors })?;

    let rules_as_types = dbg!(ast)
        .into_iter()
        .filter(|rule| rule.ty != RuleType::Silent)
        .map(rule_to_rust)
        .collect::<Result<TokenStream, Error>>()?;

    Ok(quote! {
        #rules_as_types

        pub(crate) fn span_into_str(span: pest::Span) -> &str {
            span.as_str()
        }
    })
}

fn rule_to_rust(rule: Rule) -> Result<TokenStream, Error> {
    use ast_helpers::*;

    let name = ident(&rule.name)?;
    let type_name = ident(&rule.name.to_camel_case())?;

    Ok(match rule.expr {
        Expr::Str(..) | Expr::Insens(..) => {
            quote! {
                #[derive(Debug, Clone, Copy, PartialEq, Eq, FromPest)]
                #[pest_ast(rule(Rule::#name))]
                pub struct #type_name<'src> {
                    marker: core::marker::PhantomData<&'src str>,
                }
            }
        }
        Expr::Seq(a, b) => {
            let props: Vec<_> = flatten_seq(*a)
                .chain(flatten_seq(*b))
                .enumerate()
                .filter_map(|(_idx, expr)| match expr {
                    Expr::Ident(s) => {
                        let name = ident(&s.to_snake_case()).unwrap();
                        let type_name = ident(&s.to_camel_case()).unwrap();
                        Some(quote! { #name: #type_name <'src> })
                    }
                    _ => None,
                })
                .collect::<Vec<TokenStream>>();

            quote! {
                #[derive(Debug, Clone, PartialEq, Eq, FromPest)]
                #[pest_ast(rule(Rule::#name))]
                pub struct #type_name<'src> {
                    #(
                        pub #props ,
                    )*
                }
            }
        }
        Expr::Choice(a, b) => {
            let (variants, errors) = flatten_choices(*a)
                .chain(flatten_choices(*b))
                .map(|expr: Expr| -> Result<_, Error> {
                    match expr {
                        Expr::Str(..) | Expr::Insens(..) => {
                            Err(Error::Unsupported { description: "Literal in choice" })?
                        }
                        Expr::Ident(s) => {
                            let name = ident(&s.to_camel_case())?;
                            let inner = ident(&s.to_camel_case())?;
                            Ok(quote! { #name ( #inner <'src> ) })
                        }
                        _ => Ok(quote! {}),
                    }
                })
                .fold((vec![], vec![]), |(mut oks, mut errs), res| {
                    match res {
                        Ok(x) => {
                            if !x.is_empty() {
                                oks.push(x);
                            }
                        }
                        Err(e) => errs.push(e),
                    };
                    (oks, errs)
                });

            if !errors.is_empty() {
                Err(Error::CodeGenerationErrors { errors })?
            }

            quote! {
                #[derive(Debug, Clone, PartialEq, Eq, FromPest)]
                #[pest_ast(rule(Rule::#name))]
                pub enum #type_name<'src> {
                    #(
                        #variants ,
                    )*
                }
            }
        }
        _ => quote! {
            #[derive(Debug, Clone, PartialEq, Eq, FromPest)]
            #[pest_ast(rule(Rule::#name))]
            pub struct #type_name<'src> {
                #[pest_ast(outer(with(span_into_str)))]
                pub content: &'src str,
            }
        },
    })
}

fn ident(input: &str) -> Result<syn::Ident, Error> {
    syn::parse_str(input).with_context(|| error::SynError { token: input.to_owned() })
}

mod error {
    use itertools::Itertools;
    use snafu::Snafu;

    #[derive(Debug, Snafu)]
    #[snafu(visibility = "pub(crate)")]
    pub enum Error {
        #[snafu(display("Could not parse grammar: {}", source))]
        ParseGrammar { source: pest::error::Error<pest_meta::parser::Rule> },
        #[snafu(display("Could not read grammar, errors: {:#?}", errors))]
        ReadGrammar { errors: Vec<pest::error::Error<pest_meta::parser::Rule>> },
        #[snafu(display("Syn error at `{}`: {}", token, source))]
        SynError { token: String, source: syn::Error },
        #[snafu(display("Unsupported: {}", description))]
        Unsupported { description: &'static str },
        #[snafu(display("Code generation failed: {}", errors.iter().map(Error::to_string).join("\n")))]
        CodeGenerationErrors { errors: Vec<Error> },
    }
}
