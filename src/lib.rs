#![recursion_limit = "512"]

pub use self::error::Error;
use heck::{CamelCase, SnakeCase};
use pest_meta::{
    ast::{Expr, Rule, RuleType},
    parser::{self, consume_rules, parse},
};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use snafu::ResultExt;

mod ast_helpers;

pub fn grammar_to_rust(name: &str, grammar: &str) -> Result<TokenStream, Error> {
    let parser_type = ident(&name.to_camel_case())?;
    let tokens = parse(parser::Rule::grammar_rules, grammar).context(error::ParseGrammar)?;
    let ast = consume_rules(tokens).map_err(|errors| Error::ReadGrammar { errors })?;

    let rules_as_types = ast
        .into_iter()
        .filter(|rule| rule.ty != RuleType::Silent)
        .map(rule_to_rust)
        .collect::<Result<TokenStream, Error>>()?;

    Ok(quote! {
        #rules_as_types

        #[derive(Debug, Clone, pest_derive::Parser)]
        #[grammar_inline = #grammar]
        pub struct #parser_type;

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
            let impl_from_pest = impl_from_pest_for_unit_struct(&type_name, ident("Rule")?, name);

            quote! {
                #[derive(Debug, Clone, Copy, PartialEq, Eq)]
                pub struct #type_name<'src> {
                    marker: core::marker::PhantomData<&'src str>,
                }

                #impl_from_pest
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
                #[derive(Debug, Clone, PartialEq, Eq, pest_ast::FromPest)]
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
                #[derive(Debug, Clone, PartialEq, Eq, pest_ast::FromPest)]
                #[pest_ast(rule(Rule::#name))]
                pub enum #type_name<'src> {
                    #(
                        #variants ,
                    )*
                }
            }
        }
        _ => quote! {
            #[derive(Debug, Clone, PartialEq, Eq, pest_ast::FromPest)]
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

fn impl_from_pest_for_unit_struct(
    type_name: impl ToTokens,
    rule_type: impl ToTokens,
    rule_name: impl ToTokens,
) -> TokenStream {
    quote! {
        impl<'a> ::from_pest::FromPest<'a> for #type_name<'a> {
            type Rule = #rule_type;
            type FatalError = ::from_pest::Void;

            fn from_pest(
                pest: &mut ::from_pest::pest::iterators::Pairs<'a, Self::Rule>,
            ) -> ::std::result::Result<Self, ::from_pest::ConversionError<::from_pest::Void>>
            {
                let mut clone = pest.clone();
                let pair = clone.next().ok_or(::from_pest::ConversionError::NoMatch)?;
                if pair.as_rule() == #rule_type :: #rule_name {
                    let mut inner = pair.into_inner();
                    let inner = &mut inner;
                    let this = #type_name { marker: core::marker::PhantomData };
                    if inner.clone().next().is_some() {
                        Err(::from_pest::ConversionError::Extraneous {
                            current_node: "KeyModeReplace",
                        })?;
                    }
                    *pest = clone;
                    Ok(this)
                } else {
                    Err(::from_pest::ConversionError::NoMatch)
                }
            }
        }
    }
}
