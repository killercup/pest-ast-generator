#![recursion_limit = "512"]

pub use self::error::Error;
use heck::{CamelCase, SnakeCase};
use itertools::Itertools;
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
        .map(rule_to_rust_structure)
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

fn rule_to_rust_structure(rule: Rule) -> Result<TokenStream, Error> {
    use ast_helpers::*;

    let rule_type = ident("Rule")?;
    let rule_name = ident(&rule.name)?;
    let type_name = ident(&rule.name.to_camel_case())?;

    Ok(match rule.expr {
        Expr::Str(..) | Expr::Insens(..) => unit_struct(type_name, rule_type, rule_name),
        Expr::Seq(a, b) => {
            let props: Vec<_> = flatten_seq(*a)
                .chain(flatten_seq(*b))
                .filter(|x| match x {
                    Expr::Str(..) | Expr::Insens(..) => false,
                    _ => true,
                })
                .enumerate()
                .map(|(idx, expr)| match expr_to_rust_type_spec(expr) {
                    Ok((Some(field_name), def)) => Ok((field_name, def)),
                    Ok((None, def)) => Ok((ident(&format!("_{}", idx))?, def)),
                    Err(e) => Err(e),
                })
                .map_results(|(prop_name, typ)| quote! { #prop_name: #typ })
                .collect::<Result<Vec<TokenStream>, Error>>()?;

            if props.is_empty() {
                return Ok(consume_struct(type_name, rule_type, rule_name));
            }

            quote! {
                #[derive(Debug, Clone, PartialEq, Eq, pest_ast::FromPest)]
                #[pest_ast(rule(Rule::#rule_name))]
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
                            let rule_name = ident(&s.to_camel_case())?;
                            let inner = ident(&s.to_camel_case())?;
                            Ok(quote! { #rule_name ( #inner <'src> ) })
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

            if variants.is_empty() {
                return Ok(consume_struct(type_name, rule_type, rule_name));
            }

            quote! {
                #[derive(Debug, Clone, PartialEq, Eq, pest_ast::FromPest)]
                #[pest_ast(rule(#rule_type::#rule_name))]
                pub enum #type_name<'src> {
                    #(
                        #variants ,
                    )*
                }
            }
        }
        _ => consume_struct(type_name, rule_type, rule_name),
    })
}

fn expr_to_rust_type_spec(expr: Expr) -> Result<(Option<syn::Ident>, TokenStream), Error> {
    match expr {
        Expr::Str(..) | Expr::Insens(..) => {
            Err(Error::Unsupported { description: "Literal as type" })
        }
        Expr::Range(..) => Ok((None, quote! { char })),
        Expr::Ident(s) => {
            let prop_name = ident(&s.to_snake_case())?;
            let type_name = ident(&s.to_camel_case())?;
            Ok((Some(prop_name), quote! { #type_name <'src> }))
        }
        Expr::PeekSlice(..)
        | Expr::PosPred(..)
        | Expr::NegPred(..)
        | Expr::Skip(_)
        | Expr::Push(_) => Err(Error::Unsupported { description: "not a pattern" }),
        Expr::Seq(..) => Err(Error::Unsupported { description: "seq as type" }),
        Expr::Choice(..) => Err(Error::Unsupported { description: "choice as type" }),
        Expr::Opt(x) => {
            let (prop_name, inner) = expr_to_rust_type_spec(*x)?;
            Ok((prop_name, quote! { Option<#inner> }))
        }
        Expr::Rep(x)
        | Expr::RepOnce(x)
        | Expr::RepExact(x, ..)
        | Expr::RepMin(x, ..)
        | Expr::RepMax(x, ..)
        | Expr::RepMinMax(x, ..) => {
            let (inner_prop_name, inner) = expr_to_rust_type_spec(*x)?;
            let prop_name = if let Some(n) = inner_prop_name {
                Some(ident(&format!("{}_items", n.to_string()))?)
            } else {
                None
            };
            Ok((prop_name, quote! { Vec<#inner> }))
        }
    }
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

fn consume_struct(
    type_name: impl ToTokens,
    rule_type: impl ToTokens,
    rule_name: impl ToTokens,
) -> TokenStream {
    quote! {
        #[derive(Debug, Clone, PartialEq, Eq, pest_ast::FromPest)]
        #[pest_ast(rule(#rule_type::#rule_name))]
        pub struct #type_name<'src> {
            #[pest_ast(outer(with(span_into_str)))]
            pub content: &'src str,
        }
    }
}

fn unit_struct(
    type_name: impl ToTokens,
    rule_type: syn::Ident,
    rule_name: impl ToTokens,
) -> TokenStream {
    let impl_from_pest = impl_from_pest_for_unit_struct(&type_name, rule_type, rule_name);

    quote! {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        pub struct #type_name<'src> {
            marker: core::marker::PhantomData<&'src str>,
        }

        #impl_from_pest
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
