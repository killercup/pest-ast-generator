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
use ast_helpers::*;

pub fn grammar_to_rust(name: &str, grammar: &str) -> Result<TokenStream, Error> {
    let parser_type = ident(&name.to_camel_case())?;
    let tokens = parse(parser::Rule::grammar_rules, grammar).context(error::ParseGrammar)?;
    let ast = consume_rules(tokens).map_err(|errors| Error::ReadGrammar { errors })?;

    let rules_as_types = ast
        .into_iter()
        .filter(|rule| rule.ty != RuleType::Silent)
        .filter(|r| is_regular_rule(&r.name))
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
    let rule_type = ident("Rule")?;
    let rule_name = ident(&rule.name)?;
    let type_name = ident(&rule.name.to_camel_case())?;

    Ok(match rule.clone().expr {
        Expr::Str(..) | Expr::Insens(..) => unit_struct(type_name, rule_type, rule_name),
        Expr::Seq(a, b) => {
            let props: Vec<_> = flatten_seqs(*a, *b)
                .into_iter()
                .enumerate()
                .map(|(idx, expr)| match expr_to_rust_type_spec(expr, rule.clone()) {
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
            let choices: Vec<Expr> = flatten_choices(*a).chain(flatten_choices(*b)).collect();
            let all_strings = choices.iter().all(|expr| match expr {
                Expr::Str(..) | Expr::Insens(..) => true,
                _ => false,
            });
            if all_strings {
                simple_enum(type_name, rule_type, rule_name, choices.into_iter())
            } else {
                let (variants, errors) = choices
                    .into_iter()
                    .map(|expr: Expr| -> Result<_, Error> {
                        match expr {
                            Expr::Str(s) | Expr::Insens(s) => Err(Error::Unsupported {
                                description: format!("Literal `{}` in choice", s),
                                rule: rule.clone(),
                            })?,
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
        }
        _ => consume_struct(type_name, rule_type, rule_name),
    })
}

fn expr_to_rust_type_spec(
    expr: Expr,
    rule: Rule,
) -> Result<(Option<syn::Ident>, TokenStream), Error> {
    match expr {
        Expr::Str(s) | Expr::Insens(s) => {
            Err(Error::Unsupported { description: format!("Literal `{}` as type", s), rule })
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
        | Expr::Push(_) => Err(Error::Unsupported { description: "pattern".into(), rule }),
        Expr::Seq(a, b) => {
            let props: Vec<_> = flatten_seqs(*a, *b);

            fn get_simple_rule(props: &Expr) -> Option<Expr> {
                match props {
                    Expr::Ident(s) => return Some(Expr::Ident(s.to_owned())),
                    Expr::Opt(s) => {
                        if let Expr::Ident(ref ident) = **s {
                            if is_regular_rule(&ident) {
                                return Some(Expr::Ident(ident.to_owned()));
                            }
                        }
                    }
                    _ => {}
                };
                None
            }

            let simple_props: Vec<Expr> = props.iter().filter_map(get_simple_rule).collect();

            if let [ident] = simple_props.as_slice() {
                expr_to_rust_type_spec(ident.clone(), rule)
            } else {
                eprintln!("seq too complex {:?}", props);
                Err(Error::Unsupported { description: "complex seq as type".into(), rule })
            }
        }
        Expr::Choice(..) => Err(Error::Unsupported { description: "choice as type".into(), rule }),
        Expr::Opt(x) => {
            let (prop_name, inner) = expr_to_rust_type_spec(*x, rule)?;
            Ok((prop_name, quote! { Option<#inner> }))
        }
        Expr::Rep(x)
        | Expr::RepOnce(x)
        | Expr::RepExact(x, ..)
        | Expr::RepMin(x, ..)
        | Expr::RepMax(x, ..)
        | Expr::RepMinMax(x, ..) => {
            let (inner_prop_name, inner) = expr_to_rust_type_spec(*x, rule)?;
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

/// Treat UPPERCASE_RULES as special, by which I mean "ignore them".
///
/// This makes sure we skip generating AST structs for rules like `WHITESPACE` and `COMMENT`.
fn is_regular_rule(rule_name: &str) -> bool {
    !rule_name.chars().all(|c| c.is_uppercase() || c == '_')
}

fn flatten_seqs(a: Expr, b: Expr) -> Vec<Expr> {
    flatten_seq(a)
        .chain(flatten_seq(b))
        .filter(|x| match x {
            Expr::Str(..) | Expr::Insens(..) => false,
            Expr::Ident(s) => is_regular_rule(s),
            _ => true,
        })
        .collect()
}

mod error {
    use itertools::Itertools;
    use pest_meta::ast::Rule;
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
        #[snafu(display("Unsupported {} (in rule `{}`)", description, rule.name))]
        Unsupported { description: String, rule: Rule },
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

fn simple_enum(
    type_name: impl ToTokens + Clone,
    rule_type: impl ToTokens,
    rule_name: impl ToTokens,
    choices: impl Iterator<Item = Expr>,
) -> TokenStream {
    let variants: Vec<String> = choices
        .map(|expr| match expr {
            Expr::Str(s) | Expr::Insens(s) => s,
            _ => unreachable!(),
        })
        .collect();

    let idents = variants.iter().map(|x| ident(&x.to_camel_case()).unwrap());
    let idents2 = idents.clone();

    let enum_def = quote! {
        #[derive(Debug, Clone, PartialEq, Eq, pest_ast::FromPest)]
        #[pest_ast(rule(#rule_type::#rule_name))]
        pub enum #type_name<'src> {
            #(
                #idents2 ,
            )*
        }
    };

    let type_names = std::iter::repeat(type_name.clone()).take(variants.len());
    let variants2 = variants.clone();
    let idents2 = idents.clone();

    let from_pest_impl = quote! {
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
                    let this = match pair.as_str() {
                        #(
                            #variants2 => #type_names::#idents2
                        ),*
                        _ => Err(::from_pest::ConversionError::Malformed(::from_pest::Void))?,
                    };

                    let mut inner = pair.into_inner();
                    let inner = &mut inner;
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
    };

    quote! {
        #enum_def
        #from_pest_impl
    }
}
