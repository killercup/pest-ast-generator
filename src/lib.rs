pub use self::error::Error;
use heck::CamelCase;
use pest_meta::{
    ast::{Expr, Rule, RuleType},
    parser::{self, consume_rules, parse},
};
use proc_macro2::TokenStream;
use quote::quote;
use snafu::ResultExt;

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
    let name = &rule.name;
    let type_name: syn::Ident = syn::parse_str(&rule.name.to_camel_case()).with_context(|| {
        error::SynError { target: "type name".to_string(), token: rule.name.to_string() }
    })?;

    Ok(match rule.expr {
        Expr::Seq(a, b) => {
            let seq: Vec<_> = flatten_seq(*a).chain(flatten_seq(*b)).collect();
            dbg!(seq);
            quote! {}
        }
        Expr::Choice(a, b) => {
            let alternates: Vec<_> = flatten_choices(*a).chain(flatten_choices(*b)).collect();
            dbg!(alternates);
            quote! {}
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

fn flatten_choices(expr: Expr) -> Box<dyn Iterator<Item = Expr>> {
    match expr {
        Expr::Choice(a, b) => Box::new(flatten_choices(*a).chain(flatten_choices(*b))),
        x => Box::new(std::iter::once(x)),
    }
}

fn flatten_seq(expr: Expr) -> Box<dyn Iterator<Item = Expr>> {
    match expr {
        Expr::Seq(a, b) => Box::new(flatten_seq(*a).chain(flatten_seq(*b))),
        x => Box::new(std::iter::once(x)),
    }
}

mod error {
    use snafu::Snafu;

    #[derive(Debug, Snafu)]
    #[snafu(visibility = "pub(crate)")]
    pub enum Error {
        #[snafu(display("Could not parse grammar: {}", source))]
        ParseGrammar { source: pest::error::Error<pest_meta::parser::Rule> },
        #[snafu(display("Could not read grammar, errors: {:#?}", errors))]
        ReadGrammar { errors: Vec<pest::error::Error<pest_meta::parser::Rule>> },
        #[snafu(display(
            "While trying to build {} you've synned at `{}`: {}",
            target,
            token,
            source
        ))]
        SynError { token: String, target: String, source: syn::Error },
    }
}
