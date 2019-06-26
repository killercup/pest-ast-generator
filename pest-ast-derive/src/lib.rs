extern crate proc_macro;
use proc_macro::TokenStream;
use std::{
    env,
    fs::File,
    io::{self, Read},
    path::Path,
};
use syn::{Attribute, DeriveInput, Generics, Ident, Lit, Meta};

#[proc_macro_derive(ParserAst, attributes(grammar, grammar_inline))]
pub fn derive_parser(input: TokenStream) -> TokenStream {
    let ast: DeriveInput = syn::parse(input).unwrap();
    let (name, _generics, content) = parse_derive(ast);

    let (data, _path) = match content {
        GrammarSource::File(ref path) => {
            let root = env::var("CARGO_MANIFEST_DIR").unwrap_or_else(|_| ".".into());
            let path = Path::new(&root).join("src/").join(&path);
            let file_name = match path.file_name() {
                Some(file_name) => file_name,
                None => panic!("grammar attribute should point to a file"),
            };

            let data = match read_file(&path) {
                Ok(data) => data,
                Err(error) => panic!("error opening {:?}: {}", file_name, error),
            };
            (data, Some(path.clone()))
        }
        GrammarSource::Inline(content) => (content, None),
    };

    match pest_ast_generator::grammar_to_rust(&name.to_string(), &data) {
        Ok(x) => x.into(),
        Err(e) => panic!("{}", e),
    }
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<String> {
    let mut file = File::open(path.as_ref())?;
    let mut string = String::new();
    file.read_to_string(&mut string)?;
    Ok(string)
}

#[derive(Debug, PartialEq)]
enum GrammarSource {
    File(String),
    Inline(String),
}

fn parse_derive(ast: DeriveInput) -> (Ident, Generics, GrammarSource) {
    let name = ast.ident;
    let generics = ast.generics;

    let grammar: Vec<&Attribute> = ast
        .attrs
        .iter()
        .filter(|attr| match attr.interpret_meta() {
            Some(Meta::NameValue(name_value)) => {
                (name_value.ident == "grammar" || name_value.ident == "grammar_inline")
            }
            _ => false,
        })
        .collect();

    let argument = match grammar.len() {
        0 => panic!("a grammar file needs to be provided with the #[grammar = \"PATH\"] or #[grammar_inline = \"GRAMMAR CONTENTS\"] attribute"),
        1 => get_attribute(grammar[0]),
        _ => panic!("only 1 grammar file can be provided"),
    };

    (name, generics, argument)
}

fn get_attribute(attr: &Attribute) -> GrammarSource {
    match attr.interpret_meta() {
        Some(Meta::NameValue(name_value)) => match name_value.lit {
            Lit::Str(string) => {
                if name_value.ident == "grammar" {
                    GrammarSource::File(string.value())
                } else {
                    GrammarSource::Inline(string.value())
                }
            }
            _ => panic!("grammar attribute must be a string"),
        },
        _ => panic!("grammar attribute must be of the form `grammar = \"...\"`"),
    }
}
