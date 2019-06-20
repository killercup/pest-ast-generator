#[test]
fn simple1() {
    let grammar = r#"
        foo = { a | b | c }
        a = { "a" }
        b = { "b" }
        c = { "foo" ~ integer ~ "bar" }
        integer = @{ (ASCII_DIGIT){1,10} }
    "#;
    println!("{}", rustfmt(pest_ast_generator::grammar_to_rust(grammar).unwrap()));
}

fn rustfmt(tokens: proc_macro2::TokenStream) -> String {
    duct::cmd!("rustfmt").input(tokens.to_string()).read().unwrap()
}
