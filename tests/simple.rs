#[macro_use]
mod utils;

test!(capture: "integer = @{ ASCII_DIGIT+ }");

test!(simple_choice_and_seq: r#"
    foo = { a | b | c }
    a = { "a" }
    b = { "b" }
    c = { "foo" ~ integer ~ "bar" }
    integer = @{ ASCII_DIGIT+ }
"#);

test!(repeat: r#"
    array = { "[" ~ integer* ~ "]" }
    integer = @{ ASCII_DIGIT+ }
"#);
