use pest_meta::ast::Expr;

pub(crate) fn flatten_choices(expr: Expr) -> Box<dyn Iterator<Item = Expr>> {
    match expr {
        Expr::Choice(a, b) => Box::new(flatten_choices(*a).chain(flatten_choices(*b))),
        x => Box::new(std::iter::once(x)),
    }
}

#[test]
fn test_flatten_choices() {
    let expr = Expr::Choice(
        Box::new(Expr::Str(String::from("a"))),
        Box::new(Expr::Str(String::from("b"))),
    );

    assert_eq!(
        flatten_choices(expr).collect::<Vec<_>>(),
        vec![Expr::Str(String::from("a")), Expr::Str(String::from("b"))]
    );
}

pub(crate) fn flatten_seq(expr: Expr) -> Box<dyn Iterator<Item = Expr>> {
    match expr {
        Expr::Seq(a, b) => Box::new(flatten_seq(*a).chain(flatten_seq(*b))),
        x => Box::new(std::iter::once(x)),
    }
}
