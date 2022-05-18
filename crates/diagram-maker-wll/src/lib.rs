use wolfram_library_link::{self as wll, export, expr::Expr};

#[export(wstp, hidden)]
fn load_diagram_maker_functions(args: Vec<Expr>) -> Expr {
    assert_eq!(args.len(), 0);
    wll::exported_library_functions_association(None)
}

#[export(wstp)]
fn render_diagram(args: Vec<Expr>) -> Expr {
    todo!("render_diagram()")
}
