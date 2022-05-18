use wolfram_library_link::{
    self as wll, export,
    expr::{Expr, Symbol},
    wstp::Link,
};

use diagram_maker::{
    layout::{layout, LayoutAlgorithm},
    Diagram,
};

#[export(wstp, hidden)]
fn load_diagram_maker_functions(args: Vec<Expr>) -> Expr {
    assert_eq!(args.len(), 0);
    wll::exported_library_functions_association(None)
}

#[export(wstp)]
fn diagram_image(link: &mut Link) {
    let args = link.get_expr().unwrap();
    let args = args.try_normal().unwrap();
    assert!(args.has_head(&Symbol::new("System`List")));
    let args = args.elements();

    let diagram = Diagram::try_from(&args[0]).unwrap();

    let placed = layout(&diagram, LayoutAlgorithm::Row);

    let png_data: Vec<u8> = placed.render_to_png_bytes();

    link.put_u8_array(png_data.as_slice(), &[png_data.len()])
        .unwrap();
}
