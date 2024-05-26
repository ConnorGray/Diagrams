use wolfram_expr::ExprKind;
use wolfram_library_link::{
    self as wll, export,
    expr::{Expr, Symbol},
    wstp::Link,
    NumericArray,
};

use crate::{
    graphics::Graphics,
    layout::{layout, LayoutAlgorithm},
    Diagram,
};

#[allow(dead_code, non_snake_case)]
pub(crate) fn Print(message: &str) {
    let _ = wll::evaluate(&Expr::normal(
        Symbol::new("System`Print"),
        vec![Expr::string(message)],
    ));
}

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

#[export(wstp)]
fn graphics_image(link: &mut Link) {
    let args = link.get_expr().unwrap();
    let args = args.try_normal().unwrap();
    assert!(args.has_head(&Symbol::new("System`List")));
    let args = args.elements();

    let graphics = Graphics::try_from(&args[0]).unwrap();

    let png_data: Vec<u8> = graphics.render_to_png_bytes();

    link.put_u8_array(png_data.as_slice(), &[png_data.len()])
        .unwrap();
}

/// Returns the width and height in pixels that is occupied by rendering the
/// given text using Skia.
#[export(wstp)]
fn rendered_text_size(args: Vec<Expr>) -> Expr {
    if args.len() != 2 {
        panic!("expected 2 arguments, got {}", args.len());
    }

    let [text, width]: &[Expr; 2] = args
        .as_slice()
        .try_into()
        .unwrap_or_else(|_| panic!("expected 2 arguments, got {}", args.len()));

    let text = match text.kind() {
        ExprKind::String(text) => text,
        _ => panic!("expected 1st argument to be String, got: {text}"),
    };

    let width = match *width.kind() {
        ExprKind::Integer(value) => value as f32,
        ExprKind::Real(value) => *value as f32,
        _ => panic!("expected 2nd argument to be a number, got: {width}"),
    };

    let (width, height) = crate::render::rendered_text_size(text, width);

    Expr::list(vec![
        Expr::real(f64::from(width)),
        Expr::real(f64::from(height)),
    ])
}

//==========================================================
// String Encodings
//==========================================================

#[wll::export]
fn encode_string(string: String, encoding: String) -> NumericArray<u8> {
    match encoding.as_str() {
        "UTF-8" => NumericArray::from_slice(string.as_bytes()),
        // FIXME: Support "UTF-16LE" and "UTF-16BE"
        "UTF-16" => {
            let utf16: Vec<u16> = string.encode_utf16().collect();
            let utf16_bytes: Vec<u8> = utf16
                .iter()
                .flat_map(|code_unit: &u16| code_unit.to_le_bytes())
                .collect();
            assert_eq!(2 * utf16.len(), utf16_bytes.len());
            return NumericArray::from_slice(&utf16_bytes);
        },
        // FIXME: Support "UTF-32LE" and "UTF-32BE"
        "UTF-32" => {
            let utf32: Vec<char> = string.chars().collect();
            let utf32_bytes: Vec<u8> = utf32
                .iter()
                .flat_map(|codepoint: &char| {
                    u32::from(*codepoint).to_le_bytes()
                })
                .collect();
            assert_eq!(4 * utf32.len(), utf32_bytes.len());
            return NumericArray::from_slice(&utf32_bytes);
        },
        _ => panic!("unsupported or unrecognized encoding: {encoding}"),
    }
}
