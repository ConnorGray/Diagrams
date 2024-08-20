use wolfram_expr::ExprKind;
use wolfram_library_link::{
    self as wll, export,
    expr::{convert::FromExpr, Expr, Symbol},
    wstp::Link,
    NumericArray,
};

use unicode_segmentation::UnicodeSegmentation;

use unicode_data::{GeneralCategory, MappedCharacterSet};

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
    let args = args.try_as_normal().unwrap();
    assert!(args.has_head(&Symbol::new("System`List")));
    let args = args.elements();

    let diagram = Diagram::from_expr_req(&args[0]).unwrap();

    let placed = layout(&diagram, LayoutAlgorithm::Row);

    let png_data: Vec<u8> = placed.render_to_png_bytes();

    link.put_u8_array(png_data.as_slice(), &[png_data.len()])
        .unwrap();
}

#[export(wstp)]
fn graphics_image(link: &mut Link) {
    let args = link.get_expr().unwrap();
    let args = args.try_as_normal().unwrap();
    assert!(args.has_head(&Symbol::new("System`List")));
    let args = args.elements();

    let graphics = Graphics::from_expr_req(&args[0]).unwrap();

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
    // Normalize endian-specific encodings.
    let encoding = match encoding.as_str() {
        "UTF-16" => {
            if cfg!(target_endian = "little") {
                "UTF-16LE"
            } else {
                "UTF-16BE"
            }
        },
        "UTF-32" => {
            if cfg!(target_endian = "little") {
                "UTF-32LE"
            } else {
                "UTF-32BE"
            }
        },
        other => other,
    };

    match encoding {
        "UTF-8" => NumericArray::from_slice(string.as_bytes()),
        "UTF-16LE" => {
            let utf16: Vec<u16> = string.encode_utf16().collect();
            let utf16_bytes: Vec<u8> = utf16
                .iter()
                .flat_map(|code_unit: &u16| code_unit.to_le_bytes())
                .collect();
            assert_eq!(2 * utf16.len(), utf16_bytes.len());
            return NumericArray::from_slice(&utf16_bytes);
        },
        "UTF-16BE" => {
            let utf16: Vec<u16> = string.encode_utf16().collect();
            let utf16_bytes: Vec<u8> = utf16
                .iter()
                .flat_map(|code_unit: &u16| code_unit.to_be_bytes())
                .collect();
            assert_eq!(2 * utf16.len(), utf16_bytes.len());
            return NumericArray::from_slice(&utf16_bytes);
        },
        "UTF-32LE" => {
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
        "UTF-32BE" => {
            let utf32: Vec<char> = string.chars().collect();
            let utf32_bytes: Vec<u8> = utf32
                .iter()
                .flat_map(|codepoint: &char| {
                    u32::from(*codepoint).to_be_bytes()
                })
                .collect();
            assert_eq!(4 * utf32.len(), utf32_bytes.len());
            return NumericArray::from_slice(&utf32_bytes);
        },
        _ => panic!("unsupported or unrecognized encoding: {encoding}"),
    }
}

//======================================
// Unicode
//======================================

#[wll::export(wstp)]
fn grapheme_clusters(args: Vec<Expr>) -> Expr {
    let [text]: &[Expr; 1] = args.as_slice().try_into().unwrap_or_else(|_| {
        panic!("expected one argument, got {}", args.len())
    });

    let text = match text.kind() {
        ExprKind::String(text) => text,
        _ => panic!("expected 1st argument to be String, got: {text}"),
    };

    let mut list = Vec::<Expr>::new();

    list.extend(text.graphemes(true).map(Expr::string));

    return Expr::list(list);
}

//--------------------------------------
// Unicode Data
//--------------------------------------

#[wll::export(wstp)]
fn get_unicode_data(args: Vec<Expr>) -> Expr {
    let []: &[Expr; 0] = args.as_slice().try_into().unwrap_or_else(|_| {
        panic!("expected zero arguments, got {}", args.len())
    });

    return Expr::string(unicode_data::data::unicode_15_1_0::UNICODE_DATA_TXT);
}

#[wll::export(wstp)]
fn unicode_properties(args: Vec<Expr>) -> Expr {
    let []: &[Expr; 0] = args.as_slice().try_into().unwrap_or_else(|_| {
        panic!("expected zero arguments, got {}", args.len())
    });

    let list = GeneralCategory::variants()
        .into_iter()
        .map(|variant| {
            Expr::list(vec![
                Expr::string(variant.short()),
                Expr::string(variant.name()),
                Expr::string(variant.description()),
            ])
        })
        .collect();

    Expr::list(list)
}

#[wll::export(wstp)]
fn get_mapped_character_set_codepoints(args: Vec<Expr>) -> Expr {
    let [char_set]: &[Expr; 1] =
        args.as_slice().try_into().unwrap_or_else(|_| {
            panic!("expected zero arguments, got {}", args.len())
        });

    let Some(char_set) = char_set.try_as_str() else {
        if *char_set == Symbol::new("System`All") {
            let mut list = Vec::new();

            for char_set in MappedCharacterSet::variants().into_iter().copied()
            {
                list.push(Expr::rule(
                    Expr::string(char_set.variant_name()),
                    char_set_to_codepoints_expr(char_set),
                ))
            }

            return Expr::list(list);
        }

        panic!("expected 1st argument to be mapped character set string or All")
    };

    let Some(char_set) = MappedCharacterSet::from_variant_name(char_set) else {
        panic!("1st argument was not a recognized mapped character set name");
    };

    char_set_to_codepoints_expr(char_set)
}

fn char_set_to_codepoints_expr(char_set: MappedCharacterSet) -> Expr {
    let mut list = Vec::new();

    for (codepoint, unicode_codepoint) in char_set.to_unicode_codepoints() {
        list.push(Expr::rule(
            Expr::from(codepoint),
            Expr::from(u32::from(unicode_codepoint)),
        ));
    }

    Expr::list(list)
}

//==========================================================
// MLIR
//==========================================================

#[wll::export(wstp)]
fn mlir_thing(args: Vec<Expr>) -> Expr {
    use melior::{
        dialect::{arith, func, DialectRegistry},
        ir::{
            attribute::{StringAttribute, TypeAttribute},
            r#type::FunctionType,
            *,
        },
        utility::register_all_dialects,
        Context,
    };

    let registry = DialectRegistry::new();
    register_all_dialects(&registry);

    let context = Context::new();
    context.append_dialect_registry(&registry);
    context.load_all_available_dialects();

    let location = Location::unknown(&context);
    let mut module = Module::new(location);

    let index_type = Type::index(&context);

    module.body().append_operation(func::func(
        &context,
        StringAttribute::new(&context, "add"),
        TypeAttribute::new(
            FunctionType::new(
                &context,
                &[index_type, index_type],
                &[index_type],
            )
            .into(),
        ),
        {
            let block =
                Block::new(&[(index_type, location), (index_type, location)]);

            let sum = block.append_operation(arith::addi(
                block.argument(0).unwrap().into(),
                block.argument(1).unwrap().into(),
                location,
            ));

            block.append_operation(func::r#return(
                &[sum.result(0).unwrap().into()],
                location,
            ));

            let region = Region::new();
            region.append_block(block);
            region
        },
        &[],
        location,
    ));

    assert!(module.as_operation().verify());

    // return Expr::string(format!("{}", module.as_operation()));

    //----------------------------------
    // Convert to LLVM dialect
    //----------------------------------

    let manager = melior::pass::PassManager::new(&context);

    manager.add_pass(melior::pass::conversion::create_func_to_llvm());
    manager.add_pass(melior::pass::conversion::create_arith_to_llvm());

    let () = manager.run(&mut module).expect("pass running failed");

    return Expr::string(format!("{}", module.as_operation()));
}
