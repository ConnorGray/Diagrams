use crate::{
    diagram::{Arrow, Box, Id, Text, Theme},
    graphics::{
        self, Command, Coord, Directive, Graphics, Line, Primitive, Rectangle,
        SizedText,
    },
    Diagram,
};

use wolfram_expr::{
    convert::{
        forms::{List, RGBColor, Rule},
        parse_headed, parse_headed_args, parse_opt_headed_len, FromExpr,
        FromExprError,
    },
    Expr, ExprKind, Number, SymbolStr,
};

mod st {
    use wolfram_expr::symbol::unsafe_symbol_str;

    unsafe_symbol_str! {
        pub const List = "System`List";

        pub const Graphics = "System`Graphics";
        pub const Line = "System`Line";
        pub const Rectangle = "System`Rectangle";
        pub const AbsoluteThickness = "System`AbsoluteThickness";
        pub const EdgeForm = "System`EdgeForm";
        pub const RGBColor = "System`RGBColor";

        pub const Diagram = "DiagramMaker`Diagram";
        pub const DiaBox = "DiagramMaker`DiaBox";
        pub const DiaArrow = "DiagramMaker`DiaArrow";
    }
}
//======================================
// TryFrom<&Expr> impls
//======================================

impl FromExpr<'_> for Diagram {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        let elements =
            parse_headed(e, st::Diagram).map_err(FromExprError::Unexpected)?;

        if elements.len() < 2 {
            // TODO: Add ExprError::ExpectedLengthMin and use it here?
            return Err(FromExprError::malformed_custom(
                e,
                "expected Diagram[_, _] with 2 arguments",
            ));
        }

        let List(boxes) = List::<Box>::from_expr_req(&elements[0])?;
        let List(arrows) = List::<Arrow>::from_expr_req(&elements[1])?;

        let theme = parse_theme_options(&elements[2..])?;

        Ok(Diagram {
            boxes,
            arrows,
            theme,
        })
    }
}

fn parse_theme_options(opts: &[Expr]) -> Result<Theme, FromExprError> {
    let mut theme = Theme::default();

    for opt in opts {
        let Rule(lhs, rhs) = Rule::<Expr, Expr>::from_expr_req(opt)?;

        match lhs.kind() {
            ExprKind::String(s) if s == "BoxBackground" => {
                theme.box_background = get_color(&rhs)?
            },
            ExprKind::String(s) if s == "BoxTextColor" => {
                theme.box_text_color = get_color(&rhs)?
            },
            ExprKind::String(s) if s == "BoxBorder" => {
                theme.box_border = get_color(&rhs)?
            },
            ExprKind::String(s) if s == "ArrowStroke" => {
                theme.arrow_stroke = get_color(&rhs)?
            },
            // Ignore unrecognized options
            _ => (),
        }
    }

    Ok(theme)
}

fn get_color(expr: &Expr) -> Result<skia::Color, FromExprError> {
    let rgb = RGBColor::from_expr_req(expr)?;

    return Ok(graphics::rgb_color_to_skia_color(&rgb));
}

impl FromExpr<'_> for Box {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        let (id,): (String,) = parse_headed_args(e, st::DiaBox)?;

        Ok(Box {
            id: Id::new(id.clone()),
            text: Text(id),
        })
    }
}

impl FromExpr<'_> for Id {
    fn parse_from_expr(expr: &'_ Expr) -> Result<Self, FromExprError> {
        let id = match expr.kind() {
            ExprKind::String(s) => s,
            _ => {
                return Err(FromExprError::unexpected_custom(
                    expr,
                    "expected String diagram element ID",
                ))
            },
        };

        Ok(Id::new(id))
    }
}

impl FromExpr<'_> for Arrow {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        let (Rule(start, end), text): (Rule<_, _>, _) =
            parse_headed_args(e, st::DiaArrow)?;

        Ok(Arrow {
            start,
            end,
            text: Text(text),
            // TODO: Support Attachment option on DiaArrow, use that here if specified.
            start_at: None,
            end_at: None,
        })
    }
}

//======================================
// System symbols
//======================================

impl FromExpr<'_> for Graphics {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        let (List(commands),) = parse_headed_args(e, st::Graphics)?;

        Ok(Graphics { commands })
    }
}

impl FromExpr<'_> for Command {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        if let Some(primitive) = Primitive::from_expr_opt(e)? {
            return Ok(Command::Primitive(primitive));
        }

        if let Some(directive) = Directive::from_expr_opt(e)? {
            return Ok(Command::Directive(directive));
        }

        return Err(FromExprError::malformed_custom(
            e,
            "unrecognized graphics command",
        ));
    }
}

impl FromExpr<'_> for Primitive {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        if let Some(line) = Line::from_expr_opt(e)? {
            return Ok(Primitive::Line(line));
        }

        if let Some(rect) = Rectangle::from_expr_opt(e)? {
            return Ok(Primitive::Rectangle(rect));
        }

        if let Some(sized_text) = SizedText::from_expr_opt(e)? {
            return Ok(Primitive::SizedText(sized_text));
        }

        Err(FromExprError::malformed_custom(
            e,
            "unrecognized graphics primitive",
        ))
    }
}

impl FromExpr<'_> for Directive {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        if let Some(rgb) = RGBColor::from_expr_opt(e)? {
            return Ok(Directive::RGBColor(rgb));
        }

        if let Some([arg]) = parse_opt_headed_len(e, st::AbsoluteThickness)? {
            let num = Number::from_expr_req(arg)?;

            return Ok(Directive::AbsoluteThickness(as_real(num) as f32));
        }

        if let Ok(args) = parse_headed(e, st::EdgeForm) {
            let directives = args
                .into_iter()
                .map(Directive::from_expr_req)
                .collect::<Result<Vec<_>, _>>()?;

            return Ok(Directive::EdgeForm(directives));
        }

        return Err(FromExprError::malformed_custom(
            e,
            "unrecognized graphics directive",
        ));
    }
}

impl FromExpr<'_> for Rectangle {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        let (mins, maxs): (Coord, Coord) = parse_headed_args(e, st::Rectangle)?;

        // FIXME: Parse this from the expression;
        let rounding_radius = 0.0;

        Ok(Rectangle {
            left: mins.x,
            right: maxs.x,
            top: maxs.y,
            bottom: mins.y,

            rounding_radius,
        })
    }
}

impl FromExpr<'_> for Line {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        let (List(coords),) = parse_headed_args(e, st::Line)?;

        Ok(Line { coords })
    }
}

impl FromExpr<'_> for SizedText {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        let (string, rect) =
            parse_headed_args(e, SymbolStr::new("Diagrams`Render`SizedText"))?;

        Ok(SizedText { string, rect })
    }
}

fn as_real(num: Number) -> f64 {
    match num {
        Number::Real(real) => *real,
        Number::Integer(int) => int as f64,
    }
}

impl FromExpr<'_> for Coord {
    fn parse_from_expr(e: &Expr) -> Result<Self, FromExprError> {
        let (x, y): (Number, Number) = parse_headed_args(e, st::List)?;

        let x = as_real(x) as f32;
        let y = as_real(y) as f32;

        Ok(Coord { x, y })
    }
}
