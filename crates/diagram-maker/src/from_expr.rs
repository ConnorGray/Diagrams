use crate::{
    diagram::{Arrow, Box, Id, Text, Theme},
    graphics::{
        Command, Coord, Directive, Graphics, Line, Primitive, RGBColor,
        Rectangle, SizedText,
    },
    Diagram,
};

use wolfram_expr::{Expr, ExprKind, Number, Symbol};

pub struct List(Vec<Expr>);

pub struct Rule {
    pub lhs: Expr,
    pub rhs: Expr,
}

//======================================
// TryFrom<&Expr> impls
//======================================

impl TryFrom<&Expr> for Diagram {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let e = match e.try_as_normal() {
            Some(value) => value,
            None => return Err(format!("expected Diagram[..]")),
        };

        if !e.has_head(&Symbol::new("DiagramMaker`Diagram")) {
            return Err(format!("expected Diagram[..]"));
        }

        if e.elements().len() < 2 {
            return Err(format!("expected Diagram[_, _]"));
        }

        let List(boxes) = List::try_from(&e.elements()[0])?;
        let List(arrows) = List::try_from(&e.elements()[1])?;

        let boxes: Vec<Box> =
            boxes.iter().map(Box::try_from).collect::<Result<_, _>>()?;

        let arrows: Vec<Arrow> = arrows
            .iter()
            .map(Arrow::try_from)
            .collect::<Result<_, _>>()?;

        let theme = parse_theme_options(&e.elements()[2..])?;

        Ok(Diagram {
            boxes,
            arrows,
            theme,
        })
    }
}

fn parse_theme_options(opts: &[Expr]) -> Result<Theme, String> {
    let mut theme = Theme::default();

    for opt in opts {
        let Rule { lhs, rhs } = Rule::try_from(opt)?;

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

fn get_color(expr: &Expr) -> Result<skia::Color, String> {
    if let Ok(RGBColor { r, g, b }) = RGBColor::try_from(expr) {
        return Ok(skia::Color::from_rgb(r, g, b));
    }

    return Err("unrecognized color specification".into());
}

impl TryFrom<&Expr> for Box {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let e = match e.try_as_normal() {
            Some(value) => value,
            None => return Err(format!("expected DiaBox[..]")),
        };

        if !e.has_head(&Symbol::new("DiagramMaker`DiaBox")) {
            return Err(format!("expected DiaBox[..]"));
        }

        if e.elements().len() != 1 {
            return Err(format!("expected DiaBox[_]"));
        }

        let id = match e.elements()[0].kind() {
            ExprKind::String(s) => s,
            _ => return Err(format!("expected DiaBox[_String]")),
        };

        Ok(Box {
            id: Id::new(id),
            text: Text(id.clone()),
        })
    }
}

impl TryFrom<&Expr> for Arrow {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let e = match e.try_as_normal() {
            Some(value) => value,
            None => return Err(format!("expected DiaArrow[..]")),
        };

        if !e.has_head(&Symbol::new("DiagramMaker`DiaArrow")) {
            return Err(format!("expected DiaArrow[..]"));
        }

        if e.elements().len() != 2 {
            return Err(format!("expected DiaArrow[_, _]"));
        }

        let rule = Rule::try_from(&e.elements()[0])?;

        let start = match rule.lhs.kind() {
            ExprKind::String(s) => Id::new(s),
            _ => return Err(format!("expected DiaArrow[_String -> _, _]")),
        };
        let end = match rule.rhs.kind() {
            ExprKind::String(s) => Id::new(s),
            _ => {
                return Err(format!("expected DiaArrow[_String -> _String, _]"))
            },
        };

        let text = match e.elements()[1].kind() {
            ExprKind::String(s) => Text(s.clone()),
            _ => return Err(format!("expected DiaArrow[_, _String]")),
        };

        Ok(Arrow {
            start,
            end,
            text,
            // TODO: Support Attachment option on DiaArrow, use that here if specified.
            start_at: None,
            end_at: None,
        })
    }
}

//======================================
// System symbols
//======================================

impl TryFrom<&Expr> for List {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let e = match e.try_as_normal() {
            Some(value) => value,
            None => return Err(format!("expected List[..]")),
        };

        if !e.has_head(&Symbol::new("System`List")) {
            return Err(format!("expected List[..]"));
        }

        Ok(List(e.elements().to_owned()))
    }
}

impl TryFrom<&Expr> for Rule {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let e = match e.try_as_normal() {
            Some(value) => value,
            None => return Err(format!("expected Rule[..]")),
        };

        if !e.has_head(&Symbol::new("System`Rule")) {
            return Err(format!("expected Rule[..]"));
        }

        if e.elements().len() != 2 {
            return Err(format!("expected Rule[_, _]"));
        }

        Ok(Rule {
            lhs: e.elements()[0].clone(),
            rhs: e.elements()[1].clone(),
        })
    }
}

impl TryFrom<&Expr> for Graphics {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let arg: &Expr =
            &try_headed_len(e, Symbol::new("System`Graphics"), 1)?[0];

        let commands = try_headed(arg, Symbol::new("System`List"))?;

        let commands = commands
            .into_iter()
            .map(Command::try_from)
            .collect::<Result<Vec<_>, String>>()?;

        Ok(Graphics { commands })
    }
}

impl TryFrom<&Expr> for Command {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        if let Ok(primitive) = Primitive::try_from(e) {
            return Ok(Command::Primitive(primitive));
        }

        if let Ok(directive) = Directive::try_from(e) {
            return Ok(Command::Directive(directive));
        }

        return Err(format!("unrecognized graphics command: {}", e));
    }
}

impl TryFrom<&Expr> for Primitive {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        if let Ok(line) = Line::try_from(e) {
            return Ok(Primitive::Line(line));
        }

        if let Ok(rect) = Rectangle::try_from(e) {
            return Ok(Primitive::Rectangle(rect));
        }

        if let Ok(sized_text) = SizedText::try_from(e) {
            return Ok(Primitive::SizedText(sized_text));
        }

        Err(format!("unrecognized graphics primitive: {}", e))
    }
}

impl TryFrom<&Expr> for Directive {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        if let Ok(rgb) = RGBColor::try_from(e) {
            return Ok(Directive::RGBColor(rgb));
        }

        if let Ok(args) = try_headed(e, Symbol::new("System`AbsoluteThickness"))
        {
            if args.len() != 1 {
                return Err(format!("expected AbsoluteThickness[_]"));
            }

            let arg = &args[0];

            let num = arg.try_as_number().ok_or_else(|| {
                format!("expected thickness to be a number, got: {}", arg)
            })?;

            return Ok(Directive::AbsoluteThickness(as_real(num) as f32));
        }

        if let Ok(args) = try_headed(e, Symbol::new("System`EdgeForm")) {
            let directives = args
                .into_iter()
                .map(Directive::try_from)
                .collect::<Result<Vec<_>, _>>()?;

            return Ok(Directive::EdgeForm(directives));
        }

        return Err(format!("unrecognized graphics directive: {}", e));
    }
}

impl TryFrom<&Expr> for RGBColor {
    type Error = String;

    fn try_from(expr: &Expr) -> Result<Self, Self::Error> {
        let normal = expr
            .try_as_normal()
            .ok_or_else(|| format!("expected RGBColor[..]"))?;

        if !normal.has_head(&Symbol::new("System`RGBColor")) {
            return Err(format!("unrecognized color specification: {}", expr));
        }

        let elems = normal.elements();

        if elems.len() != 3 {
            return Err(format!(
                "invalid RGBColor[..]: wrong number of arguments: {}",
                expr
            ));
        }

        let r = elems[0]
            .try_as_number()
            .ok_or_else(|| format!("invalid red channel"))?;
        let g = elems[1]
            .try_as_number()
            .ok_or_else(|| format!("invalid green channel"))?;
        let b = elems[2]
            .try_as_number()
            .ok_or_else(|| format!("invalid blue channel"))?;

        let to_u8_color = RGBColor::to_u8_color;

        let (r, g, b) = (to_u8_color(r), to_u8_color(g), to_u8_color(b));

        Ok(RGBColor { r, g, b })
    }
}

impl TryFrom<&Expr> for Rectangle {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let args = try_headed_len(e, Symbol::new("System`Rectangle"), 2)?;

        let mins = Coord::try_from(&args[0])?;
        let maxs = Coord::try_from(&args[1])?;

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

impl TryFrom<&Expr> for Line {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let arg = try_headed_len(e, Symbol::new("System`Line"), 1)?;
        let arg = &arg[0];

        let elems = try_headed(arg, Symbol::new("System`List"))?;

        let coords = elems
            .into_iter()
            .map(Coord::try_from)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Line { coords })
    }
}

impl TryFrom<&Expr> for SizedText {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let args =
            try_headed_len(e, Symbol::new("Diagrams`Render`SizedText"), 2)?;

        let text = match args[0].kind() {
            ExprKind::String(s) => s.clone(),
            _ => return Err(format!("expected SizedText[_String, _]")),
        };

        let rect = match Rectangle::try_from(&args[1]) {
            Ok(rect) => rect,
            Err(err) => {
                return Err(format!("expected SizedText[_, _Rectangle]: {err}"))
            },
        };

        Ok(SizedText { string: text, rect })
    }
}

fn as_real(num: Number) -> f64 {
    match num {
        Number::Real(real) => *real,
        Number::Integer(int) => int as f64,
    }
}

impl TryFrom<&Expr> for Coord {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let args = try_headed_len(e, Symbol::new("System`List"), 2)?;

        let x = args[0].try_as_number().ok_or_else(|| {
            format!("expected coordinate to be a number, got: {}", args[0])
        })?;
        let y = args[1].try_as_number().ok_or_else(|| {
            format!("expected coordinate to be a number, got: {}", args[1])
        })?;

        let x = as_real(x) as f32;
        let y = as_real(y) as f32;

        Ok(Coord { x, y })
    }
}

fn try_headed(e: &Expr, head: Symbol) -> Result<&[Expr], String> {
    let e = match e.try_as_normal() {
        Some(value) => value,
        None => return Err(format!("expected {}[..]", head.symbol_name())),
    };

    if !e.has_head(&head) {
        return Err(format!("expected {}[..]", head.symbol_name()));
    }

    Ok(e.elements())
}

fn try_headed_len(
    e: &Expr,
    head: Symbol,
    len: usize,
) -> Result<&[Expr], String> {
    let elems = try_headed(e, head.clone())?;

    if elems.len() != len {
        return Err(format!(
            "expected {}[..] with length {len}",
            head.symbol_name()
        ));
    }

    Ok(elems)
}
