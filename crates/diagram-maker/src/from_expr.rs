use crate::{
    diagram::{Arrow, Box, Id, Text, Theme},
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
        let e = match e.try_normal() {
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
    match expr.kind() {
        ExprKind::Normal(normal) => {
            if !normal.has_head(&Symbol::new("System`RGBColor")) {
                return Err(format!(
                    "unrecognized color specification: {}",
                    expr
                ));
            }

            let elems = normal.elements();

            if elems.len() != 3 {
                return Err(format!(
                    "invalid RGBColor[..]: wrong number of arguments: {}",
                    expr
                ));
            }

            let r = elems[0]
                .try_number()
                .ok_or_else(|| format!("invalid red channel"))?;
            let g = elems[1]
                .try_number()
                .ok_or_else(|| format!("invalid green channel"))?;
            let b = elems[2]
                .try_number()
                .ok_or_else(|| format!("invalid blue channel"))?;

            fn to_u8_color(num: Number) -> Result<u8, String> {
                let value: f64 = match num {
                    Number::Real(real) => *real,
                    Number::Integer(int) => int as f64,
                };

                let value = (255.0 * value) as u8;

                Ok(value)
            }

            let (r, g, b) = (to_u8_color(r)?, to_u8_color(g)?, to_u8_color(b)?);

            Ok(skia::Color::from_rgb(r, g, b))
        },
        ExprKind::Integer(_)
        | ExprKind::Real(_)
        | ExprKind::String(_)
        | ExprKind::Symbol(_) => {
            return Err("unrecognized color specification".into())
        },
    }
}

impl TryFrom<&Expr> for Box {
    type Error = String;

    fn try_from(e: &Expr) -> Result<Self, Self::Error> {
        let e = match e.try_normal() {
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
        let e = match e.try_normal() {
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
        let e = match e.try_normal() {
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
        let e = match e.try_normal() {
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
