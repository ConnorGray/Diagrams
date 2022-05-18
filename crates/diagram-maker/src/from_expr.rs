use crate::{
    diagram::{Arrow, Attachment, Box, Id, Side, Text},
    Diagram,
};

use wolfram_expr::{Expr, ExprKind, Symbol};

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

        if e.elements().len() != 2 {
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

        Ok(Diagram { boxes, arrows })
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
            _ => return Err(format!("expected DiaArrow[_String -> _String, _]")),
        };

        let text = match e.elements()[1].kind() {
            ExprKind::String(s) => Text(s.clone()),
            _ => return Err(format!("expected DiaArrow[_, _String]")),
        };

        Ok(Arrow {
            start,
            end,
            text,
            // FIXME: Use Automatic attachment, or user-specified attachment.
            start_at: Attachment::Border(Side::Bottom, None),
            end_at: Attachment::Border(Side::Bottom, None),
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
