use wolfram_expr::convert::forms::RGBColor;

pub struct Graphics {
    pub commands: Vec<Command>,
}

#[derive(Debug)]
pub enum Command {
    Primitive(Primitive),
    Directive(Directive),
}

#[derive(Debug)]
pub enum Primitive {
    Line(Line),
    Rectangle(Rectangle),
    SizedText(SizedText),
}

#[derive(Debug)]
pub enum Directive {
    AbsoluteThickness(f32),
    RGBColor(RGBColor),
    EdgeForm(Vec<Directive>),
}

//--------------------------------------
// Primitives
//--------------------------------------

#[derive(Debug)]
pub struct Line {
    pub coords: Vec<Coord>,
}

#[derive(Debug)]
pub struct Rectangle {
    pub bottom: f32,
    pub top: f32,
    pub left: f32,
    pub right: f32,

    pub rounding_radius: f32,
}

#[derive(Debug)]
pub struct SizedText {
    pub string: String,
    pub rect: Rectangle,
}

/// An (x, y) coordinate.
///
/// This is distinct from Point, which represents a circular point to be drawn.
#[derive(Debug, Copy, Clone)]
pub struct Coord {
    pub x: f32,
    pub y: f32,
}

//======================================
// Impls
//======================================

impl Rectangle {
    #[rustfmt::skip]
    pub(crate) fn to_skia(&self) -> skia::Rect {
        let Rectangle { left, right, top, bottom, rounding_radius: _ } = *self;

        skia::Rect { left, right, top, bottom }
    }

    #[rustfmt::skip]
    pub(crate) fn to_layout_rect(&self) -> crate::layout::Rect {
        let Rectangle { left, right, top, bottom, rounding_radius: _ } = *self;

        crate::layout::Rect {
            left,
            right,
            // Flip the top and bottom values, because WL graphics place the
            // origin at the bottom left, but Skia places the origin in the
            // top left.
            top: bottom,
            bottom: top
        }
    }
}

pub(crate) fn rgb_color_to_skia_color(rgb: &RGBColor) -> skia::Color {
    let RGBColor { r, g, b, a } = *rgb;
    match a {
        Some(a) => skia::Color::from_argb(a, r, g, b),
        None => skia::Color::from_rgb(r, g, b),
    }
}
