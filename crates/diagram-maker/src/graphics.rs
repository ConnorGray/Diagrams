use wolfram_expr::Number;

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

/// An (x, y) coordinate.
///
/// This is distinct from Point, which represents a circular point to be drawn.
#[derive(Debug, Copy, Clone)]
pub struct Coord {
    pub x: f32,
    pub y: f32,
}

//--------------------------------------
// Directives
//--------------------------------------

#[derive(Debug)]
pub struct RGBColor {
    pub r: u8,
    pub g: u8,
    pub b: u8,
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
}

impl RGBColor {
    /// Convenience function to convert a Wolfram [`Number`] representing an
    /// RGB color value into a `u8`.
    pub fn to_u8_color(num: Number) -> u8 {
        let value: f64 = match num {
            Number::Real(real) => *real,
            Number::Integer(int) => int as f64,
        };

        let value = (255.0 * value) as u8;

        value
    }

    pub(crate) fn to_skia(&self) -> skia::Color {
        let RGBColor { r, g, b } = *self;
        skia::Color::from_rgb(r, g, b)
    }
}
