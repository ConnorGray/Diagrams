#[derive(Debug, Clone)]
pub struct Diagram {
    pub boxes: Vec<Box>,
    pub arrows: Vec<Arrow>,
    // pub callouts: Vec<Callout>,
}

/// A unique identifier used to refer to an element of a diagram.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Id(pub String);

/// Text displayed inside or alongside an element of a diagram.
// TODO: Support styled text.
#[derive(Debug, Clone)]
pub struct Text(pub String);

#[derive(Debug, Clone)]
pub struct Box {
    /// Unique identifier for this box.
    ///
    /// This is used to refer to this box from other boxes and arrows.
    pub id: Id,

    /// The text displayed in this box.
    pub text: Text,
}

/// An arrow that relates two elements in the diagram.
#[derive(Debug, Clone)]
pub struct Arrow {
    pub start: Id,
    pub end: Id,

    /// The text displayed alongside this arrow.
    pub text: Text,

    pub start_at: Option<Attachment>,
    pub end_at: Option<Attachment>,
}

/// Description of where an [`Arrow`] attaches to the element it is starting from or
/// ending at.
#[derive(Debug, Clone)]
pub enum Attachment {
    /// Attach to specified side of the rectangular border.
    ///
    /// The second field is the position along the specified edge that the arrow should
    /// attach to. This should be a value between 0.0 and 1.0, and is linearly
    /// proportional to the final length of the specified border.
    ///
    /// 0.0 is minimum X or minimum Y depending on the axis of the [`Side`].
    ///
    /// 1.0 is maximum X or maximum Y.
    Border(Side, Option<f32>),
    /// Point along the border of the element at the specified angle.
    Angle(f32),
}

#[derive(Debug, Copy, Clone)]
pub enum Side {
    Left,
    Right,
    Top,
    Bottom,
}

//======================================
// Impls
//======================================

impl Id {
    pub fn new<T: Into<String>>(s: T) -> Id {
        Id(s.into())
    }
}

impl Text {
    pub fn new<T: Into<String>>(s: T) -> Text {
        Text(s.into())
    }
}
