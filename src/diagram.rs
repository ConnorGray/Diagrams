#[derive(Debug, Clone)]
pub struct Diagram {
    pub boxes: Vec<Box>,
    pub arrows: Vec<Arrow>,
    // pub callouts: Vec<Callout>,
}

/// A unique user-visible label for an element of a diagram.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label(pub String);

/// Text displayed inside or alongside an element of a diagram.
// TODO: Support styled text.
#[derive(Debug, Clone)]
pub struct Text(pub String);

#[derive(Debug, Clone)]
pub struct Box {
    /// Unique identifier for this box.
    ///
    /// This is used to refer to this box from other boxes and arrows.
    pub label: Label,

    /// The text displayed in this box.
    pub text: Text,
}

/// An arrow that relates two elements in the diagram.
#[derive(Debug, Clone)]
pub struct Arrow {
    pub start: Label,
    pub end: Label,

    /// The text displayed alongside this arrow.
    pub text: Text,

    pub start_at: Attachment,
    pub end_at: Attachment,
}

/// Description of where an [`Arrow`] attaches to the element it is starting from or
/// ending at.
#[derive(Debug, Clone)]
pub enum Attachment {
    /// Specified side of the rectangular border.
    Border(Border),
    /// Point along the border of the element at the specified angle.
    Angle(f32),
}

#[derive(Debug, Clone)]
pub enum Border {
    Left,
    Right,
    Top,
    Bottom,
}
