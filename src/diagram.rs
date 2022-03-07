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
    pub subject: Label,
    pub object: Label,

    /// The text displayed alongside this arrow.
    pub text: Text,
}
