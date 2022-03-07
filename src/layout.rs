use std::collections::HashMap;

use crate::diagram::{Box, Diagram, Label};

use skia::Point;

#[derive(Debug, Copy, Clone)]
pub struct Rect {
    pub left: f32,
    pub right: f32,
    pub top: f32,
    pub bottom: f32,
}

pub struct FinishedLayout {
    pub boxes: HashMap<Label, (Box, Rect)>,
    // pub arrows:
}

pub enum LayoutAlgorithm {
    Row,
    #[allow(dead_code)]
    Column,
    #[allow(dead_code)]
    Multicolumn {
        columns: usize,
    },
}

pub fn layout(diagram: &Diagram, algo: LayoutAlgorithm) -> FinishedLayout {
    match algo {
        LayoutAlgorithm::Row => layout_row(diagram),
        LayoutAlgorithm::Column | LayoutAlgorithm::Multicolumn { .. } => todo!(),
    }
}

//==========================================================
// Layout Algorithms
//==========================================================

//======================================
// LayoutAlgorithm::Row
//======================================

fn layout_row(diagram: &Diagram) -> FinishedLayout {
    let Diagram { boxes, arrows } = diagram;

    // TODO: Don't use a fixed width/height for boxes.
    const BOX_WIDTH: f32 = 64.0;
    const BOX_HEIGHT: f32 = 64.0;
    const MARGIN: f32 = 16.0;

    let mut horiz_offset = 0.0;
    let mut boxes_layout = HashMap::new();

    for box_ @ Box { label, text: _ } in boxes {
        boxes_layout.insert(
            label.clone(),
            (
                box_.clone(),
                Rect {
                    left: horiz_offset,
                    right: horiz_offset + BOX_WIDTH,
                    top: 0.0,
                    bottom: BOX_HEIGHT,
                },
            ),
        );

        horiz_offset += BOX_WIDTH + MARGIN;
    }

    FinishedLayout {
        boxes: boxes_layout,
    }
}

//==========================================================
// Utilities
//==========================================================

impl Rect {
    pub fn width(&self) -> f32 {
        assert!(self.left <= self.right);
        self.right - self.left
    }

    pub fn height(&self) -> f32 {
        assert!(self.bottom <= self.top);
        self.top - self.bottom
    }

    pub fn top_left(&self) -> Point {
        Point {
            x: self.left,
            y: self.top,
        }
    }
}
