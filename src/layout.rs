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

/// A [`Diagram`] whose elements have been placed in their final position by a layout
/// algorithm.
pub struct PlacedDiagram {
    pub boxes: HashMap<Label, PlacedBox>,
}

/// A [`Box`] that has been placed in its final position by a layout algorithm.
pub struct PlacedBox {
    pub box_: Box,

    pub text_rect: Rect,
    pub border_rect: Rect,
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

pub fn layout(diagram: &Diagram, algo: LayoutAlgorithm) -> PlacedDiagram {
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

fn layout_row(diagram: &Diagram) -> PlacedDiagram {
    let Diagram { boxes, arrows } = diagram;

    // TODO: Don't use a fixed width/height for boxes.
    const TEXT_WIDTH: f32 = 64.0;
    const TEXT_HEIGHT: f32 = 64.0;

    const MARGIN: f32 = 16.0;
    const PADDING: f32 = 8.0;

    let mut x_offset = 0.0;
    let mut placed_boxes = HashMap::new();

    for box_ @ Box { label, text: _ } in boxes {
        let border_left = x_offset;
        let text_left = x_offset + PADDING;

        let text_right = text_left + TEXT_WIDTH;
        let border_right = text_right + PADDING;

        let placed_box = PlacedBox {
            box_: box_.clone(),
            text_rect: Rect {
                left: text_left,
                right: text_right,
                top: PADDING,
                bottom: PADDING + TEXT_HEIGHT,
            },
            border_rect: Rect {
                left: border_left,
                right: border_right,
                top: 0.0,
                bottom: PADDING + TEXT_HEIGHT + PADDING,
            },
        };

        // assert_eq!(
        //     placed_box.border_rect.width(),
        //     2.0 * PADDING + TEXT_WIDTH + MARGIN
        // );

        x_offset += placed_box.border_rect.width() + MARGIN;

        placed_boxes.insert(label.clone(), placed_box);
    }

    PlacedDiagram {
        boxes: placed_boxes,
    }
}

//==========================================================
// Utilities
//==========================================================

impl Rect {
    #[rustfmt::skip]
    pub(crate) fn into_skia(self) -> skia::Rect {
        let Rect { left, right, top, bottom } = self;

        skia::Rect { left, right,top, bottom, }
    }

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
