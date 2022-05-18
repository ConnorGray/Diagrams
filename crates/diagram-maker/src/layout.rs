use std::collections::HashMap;

use crate::diagram::{Arrow, Attachment, Box, Diagram, Id, Side};

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
#[derive(Debug, Clone)]
pub struct PlacedDiagram {
    pub boxes: HashMap<Id, PlacedBox>,
    pub arrows: Vec<PlacedArrow>,
}

/// A [`Box`] that has been placed in its final position by a layout algorithm.
#[derive(Debug, Clone)]
pub struct PlacedBox {
    pub box_: Box,

    pub text_rect: Rect,
    pub border_rect: Rect,
}

#[derive(Debug, Clone)]
pub struct PlacedArrow {
    pub arrow: Arrow,

    pub start_point: Point,
    pub end_point: Point,
}

pub enum LayoutAlgorithm {
    Row,
    Column,
    Multicolumn { columns: usize },
}

pub fn layout(diagram: &Diagram, algo: LayoutAlgorithm) -> PlacedDiagram {
    match algo {
        LayoutAlgorithm::Row => layout_row(diagram),
        LayoutAlgorithm::Column | LayoutAlgorithm::Multicolumn { .. } => {
            todo!()
        },
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

    const MARGIN: f32 = 32.0;
    const PADDING: f32 = 8.0;

    let mut x_offset = 0.0;
    let mut placed_boxes = HashMap::new();

    //------------
    // Place boxes
    //------------

    for box_ @ Box { id, text: _ } in boxes {
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

        if let Some(_old) = placed_boxes.insert(id.clone(), placed_box) {
            todo!()
        }
    }

    //-------------
    // Place arrows
    //-------------

    let mut placed_arrows = Vec::new();

    for arrow @ Arrow {
        start,
        end,
        // FIXME: Include this text in the rendered arrow.
        text: _,
        start_at,
        end_at,
    } in arrows
    {
        let start_box = match placed_boxes.get(start) {
            Some(box_) => box_,
            None => todo!(),
        };

        let end_box = match placed_boxes.get(end) {
            Some(box_) => box_,
            None => todo!(),
        };

        let start_point = start_box.attachment_point(start_at);
        let end_point = end_box.attachment_point(end_at);

        placed_arrows.push(PlacedArrow {
            arrow: arrow.clone(),
            start_point,
            end_point,
        });
    }

    PlacedDiagram {
        boxes: placed_boxes,
        arrows: placed_arrows,
    }
}

//==========================================================
// impl Placed*
//==========================================================

impl PlacedBox {
    /// Get the location of the specified [`Attachment`] point for this box.
    pub fn attachment_point(&self, attachment: &Attachment) -> Point {
        let PlacedBox {
            box_: _,
            text_rect: _,
            border_rect,
        } = *self;

        let point = match attachment {
            Attachment::Border(side, lerp_factor) => {
                // Factor used for linear interpolation.
                let lerp_factor = lerp_factor.unwrap_or(0.5);
                let lerp_factor = f32::clamp(lerp_factor, 0.0, 1.0);

                match side {
                    Side::Left => {
                        let x = border_rect.left;
                        // Center Y
                        // TODO: This assumes +Y points downward.
                        let y = border_rect.top
                            + lerp_factor * border_rect.height();
                        Point { x, y }
                    },
                    Side::Right => {
                        let x = border_rect.right;
                        // Center Y
                        // TODO: This assumes +Y points downward.
                        let y = border_rect.top
                            + lerp_factor * border_rect.height();
                        Point { x, y }
                    },
                    Side::Top => {
                        // Center X
                        let x = border_rect.left
                            + lerp_factor * border_rect.width();
                        let y = border_rect.top;
                        Point { x, y }
                    },
                    Side::Bottom => {
                        // Center X
                        let x = border_rect.left
                            + lerp_factor * border_rect.width();
                        let y = border_rect.bottom;
                        Point { x, y }
                    },
                }
            },
            Attachment::Angle(_angle) => todo!(),
        };

        point
    }
}

//==========================================================
// Utilities
//==========================================================

impl Rect {
    #[rustfmt::skip]
    pub(crate) fn into_skia(self) -> skia::Rect {
        let Rect { left, right, top, bottom } = self;

        skia::Rect { left, right, top, bottom }
    }

    pub fn width(&self) -> f32 {
        assert!(self.left <= self.right);
        self.right - self.left
    }

    pub fn height(&self) -> f32 {
        // TODO: This assumes that +Y points downward.
        assert!(self.top <= self.bottom);
        self.bottom - self.top
    }

    pub fn top_left(&self) -> Point {
        Point {
            x: self.left,
            y: self.top,
        }
    }
}
