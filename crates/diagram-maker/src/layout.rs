use std::collections::HashMap;

use crate::diagram::{Arrow, Attachment, Box, Diagram, Id, Side, Text, Theme};

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
    pub theme: Theme,
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
    let Diagram {
        boxes,
        arrows,
        theme: _,
    } = diagram;

    // TODO: Don't use a fixed width/height for boxes.
    const TEXT_WIDTH: f32 = 256.0;

    const MARGIN: f32 = 32.0;
    const PADDING: f32 = 8.0;

    let mut x_offset = 0.0;
    let mut placed_boxes = HashMap::new();

    //------------
    // Place boxes
    //------------

    for box_ @ Box {
        id,
        text: Text(text),
    } in boxes
    {
        let border_left = x_offset;
        let text_left = x_offset + PADDING;

        let (text_width, text_height) =
            crate::render::rendered_text_size(text, TEXT_WIDTH);

        // Note: +0.7 fudge factor to prevent text wrapping done by Skia,
        //       even though we're using the width it told us.
        let text_width = text_width + 1.0;

        let text_height = text_height;

        let text_right = text_left + text_width;
        let border_right = text_right + PADDING;

        let placed_box = PlacedBox {
            box_: box_.clone(),
            text_rect: Rect {
                left: text_left,
                right: text_right,
                top: PADDING,
                bottom: PADDING + text_height,
            },
            border_rect: Rect {
                left: border_left,
                right: border_right,
                top: 0.0,
                bottom: PADDING + text_height + PADDING,
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

    let get_box = |id: &Id| -> &PlacedBox {
        match placed_boxes.get(id) {
            Some(box_) => box_,
            None => todo!(),
        }
    };

    for arrow @ Arrow {
        start,
        end,
        // FIXME: Include this text in the rendered arrow.
        text: _,
        start_at,
        end_at,
    } in arrows
    {
        let start_box = get_box(start);
        let end_box = get_box(end);

        // The sides to use automatically if no explicit Attachment has been
        // specified by the user.
        let (auto_start_side, auto_end_side) =
            closest_sides(start_box.border_rect, end_box.border_rect);

        let start_at = match start_at {
            Some(attachment) => attachment.clone(),
            None => Attachment::Border(auto_start_side, None),
        };

        let end_at = match end_at {
            Some(attachment) => attachment.clone(),
            None => Attachment::Border(auto_end_side, None),
        };

        let start_point = start_box.attachment_point(&start_at);
        let end_point = end_box.attachment_point(&end_at);

        placed_arrows.push(PlacedArrow {
            arrow: arrow.clone(),
            start_point,
            end_point,
        });
    }

    PlacedDiagram {
        boxes: placed_boxes,
        arrows: placed_arrows,
        theme: diagram.theme,
    }
}

fn closest_sides(a: Rect, b: Rect) -> (Side, Side) {
    let sides = [Side::Left, Side::Right, Side::Top, Side::Bottom];

    let mut best: (f32, (Side, Side)) = (f32::MAX, (sides[0], sides[0]));

    for a_side in sides {
        for b_side in sides {
            let a_points = a.side_points(a_side);
            let b_points = b.side_points(b_side);

            let distance = sides_distance_factor(a_points, b_points);

            if distance < best.0 {
                best = (distance, (a_side, b_side));
            }
        }
    }

    best.1
}

fn sides_distance_factor(a: (Point, Point), b: (Point, Point)) -> f32 {
    distance(a.0, b.0)
        + distance(a.0, b.1)
        + distance(a.1, b.0)
        + distance(a.1, b.1)
}

fn distance(a: Point, b: Point) -> f32 {
    (a - b).length()
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

                let x = match side {
                    Side::Left => border_rect.left,
                    Side::Right => border_rect.right,
                    // Center X
                    Side::Top | Side::Bottom => {
                        border_rect.left + lerp_factor * border_rect.width()
                    },
                };

                let y = match side {
                    // Center Y
                    // TODO: This assumes +Y points downward.
                    Side::Left | Side::Right => {
                        border_rect.top + lerp_factor * border_rect.height()
                    },
                    Side::Top => border_rect.top,
                    Side::Bottom => border_rect.bottom,
                };

                Point { x, y }
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

    pub fn top_right(&self) -> Point {
        Point {
            x: self.right,
            y: self.top,
        }
    }

    pub fn bottom_left(&self) -> Point {
        Point {
            x: self.left,
            y: self.bottom,
        }
    }

    pub fn bottom_right(&self) -> Point {
        Point {
            x: self.right,
            y: self.bottom,
        }
    }

    pub fn side_points(&self, side: Side) -> (Point, Point) {
        match side {
            Side::Left => (self.top_left(), self.bottom_left()),
            Side::Right => (self.top_right(), self.bottom_right()),
            Side::Top => (self.top_left(), self.top_right()),
            Side::Bottom => (self.bottom_left(), self.bottom_right()),
        }
    }

    pub fn intersects(&self, other: Rect) -> bool {
        let x_overlap: bool = {
            self.left >= other.left && self.left <= other.right
                || self.left <= other.right && self.left >= other.left
        };
        let y_overlap: bool = {
            self.top >= other.bottom && self.top <= other.top
                || self.bottom >= other.bottom && self.bottom <= other.top
        };

        x_overlap && y_overlap
    }
}
