use skia::Color;

use crate::diagram::Theme;

pub const THEMES: &[Theme] = &[
    // https://coolors.co/palette/264653-2a9d8f-e9c46a-f4a261-e76f51
    Theme {
        box_text_color: Color::from_rgb(233, 196, 106),
        box_border: Color::from_rgb(38, 70, 83),
        box_background: Color::from_rgb(42, 157, 143),
        arrow_stroke: Color::from_rgb(244, 162, 97),
    },
    // https://coolors.co/palette/e63946-f1faee-a8dadc-457b9d-1d3557
    Theme {
        box_text_color: Color::from_rgb(69, 123, 157),
        box_border: Color::from_rgb(230, 57, 70),
        box_background: Color::from_rgb(241, 250, 238),
        arrow_stroke: Color::from_rgb(29, 53, 87),
    },
    // https://coolors.co/palette/072ac8-1e96fc-a2d6f9-fcf300-ffc600
    Theme {
        box_text_color: Color::from_rgb(7, 42, 200),
        box_border: Color::from_rgb(30, 150, 252),
        box_background: Color::from_rgb(162, 214, 249),
        arrow_stroke: Color::from_rgb(255, 198, 0),
        // arrow_stroke: Color::from_rgb(252, 243, 0),
    },
];
