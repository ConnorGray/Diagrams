use std::path::Path;

use diagram_maker::{
    diagram::{Arrow, Attachment, Box, Id, Side, Text},
    layout::{layout, LayoutAlgorithm},
    Diagram,
};

fn main() {
    let lib_a = Id::new("Lib A");
    let lib_b = Id::new("Lib B");

    let diagram = Diagram {
        boxes: vec![
            Box {
                id: lib_a.clone(),
                text: Text::new("foo.dylib"),
            },
            Box {
                id: lib_b.clone(),
                text: Text::new("foo.dylib"),
            },
        ],
        arrows: vec![
            Arrow {
                start: lib_a.clone(),
                end: lib_b.clone(),
                text: Text::new(""),
                start_at: Attachment::Border(Side::Right, Some(0.8)),
                end_at: Attachment::Border(Side::Left, Some(0.8)),
            },
            Arrow {
                start: lib_a.clone(),
                end: lib_b.clone(),
                text: Text::new(""),
                start_at: Attachment::Border(Side::Right, Some(0.2)),
                end_at: Attachment::Border(Side::Left, Some(0.2)),
            },
        ],
    };

    let placed = layout(&diagram, LayoutAlgorithm::Row);

    placed.save_to_png(Path::new("./diagram.png")).unwrap()
}
