use std::path::Path;

use diagram_maker::{
    diagram::{Arrow, Attachment, Border, Box, Label, Text},
    layout::{layout, LayoutAlgorithm},
    Diagram,
};

fn main() {
    let diagram = Diagram {
        boxes: vec![
            Box {
                label: Label("Box A".into()),
                text: Text("This is Box A".into()),
            },
            Box {
                label: Label("Box B".into()),
                text: Text("This is Box B".into()),
            },
        ],
        arrows: vec![Arrow {
            start: Label("Box A".into()),
            end: Label("Box B".into()),

            text: Text("An arrow".into()),

            start_at: Attachment::Border(Border::Right),
            end_at: Attachment::Border(Border::Left),
        }],
    };

    let placed = layout(&diagram, LayoutAlgorithm::Row);

    placed.save_to_png(Path::new("./diagram.png")).unwrap()
}
