use std::path::Path;

use diagram_maker::{
    diagram::{Arrow, Attachment, Border, Box, Id, Text},
    layout::{layout, LayoutAlgorithm},
    Diagram,
};

fn main() {
    let diagram = Diagram {
        boxes: vec![
            Box {
                id: Id::new("Box A"),
                text: Text("This is Box A".into()),
            },
            Box {
                id: Id::new("Box B"),
                text: Text("This is Box B".into()),
            },
        ],
        arrows: vec![Arrow {
            start: Id::new("Box A"),
            end: Id::new("Box B"),

            text: Text("An arrow".into()),

            start_at: Attachment::Border(Border::Right, None),
            end_at: Attachment::Border(Border::Left, None),
        }],
    };

    let placed = layout(&diagram, LayoutAlgorithm::Row);

    placed.save_to_png(Path::new("./diagram.png")).unwrap()
}
