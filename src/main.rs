use std::path::Path;

use diagram_maker::{
    diagram::{Box, Label, Text},
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
        arrows: vec![],
    };

    diagram.save_to_png(Path::new("./diagram.png")).unwrap()
}
