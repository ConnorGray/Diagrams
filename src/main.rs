use std::path::Path;

use diagram_maker::Diagram;

fn main() {
    let diagram = Diagram {
        boxes: vec![],
        arrows: vec![],
    };

    diagram.save_to_png(Path::new("./diagram.png")).unwrap()
}
