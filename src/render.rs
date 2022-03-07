use std::{fs, io::Write, path::Path as StdPath};

use skia::{
    paint,
    textlayout::{FontCollection, ParagraphBuilder, ParagraphStyle, TextStyle},
    Canvas, Color, EncodedImageFormat, FontMgr, Image, Paint, Path, Point, Surface,
};

use crate::{Diagram, Error};

impl Diagram {
    fn draw(&self, canvas: &mut Canvas) {
        let mut paint = Paint::default();
        paint.set_anti_alias(true);
        let mut path = Path::default();
        path.move_to((124, 108))
            .line_to((172, 24))
            .add_circle((50, 50), 30.0, None)
            .move_to((36, 148))
            .quad_to((66, 188), (120, 136));
        canvas.draw_path(&path, &paint);
        paint
            .set_style(paint::Style::Stroke)
            .set_color(Color::BLUE)
            .set_stroke_width(3.0);
        canvas.draw_path(&path, &paint);
    }

    fn render_to_skia_surface(&self) -> skia::Surface {
        // TODO: Determine these dimensions based on the diagram layout.
        let (width, height) = (500, 500);

        let mut surface = Surface::new_raster_n32_premul((width, height))
            .expect("unable to construct Skia Surface for rendering");

        self.draw(surface.canvas());

        surface
    }

    fn render_to_skia_image(&self) -> Image {
        let mut surface: Surface = self.render_to_skia_surface();
        let image = surface.image_snapshot();
        image
    }

    /// Render this diagram to a PNG file.
    pub fn save_to_png(&self, output: &StdPath) -> Result<(), Error> {
        let image: Image = self.render_to_skia_image();

        save_skia_image_to_png(&image, output)
    }
}

fn draw_text(canvas: &mut Canvas, text: &str) {
    let mut font_collection = FontCollection::new();
    font_collection.set_default_font_manager(FontMgr::new(), None);

    let paragraph_style = ParagraphStyle::new();

    let mut paragraph_builder = ParagraphBuilder::new(&paragraph_style, font_collection);

    let ts = {
        let mut ts = TextStyle::new();
        ts.set_foreground_color(Paint::default());
        ts
    };
    paragraph_builder.push_style(&ts);
    paragraph_builder.add_text(text);

    let mut paragraph = paragraph_builder.build();

    paragraph.layout(64.0);

    paragraph.paint(canvas, Point { x: 0.0, y: 0.0 });
}

fn save_skia_image_to_png(image: &Image, output: &StdPath) -> Result<(), Error> {
    // TODO: use encode_to_data_with_quality()?
    let png_data = image.encode_to_data(EncodedImageFormat::PNG).unwrap();

    fs::create_dir_all(output.parent().unwrap())?;

    let mut file = fs::File::create(output)?;
    file.write_all(png_data.as_bytes())?;

    Ok(())
}
