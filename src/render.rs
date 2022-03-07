use std::{fs, io::Write, path::Path as StdPath};

use skia::{
    paint,
    textlayout::{FontCollection, ParagraphBuilder, ParagraphStyle, TextStyle},
    Canvas, Color, EncodedImageFormat, FontMgr, Image, Paint, Path, Point, Surface,
};

use cgmath::{InnerSpace, Rad, Vector2};

use crate::{
    layout::{self, PlacedArrow, PlacedBox, PlacedDiagram},
    Error,
};

impl PlacedDiagram {
    fn draw(&self, canvas: &mut Canvas) {
        let PlacedDiagram { boxes, arrows } = self;

        //-----------
        // Draw boxes
        //-----------

        for (_id, placed_box) in boxes {
            let PlacedBox {
                ref box_,
                text_rect,
                border_rect,
            } = *placed_box;

            // assert!(text_rect.width() <= border_rect.width());
            // assert!(text_rect.height() <= border_rect.height());

            draw_text(canvas, &box_.text.0, text_rect);
            draw_border(canvas, border_rect);
        }

        //------------
        // Draw arrows
        //------------

        for PlacedArrow {
            arrow: _,
            start_point,
            end_point,
        } in arrows
        {
            draw_arrow(canvas, *start_point, *end_point);
        }
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

fn draw_text(canvas: &mut Canvas, text: &str, rect: layout::Rect) {
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

    paragraph.paint(canvas, rect.top_left());
}

fn draw_border(canvas: &mut Canvas, rect: layout::Rect) {
    let mut paint = Paint::default();
    paint.set_anti_alias(true);
    paint
        .set_style(paint::Style::Stroke)
        .set_color(Color::BLUE)
        .set_stroke_width(3.0);

    canvas.draw_round_rect(rect.into_skia(), 5.0, 5.0, &paint);
}

fn draw_arrow(canvas: &mut Canvas, start_point: Point, end_point: Point) {
    use std::f32;

    // Style to use for the arrow.
    let paint = {
        let mut paint = Paint::default();
        paint.set_anti_alias(true);
        paint
            .set_style(paint::Style::Stroke)
            .set_color(Color::GREEN)
            .set_stroke_width(3.0);
        paint
    };

    let mut path = Path::default();

    // Draw the main arrow shaft.
    path.move_to(start_point).line_to(end_point);
    canvas.draw_path(&path, &paint);

    // Draw the arrow 'ticks'
    {
        // Get the normalized direction vector of start_point => end_point.
        let vector =
            Vector2::new(start_point.x - end_point.x, start_point.y - end_point.y)
                .normalize();

        draw_arrow_tick(
            canvas,
            end_point,
            vector,
            Rad(0.25 * f32::consts::PI),
            &paint,
        );
        draw_arrow_tick(
            canvas,
            end_point,
            vector,
            Rad(-0.25 * f32::consts::PI),
            &paint,
        );
    }
}

/// `end_point` is the point where the arrow ends.
///
/// `arrow_vector` is the direction vector of the main arrow shaft.
///
/// `tick_angle` is the angle in radians to offset the arrow tick from `arrow_vector`.
fn draw_arrow_tick(
    canvas: &mut Canvas,
    end_point: skia::Point,
    arrow_vector: Vector2<f32>,
    tick_angle_rad: Rad<f32>,
    paint: &Paint,
) {
    use cgmath::{Basis2, Point2 as CgPoint2, Rotation, Rotation2};

    // Draw the left arrow 'tick'
    let rot: Basis2<f32> = Rotation2::from_angle(tick_angle_rad);

    // Rotate the vector using the two-dimensional rotation matrix:
    let left_tick_dir = rot.rotate_vector(arrow_vector);
    let left_tick_start = CgPoint2 {
        x: end_point.x,
        y: end_point.y,
    };
    let left_tick_end = left_tick_start + 20.0 * left_tick_dir;

    let mut path = Path::default();
    path.move_to((left_tick_start.x, left_tick_start.y))
        .line_to((left_tick_end.x, left_tick_end.y));
    canvas.draw_path(&path, paint);
}

fn save_skia_image_to_png(image: &Image, output: &StdPath) -> Result<(), Error> {
    // TODO: use encode_to_data_with_quality()?
    let png_data = image.encode_to_data(EncodedImageFormat::PNG).unwrap();

    fs::create_dir_all(output.parent().unwrap())?;

    let mut file = fs::File::create(output)?;
    file.write_all(png_data.as_bytes())?;

    Ok(())
}
