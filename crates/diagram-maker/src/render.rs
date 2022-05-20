use std::{fs, io::Write, path::Path as StdPath};

use skia::{
    paint,
    textlayout::{
        FontCollection, LineMetrics, Paragraph, ParagraphBuilder,
        ParagraphStyle, TextStyle,
    },
    Canvas, Color, EncodedImageFormat, FontMgr, Image, Paint, Path, Point,
    Surface,
};

use cgmath::{InnerSpace, Rad, Vector2};

use crate::{
    layout::{self, PlacedArrow, PlacedBox, PlacedDiagram},
    Error,
};

// TODO: Add box corner radius as a themeable field.
#[derive(Debug, Copy, Clone)]
struct Theme {
    box_text_color: Color,
    box_border: Color,
    box_background: Color,
    arrow_stroke: Color,
}

const THEMES: &[Theme] = &[
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

impl PlacedDiagram {
    fn draw(&self, canvas: &mut Canvas) {
        let theme = THEMES[2];

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

            draw_box(
                canvas,
                border_rect,
                theme.box_background,
                theme.box_border,
            );
            draw_text(canvas, &box_.text.0, text_rect, theme.box_text_color);
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
            draw_arrow(canvas, *start_point, *end_point, theme.arrow_stroke);
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

    /// Render this diagram to a PNG, and return the resulting encoded PNG image
    /// data.
    pub fn render_to_png_bytes(&self) -> Vec<u8> {
        let image: Image = self.render_to_skia_image();

        // TODO: use encode_to_data_with_quality()?
        let png_data = image
            .encode_to_data(EncodedImageFormat::PNG)
            .expect("failed to encode diagram Skia image as PNG bytes");

        png_data.as_bytes().to_vec()
    }
}

fn draw_text(
    canvas: &mut Canvas,
    text: &str,
    rect: layout::Rect,
    color: Color,
) {
    let mut paragraph = make_paragraph(text, color);

    paragraph.layout(rect.width());

    // println!(
    //     "Width: {} ({}), Height: {}",
    //     paragraph.max_width(),
    //     paragraph.max_intrinsic_width(),
    //     paragraph.height()
    // );

    paragraph.paint(canvas, rect.top_left());
}

fn make_paragraph(text: &str, color: Color) -> Paragraph {
    let mut font_collection = FontCollection::new();
    font_collection.set_default_font_manager(FontMgr::new(), None);

    let paragraph_style = ParagraphStyle::new();

    let mut paragraph_builder =
        ParagraphBuilder::new(&paragraph_style, font_collection);

    let ts = {
        let mut ts = TextStyle::new();
        let mut paint = Paint::default();
        paint.set_color(color);
        ts.set_foreground_color(paint);
        ts.set_font_size(20.0);
        ts
    };
    paragraph_builder.push_style(&ts);
    paragraph_builder.add_text(text);

    paragraph_builder.build()
}

pub fn rendered_text_size(text: &str, width: f32) -> (f32, f32) {
    let mut paragraph = make_paragraph(text, Color::BLACK);

    paragraph.layout(width);

    let metrics: Vec<LineMetrics> = paragraph.get_line_metrics();

    assert!(metrics.len() > 0);

    let width = metrics
        .iter()
        .map(|line| line.width)
        .fold(0.0f32, |a, b| a.max(b as f32));

    (width, paragraph.height())
}

fn draw_box(
    canvas: &mut Canvas,
    rect: layout::Rect,
    fill: Color,
    border: Color,
) {
    let fill = {
        let mut paint = Paint::default();
        paint.set_anti_alias(true);
        paint.set_style(paint::Style::Fill).set_color(fill);
        paint
    };

    let border = {
        let mut paint = Paint::default();
        paint.set_anti_alias(true);
        paint
            .set_style(paint::Style::Stroke)
            .set_color(border)
            .set_stroke_width(3.0);
        paint
    };

    canvas.draw_round_rect(rect.into_skia(), 5.0, 5.0, &fill);
    canvas.draw_round_rect(rect.into_skia(), 5.0, 5.0, &border);
}

fn draw_arrow(
    canvas: &mut Canvas,
    start_point: Point,
    end_point: Point,
    color: Color,
) {
    use std::f32;

    // Style to use for the arrow.
    let paint = {
        let mut paint = Paint::default();
        paint.set_anti_alias(true);
        paint
            .set_style(paint::Style::Stroke)
            .set_color(color)
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
        let vector = Vector2::new(
            start_point.x - end_point.x,
            start_point.y - end_point.y,
        )
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

fn save_skia_image_to_png(
    image: &Image,
    output: &StdPath,
) -> Result<(), Error> {
    // TODO: use encode_to_data_with_quality()?
    let png_data = image.encode_to_data(EncodedImageFormat::PNG).unwrap();

    fs::create_dir_all(output.parent().unwrap())?;

    let mut file = fs::File::create(output)?;
    file.write_all(png_data.as_bytes())?;

    Ok(())
}
