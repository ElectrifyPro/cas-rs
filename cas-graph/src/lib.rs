pub mod graph;
mod text_align;

pub use graph::Graph;

use cairo::{Context, Format, ImageSurface};
use std::fs::File;

pub fn draw() {
    // draw a test graph
    let surface = ImageSurface::create(Format::ARgb32, 1000, 1000).unwrap();
    let context = Context::new(&surface).unwrap();
    let mut graph = Graph::new();
    graph.add(cas_parser::parser::Parser::new("erf(x)").try_parse_full::<cas_parser::parser::expr::Expr>().unwrap());
    graph.points.push((0.0, 8.1).into());
    graph.points.push((1.2, 6.2).into());
    graph.points.push((2.3, 4.3).into());
    graph.points.push((3.4, 2.4).into());
    graph.points.push((4.5, 0.5).into());
    graph.center_on_points();
    graph.draw(&context).unwrap();

    let mut file = File::create("output.png").unwrap();
    surface.write_to_png(&mut file).unwrap();
}
