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
    graph.add(cas_parser::parser::Parser::new("{ t = 4; j = 1; t + j }").try_parse_full::<cas_parser::parser::expr::Expr>().unwrap());
    graph.draw(context).unwrap();

    let mut file = File::create("output.png").unwrap();
    surface.write_to_png(&mut file).unwrap();
}
