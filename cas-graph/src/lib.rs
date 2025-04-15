#![doc = include_str!("../README.md")]

pub mod graph;
mod text_align;

pub use graph::{
    analyzed::AnalyzedExpr,
    CanvasPoint, Graph, GraphOptions, GraphPoint, Point,
};
