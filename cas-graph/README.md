# cas-graph

This subcrate provides a customizable graphing calculator for `cas-rs`. See the [Rust module-level documentation](src/graph/mod.rs) for more information.

# Example

Graph the expressions `x = erf(y)` and `y = erf(x)` with the viewport centered on the points `(0, 8.1)`, `(1.2, 6.2)`, `(2.3, 4.3)`, `(3.4, 2.4)`, and `(4.5, 0.5)`, then write the result to `output.png`:

```rust
use cas_graph::Graph;
use std::fs::File;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let surface = Graph::default()
        .try_add_expr("erf(y)").unwrap()
        .try_add_expr("erf(x)").unwrap()
        .add_point((0.0, 8.1).into())
        .add_point((1.2, 6.2).into())
        .add_point((2.3, 4.3).into())
        .add_point((3.4, 2.4).into())
        .add_point((4.5, 0.5).into())
        .center_on_points()
        .draw()?;

    let mut file = File::create("output.png")?;
    surface.write_to_png(&mut file)?;

    Ok(())
}
```
