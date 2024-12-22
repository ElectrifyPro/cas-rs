# cas-math

This subcrate provides useful mathematical utilities, such as unit conversion.

# Unit conversion

```rust
use cas_math::unit_conversion::{Length, Measurement};

let m = Measurement::new(2.0, Length::Mile);
let m2 = m.convert(Length::Decimeter).unwrap();
assert_eq!(m2.value(), &32186.88);
```

## Demo

This repository includes a simple CLI tool to convert units using this library.
To run it, run the following command from the root of the repository:

```sh
cargo run --package convert-unit-repl
```

This will start a REPL where you can enter conversion commands. For example:

```sh
> 1.5 yd ft
1.5 yd = 4.5 ft
```
