# cas-math

This subcrate provides useful mathematical utilities, such as unit conversion.

# Unit conversion

```rust
use cas_math::unit_conversion::{Length, Measurement};

let m = Measurement::new(2.0, Length::Mile);
let m2 = m.convert(Length::Decimeter).unwrap();
assert_eq!(m2.value(), &32186.88);
```
