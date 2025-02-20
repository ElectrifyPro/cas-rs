A module for generic conversion between units.

# Usage

The [`Measurement`] type packs a numeric value and the unit it represents. It
can be converted to other units within the same quantity kind. Here's an example
of converting a length from miles to decimeters:

```rust
use cas_unit_convert::{Base, Length, Measurement, Unit};

let m = Measurement::new(2.0, Unit::new(Base::Length(Length::Mile)));
let m2 = m.convert(Unit::new(Base::Length(Length::Decimeter))).unwrap();
assert_eq!(m2.value(), &32186.88);
```

Note that the arguments to [`Measurement::new`] and [`Measurement::convert`]
accept any type that implements [`Into<Unit>`], which is implemented for all
provided units and quantities. This allows you to write the above example more
concisely:

```rust
use cas_unit_convert::{Length, Measurement};

let m = Measurement::new(2.0, Length::Mile);
let m2 = m.convert(Length::Decimeter).unwrap();
assert_eq!(m2.value(), &32186.88);
```

## Derived units

There is rudimentary support for derived units, which are units that are defined
in terms of other units. For example, area is defined in terms of length
squared, and volume is defined in terms of length cubed, and you can convert
between them. The `.squared()`, `.cubed()`, and `.pow()` methods on [`Unit`]s
and [`Base`]s allow you to create derived units easily:

```rust
use cas_unit_convert::{Area, Length, Measurement};

let m = Measurement::new(57600.0, Length::Foot.squared());
let m2 = m.convert(Area::Acre).unwrap();
assert_eq!(m2.value(), &1.322314049586777);
```

Additionally, compound units are supported, such as `m/s` (meters per second)
and `kg*m/s^2` (kilogram meters per second squared, aka Newtons):

```rust
use cas_unit_convert::{CompoundUnit, Measurement, Unit};

// easiest way to create these is with string parsing
let m = Measurement::new(30.0, "mi/hr".parse::<CompoundUnit>().unwrap());
let m2 = m.convert("km/s".parse::<CompoundUnit>().unwrap()).unwrap();
assert_eq!(m2.value(), &0.0134112);

// it can also be done manually with `CompoundUnit`
```

## Note on derived units

It is possible to declare a compound unit with two units of the same type, such
as "mi*mi" or even "mi*km". This behavior is currently unsupported, and while it
won't cause undefined behavior (no `unsafe` code), it can lead to strange
results.

# Rounding errors

Floating point arithmetic is inherently imprecise; there are infinitely many
real numbers, but only so many floating point numbers to represent them. As a
result, small, but noticeable rounding errors can occur when converting between
units.

In general, you should not use `==` to compare floating point numbers. Instead,
use a function that checks for approximate equality (through subtraction and
comparison with a small epsilon). Crates that provide this functionality include
[`assert_float_eq`] (with assertions) and [`approx`] (for general use).

This example below will fail, even though the conversion is mathematically
correct:

```rust,should_panic
use cas_unit_convert::{Length, Measurement, Volume};

let m = Measurement::new(1.0, Length::Centimeter.cubed());
let m2 = m.convert(Volume::Milliliter).unwrap();
assert_eq!(m2.value(), &1.0); // panics! (the result is 1.0000000000000002)
```

Instead, here's a better way to write the test, using [`assert_float_eq`]:

```rust
#[macro_use]
extern crate assert_float_eq;

use cas_unit_convert::{Length, Measurement, Volume};

# fn main() {
let m = Measurement::new(1.0, Length::Centimeter.cubed());
let m2 = m.convert(Volume::Milliliter).unwrap();
assert_float_relative_eq!(*m2.value(), 1.0);
# }
```

If you need to compare floating point numbers outside of a test, you can use
[`approx`]:

```rust
use approx::abs_diff_eq;
use cas_unit_convert::{Length, Measurement, Volume};

let m = Measurement::new(1.0, Length::Centimeter.cubed());
let m2 = m.convert(Volume::Milliliter).unwrap();
if !abs_diff_eq!(*m2.value(), 1.0) {
    // this will not panic. we're safe!
    panic!("1 centimeter cubed is not equal to 1 milliliter");
}
```

In practice, the precision errors are usually small enough that this is not a
problem, but it's something you should be aware of.

# Demo

This repository includes a simple CLI tool to convert units using this library.
To run it, run the following command from the root of the repository:

```sh
cargo run --example convert-unit-repl
```

This will start a REPL where you can enter conversion commands. Commands are
given in the form `<value> <from unit> <to unit>`. For example:

```sh
> 1.5 yd ft
1.5 yd = 4.5 ft
> 500 mL cm^3
500 mL = 500 cm^3
> 5.75 g/mL lb/gal
5.75 g/mL = 47.986... lb/gal
```
