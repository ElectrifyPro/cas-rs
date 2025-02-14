//! Simple command line tool to convert between units of measurement.
//!
//! Type a conversion in the form: `<value> <from> <to>`, e.g. `1.5 m ft`.
//!
//! You can convert between derived units, i.e. volume to length^3, e.g. `500 mL cm^3`.

mod error;

use cas_math::unit_conversion::*;
use error::Error;
use rustyline::{error::ReadlineError, DefaultEditor};

fn convert(input: &str) -> Result<(), Error> {
    let parts: [&str; 3] = input
        .split_whitespace()
        .collect::<Vec<_>>()
        .try_into()?;
    let [value, from, to] = parts;

    let value = value.parse::<f64>()?;
    let from = CompoundUnit::try_from(from)?;
    let to = CompoundUnit::try_from(to)?;

    let m = Measurement::new(value, from.clone());
    let m2 = m.convert(to.clone())?;
    println!("{} {} = {} {}", value, from, m2.value(), to);

    Ok(())
}

fn main() {
    // run the repl / interactive mode
    let mut rl = DefaultEditor::new().unwrap();

    fn process_line(rl: &mut DefaultEditor) -> Result<(), ReadlineError> {
        let input = rl.readline("> ")?;
        rl.add_history_entry(&input)?;
        if let Err(e) = convert(&input) {
            eprintln!("{}", e);
        }
        Ok(())
    }

    loop {
        if let Err(err) = process_line(&mut rl) {
            match err {
                ReadlineError::Eof | ReadlineError::Interrupted => (),
                _ => eprintln!("{}", err),
            }
            break;
        }
    }
}
