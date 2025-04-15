mod error;

use cas_compute::numerical::{ctxt::Ctxt, eval::eval_stmts, fmt::{FormatOptionsBuilder, NumberFormat, Scientific, Separator}, value::Value};
use cas_parser::parser::Parser;
use error::Error;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{fs::File, io::{self, BufReader, IsTerminal, Read}};

/// Parses and evaluates the given input string, returning the results of both operations.
fn parse_eval(input: &str, ctxt: &mut Ctxt) -> Result<Value, Error> {
    let ast = Parser::new(input).try_parse_full_many()?;
    let res = eval_stmts(&ast, ctxt)?;
    Ok(res)
}

/// Reads from the provided file or stdin and parses / evaluates the input, printing the success or
/// failure.
fn read_eval(input: &str, ctxt: &mut Ctxt) {
    // let fmt = FormatOptions {
    let fmt = FormatOptionsBuilder::new()
        .number(NumberFormat::Auto)
        .scientific(Scientific::Times)
        .precision(Some(150))
        .separators(Separator::Never)
        .build();

    match parse_eval(input, ctxt) {
        Ok(Value::Unit) => (), // intentionally print nothing
        Ok(res) => println!("{}", res.fmt(fmt)),
        Err(err) => err.report_to_stderr(input),
    }
}

fn main() {
    let mut args = std::env::args();
    args.next();

    let mut ctxt = Ctxt::default();

    if let Some(filename) = args.next() {
        // run source file
        let mut file = BufReader::new(File::open(filename).unwrap());
        let mut input = String::new();
        file.read_to_string(&mut input).unwrap();

        read_eval(&input, &mut ctxt);
    } else if !io::stdin().is_terminal() {
        // read source from stdin
        let mut input = String::new();
        io::stdin().read_to_string(&mut input).unwrap();

        read_eval(&input, &mut ctxt);
    } else {
        // run the repl / interactive mode
        let mut rl = DefaultEditor::new().unwrap();

        fn process_line(rl: &mut DefaultEditor, ctxt: &mut Ctxt) -> Result<(), ReadlineError> {
            let input = rl.readline("> ")?;
            rl.add_history_entry(&input)?;
            read_eval(&input, ctxt);
            Ok(())
        }

        loop {
            if let Err(err) = process_line(&mut rl, &mut ctxt) {
                match err {
                    ReadlineError::Eof | ReadlineError::Interrupted => (),
                    _ => eprintln!("{}", err),
                }
                break;
            }
        }
    }
}
