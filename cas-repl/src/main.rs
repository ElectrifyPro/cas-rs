mod error;

use cas_compute::numerical::{
    fmt::{FormatOptions, FormatOptionsBuilder, NumberFormat, Scientific, Separator},
    value::Value,
};
use cas_parser::parser::Parser;
use cas_vm::{ReplVm, Vm};
use error::Error;
use rustyline::{error::ReadlineError, DefaultEditor};
use std::{fs::File, io::{self, BufReader, IsTerminal, Read}};

/// Executes the given input string in a new VM, returning the results of the execution.
#[inline]
fn execute(input: &str) -> Result<Value, Error> {
    let ast = Parser::new(&input).try_parse_full_many()?;
    let mut vm = Vm::compile_program(ast)?;
    let value = vm.run()?;
    Ok(value)
}

/// Executes the given input string in a new VM, printing the results.
fn execute_and_print(source_name: &str, input: &str, fmt: FormatOptions) {
    output(source_name, input, execute(input), fmt);
}

/// Executes the given input string in the REPL VM, returning the results of the execution.
#[inline]
fn repl_execute(input: &str, vm: &mut ReplVm) -> Result<Value, Error> {
    let ast = Parser::new(&input).try_parse_full_many()?;
    let value = vm.execute(ast)?;
    Ok(value)
}

/// Executes the given input string in the REPL VM, printing the results.
fn repl_execute_and_print(source_name: &str, input: &str, vm: &mut ReplVm, fmt: FormatOptions) {
    output(source_name, input, repl_execute(input, vm), fmt);
}

/// Prints the result of the execution.
#[inline]
fn output(source_name: &str, input: &str, res: Result<Value, Error>, fmt: FormatOptions) {
    match res {
        Ok(Value::Unit) => (), // intentionally print nothing
        Ok(res) => println!("{}", res.fmt(fmt)),
        Err(err) => err.report_to_stderr(source_name, input),
    }
}

fn main() {
    let mut args = std::env::args();
    args.next();

    let fmt = FormatOptionsBuilder::new()
        .number(NumberFormat::Auto)
        .scientific(Scientific::Times)
        .precision(Some(150))
        .separators(Separator::Never)
        .build();

    if let Some(filename) = args.next() {
        // run source file
        let mut file = BufReader::new(File::open(&filename).unwrap());
        let mut input = String::new();
        file.read_to_string(&mut input).unwrap();

        execute_and_print(&filename, &input, fmt);
    } else if !io::stdin().is_terminal() {
        // read source from stdin
        let mut input = String::new();
        io::stdin().read_to_string(&mut input).unwrap();

        execute_and_print("stdin", &input, fmt);
    } else {
        // run the repl / interactive mode
        let mut entry = 0;
        let mut rl = DefaultEditor::new().unwrap();
        let mut vm = ReplVm::new();

        fn process_line(
            entry: usize,
            rl: &mut DefaultEditor,
            vm: &mut ReplVm,
            fmt: FormatOptions,
        ) -> Result<(), ReadlineError> {
            let input = rl.readline("> ")?;
            if input.trim().is_empty() {
                return Ok(());
            }

            rl.add_history_entry(&input)?;

            let source_name = format!("repl:{}", entry);
            repl_execute_and_print(&source_name, &input, vm, fmt);
            Ok(())
        }

        loop {
            entry += 1;

            if let Err(err) = process_line(entry, &mut rl, &mut vm, fmt) {
                match err {
                    ReadlineError::Eof | ReadlineError::Interrupted => (),
                    _ => eprintln!("{}", err),
                }
                break;
            }
        }
    }
}
