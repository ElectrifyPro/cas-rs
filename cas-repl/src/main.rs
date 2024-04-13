mod error;

use cas_compute::numerical::{ctxt::Ctxt, eval::eval_stmts, fmt::{FormatOptionsBuilder, NumberFormat, Scientific, Separator}, value::Value};
use cas_parser::parser::Parser;
use cas_vm::{ReplVm, Vm};
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

/// Compiles the given input string in a new VM, returning the VM.
fn compile(input: &str) -> Result<Vm, Error> {
    let ast = Parser::new(input).try_parse_full_many()?;
    Ok(Vm::compile_program(ast).unwrap())
}

/// Executes the given input string in a new VM, returning the results of the execution.
fn execute(input: String) {
    let Ok(mut vm) = compile(&input) else {
        return;
    };
    let res = vm.run();
    match res {
        Ok(Value::Unit) => (), // intentionally print nothing
        Ok(res) => println!("{}", res),
        Err(err) => err.build_report().eprint(("input", ariadne::Source::from(input))).unwrap(),
    }
}

/// Executes the given input string in the REPL VM.
fn repl_execute(input: &str, vm: &mut ReplVm) {
    let ast = Parser::new(&input).try_parse_full_many().unwrap();
    match vm.execute(ast) {
        Ok(Value::Unit) => (),
        Ok(res) => println!("{}", res),
        Err(err) => Error::from(err).report_to_stderr(input),
    }
}

fn main() {
    let mut args = std::env::args();
    args.next();

    if let Some(filename) = args.next() {
        // run source file
        let mut file = BufReader::new(File::open(filename).unwrap());
        let mut input = String::new();
        file.read_to_string(&mut input).unwrap();

        execute(input);
    } else if !io::stdin().is_terminal() {
        // read source from stdin
        let mut input = String::new();
        io::stdin().read_to_string(&mut input).unwrap();

        execute(input);
    } else {
        // run the repl / interactive mode
        let mut rl = DefaultEditor::new().unwrap();
        let mut vm = ReplVm::new();

        fn process_line(rl: &mut DefaultEditor, vm: &mut ReplVm) -> Result<(), ReadlineError> {
            let input = rl.readline("> ")?;
            if input.trim().is_empty() {
                return Ok(());
            }

            rl.add_history_entry(&input)?;

            repl_execute(&input, vm);
            Ok(())
        }

        loop {
            if let Err(err) = process_line(&mut rl, &mut vm) {
                match err {
                    ReadlineError::Eof | ReadlineError::Interrupted => (),
                    _ => eprintln!("{}", err),
                }
                break;
            }
        }
    }
}
