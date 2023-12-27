mod error;

use cas_compute::numerical::{ctxt::Ctxt, eval::eval_stmts, value::Value};
use cas_parser::parser::{ast::stmt::Stmt, Parser};
use error::Error;
use std::{fs::File, io::{self, BufReader, IsTerminal, Read, Write}};

/// Parses and evaluates the given input string, returning the results of both operations.
fn parse_eval(input: &str, ctxt: &mut Ctxt) -> Result<Value, Error> {
    let ast = Parser::new(input).try_parse_full_many::<Stmt>()?;
    let res = eval_stmts(&ast, ctxt)?;
    Ok(res)
}

/// Reads from the provided file or stdin and parses / evaluates the input, printing the success or
/// failure.
fn read_eval(input: &str, ctxt: &mut Ctxt) {
    match parse_eval(input, ctxt) {
        Ok(res) => println!("{}", res),
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
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            let mut input = String::new();
            io::stdin().read_line(&mut input).unwrap();

            read_eval(&input, &mut ctxt);
        }
    }
}
