use ariadne::Source;
use cas_eval::{ctxt::Ctxt, eval::Eval};
use cas_parser::parser::{expr::Expr, Parser};
use std::io;

fn main() {
    let ctxt = Ctxt::with_defaults();
    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let expr = Parser::new(input.trim()).try_parse_full::<Expr>();
        match expr {
            Ok(ast) => {
                let result = ast.eval_with(&ctxt);
                match result {
                    Some(res) => println!("{}", res),
                    None => eprintln!("Error: Could not evaluate expression"),
                }
            },
            Err(e) => {
                let report = e.build_report();
                report.eprint(("input", Source::from(input))).unwrap();
            },
        }
    }
}
