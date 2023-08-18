use ariadne::Source;
use cas_eval::eval::Eval;
use cas_parser::parser::{expr::Expr, Parser};
use std::io;

fn main() {
    let mut ctxt = Default::default();
    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let expr = Parser::new(input.trim()).try_parse_full::<Expr>();
        match expr {
            Ok(ast) => {
                let result = ast.eval(&mut ctxt);
                match result {
                    Ok(res) => println!("{}", res),
                    Err(e) => {
                        let report = e.build_report();
                        report.eprint(("input", Source::from(input))).unwrap();
                    },
                }
            },
            Err(e) => {
                let report = e.build_report();
                report.eprint(("input", Source::from(input))).unwrap();
            },
        }
    }
}
