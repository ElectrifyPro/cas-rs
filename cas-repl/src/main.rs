use ariadne::Source;
use cas_eval::{ctxt::Ctxt, eval::Eval};
use cas_parser::parser::{expr::Expr, Parser};
use std::io;

fn main() {
    let mut ctxt = Ctxt::default();
    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();
        let expr = Parser::new(&input).try_parse_full::<Expr>();
        match expr {
            Ok(ast) => {
                let ctxt_copy = ctxt.clone();
                let result = ast.eval(&mut ctxt);
                match result {
                    Ok(res) => println!("{}", res),
                    Err(e) => {
                        ctxt = ctxt_copy;
                        let report = e.build_report();
                        report.eprint(("input", Source::from(input))).unwrap();
                    },
                }
            },
            Err(errs) => {
                for err in errs {
                    let report = err.build_report();
                    report.eprint(("input", Source::from(&input))).unwrap();
                }
            },
        }
    }
}
