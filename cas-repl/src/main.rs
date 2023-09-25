use ariadne::Source;
use cas_eval::{ctxt::Ctxt, eval::eval_stmts};
use cas_parser::parser::{ast::stmt::Stmt, Parser};
use std::io::{self, IsTerminal, Read, Write};

fn main() {
    let mut ctxt = Ctxt::default();
    loop {
        let mut input = String::new();
        let expr = if io::stdin().is_terminal() {
            print!("> ");
            io::stdout().flush().unwrap();
            io::stdin().read_line(&mut input).unwrap();

            if input.trim().is_empty() {
                continue;
            }

            Parser::new(&input).try_parse_full::<Stmt>()
                .map(|stmt| vec![stmt])
        } else {
            io::stdin().read_to_string(&mut input).unwrap();
            Parser::new(&input).try_parse_full_many::<Stmt>()
        };

        match expr {
            Ok(ast) => {
                match eval_stmts(&ast, &mut ctxt) {
                    Ok(res) => println!("{}", res),
                    Err(e) => {
                        let report = e.build_report();
                        report.eprint(("input", Source::from(&input))).unwrap();
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

        if !io::stdin().is_terminal() {
            break;
        }
    }
}
