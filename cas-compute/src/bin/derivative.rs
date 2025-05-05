use cas_compute::symbolic::derivative::derivative;
use cas_compute::numerical::{ctxt::Ctxt, eval::Eval};
use cas_parser::parser::{ast::Expr, Parser};

fn main() {

    let mut parser = Parser::new("x^2 + 5x + 6");
    let ast_expr = parser.try_parse_full::<Expr>().unwrap();

    let deriv = derivative(ast_expr.clone().into(), "x");
    println!("{deriv}");
    println!("{deriv:?}");

    let mut ctxt = Ctxt::default();
    ctxt.add_var("x", 2.into());

    let result = ast_expr.eval(&mut ctxt).unwrap();
}
