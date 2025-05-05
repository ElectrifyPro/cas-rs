use cas_compute::symbolic::derivative::derivative;
use cas_compute::numerical::{ctxt::Ctxt, eval::Eval};
use cas_compute::symbolic::simplify;
use cas_parser::parser::fmt::Latex;
use cas_parser::parser::{ast::Expr, Parser};

fn main() {

    let hard = "sqrt((x-a)^2 + (y-b)^2 + (z-c)^2) - r";
    let easy = "x^2 + 5x + 6";
    let mut parser = Parser::new(hard);
    let ast_expr = parser.try_parse_full::<Expr>().unwrap();

    let deriv = derivative(ast_expr.clone().into(), "x");
    println!("{deriv}");
    println!("{deriv:?}");

    let simplified = simplify(&deriv);

    let formattable = cas_parser::parser::ast::expr::Expr::from(simplified.clone());

    println!("{}", formattable.as_display());

    println!("{}", simplified);

    let mut ctxt = Ctxt::default();
    ctxt.add_var("x", 2.into());

    //let result = ast_expr.eval(&mut ctxt).unwrap();
}
