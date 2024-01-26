//! # Features
//!
//! - `mysql`: Derives [`mysql_common`] traits for various types provided by this crate.

pub mod builtins;
pub mod consts;
pub mod ctxt;
pub mod error;
pub mod eval;
pub mod fmt;
pub mod funcs;
pub mod value;

#[cfg(test)]
mod tests {
    use cas_parser::parser::{ast::stmt::Stmt, Parser};
    use super::{consts::float, eval::eval_stmts, value::Value};

    /// Evaluates the given statements and returns the result.
    fn eval(source: &str) -> Value {
        let ast = Parser::new(source).try_parse_full_many::<Stmt>().unwrap();
        eval_stmts(&ast, &mut Default::default()).unwrap()
    }

    #[test]
    fn bad_lcm() {
        let source = include_str!("../../../examples/bad_lcm.calc");
        assert_eq!(eval(source), 1517.0.into());
    }

    #[test]
    fn factorial() {
        let source = include_str!("../../../examples/factorial.calc");
        assert_eq!(eval(source), true.into());
    }

    #[test]
    fn function_scope() {
        let source = include_str!("../../../examples/function_scope.calc");
        assert_eq!(eval(source), 14.0.into());
    }

    #[test]
    fn if_branching() {
        let source = include_str!("../../../examples/if_branching.calc");
        assert_eq!(eval(source).coerce_float(), float(5).log2().into());
    }

    #[test]
    fn manual_abs() {
        let source = include_str!("../../../examples/manual_abs.calc");
        assert_eq!(eval(source), 4.0.into());
    }
}
