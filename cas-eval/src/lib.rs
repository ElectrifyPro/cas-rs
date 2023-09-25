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
    use super::{consts::float, eval::eval_stmts};

    #[test]
    fn bad_lcm() {
        let source = include_str!("../../examples/bad_lcm.calc");
        let ast = Parser::new(source).try_parse_full_many::<Stmt>().unwrap();
        assert_eq!(eval_stmts(&ast, &mut Default::default()).unwrap(), 1517.0.into());
    }

    #[test]
    fn factorial() {
        let source = include_str!("../../examples/factorial.calc");
        let ast = Parser::new(source).try_parse_full_many::<Stmt>().unwrap();
        assert_eq!(eval_stmts(&ast, &mut Default::default()).unwrap(), true.into());
    }

    #[test]
    fn function_scope() {
        let source = include_str!("../../examples/function_scope.calc");
        let ast = Parser::new(source).try_parse_full_many::<Stmt>().unwrap();
        assert_eq!(eval_stmts(&ast, &mut Default::default()).unwrap(), 14.0.into());
    }

    #[test]
    fn if_branching() {
        let source = include_str!("../../examples/if_branching.calc");
        let ast = Parser::new(source).try_parse_full_many::<Stmt>().unwrap();
        assert_eq!(
            eval_stmts(&ast, &mut Default::default()).unwrap().coerce_real(),
            float(5).log2().into(),
        );
    }

    #[test]
    fn manual_abs() {
        let source = include_str!("../../examples/manual_abs.calc");
        let ast = Parser::new(source).try_parse_full_many::<Stmt>().unwrap();
        assert_eq!(eval_stmts(&ast, &mut Default::default()).unwrap(), 4.0.into());
    }
}
