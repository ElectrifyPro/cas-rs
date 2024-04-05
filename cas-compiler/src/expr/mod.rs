mod assign;
mod binary;
mod break_expr;
mod call;
mod continue_expr;
mod expr;
mod if_expr;
mod literal;
mod loops;
mod stmt;
mod unary;

pub use stmt::compile_stmts;
