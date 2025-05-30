mod assign;
mod binary;
mod block;
mod branch;
mod break_expr;
mod call;
mod continue_expr;
mod if_expr;
mod index;
mod literal;
mod loops;
mod product;
mod range;
mod return_expr;
mod stmt;
mod sum;
mod unary;

pub use stmt::compile_stmts;

use cas_error::Error;
use cas_parser::parser::ast::expr::Expr;
use crate::{Compile, Compiler};

impl Compile for Expr {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        match self {
            Expr::Literal(literal) => literal.compile(compiler),
            Expr::Paren(paren) => paren.expr.compile(compiler),
            Expr::Block(block) => block.compile(compiler),
            Expr::Sum(sum) => sum.compile(compiler),
            Expr::Product(product) => product.compile(compiler),
            Expr::If(if_expr) => if_expr.compile(compiler),
            Expr::Loop(loop_expr) => loop_expr.compile(compiler),
            Expr::While(while_expr) => while_expr.compile(compiler),
            Expr::For(for_expr) => for_expr.compile(compiler),
            Expr::Then(then_expr) => then_expr.compile(compiler),
            Expr::Of(of_expr) => of_expr.compile(compiler),
            Expr::Break(break_expr) => break_expr.compile(compiler),
            Expr::Continue(continue_expr) => continue_expr.compile(compiler),
            Expr::Return(return_expr) => return_expr.compile(compiler),
            Expr::Call(call) => call.compile(compiler),
            Expr::Index(index) => index.compile(compiler),
            Expr::Unary(unary) => unary.compile(compiler),
            Expr::Binary(binary) => binary.compile(compiler),
            Expr::Assign(assign) => assign.compile(compiler),
            Expr::Range(range) => range.compile(compiler),
        }
    }
}
