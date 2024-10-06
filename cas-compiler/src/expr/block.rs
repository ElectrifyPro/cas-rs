use cas_error::Error;
use cas_parser::parser::ast::Block;
use crate::{Compile, Compiler};
use super::compile_stmts;

impl Compile for Block {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        compiler.new_scope(|compiler| compile_stmts(&self.stmts, compiler))?;
        Ok(())
    }
}
