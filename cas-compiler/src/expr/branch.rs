use cas_error::Error;
use cas_parser::parser::ast::branch::Then;
use crate::{Compile, Compiler};

impl Compile for Then {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.expr.compile(compiler)
    }
}
