use cas_error::Error;
use cas_parser::parser::ast::branch::{Of, Then};
use crate::{Compile, Compiler};

impl Compile for Then {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.expr.compile(compiler)
    }
}

impl Compile for Of {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.expr.compile(compiler)
    }
}
