use cas_parser::parser::ast::index::Index;
use crate::{error::Error, Compile, Compiler, Instruction};

impl Compile for Index {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        // load the (hopefully) list value onto the stack
        self.target.compile(compiler)?;

        // load the index value onto the stack
        self.index.compile(compiler)?;

        // load the value at the index onto the stack
        compiler.add_instr(Instruction::LoadIndexed);
        Ok(())
    }
}
