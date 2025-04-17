use cas_error::Error;
use cas_parser::parser::ast::range::Range;
use crate::{Compile, Compiler, InstructionKind};

impl Compile for Range {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.start.compile(compiler)?;
        self.end.compile(compiler)?;
        compiler.add_instr(InstructionKind::CreateRange(self.kind));
        Ok(())
    }
}
