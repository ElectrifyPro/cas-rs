use cas_error::Error;
use cas_parser::parser::ast::binary::Binary;
use crate::{Compile, Compiler, InstructionKind};

impl Compile for Binary {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.lhs.compile(compiler)?;
        self.rhs.compile(compiler)?;
        compiler.add_instr_with_spans(
            InstructionKind::Binary(self.op.kind),
            vec![self.lhs.span(), self.op.span.clone(), self.rhs.span()],
        );
        Ok(())
    }
}
