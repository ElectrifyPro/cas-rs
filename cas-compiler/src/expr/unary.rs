use cas_error::Error;
use cas_parser::parser::ast::unary::Unary;
use crate::{Compile, Compiler, InstructionKind};

impl Compile for Unary {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.operand.compile(compiler)?;
        compiler.add_instr_with_spans(
            InstructionKind::Unary(self.op.kind),
            vec![self.operand.span(), self.op.span.clone()],
        );
        Ok(())
    }
}
