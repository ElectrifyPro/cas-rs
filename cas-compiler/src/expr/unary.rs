use cas_parser::parser::ast::unary::Unary;
use crate::{Compiler, Compile, Instruction};

impl Compile for Unary {
    fn compile(&self, compiler: &mut Compiler) {
        self.operand.compile(compiler);
        compiler.add_instr(Instruction::Unary(self.op.kind));
    }
}
