use cas_parser::parser::ast::binary::Binary;
use crate::{Compiler, Compile, Instruction};

impl Compile for Binary {
    fn compile(&self, compiler: &mut Compiler) {
        self.lhs.compile(compiler);
        self.rhs.compile(compiler);
        compiler.add_instr(Instruction::Binary(self.op.kind));
    }
}
