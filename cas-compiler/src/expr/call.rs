use cas_parser::parser::ast::call::Call;
use crate::{Compiler, Compile, Instruction};

impl Compile for Call {
    fn compile(&self, compiler: &mut Compiler) {
        for arg in self.args.iter() {
            arg.compile(compiler);
        }
        compiler.add_instr(Instruction::Call(compiler.resolve_function(&self.name.name)));
    }
}
