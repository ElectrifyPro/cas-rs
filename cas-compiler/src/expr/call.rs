use cas_parser::parser::ast::call::Call;
use crate::{Compiler, Compile};

impl Compile for Call {
    fn compile(&self, compiler: &mut Compiler) {
        for arg in self.args.iter() {
            arg.compile(compiler);
        }
        // TODO
        // compiler.add_instr(Instruction::Call(self.name.name.clone()));
    }
}
