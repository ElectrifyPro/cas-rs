use cas_parser::parser::ast::loop_expr::Continue;
use crate::{Compiler, Compile, Instruction};

impl Compile for Continue {
    fn compile(&self, compiler: &mut Compiler) {
        let loop_start = compiler.state.loop_start.unwrap();
        compiler.add_instr(Instruction::Jump(loop_start));
    }
}
