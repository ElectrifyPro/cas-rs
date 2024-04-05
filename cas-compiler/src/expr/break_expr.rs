use cas_parser::parser::ast::loop_expr::Break;
use crate::{Compiler, Compile, Instruction};

impl Compile for Break {
    fn compile(&self, compiler: &mut Compiler) {
        if let Some(value) = &self.value {
            value.compile(compiler);
        }

        let loop_end = compiler.state.loop_end.unwrap();
        compiler.add_instr(Instruction::Jump(loop_end));
    }
}
