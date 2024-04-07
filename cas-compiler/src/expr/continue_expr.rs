use cas_parser::parser::ast::loop_expr::Continue;
use crate::{error::Error, Compile, Compiler, Instruction};

impl Compile for Continue {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        let loop_start = compiler.state.loop_start.unwrap();
        compiler.add_instr(Instruction::Jump(loop_start));
        Ok(())
    }
}
