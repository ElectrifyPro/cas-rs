use cas_compute::numerical::value::Value;
use cas_parser::parser::ast::if_expr::If;
use crate::{error::Error, Compile, Compiler, Instruction};

impl Compile for If {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        let condition_start = compiler.new_end_label();
        self.condition.compile(compiler)?;

        let else_start = compiler.new_unassociated_label();
        compiler.add_instr(Instruction::JumpIfFalse(else_start));

        self.then_expr.compile(compiler)?;

        let if_end = compiler.new_unassociated_label();
        compiler.add_instr(Instruction::Jump(if_end));

        compiler.set_end_label(else_start);
        if let Some(else_expr) = &self.else_expr {
            else_expr.compile(compiler)?;
        } else {
            compiler.add_instr(Instruction::LoadConst(Value::Unit));
        }

        compiler.set_end_label(if_end);

        Ok(())
    }
}
