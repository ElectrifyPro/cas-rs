use cas_compute::numerical::value::Value;
use cas_error::Error;
use cas_parser::parser::ast::if_expr::If;
use crate::{Compile, Compiler, InstructionKind};

impl Compile for If {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.condition.compile(compiler)?;

        let else_start = compiler.new_unassociated_label();
        compiler.add_instr_with_spans(
            InstructionKind::JumpIfFalse(else_start),
            // for error if condition doesn't evaluate to boolean
            vec![self.condition.span(), self.then_expr.span()],
        );

        self.then_expr.compile(compiler)?;

        let if_end = compiler.new_unassociated_label();
        compiler.add_instr(InstructionKind::Jump(if_end));

        compiler.set_end_label(else_start);
        if let Some(else_expr) = &self.else_expr {
            else_expr.compile(compiler)?;
        } else {
            compiler.add_instr(InstructionKind::LoadConst(Value::Unit));
        }

        compiler.set_end_label(if_end);

        Ok(())
    }
}
