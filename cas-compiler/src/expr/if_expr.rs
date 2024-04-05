use cas_parser::parser::ast::if_expr::If;
use crate::{Compiler, Compile, Instruction};

impl Compile for If {
    fn compile(&self, compiler: &mut Compiler) {
        let condition_start = compiler.new_end_label();
        self.condition.compile(compiler);

        if let Some(else_expr) = &self.else_expr {
            let else_start = compiler.new_unassociated_label();
            compiler.add_instr(Instruction::JumpIfFalse(else_start));

            self.then_expr.compile(compiler);

            let if_end = compiler.new_unassociated_label();
            compiler.add_instr(Instruction::Jump(if_end));

            compiler.set_end_label(else_start);
            else_expr.compile(compiler);

            compiler.set_end_label(if_end);
        } else {
            let then_end = compiler.new_unassociated_label();
            compiler.add_instr(Instruction::JumpIfFalse(then_end));

            self.then_expr.compile(compiler);

            compiler.set_end_label(then_end);
        }
    }
}
