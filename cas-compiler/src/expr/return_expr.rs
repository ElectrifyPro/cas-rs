use cas_compute::numerical::value::Value;
use cas_parser::parser::ast::return_expr::Return;
use crate::{error::Error, Compile, Compiler, Instruction};

impl Compile for Return {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        if let Some(expr) = &self.value {
            expr.compile(compiler)?;
        } else {
            compiler.add_instr(Instruction::LoadConst(Value::Unit));
        }
        compiler.add_instr(Instruction::Return);
        Ok(())
    }
}
