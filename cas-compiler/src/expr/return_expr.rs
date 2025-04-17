use cas_compute::numerical::value::Value;
use cas_error::Error;
use cas_parser::parser::ast::return_expr::Return;
use crate::{Compile, Compiler, InstructionKind};

impl Compile for Return {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        if let Some(expr) = &self.value {
            expr.compile(compiler)?;
        } else {
            compiler.add_instr(InstructionKind::LoadConst(Value::Unit));
        }
        compiler.add_instr(InstructionKind::Return);
        Ok(())
    }
}
