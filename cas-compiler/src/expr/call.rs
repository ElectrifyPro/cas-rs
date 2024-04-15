use cas_parser::parser::ast::call::Call;
use crate::{error::Error, Compile, Compiler, InstructionKind};

impl Compile for Call {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        for arg in self.args.iter() {
            arg.compile(compiler)?;
        }
        compiler.add_instr(InstructionKind::Call(compiler.resolve_function(self)?));
        Ok(())
    }
}
