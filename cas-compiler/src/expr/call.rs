use cas_parser::parser::ast::call::Call;
use crate::{error::{kind, Error}, Compile, Compiler, InstructionKind};

impl Compile for Call {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        for arg in self.args.iter() {
            arg.compile(compiler)?;
        }

        let func = compiler.resolve_function(self)?;

        if self.derivatives > 0 {
            if func.arity() == 1 {
                compiler.add_instr_with_spans(
                    InstructionKind::CallDerivative(func, self.derivatives),
                    self.outer_span().to_vec(),
                );
            } else {
                return Err(Error::new(
                    vec![self.name.span.clone()],
                    kind::InvalidDifferentiation {
                        name: self.name.name.to_string(),
                        actual: func.arity(),
                    },
                ));
            }
        } else {
            compiler.add_instr(InstructionKind::Call(func));
        }

        Ok(())
    }
}
