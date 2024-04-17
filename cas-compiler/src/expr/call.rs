use cas_parser::parser::ast::{call::Call, Expr, Param};
use crate::{error::{kind, Error}, item::Func, Compile, Compiler, InstructionKind};

/// Compiles the correct instructions that generate the arguments for the function call, in
/// order to check if the user is relying on the default value of an optional parameter.
fn compile_args(compiler: &mut Compiler, resolved: &Func, args: &[Expr]) -> Result<(), Error> {
    for arg in args {
        arg.compile(compiler)?;
    }

    // for user-defined functions, generate the default values for the optional parameters
    // builtin functions compute the default values themselves, so we don't need to do anything for
    // them
    if let Func::User(call) = resolved {
        if let Some(defaults_used) = resolved.num_defaults_used() {
            // default arguments must be at the end of the argument list
            // so simply compile the last `defaults_used` arguments
            for i in 0..defaults_used {
                let Param::Default(_, expr) = &call.signature[call.signature.len() - defaults_used + i] else {
                    todo!();
                };
                expr.compile(compiler)?;
            }
        }
    }

    Ok(())
}

impl Compile for Call {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        let func = compiler.resolve_function(self)?;
        compile_args(compiler, &func, &self.args)?;

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
