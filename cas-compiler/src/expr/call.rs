use cas_error::Error;
use cas_parser::parser::ast::call::Call;
use crate::{Compile, Compiler, InstructionKind};

impl Compile for Call {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        self.args.iter().try_for_each(|arg| arg.compile(compiler))?;

        // TODO: allow calling any receiever when it is supported
        // load the function
        let symbol_id = compiler.resolve_symbol(&self.name)?;
        compiler.add_instr_with_spans(
            InstructionKind::LoadVar(symbol_id),
            vec![self.name.span.clone()],
        );

        let spans = self.outer_span()
            .into_iter()
            .chain(self.args.iter().map(|arg| arg.span()))
            .collect();

        if self.derivatives > 0 {
            // compute derivative of function
            compiler.add_instr_with_spans(
                InstructionKind::CallDerivative(self.derivatives, self.args.len()),
                spans,
            );
        } else {
            // call the function
            compiler.add_instr_with_spans(InstructionKind::Call(self.args.len()), spans);
        }

        Ok(())
    }
}
