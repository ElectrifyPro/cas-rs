use cas_compute::{numerical::value::Value, primitive::int};
use cas_error::Error;
use cas_parser::parser::{ast::{RangeKind, Sum}, token::op::BinOpKind};
use crate::{item::Symbol, Compile, Compiler, InstructionKind};

impl Compile for Sum {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        // ```
        // sum n in 1..10 of n
        // ```
        //
        // equivalent to:
        //
        // ```
        // {
        //     out = 0
        //     for n in 1..10 {
        //         out += n
        //     }
        //     out
        // }
        // ```
        //
        // but there is **no** control flow

        compiler.new_scope(|compiler| {
            // initialize the sum to 0
            compiler.add_instr(InstructionKind::LoadConst(Value::Integer(int(0))));

            // assign: initialize index in range
            self.range.start.compile(compiler)?;
            let symbol_id = compiler.resolve_user_symbol_or_insert(&self.variable)?;
            compiler.add_instr(InstructionKind::AssignVar(symbol_id));

            // condition: continue summing while the variable is in the range:
            // `symbol_id < self.range.end`
            let condition_start = compiler.new_end_label();
            compiler.add_instr(InstructionKind::LoadVar(Symbol::User(symbol_id)));
            self.range.end.compile(compiler)?;
            match self.range.kind {
                RangeKind::HalfOpen => compiler.add_instr(InstructionKind::Binary(BinOpKind::Less)),
                RangeKind::Closed => compiler.add_instr(InstructionKind::Binary(BinOpKind::LessEq)),
            }

            let loop_end = compiler.new_unassociated_label();
            compiler.add_instr(InstructionKind::JumpIfFalse(loop_end));

            // body: compute body, add it to cummulative sum
            self.body.compile(compiler)?;
            compiler.add_instr(InstructionKind::Binary(BinOpKind::Add));

            // increment index
            compiler.add_instr(InstructionKind::LoadVar(Symbol::User(symbol_id)));
            compiler.add_instr(InstructionKind::LoadConst(Value::Integer(int(1))));
            compiler.add_instr(InstructionKind::Binary(BinOpKind::Add));
            compiler.add_instr(InstructionKind::AssignVar(symbol_id));

            compiler.add_instr(InstructionKind::Jump(condition_start));

            compiler.set_end_label(loop_end);
            Ok(())
        })?;
        Ok(())
    }
}
