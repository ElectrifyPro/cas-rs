use cas_compute::{numerical::value::Value, primitive::int};
use cas_error::Error;
use cas_parser::parser::{ast::{Product, RangeKind}, token::op::BinOpKind};
use crate::{item::Symbol, Compile, Compiler, InstructionKind};

impl Compile for Product {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        // ```
        // product n in 1..10 of n
        // ```
        //
        // equivalent to:
        //
        // ```
        // {
        //     out = 1
        //     for n in 1..10 {
        //         out *= n
        //     }
        //     out
        // }
        // ```
        //
        // but there is **no** control flow

        compiler.new_scope(|compiler| {
            // initialize the product to 1
            compiler.add_instr(InstructionKind::LoadConst(Value::Integer(int(1))));

            // compile range end up here so that the index variable isn't in scope, then insert it
            // down at the condition
            let chunk = compiler.new_chunk_get(|compiler| {
                self.range.end.compile(compiler)
            })?;

            // assign: initialize index in range
            self.range.start.compile(compiler)?;
            let symbol_id = compiler.add_symbol(&self.variable)?;
            compiler.add_instr(InstructionKind::AssignVar(symbol_id));

            // condition: continue summing while the variable is in the range:
            // `symbol_id < self.range.end`
            let condition_start = compiler.new_end_label();
            compiler.add_instr(InstructionKind::LoadVar(Symbol::User(symbol_id)));
            compiler.add_chunk_instrs(chunk);
            match self.range.kind {
                RangeKind::HalfOpen => compiler.add_instr(InstructionKind::Binary(BinOpKind::Less)),
                RangeKind::Closed => compiler.add_instr(InstructionKind::Binary(BinOpKind::LessEq)),
            }

            let loop_end = compiler.new_unassociated_label();
            compiler.add_instr(InstructionKind::JumpIfFalse(loop_end));

            // body: compute body, multiply it to cummulative product
            self.body.compile(compiler)?;
            compiler.add_instr(InstructionKind::Binary(BinOpKind::Mul));

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
