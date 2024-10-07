use cas_compute::{numerical::value::Value, primitive::int};
use cas_error::Error;
use cas_parser::parser::{
    ast::{for_expr::For, loop_expr::Loop, while_expr::While, RangeKind},
    token::op::BinOpKind,
};
use crate::{item::Symbol, Compile, Compiler, InstructionKind};

impl Compile for Loop {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        let loop_start = compiler.new_end_label();
        let loop_end = compiler.new_unassociated_label();

        compiler.with_state(|state| {
            // in case `continue` and `break` expressions are inside, we need the loop start and
            // end labels for their jumps
            state.loop_start = Some(loop_start);
            state.loop_end = Some(loop_end);
        }, |compiler| {
            compiler.new_scope(|compiler| self.body.compile(compiler))?;
            compiler.add_instr(InstructionKind::Drop);
            Ok(())
        })?;

        compiler.add_instr(InstructionKind::Jump(loop_start));

        // NOTE: we don't have to do the same "hack" as in `while` loops, since `loop`s cannot
        // terminate without a `break` expression

        compiler.set_end_label(loop_end);
        Ok(())
    }
}

impl Compile for While {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        let condition_start = compiler.new_end_label();
        self.condition.compile(compiler)?;

        let end_with_no_break = compiler.new_unassociated_label();
        let loop_end = compiler.new_unassociated_label();
        compiler.add_instr(InstructionKind::JumpIfFalse(end_with_no_break));
        compiler.with_state(|state| {
            // in case `continue` and `break` expressions are inside, we need the loop start and
            // end labels for their jumps
            state.loop_start = Some(condition_start);
            state.loop_end = Some(loop_end);
        }, |compiler| {
            compiler.new_scope(|compiler| self.body.compile(compiler))?;
            compiler.add_instr(InstructionKind::Drop);
            Ok(())
        })?;

        compiler.add_instr(InstructionKind::Jump(condition_start));

        // if the loop doesn't terminate through a `break` expression, we need to load something to
        // the stack so that the automatically generated `Drop` instruction has something to drop
        //
        // TODO: this can be optimized out at some point
        compiler.set_end_label(end_with_no_break);
        compiler.add_instr(InstructionKind::LoadConst(Value::Unit));

        compiler.set_end_label(loop_end);
        Ok(())
    }
}

impl Compile for For {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        // ```
        // for i in 0..10 then print(i)
        // ```
        //
        // equivalent to:
        //
        // ```
        // i = 0
        // while i < 10 {
        //     print(i)
        //     i += 1
        // }
        // ```
        //
        // but with control flow; specifically, `continue` also increments `i`
        //
        // TODO: this will one day be generalized to work on any iterator

        compiler.new_scope(|compiler| {
            // compile range end up here so that the index variable isn't in scope, then insert it
            // down at the condition
            let chunk = compiler.new_chunk_get(|compiler| {
                self.range.end.compile(compiler)
            })?;

            // assign: initialize index in range, jump past initial increment
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

            let end_with_no_break = compiler.new_unassociated_label();
            let loop_end = compiler.new_unassociated_label();
            compiler.add_instr(InstructionKind::JumpIfFalse(end_with_no_break));

            // body: run body
            let index_start = compiler.new_unassociated_label();
            compiler.with_state(|state| {
                // in case `continue` and `break` expressions are inside, we need the loop start and
                // end labels for their jumps
                state.loop_start = Some(index_start);
                state.loop_end = Some(loop_end);
            }, |compiler| {
                self.body.compile(compiler)?;
                compiler.add_instr(InstructionKind::Drop);
                Ok(())
            })?;

            // increment index
            compiler.set_end_label(index_start);
            compiler.add_instr(InstructionKind::LoadVar(Symbol::User(symbol_id)));
            compiler.add_instr(InstructionKind::LoadConst(Value::Integer(int(1))));
            compiler.add_instr(InstructionKind::Binary(BinOpKind::Add));
            compiler.add_instr(InstructionKind::AssignVar(symbol_id));

            // jump back to condition
            compiler.add_instr(InstructionKind::Jump(condition_start));

            // if the loop doesn't terminate through a `break` expression, we need to load
            // something to the stack so that the automatically generated `Drop` instruction has
            // something to drop
            //
            // TODO: same potential optimization as in `while` loops
            compiler.set_end_label(end_with_no_break);
            compiler.add_instr(InstructionKind::LoadConst(Value::Unit));

            compiler.set_end_label(loop_end);
            Ok(())
        })?;
        Ok(())
    }
}
