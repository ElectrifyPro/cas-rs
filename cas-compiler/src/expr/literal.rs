use cas_compute::{
    numerical::value::Value,
    primitive::{float_from_str, from_str_radix, int_from_str},
};
use cas_parser::parser::ast::literal::Literal;
use crate::{error::Error, Compile, Compiler, InstructionKind};

impl Compile for Literal {
    fn compile(&self, compiler: &mut Compiler) -> Result<(), Error> {
        match self {
            Literal::Integer(int) => compiler.add_instr(InstructionKind::LoadConst(Value::Integer(int_from_str(&int.value)))),
            Literal::Float(float) => compiler.add_instr(InstructionKind::LoadConst(Value::Float(float_from_str(&float.value)))),
            Literal::Radix(radix) => compiler.add_instr(InstructionKind::LoadConst(Value::Integer(from_str_radix(radix.value.as_str(), radix.base)))),
            Literal::Boolean(boolean) => compiler.add_instr(InstructionKind::LoadConst(Value::Boolean(boolean.value))),
            Literal::Symbol(sym) => compiler.add_instr(InstructionKind::LoadVar(compiler.resolve_symbol(sym)?)),
            Literal::Unit(_) => compiler.add_instr(InstructionKind::LoadConst(Value::Unit)),
            Literal::List(list) => {
                // compile the elements of the list
                for element in list.values.iter() {
                    element.compile(compiler)?;
                }

                // create a list with the number of elements on the stack
                compiler.add_instr(InstructionKind::CreateList(list.values.len()));
            },
            Literal::ListRepeat(list_repeat) => {
                // compile the element to repeat
                list_repeat.value.compile(compiler)?;

                // compile the number of times to repeat the element
                list_repeat.count.compile(compiler)?;

                // create a list with the number of elements on the stack
                compiler.add_instr(InstructionKind::CreateListRepeat);
            },
        };

        Ok(())
    }
}
