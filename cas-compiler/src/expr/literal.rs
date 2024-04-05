use cas_compute::{
    numerical::value::Value,
    primitive::{float_from_str, from_str_radix, int_from_str},
};
use cas_parser::parser::ast::literal::Literal;
use crate::{Compiler, Compile, Instruction};

impl Compile for Literal {
    fn compile(&self, compiler: &mut Compiler) {
        match self {
            Literal::Integer(int) => compiler.add_instr(Instruction::LoadConst(Value::Integer(int_from_str(&int.value)))),
            Literal::Float(float) => compiler.add_instr(Instruction::LoadConst(Value::Float(float_from_str(&float.value)))),
            Literal::Radix(radix) => compiler.add_instr(Instruction::LoadConst(Value::Integer(from_str_radix(radix.value.as_str(), radix.base)))),
            Literal::Boolean(boolean) => compiler.add_instr(Instruction::LoadConst(Value::Boolean(boolean.value))),
            Literal::Symbol(sym) => compiler.add_instr(Instruction::LoadVar(sym.name.clone())),
            Literal::Unit(_) => compiler.add_instr(Instruction::LoadConst(Value::Unit)),
            Literal::List(_) => todo!(),
        }
    }
}
