mod instruction;

use cas_compute::numerical::value::Value;
use cas_parser::parser::ast::Stmt;
use cas_compiler::{
    frame::Frame,
    instruction::Instruction,
    Compile,
    Compiler,
    Label,
};
use instruction::{exec_binary_instruction, exec_unary_instruction};
use std::collections::HashMap;

/// A virtual machine that executes bytecode instructions generated by the compiler (see
/// [`Compile`]).
#[derive(Debug)]
pub struct Vm {
    /// The bytecode instructions to execute.
    pub instructions: Vec<Instruction>,

    /// Labels generated by the compiler, mapped to the index of the instruction they reference.
    labels: HashMap<Label, usize>,
}

impl Vm {
    /// Creates a [`Vm`] by compiling the given source AST.
    pub fn compile<T: Compile>(expr: T) -> Self {
        let compiler = Compiler::compile(expr);
        Self {
            instructions: compiler.instructions,
            labels: compiler.labels
                .into_iter()
                .map(|(label, index)| (label, index.unwrap()))
                .collect(),
        }
    }

    /// Creates a [`Vm`] by compiling multiple statements.
    pub fn compile_program(stmts: Vec<Stmt>) -> Self {
        let compiler = Compiler::compile_program(stmts);
        Self {
            instructions: compiler.instructions,
            labels: compiler.labels
                .into_iter()
                .map(|(label, index)| (label, index.unwrap()))
                .collect(),
        }
    }

    /// Executes the bytecode instructions.
    pub fn run(&self) -> Value {
        println!("{:#?}", self.instructions.iter().enumerate().collect::<Vec<_>>());
        println!("{:#?}", self.labels);
        let mut call_stack = vec![Frame::default()];
        let mut value_stack = vec![];
        let mut instruction_pointer = 0;

        while instruction_pointer < self.instructions.len() {
            let instruction = &self.instructions[instruction_pointer];
            match instruction {
                Instruction::LoadConst(value) => value_stack.push(value.clone()),
                Instruction::LoadVar(var) => {
                    let value = call_stack.iter()
                        .rev()
                        .find_map(|frame| frame.get_var(&var))
                        .unwrap();
                    value_stack.push(value);
                },
                Instruction::StoreVar(var) => {
                    let last_frame = call_stack.last_mut().unwrap();
                    let value = value_stack.last().cloned().unwrap();
                    last_frame.add_var(var.to_owned(), value);
                },
                Instruction::AssignVar(var) => {
                    let last_frame = call_stack.last_mut().unwrap();
                    last_frame.add_var(var.to_owned(), value_stack.pop().unwrap());
                },
                Instruction::Drop => {
                    value_stack.pop(); // TODO: maybe add .unwrap()?
                },
                Instruction::Binary(op) => exec_binary_instruction(*op, &mut value_stack),
                Instruction::Unary(op) => exec_unary_instruction(*op, &mut value_stack),
                Instruction::Jump(label) => {
                    instruction_pointer = self.labels[label];
                    continue;
                },
                Instruction::JumpIfFalse(label) => {
                    let value = value_stack.pop().unwrap();
                    if let Value::Boolean(b) = value {
                        if !b {
                            instruction_pointer = self.labels[label];
                            continue;
                        }
                    } else {
                        todo!()
                    }
                },
                Instruction::Output => todo!(),
            }
            println!("executed instruction: {:?}", instruction);
            println!("value stack: {:?}", value_stack.iter().map(|v| v.to_string()).collect::<Vec<_>>());

            instruction_pointer += 1;
        }
        assert!(value_stack.len() <= 1);
        value_stack.pop().unwrap_or(Value::Unit)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cas_compute::primitive::{float, int};
    use cas_parser::parser::{ast::stmt::Stmt, Parser};

    // /// Compile the given expression and execute the resulting bytecode.
    // fn run_expr(source: &str) -> Value {
    //     let mut parser = Parser::new(source);
    //     let expr = parser.try_parse_full::<Stmt>().unwrap();

    //     let vm = Vm::compile(expr);
    //     vm.run()
    // }

    /// Compile the given source code and execute the resulting bytecode.
    fn run_program(source: &str) -> Value {
        let mut parser = Parser::new(source);
        let stmts = parser.try_parse_full_many::<Stmt>().unwrap();

        let vm = Vm::compile_program(stmts);
        vm.run()
    }

    #[test]
    fn exec_literal_number() {
        let result = run_program("42");
        assert_eq!(result, Value::Integer(int(42)));
    }

    #[test]
    fn exec_multiple_assignment() {
        let result = run_program("x=y=z=5");
        assert_eq!(result, Value::Integer(int(5)));
    }

    #[test]
    fn exec_loop() {
        let result = run_program("i = 0
while i < 10 then {
    i += 1
}; i");
        assert_eq!(result, Value::Integer(int(10)));
    }

    #[test]
    fn exec_loop_with_conditions() {
        let result = run_program("i = 0
j = 2
while i < 10 && j < 15 then {
    if i < 5 then {
        i += 2
    } else {
        i += 1
        j = -j + 4
    }
}; j");
        assert_eq!(result, Value::Integer(int(2)));
    }

    #[test]
    fn exec_simple_program() {
        let result = run_program("x = 4.5
3x + 45 (x + 2) (1 + 3)");
        assert_eq!(result, Value::Float(float(1183.5)));
    }

    #[test]
    fn exec_factorial() {
        let result = run_program("n = result = 8
loop {
    n -= 1
    result *= n
    if n <= 1 then break result
}");
        assert_eq!(result, Value::Integer(int(40320)));
    }

    #[test]
    fn exec_sum_even() {
        let result = run_program("n = 200
i = 0
sum = 0
while i < n then {
    i += 1
    if i & 1 == 1 then continue
    sum += i
}; sum");
        assert_eq!(result, Value::Integer(int(10100)));
    }
}
