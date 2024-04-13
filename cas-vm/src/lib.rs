mod frame;
mod instruction;

use cas_compute::{funcs::all, numerical::{builtin::Builtin, ctxt::TrigMode, error::Error as EvalError, value::Value}};
use cas_parser::parser::ast::Stmt;
use cas_compiler::{
    error::Error as CompileError,
    instruction::Instruction,
    item::Func,
    Chunk,
    Compile,
    Compiler,
    Label,
};
use frame::Frame;
use instruction::{exec_binary_instruction, exec_unary_instruction};
use std::collections::HashMap;

/// A virtual machine that executes bytecode instructions generated by the compiler (see
/// [`Compile`]).
#[derive(Debug)]
pub struct Vm {
    /// The bytecode chunks to execute.
    pub chunks: Vec<Chunk>,

    /// Labels generated by the compiler, mapped to the index of the instruction they reference.
    labels: HashMap<Label, (usize, usize)>,
}

impl Vm {
    /// Creates a [`Vm`] by compiling the given source AST.
    pub fn compile<T: Compile>(expr: T) -> Result<Self, CompileError> {
        let compiler = Compiler::compile(expr)?;
        Ok(Self {
            chunks: compiler.chunks,
            labels: compiler.labels
                .into_iter()
                .map(|(label, location)| (label, location.unwrap()))
                .collect(),
        })
    }

    /// Creates a [`Vm`] by compiling multiple statements.
    pub fn compile_program(stmts: Vec<Stmt>) -> Result<Self, CompileError> {
        let compiler = Compiler::compile_program(stmts)?;
        Ok(Self {
            chunks: compiler.chunks,
            labels: compiler.labels
                .into_iter()
                .map(|(label, location)| (label, location.unwrap()))
                .collect(),
        })
    }

    /// Executes the bytecode instructions.
    pub fn run(&self) -> Result<Value, EvalError> {
        println!("{:#?}", self.chunks.iter().enumerate().collect::<Vec<_>>());
        println!("{:#?}", self);
        let mut call_stack = vec![Frame::new((0, 0))];
        let mut value_stack = vec![];
        let mut instruction_pointer = (0, 0);

        while instruction_pointer.1 < self.chunks[instruction_pointer.0].instructions.len() {
            let instruction = &self.chunks[instruction_pointer.0].instructions[instruction_pointer.1];
            println!("value stack: {:?}", value_stack.iter().map(|v: &Value| v.to_string()).collect::<Vec<_>>());
            println!("call stack: {:?}", call_stack);
            println!("instruction to execute: {:?}", instruction);
            match instruction {
                Instruction::LoadConst(value) => value_stack.push(value.clone()),
                Instruction::LoadVar(id) => {
                    let value = call_stack.iter()
                        .rev()
                        .find_map(|frame| frame.get_variable(*id))
                        .cloned()
                        .unwrap();
                    value_stack.push(value);
                },
                Instruction::StoreVar(id) => {
                    let last_frame = call_stack.last_mut().unwrap();
                    let value = value_stack.last().cloned().unwrap();
                    last_frame.add_variable(id.to_owned(), value);
                },
                Instruction::AssignVar(id) => {
                    let last_frame = call_stack.last_mut().unwrap();
                    last_frame.add_variable(id.to_owned(), value_stack.pop().unwrap());
                },
                // .unwrap() helps us verify that exactly the right number of values are produced
                // and popped through the program
                Instruction::Drop => {
                    value_stack.pop().unwrap(); // TODO: maybe add .unwrap()?
                },
                Instruction::Binary(op) => exec_binary_instruction(*op, &mut value_stack).unwrap(),
                Instruction::Unary(op) => exec_unary_instruction(*op, &mut value_stack),
                Instruction::Call(chunk) => {
                    match chunk {
                        Func::UserFunc(chunk) => {
                            call_stack.push(Frame::new((instruction_pointer.0, instruction_pointer.1 + 1)));
                            instruction_pointer = (*chunk, 0);
                            continue;
                        },
                        Func::Builtin(name, arity) => {
                            let builtin = all().get(name).unwrap();
                            let args = value_stack.split_off(value_stack.len() - *arity);
                            let value = builtin.eval(TrigMode::Radians, &mut args.into_iter()).unwrap();
                            value_stack.push(value);
                        },
                    }
                },
                Instruction::Return => {
                    let frame = call_stack.pop().unwrap();
                    instruction_pointer = frame.return_instruction;
                    if call_stack.is_empty() {
                        break;
                    } else {
                        continue;
                    }
                },
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

            instruction_pointer.1 += 1;
        }
        assert!(value_stack.len() <= 1);
        Ok(value_stack.pop().unwrap_or(Value::Unit))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cas_compute::primitive::{float, int};
    use cas_parser::parser::{ast::stmt::Stmt, Parser};

    /// Compile the given source code and execute the resulting bytecode.
    fn run_program(source: &str) -> Result<Value, CompileError> {
        let mut parser = Parser::new(source);
        let stmts = parser.try_parse_full_many::<Stmt>().unwrap();

        let vm = Vm::compile_program(stmts)?;
        Ok(vm.run().unwrap())
    }

    #[test]
    fn exec_literal_number() {
        let result = run_program("42").unwrap();
        assert_eq!(result, Value::Integer(int(42)));
    }

    #[test]
    fn exec_multiple_assignment() {
        let result = run_program("x=y=z=5").unwrap();
        assert_eq!(result, Value::Integer(int(5)));
    }

    #[test]
    fn exec_loop() {
        let result = run_program("i = 0
while i < 10 then {
    i += 1
}; i").unwrap();
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
}; j").unwrap();
        assert_eq!(result, Value::Integer(int(2)));
    }

    #[test]
    fn exec_simple_program() {
        let result = run_program("x = 4.5
3x + 45 (x + 2) (1 + 3)").unwrap();
        assert_eq!(result, Value::Float(float(1183.5)));
    }

    #[test]
    fn exec_factorial() {
        let result = run_program("n = result = 8
loop {
    n -= 1
    result *= n
    if n <= 1 then break result
}").unwrap();
        assert_eq!(result, Value::Integer(int(40320)));
    }

    #[test]
    fn exec_partial_factorial() {
        let result = run_program("partial_factorial(n, k) = {
    result = 1
    while n > k then {
        result *= n
        n -= 1
    }
    result
}

partial_factorial(10, 7)").unwrap();
        assert_eq!(result, Value::Integer(int(720)));
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
}; sum").unwrap();
        assert_eq!(result, Value::Integer(int(10100)));
    }

    #[test]
    fn exec_call_func() {
        let result = run_program("g() = 6
f(x) = x^2 + 5x + g()
f(32)").unwrap();
        assert_eq!(result, Value::Integer(int(1190)));
    }

    #[test]
    fn exec_scoping() {
        let err = run_program("f() = j + 6
g() = {
    j = 10
    f()
}").unwrap_err();

        // error is in the definition of `f`
        // variable `j` is defined in `g`, so `f` can only access it if `x` is passed as an
        // argument, or `j` is in a higher scope
        assert_eq!(err.spans[0], 6..7);
    }

    #[test]
    fn exec_define_and_call() {
        let result = match run_program("f(x) = 2/sqrt(x)
g(x, y) = f(x) + f(y)
g(2, 3)").unwrap() {
            Value::Float(f) => f,
            other => panic!("expected float, got {:?}", other),
        };

        let left = int(6) * float(2).sqrt();
        let right = int(4) * float(3).sqrt();
        let value = (left + right) / 6;
        assert!(float(result - value).abs() < 1e-6);
    }

    #[test]
    fn exec_branching_return() {
        let result = run_program("f(x) = {
    if x < 0 then return -x
    x
}
f(-5)").unwrap();
        assert_eq!(result, Value::Integer(int(5)));
    }

    #[test]
    fn example_bad_lcm() {
        let source = include_str!("../../examples/bad_lcm.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result, 1517.into());
    }

    #[test]
    fn example_factorial() {
        let source = include_str!("../../examples/factorial.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result, true.into());
    }

    #[test]
    fn example_function_scope() {
        let source = include_str!("../../examples/function_scope.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result, 14.into());
    }

    #[test]
    fn example_if_branching() {
        let source = include_str!("../../examples/if_branching.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result.coerce_float(), float(5).log2().into());
    }

    #[test]
    fn example_manual_abs() {
        let source = include_str!("../../examples/manual_abs.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result, 4.into());
    }

    #[test]
    fn example_ncr() {
        let source = include_str!("../../examples/ncr.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result, true.into());
    }
}
