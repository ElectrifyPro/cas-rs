//! A virtual machine for `cas-rs` that executes bytecode instructions generated by
//! [`cas-compiler`].
//!
//! ```
//! use cas_compute::numerical::value::Value;
//! use cas_parser::parser::ast::Stmt;
//! use cas_parser::parser::Parser;
//! use cas_vm::Vm;
//!
//! let source = "sin(pi / 2)";
//! let mut parser = Parser::new(source);
//! let stmts = parser.try_parse_full_many::<Stmt>().unwrap();
//!
//! let mut vm = Vm::compile_program(stmts).unwrap();
//!
//! use cas_compute::primitive::float;
//! assert_eq!(vm.run().unwrap().coerce_float(), Value::Float(float(1.0)));
//! ```

pub mod error;
mod frame;
mod instruction;

use cas_compute::{
    consts::{E, I, PHI, PI, TAU},
    numerical::{builtin::error::BuiltinError, trig_mode::TrigMode, value::Value},
    primitive::{complex, float},
};
use cas_compiler::{
    expr::compile_stmts,
    instruction::InstructionKind,
    item::{Func, Item, Symbol},
    Chunk,
    Compile,
    Compiler,
    Label,
};
use cas_error::Error;
use cas_parser::parser::ast::Stmt;
use error::{
    IndexOutOfBounds,
    IndexOutOfRange,
    InvalidIndexType,
    InvalidLengthType,
    LengthOutOfRange,
    StackOverflow,
    TypeMismatch,
};
use frame::Frame;
use instruction::{
    exec_binary_instruction,
    exec_unary_instruction,
    Derivative,
};
use std::{cell::RefCell, collections::HashMap, ops::Range, rc::Rc};

/// The maximum number of stack frames before a stack overflow error is thrown in the [`Vm`].
const MAX_STACK_FRAMES: usize = 2usize.pow(16);

/// After executing [`Vm::run_one`], indicates if execution will continue normally, or if it
/// something else, such as a jump, has occurred.
#[derive(Debug)]
enum ControlFlow {
    /// Continue executing the program sequentially.
    Continue,

    /// A jump has occurred. Do not increment the instruction pointer.
    Jump,
}

/// A virtual machine that executes bytecode instructions generated by the compiler (see
/// [`Compile`]).
#[derive(Clone, Debug)]
pub struct Vm {
    /// The trigonometric mode used when calling native functions.
    trig_mode: TrigMode,

    /// The bytecode chunks to execute.
    pub chunks: Vec<Chunk>,

    /// Labels generated by the compiler, mapped to the index of the instruction they reference.
    labels: HashMap<Label, (usize, usize)>,

    /// A symbol table that maps identifiers to information about the values they represent.
    ///
    /// This is used to store information about variables and functions that are defined in the
    /// program.
    pub symbols: HashMap<String, Item>,

    /// Variables in the global scope.
    pub variables: HashMap<usize, Value>,
}

impl Default for Vm {
    fn default() -> Self {
        Self {
            trig_mode: TrigMode::default(),
            chunks: vec![Chunk::default()], // add main chunk
            labels: HashMap::new(),
            symbols: HashMap::new(),
            variables: HashMap::new(),
        }
    }
}

impl From<Compiler> for Vm {
    fn from(compiler: Compiler) -> Self {
        Self {
            trig_mode: TrigMode::default(),
            chunks: compiler.chunks,
            labels: compiler.labels
                .into_iter()
                .map(|(label, location)| (label, location.unwrap()))
                .collect(),
            symbols: compiler.symbols,
            variables: HashMap::new(),
        }
    }
}

impl Vm {
    /// Creates a blank [`Vm`] with no chunks. This is used for `cas-repl` to compile code on the
    /// fly.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a [`Vm`] by compiling the given source AST.
    pub fn compile<T: Compile>(expr: T) -> Result<Self, Error> {
        let compiler = Compiler::compile(expr)?;
        Ok(Self {
            trig_mode: TrigMode::default(),
            chunks: compiler.chunks,
            labels: compiler.labels
                .into_iter()
                .map(|(label, location)| (label, location.unwrap()))
                .collect(),
            symbols: compiler.symbols,
            variables: HashMap::new(),
        })
    }

    /// Creates a [`Vm`] by compiling multiple statements.
    pub fn compile_program(stmts: Vec<Stmt>) -> Result<Self, Error> {
        let compiler = Compiler::compile_program(stmts)?;
        Ok(Self {
            trig_mode: TrigMode::default(),
            chunks: compiler.chunks,
            labels: compiler.labels
                .into_iter()
                .map(|(label, location)| (label, location.unwrap()))
                .collect(),
            symbols: compiler.symbols,
            variables: HashMap::new(),
        })
    }

    /// Sets the trigonometric mode used when calling native functions.
    pub fn with_trig_mode(mut self, mode: TrigMode) -> Self {
        self.trig_mode = mode;
        self
    }

    /// Executes one instruction, updating the given state.
    ///
    /// Returns a [`ControlFlow`] value that indicates whether the program should continue running
    fn run_one(
        &mut self,
        value_stack: &mut Vec<Value>,
        call_stack: &mut Vec<Frame>,
        derivative_stack: &mut Vec<Derivative>,
        instruction_pointer: &mut (usize, usize),
    ) -> Result<ControlFlow, Error> {
        /// Check for a stack overflow and return an error if one is detected.
        fn check_stack_overflow(call_span: &[Range<usize>], call_stack: &[Frame]) -> Result<(), Error> {
            // MAX_STACK_FRAMES is an arbitrary limit to prevent infinite recursion
            if call_stack.len() > MAX_STACK_FRAMES {
                return Err(Error::new(call_span.to_vec(), StackOverflow));
            }

            Ok(())
        }

        /// Extracts the `usize` index from the given [`Value`] for the indexing instructions.
        fn extract_index(value: Value, spans: Vec<Range<usize>>) -> Result<usize, Error> {
            let typename = value.typename();
            let Value::Integer(int) = value.coerce_integer() else {
                return Err(Error::new(
                    spans,
                    InvalidIndexType {
                        expr_type: typename,
                    },
                ));
            };

            int.to_usize().ok_or_else(|| Error::new(
                spans,
                IndexOutOfRange,
            ))
        }

        /// Extracts the `usize` length from the given [`Value`] for the
        /// [`InstructionKind::CreateListRepeat`] instruction, which uses slightly different error
        /// types than [`extract_index`].
        fn extract_length(value: Value, spans: Vec<Range<usize>>) -> Result<usize, Error> {
            let typename = value.typename();
            let Value::Integer(int) = value.coerce_integer() else {
                return Err(Error::new(
                    spans,
                    InvalidLengthType {
                        expr_type: typename,
                    },
                ));
            };

            int.to_usize().ok_or_else(|| Error::new(
                spans,
                LengthOutOfRange,
            ))
        }

        /// Convert a [`BuiltinError`] to an [`EvalError`].
        fn from_builtin_error(err: BuiltinError, spans: Vec<Range<usize>>) -> Error {
            match err {
                BuiltinError::TypeMismatch(err) => {
                    // remove spans of all arguments but the mentioned one
                    let spans = vec![
                        spans[0].start..spans[1].end,
                        spans[2 + err.index].clone(),
                    ];
                    Error::new(spans, TypeMismatch::from(err))
                },
                _ => todo!(),
            }
        }

        let instruction = &self.chunks[instruction_pointer.0].instructions[instruction_pointer.1];
        // println!("value stack: {:?}", value_stack.iter().map(|v: &Value| v.to_string()).collect::<Vec<_>>());
        // println!("call stack: {:?}", call_stack);
        // println!("instruction to execute: {:?} (chunk/inst: {}/{})", instruction, instruction_pointer.0, instruction_pointer.1);

        match &instruction.kind {
            InstructionKind::LoadConst(value) => value_stack.push(value.clone()),
            InstructionKind::CreateList(len) => {
                let elements = value_stack.split_off(value_stack.len() - *len);
                value_stack.push(Value::List(Rc::new(RefCell::new(elements))));
            },
            InstructionKind::CreateListRepeat => {
                let count = value_stack.pop().unwrap();
                let value = value_stack.pop().unwrap();
                let list = vec![value; extract_length(count, instruction.spans.clone())?];
                value_stack.push(Value::List(Rc::new(RefCell::new(list))));
            },
            InstructionKind::LoadVar(id) => match id {
                Symbol::User(id) => {
                    let value = call_stack
                        .iter()
                        .rev()
                        .find_map(|frame| frame.get_variable(*id))
                        .cloned()
                        .unwrap();
                    value_stack.push(value);
                }
                Symbol::Builtin(name) => {
                    value_stack.push(match *name {
                        "i" => Value::Complex(complex(&*I)),
                        "e" => Value::Float(float(&*E)),
                        "phi" => Value::Float(float(&*PHI)),
                        "pi" => Value::Float(float(&*PI)),
                        "tau" => Value::Float(float(&*TAU)),
                        _ => unreachable!(),
                    });
                }
            },
            InstructionKind::StoreVar(id) => {
                let last_frame = call_stack.last_mut().unwrap();
                let value = value_stack.last().cloned().unwrap();
                last_frame.add_variable(id.to_owned(), value);
            },
            InstructionKind::AssignVar(id) => {
                let last_frame = call_stack.last_mut().unwrap();
                last_frame.add_variable(id.to_owned(), value_stack.pop().unwrap());
            },
            InstructionKind::StoreIndexed => {
                let index = value_stack.pop().unwrap();
                let list = value_stack.pop().unwrap();
                let value = value_stack.last().cloned().unwrap();
                if let Value::List(list) = list {
                    let index = extract_index(index, instruction.spans.clone())?;

                    let mut list = list.borrow_mut();
                    let len = list.len();
                    *list.get_mut(index).ok_or_else(|| {
                        Error::new(instruction.spans.clone(), IndexOutOfBounds { len, index })
                    })? = value;
                } else {
                    todo!()
                }
            },
            InstructionKind::LoadIndexed => {
                let index = value_stack.pop().unwrap();
                let list = value_stack.pop().unwrap();
                if let Value::List(list) = list {
                    let index = extract_index(index, instruction.spans.clone())?;

                    let list = list.borrow();
                    let len = list.len();
                    let value = list.get(index).cloned().ok_or_else(|| {
                        Error::new(instruction.spans.clone(), IndexOutOfBounds { len, index })
                    })?;
                    value_stack.push(value);
                } else {
                    todo!()
                }
            },
            // .unwrap() helps us verify that exactly the right number of values are produced
            // and popped through the program
            InstructionKind::Drop => {
                value_stack.pop().unwrap();
            },
            InstructionKind::Binary(op) => {
                if let Err(err) = exec_binary_instruction(*op, value_stack) {
                    return Err(err.into_error(instruction.spans.clone()));
                }
            },
            InstructionKind::Unary(op) => {
                if let Err(err) = exec_unary_instruction(*op, value_stack) {
                    return Err(err.into_error(instruction.spans.clone()));
                }
            },
            InstructionKind::Call(func) => match func {
                Func::User(call) => {
                    call_stack.push(Frame::new((
                        instruction_pointer.0,
                        instruction_pointer.1 + 1,
                    )));
                    check_stack_overflow(&instruction.spans, call_stack)?;
                    *instruction_pointer = (call.chunk, 0);
                    return Ok(ControlFlow::Jump);
                },
                Func::Builtin(call) => {
                    let args = value_stack.split_off(value_stack.len() - call.num_given);
                    let value = call
                        .builtin
                        .eval(self.trig_mode, &mut args.into_iter())
                        .map_err(|err| from_builtin_error(err, instruction.spans.clone()))?;
                    value_stack.push(value);
                },
            },
            InstructionKind::CallDerivative(func, derivatives) => match func {
                Func::User(call) => {
                    let initial = value_stack.pop().unwrap();
                    let derivative = Derivative::new(*derivatives, initial)
                        .map_err(|err| err.into_error(instruction.spans.clone()))?;
                    call_stack.push(Frame::new((
                        instruction_pointer.0,
                        instruction_pointer.1 + 1,
                    )).with_derivative());
                    check_stack_overflow(&instruction.spans, call_stack)?;
                    value_stack.push(derivative.next_eval().unwrap());
                    derivative_stack.push(derivative);
                    *instruction_pointer = (call.chunk, 0);
                    return Ok(ControlFlow::Jump);
                },
                Func::Builtin(call) => {
                    let initial = value_stack.pop().unwrap();
                    let value = Derivative::new(*derivatives, initial)
                        .and_then(|mut derv| derv.eval_builtin(call.builtin))
                        .map_err(|err| err.into_error(instruction.spans.clone()))?;
                    value_stack.push(value);
                },
            },
            InstructionKind::Return => {
                let frame = call_stack.pop().unwrap();

                if frame.derivative {
                    // progress the derivative computation if needed
                    let derivative = derivative_stack.last_mut().unwrap();
                    let value = value_stack.pop().unwrap();
                    if let Some(value) = derivative.advance(value)
                        .map_err(|err| err.into_error(instruction.spans.clone()))?
                    {
                        value_stack.push(value);
                        *instruction_pointer = (frame.return_instruction.0, frame.return_instruction.1);
                    } else {
                        // back to the top; put the stack frame back
                        // no need to check for stack overflow here, since we're not adding a new frame
                        call_stack.push(frame);

                        value_stack.push(derivative.next_eval().unwrap());
                        *instruction_pointer = (instruction_pointer.0, 0);
                    }
                } else {
                    // return to the previous caller
                    *instruction_pointer = frame.return_instruction;
                }

                return Ok(ControlFlow::Jump);
            }
            InstructionKind::Jump(label) => {
                *instruction_pointer = self.labels[label];
                return Ok(ControlFlow::Jump);
            },
            InstructionKind::JumpIfFalse(label) => {
                let value = value_stack.pop().unwrap();
                if let Value::Boolean(b) = value {
                    if !b {
                        *instruction_pointer = self.labels[label];
                        return Ok(ControlFlow::Jump);
                    }
                } else {
                    todo!()
                }
            },
        }

        Ok(ControlFlow::Continue)
    }

    /// Executes the bytecode instructions.
    pub fn run(&mut self) -> Result<Value, Error> {
        let mut call_stack = vec![Frame::new((0, 0)).with_variables(std::mem::take(&mut self.variables))];
        let mut derivative_stack = vec![];
        let mut value_stack = vec![];
        let mut instruction_pointer = (0, 0);

        while instruction_pointer.1 < self.chunks[instruction_pointer.0].instructions.len() {
            match self.run_one(
                &mut value_stack,
                &mut call_stack,
                &mut derivative_stack,
                &mut instruction_pointer,
            ).inspect_err(|_| {
                // get the variables from the global stack frame, regardless of whether it changed
                // since we started running the program
                // this behavior is consistent with other VMs
                self.variables = std::mem::take(&mut call_stack[0].variables);
            })? {
                ControlFlow::Continue => instruction_pointer.1 += 1,
                ControlFlow::Jump => (),
            }
        }

        assert_eq!(value_stack.len(), 1);
        assert_eq!(call_stack.len(), 1);

        self.variables = call_stack.pop().unwrap().variables;
        Ok(value_stack.pop().unwrap())
    }
}

/// A virtual machine that can compile and execute bytecode instructions in a REPL-like environment
/// by maintaining state.
#[derive(Debug, Default)]
pub struct ReplVm {
    /// Compiler used to hold the current state of the VM.
    compiler: Compiler,

    /// The current VM state.
    vm: Vm,
}

impl ReplVm {
    /// Creates a new [`ReplVm`] with the default context.
    pub fn new() -> Self {
        Self::default()
    }

    /// Compiles the given source code and executes it, updating the VM state.
    pub fn execute(&mut self, stmts: Vec<Stmt>) -> Result<Value, Error> {
        // TODO: this feels kinda hacky
        let compiler_clone = self.compiler.clone();
        let vm_clone = self.vm.clone();

        self.compiler.chunks = std::mem::replace(&mut self.vm.chunks, vec![Default::default()]);
        self.compiler.chunks[0].instructions.clear();
        self.compiler.labels = std::mem::take(&mut self.vm.labels)
            .into_iter()
            .map(|(label, location)| (label, Some(location)))
            .collect();

        compile_stmts(&stmts, &mut self.compiler).inspect_err(|_| {
            // restore the previous state of the VM to ensure compilation errors don't affect the
            // current state
            self.compiler = compiler_clone;
            self.vm = vm_clone;
        })?;

        self.vm.chunks = std::mem::replace(&mut self.compiler.chunks, vec![Default::default()]);
        self.vm.labels = std::mem::take(&mut self.compiler.labels)
            .into_iter()
            .map(|(label, location)| (label, location.unwrap()))
            .collect();

        self.vm.run()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use cas_compute::{
        funcs::miscellaneous::{Abs, Factorial},
        numerical::{builtin::Builtin, value::Value},
        primitive::{float, float_from_str, int},
    };
    use cas_parser::parser::{ast::stmt::Stmt, Parser};
    use rug::ops::Pow;

    /// Compile the given source code and execute the resulting bytecode.
    fn run_program(source: &str) -> Result<Value, Error> {
        let mut parser = Parser::new(source);
        let stmts = parser.try_parse_full_many::<Stmt>().unwrap();

        let mut vm = Vm::compile_program(stmts)?;
        Ok(vm.run().unwrap())
    }

    /// Compile the given source code and execute the resulting bytecode, using degrees as the
    /// trigonometric mode.
    fn run_program_degrees(source: &str) -> Result<Value, Error> {
        let mut parser = Parser::new(source);
        let stmts = parser.try_parse_full_many::<Stmt>().unwrap();

        let mut vm = Vm::compile_program(stmts)?
            .with_trig_mode(TrigMode::Degrees);
        Ok(vm.run().unwrap())
    }

    #[test]
    fn binary_expr() {
        let result = run_program("1 + 2").unwrap();
        assert_eq!(result, Value::Integer(int(3)));
    }

    #[test]
    fn binary_expr_2() {
        let result = run_program("1 + 2 * 3").unwrap();
        assert_eq!(result, Value::Integer(int(7)));
    }

    #[test]
    fn binary_and_unary() {
        let result = run_program("3 * -5 / 5! + 6").unwrap();
        assert_eq!(result, Value::Float(float(5.875)));
    }

    #[test]
    fn parenthesized() {
        let result = run_program("((1 + 9) / 5) * 3").unwrap();
        assert_eq!(result, Value::Integer(int(6)));
    }

    #[test]
    fn degree_to_radian() {
        let result = run_program_degrees("90 * 2 * pi / 360").unwrap();
        assert_eq!(result, Value::Float(float(&*PI) / 2));
    }

    #[test]
    fn precision() {
        let result = run_program("e^2 - tau").unwrap();
        assert_eq!(result, Value::Float(float(&*E).pow(2) - float(&*TAU)));
    }

    #[test]
    fn precision_2() {
        let result = run_program("pi^2 * 17! / -4.9 + e").unwrap();

        let fac_17 = if let Value::Integer(fac_17) = Factorial::eval_static(float(17)) {
            fac_17
        } else {
            unreachable!("factorial of 17 is an integer")
        };
        let expected = float(&*PI).pow(2) * fac_17 / -float_from_str("4.9") + float(&*E);
        assert_eq!(result, Value::Float(expected));
    }

    #[test]
    fn func_call() {
        let source = [
            ("f(x) = x^2 + 5x + 6", Value::Unit),
            ("f(7)", 90.into()),
        ];

        let mut vm = ReplVm::new();
        for (stmt, expected) in source {
            let mut parser = Parser::new(stmt);
            let stmt = parser.try_parse_full::<Stmt>().unwrap();
            assert_eq!(vm.execute(vec![stmt]).unwrap(), expected);
        }
    }

    #[test]
    fn complicated_func_call() {
        let source = [
            ("f(n = 3, k = 6) = n * k", Value::Unit),
            ("f()", 18.into()),
            ("f(9)", 54.into()),
            ("f(8, 14)", 112.into()),
        ];

        let mut vm = ReplVm::new();
        for (stmt, expected) in source {
            println!("executing: {}", stmt);
            let mut parser = Parser::new(stmt);
            let stmt = parser.try_parse_full::<Stmt>().unwrap();
            assert_eq!(vm.execute(vec![stmt]).unwrap(), expected);
        }
    }

    #[test]
    fn builtin_func_arg_check() {
        assert_eq!(Abs.eval(Default::default(), &mut [Value::from(4.0)].into_iter()).unwrap().coerce_float(), 4.0.into());
        assert!(Abs.eval(Default::default(), &mut [Value::Unit].into_iter()).is_err());
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
        let result = run_program("a = 0
while a < 10 {
    a += 1
}; a").unwrap();
        assert_eq!(result, Value::Integer(int(10)));
    }

    #[test]
    fn exec_dumb_loop() {
        let result = run_program("while true break").unwrap();
        assert_eq!(result, Value::Unit);
    }

    #[test]
    fn exec_loop_with_conditions() {
        let result = run_program("a = 0
j = 2
while a < 10 && j < 15 {
    if a < 5 {
        a += 2
    } else {
        a += 1
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
    fn exec_trig_mode() {
        let result_1 = run_program("sin(pi/2)").unwrap();
        let result_2 = run_program_degrees("sin(90)").unwrap();
        assert_eq!(result_1.coerce_float(), Value::Float(float(1)));
        assert_eq!(result_2.coerce_float(), Value::Float(float(1)));
    }

    #[test]
    fn exec_factorial() {
        let result = run_program("n = result = 8
loop {
    n -= 1
    result *= n
    if n <= 1 break result
}").unwrap();
        assert_eq!(result, Value::Integer(int(40320)));
    }

    #[test]
    fn exec_partial_factorial() {
        let result = run_program("partial_factorial(n, k) = {
    result = 1
    while n > k {
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
c = 0
sum = 0
while c < n {
    c += 1
    if c & 1 == 1 continue
    sum += c
}; sum").unwrap();
        assert_eq!(result, Value::Integer(int(10100)));
    }

    #[test]
    fn exec_list_index() {
        let result = run_program("arr = [1, 2, 3]
arr[0] = 5
arr[0] + arr[1] + arr[2] == 10").unwrap();
        assert_eq!(result, Value::Boolean(true));
    }

    #[test]
    fn exec_inline_indexing() {
        let result = run_program("[1, 2, 3][im(2sqrt(-1))]").unwrap();
        assert_eq!(result, Value::Integer(int(3)));
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
    if x < 0 return -x
    x
}
f(-5)").unwrap();
        assert_eq!(result, Value::Integer(int(5)));
    }

    #[test]
    fn exec_unit_mess() {
        let result = run_program("f() = {}
f()").unwrap();
        assert_eq!(result, Value::Unit);
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
    fn example_convert_binary() {
        let source = include_str!("../../examples/convert_binary.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result, vec![true.into(); 7].into());
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
    fn example_memoized_fib() {
        let source = include_str!("../../examples/memoized_fib.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result, 6_557_470_319_842.into());
    }

    #[test]
    fn example_ncr() {
        let source = include_str!("../../examples/ncr.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result, true.into());
    }

    #[test]
    fn example_prime_notation() {
        let source = include_str!("../../examples/prime_notation.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result, vec![true.into(); 15].into());
    }

    #[test]
    fn example_resolving_calls() {
        let source = include_str!("../../examples/resolving_calls.calc");
        let result = run_program(source).unwrap();
        assert_eq!(result, true.into());
    }

    #[test]
    fn repl() {
        // ensure REPL state is restored if compilation fails
        let source = [
            "f() = x", // compile error: `x` is not defined
            "f()", // compile error: `f` is not defined
        ];

        let mut vm = ReplVm::new();
        for stmt in &source {
            let mut parser = Parser::new(stmt);
            let stmt = parser.try_parse_full::<Stmt>().unwrap();
            vm.execute(vec![stmt]).unwrap_err();
        }
    }
}
