use cas_compute::numerical::value::Value;
use std::collections::HashMap;

/// A stack frame in a call stack.
#[derive(Debug)]
pub struct Frame {
    /// The instruction to return to when this frame is popped, given as a 2-tuple of the chunk
    /// index and instruction index.
    pub return_instruction: (usize, usize),

    /// The variables stored in this frame.
    pub variables: HashMap<usize, Value>,
}

impl Frame {
    /// Creates a new [`Frame`] with the given return instruction.
    pub fn new(return_instruction: (usize, usize)) -> Self {
        Self {
            return_instruction,
            variables: HashMap::new(),
        }
    }

    /// Adds a variable to the frame.
    pub fn add_variable(&mut self, index: usize, value: Value) {
        self.variables.insert(index, value);
    }

    /// Gets a variable from the frame.
    pub fn get_variable(&self, index: usize) -> Option<&Value> {
        self.variables.get(&index)
    }
}
