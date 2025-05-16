use cas_compiler::register::Register;
use rug::Integer;

/// Register state of the virtual machine.
#[derive(Clone, Debug, Default)]
pub struct Registers {
    arg_counter: Integer,
}

impl Registers {
    /// Get a reference to the value of the specified register.
    pub fn get(&self, reg: Register) -> &Integer {
        match reg {
            Register::ArgCounter => &self.arg_counter,
        }
    }

    /// Set the value of the specified register.
    pub fn set(&mut self, reg: Register, value: Integer) {
        match reg {
            Register::ArgCounter => self.arg_counter = value,
        }
    }

    /// Increment the value of the specified register by 1.
    pub fn increment(&mut self, reg: Register) {
        match reg {
            Register::ArgCounter => self.arg_counter += 1,
        }
    }
}
