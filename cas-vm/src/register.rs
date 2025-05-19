use std::ops::Range;

/// Register state of the virtual machine.
#[derive(Clone, Debug, Default)]
pub struct Registers {
    /// Spans indicating the call site of the function, used to report errors at the correct
    /// location.
    pub call_site_spans: Vec<Range<usize>>,

    /// The name of the function being executed.
    pub fn_name: String,

    /// The signature of the function being executed.
    pub fn_signature: String,

    /// Counts the number of currently available arguments for the function during argument
    /// validation.
    ///
    /// When a function is called, its arguments are pushed onto the stack, and this counter is
    /// initalized to that number of arguments. The function header then executes and determines
    /// whether to push default arguments onto the stack, incrementing the counter if so.
    pub num_args: usize,

    /// The number of expected arguments for the function.
    pub num_params: usize,

    /// The number of default parameters in the function signature.
    pub num_default_params: usize,
}
