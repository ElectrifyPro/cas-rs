Contains the common [`ErrorKind`] trait implemented by all errors in the
`cas-rs` ecosystem.

All errors that implement this trait can be output to `stderr` in a
user-friendly way with the [`Error::report_to_stderr`] method. The
[`ErrorKind`] dervie macro is used to implement this trait with a given error
message, help message, note, and labels to apply to spans in the error report.

# Example

Here we create a custom error type and derive the [`ErrorKind`] trait for it.
Expressions in the error message are evaluated at runtime and have access to
`&self`, allowing you to use the fields of the error type in the message.

```rust
use cas_attrs::ErrorKind;
use cas_error::Error;

/// Tried to override a builtin constant.
#[derive(Debug, Clone, ErrorKind, PartialEq)]
#[error(
    message = format!("cannot override builtin constant: `{}`", self.name),
    labels = ["this variable"],
    help = "choose a different name for this variable",
    note = "builtin constants include: `i`, `e`, `phi`, `pi`, or `tau`",
)]
pub struct OverrideBuiltinConstant {
    /// The name of the variable that was attempted to be overridden.
    pub name: String,
}

let error = Error::new(
    vec![0..2], // span pointing to `pi` in the source
    OverrideBuiltinConstant {
        name: "pi".to_string(),
    },
);

// print the error to stderr with colors
error.report_to_stderr("src_id", "pi = 3").unwrap();

// or manually grab the error text (note: there is some trailing whitespace)
let error_str = {
    let mut bytes = vec![];
    error.report_to(&mut bytes, "src_id", "pi = 3").unwrap();
    strip_ansi_escapes::strip_str(String::from_utf8_lossy(&bytes))
};
assert_eq!(
    error_str,
    "Error: cannot override builtin constant: `pi`
   ╭─[src_id:1:1]
   │
 1 │ pi = 3
   │ ─┬  
   │  ╰── this variable
   │ 
   │ Help: choose a different name for this variable
   │ 
   │ Note: builtin constants include: `i`, `e`, `phi`, `pi`, or `tau`
───╯
"
);
```

[`Error::report_to_stderr`]: https://docs.rs/cas-error/latest/cas_error/struct.Error.html#method.report_to_stderr
