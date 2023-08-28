mod args;
mod error_kind;

use args::{Args, Unit};
use error_kind::ErrorKindTarget;
use proc_macro::TokenStream;
use quote::quote;
use syn::{ItemFn, parse_macro_input};

/// Derives the [`ErrorKind`] trait, provided in the `cas_error` crate, for the given item.
///
/// This trait can be derived for any kind of `struct` and `enum`.
///
/// The information of the error can be customized using the `error` attribute by adding the
/// corresponding tags to it:
/// ```
/// use cas_attrs::ErrorKind;
/// use cas_error::ErrorKind;
///
/// #[derive(Debug, ErrorKind)]
/// #[error(message = "unexpected end of file", labels = ["add something here"])]
/// pub struct Foo;
/// ```
///
/// The following tags are available:
///
/// | Tag         | Description                                                                                                                                                      |
/// | ----------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
/// | `message`   | The message displayed at the top of the error when it is displayed.                                                                                              |
/// | `labels`    | A list of labels that point to the spans of the error. The first label will be associated with the first span, the second label with the second span, and so on. |
/// | `help`      | Optional help text for the error, describing what the user can do to fix it.                                                                                     |
///
/// The `message` and `help` tags accept an expression that can be converted to a [`String`], and
/// the `labels` tag accepts an expression that can be converted to a [`Vec`] of [`String`]s. Each
/// expression is actually evaluated within an associated function with access to `$self`, so the
/// expression can use the members of the struct or determine which variant of the enum is being
/// used in the output.
///
/// [`ErrorKind`]: cas_error::ErrorKind
#[proc_macro_derive(ErrorKind, attributes(error))]
pub fn error_kind(item: TokenStream) -> TokenStream {
    let target = parse_macro_input!(item as ErrorKindTarget);
    let name = &target.name;
    quote! {
        impl ErrorKind for #name {
            #target
        }
    }.into()
}

/// An attribute that implements runtime type-checking for the function it is applied to, intended
/// for use in `cas_eval` builtins.
///
/// This attribute can be applied to any function that takes a [`Ctxt`] and a slice of [`Value`]s
/// as its argument, and returns a `Result<Value, BuiltinError>`. It will check that the number of
/// arguments given to the function is correct, and that the types of the arguments are correct.
///
/// The attribute accepts a comma-separated list of parameters in the form
///
/// `<name>: <type> ['radians' | 'degrees'] [= <default value>]`.
///
/// The name of the parameter must be a valid Rust identifier to bind to, and the type must be one
/// of the following:
///
/// | Type      | Description                                                                                         |
/// | --------- | --------------------------------------------------------------------------------------------------- |
/// | `Number`  | A number value. Numbers can freely coerce to [`Value::Complex`].                                    |
/// | `Complex` | A complex number value. Complex numbers can coerce to [`Value::Number`] if the imaginary part is 0. |
/// | `Unit`    | The unit type, analogous to `()` in Rust.                                                           |
///
/// The `radians` and `degrees` tags are optional, and specify that the builtin function's
/// implementation expects the inputs to be in radians or degrees, respectively. If the context's
/// trigonometric mode does not match the tag, the inputs will be converted to the correct mode
/// before being passed to the function. If neither tag is given, the input will not be changed.
///
/// Parameters can be given default values by using the `= <value>` syntax after the pattern. The
/// default value will be used if the argument is not given.
///
/// # Examples
///
/// ```
/// extern crate cas_attrs;
///
/// use cas_attrs::args;
/// use cas_eval::{
///     builtins::error::BuiltinError,
///     consts::float,
///     ctxt::Ctxt,
///     error::kind::{MissingArgument, TooManyArguments, TypeMismatch},
///     value::Value::{self, *},
/// };
///
/// /// Returns the absolute value of a number.
/// #[args(n: Number)]
/// fn abs(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
///    // if the argument is not a number, this will never be executed
///    Ok(Value::Number(n.abs()))
/// }
///
/// /// Returns the logarithm of a number with a given base.
/// #[args(n: Number, base: Number = float(10.0))]
/// fn log(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
///     Ok(Value::Number(n.ln() / base.ln()))
/// }
/// ```
///
/// [`Value`]: cas_eval::Value
#[proc_macro_attribute]
pub fn args(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as Args);
    let item = parse_macro_input!(item as ItemFn);
    let check_stmts = args.generate_check_stmts(&item);
    let (attrs, vis, sig) = (&item.attrs, &item.vis, &item.sig);
    quote! {
        #(#attrs)*
        #vis #sig {
            #check_stmts
        }
    }.into()
}

/// An attribute that transforms the output of a builtin function based on the trigonometric mode
/// of the given context.
///
/// This attribute can be applied to any function that takes a [`Ctxt`] and a slice of [`Value`]s
/// as its argument, and returns a `Result<Value, BuiltinError>`. It will check the trigonometric
/// mode of the context, and transform the output of the function if the output of the builtin
/// function does not match the context's mode.
///
/// The attribute accepts a single identifier, either `radians` or `degrees`, which specifies the
/// default output mode of the function.
///
/// # Examples
///
/// ```
/// extern crate cas_attrs;
///
/// use cas_attrs::{args, out};
/// use cas_eval::{
///     builtins::error::BuiltinError,
///     consts::float,
///     ctxt::{Ctxt, TrigMode},
///     error::kind::{MissingArgument, TooManyArguments, TypeMismatch},
///     value::Value::{self, *},
/// };
///
/// #[args(n: Number)]
/// #[out(radians)]
/// fn asin(ctxt: &Ctxt, args: &[Value]) -> Result<Value, BuiltinError> {
///    // the `asin` function on the `rug` crate always returns radians, so we mark this function
///    // with `out(radians)`
///    //
///    // if the context is in degrees mode, this will be automatically be converted to degrees
///    Ok(Value::Number(n.sin()))
/// }
/// ```
///
/// [`Value`]: cas_eval::Value
#[proc_macro_attribute]
pub fn out(attr: TokenStream, item: TokenStream) -> TokenStream {
    let mode = parse_macro_input!(attr as Unit);
    // notice that the `converter` functions are opposite of the `mode` given
    // `mode` gives us the default mode of the function. if the context is in the default mode, we
    // don't need to worry about the conversion; if not, we need to convert the output to the other
    // trig mode
    let converter = match mode {
        Unit::Radians => quote! { .to_degrees() },
        Unit::Degrees => quote! { .to_radians() },
    };
    let item = parse_macro_input!(item as ItemFn);
    let (attrs, vis, sig, block) = (&item.attrs, &item.vis, &item.sig, &item.block);
    quote! {
        #(#attrs)*
        #vis #sig {
            { #block }.map(|value| if ctxt.trig_mode == TrigMode::#mode {
                value
            } else {
                value #converter
            })
        }
    }.into()
}
