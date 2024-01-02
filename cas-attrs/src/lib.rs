mod builtin;
mod error_kind;

use builtin::{Builtin, Radian};
use error_kind::ErrorKindTarget;
use proc_macro::TokenStream;
use quote::quote;
use syn::parse_macro_input;

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
    quote! { #target }.into()
}

/// An attribute that implements `cas_compute`'s `Builtin` trait on `struct`s representing
/// functions.
///
/// This attribute can be applied to any `struct` that has a static implementation through an
/// associated function called `eval_static`. The attribute will use this function in the
/// implementation of the `Builtin` trait to perform runtime type-checking of the arguments passed
/// to the function.
///
/// The `eval_static` method can be implemented like any Rust function, with some limitations to
/// the types of its parameters, to match the types that [`Value`] provides. These are the accepted
/// types:
///
/// | Type      | Description                                                                                                              |
/// | --------- | ------------------------------------------------------------------------------------------------------------------------ |
/// | `Float`   | [`rug::Float`]: A floating-point value. Floats can freely coerce to `Complex`.                                           |
/// | `Integer` | [`rug::Integer`]: An integer value. Integers can freely coerce to `Complex` or `Float`.                                  |
/// | `Complex` | [`rug::Complex`]: A complex number value. Complex numbers can coerce to `Float` or `Integer` if the imaginary part is 0. |
/// | `bool`    | [`bool`]: A boolean value.                                                                                               |
/// | `()`      | [`()`]: The unit type, analogous to `()` in Rust.                                                                        |
/// | `Value`   | Any value, regardless of type. The value will be left as a [`Value`] for the function to handle.                         |
///
/// In addition, any of these types can be wrapped in an [`Option`] to make the argument optional.
/// Optional arguments should be placed at the end of the list of parameters, though the attribute
/// does not enforce this.
///
/// For trigonometric functions, the attribute can be used to indicate that the function takes
/// input in radians, or returns an output in radians. This is done by adding the `radian` tag to
/// the attribute, with the value `input` or `output`. If the user's trigonometric mode does not
/// match the function's declared mode (i.e. the user is in degree mode), the input or output will
/// be automatically converted to the correct mode. See the example below for more context.
///
/// # Examples
///
/// ```no_compile
/// extern crate cas_attrs;
///
/// use cas_attrs::builtin;
/// use cas_compute::primitive::{complex, float};
/// use cas_compute::numerical::{
///     ctxt::{Ctxt, TrigMode},
///     error::kind::{MissingArgument, TooManyArguments, TypeMismatch},
/// };
/// use rug::{Complex, Float};
///
/// /// Returns the absolute value of a floating-point number.
/// pub struct Abs;
///
/// #[builtin]
/// impl Abs {
///     pub fn eval_static(n: Float) -> Float {
///         n.abs()
///     }
/// }
///
/// /// Returns the logarithm to an arbitrary base.
/// pub struct Log;
///
/// #[builtin]
/// impl Log {
///     pub fn eval_static(n: Complex, base: Option<Complex>) -> Complex {
///         let base = base.unwrap_or(complex(10));
///         n.ln() / base.ln()
///     }
/// }
///
/// /// Returns the arcsine of a value.
/// pub struct Asin;
///
/// // the `asin` function on the `rug` crate always returns radians, so we annotate this function
/// // with `#[builtin(radian = output)]` to indicate this
/// //
/// // if the context is in degree mode, the output of this function is automatically converted to
/// // degrees
/// #[builtin(radian = output)]
/// impl Asin {
///     pub fn eval_static(n: Complex) -> Complex {
///        n.asin()
///     }
/// }
/// ```
///
/// [`Value`]: cas_compute::numerical::value::Value
 // NOTE: this cannot be a derive macro, since we need to know information about the function
 // signature; applying #[derive(Builtin)] to the marker struct does not provide that information
#[proc_macro_attribute]
pub fn builtin(attrs: TokenStream, item: TokenStream) -> TokenStream {
    let radian = parse_macro_input!(attrs as Radian);
    let builtin = parse_macro_input!(item as Builtin);
    builtin.generate(radian).into()
}
