mod structures;

use proc_macro::TokenStream;
use quote::quote;
use structures::ErrorKindTarget;
use syn::parse_macro_input;

/// Derives the [`ErrorKind`] trait for the given struct.
///
/// This trait can be derived for any kind of struct.
///
/// The information of the error can be customized using the `error` attribute by adding the
/// corresponding tags to it:
/// ```
/// use cas_attrs::ErrorKind;
/// use cas_parser::parser::error::kind::ErrorKind;
///
/// #[derive(Debug, ErrorKind)]
/// #[error(message = "unexpected end of file", label = "add something here")]
/// pub struct Foo;
/// ```
///
/// The following tags are available:
///
/// | Tag         | Description                                                                  |
/// | ----------- | ---------------------------------------------------------------------------- |
/// | `message`   | The message displayed at the top of the error when it is displayed.          |
/// | `label`     | The text of the label that points to the span of the error.                  |
/// | `help`      | Optional help text for the error, describing what the user can do to fix it. |
///
/// Each tag accepts an expression that should evaluate to a [`String`]. For structs with named
/// fields, the expression is evaluated with the members of the struct in scope, so they can be
/// used in the expression (tuple structs are not supported).
#[proc_macro_derive(ErrorKind, attributes(error))]
pub fn error_kind(item: TokenStream) -> TokenStream {
    let target = parse_macro_input!(item as ErrorKindTarget);
    let name = &target.name;
    quote! {
        impl ErrorKind for #name {
            fn as_any(&self) -> &dyn std::any::Any {
                self
            }
            #target
        }
    }.into()
}
