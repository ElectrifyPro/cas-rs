use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    Expr,
    Ident,
    ItemFn,
    Result,
    Token,
};

/// One of the possible types for the `Value` enum.
#[derive(Debug)]
pub enum Type {
    /// A number value.
    Number,

    /// The unit type, analogous to `()` in Rust.
    Unit,
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        match &*input.parse::<Ident>()?.to_string() {
            "Number" => Ok(Type::Number),
            "Unit" => Ok(Type::Unit),
            _ => Err(input.error("expected `Number` or `Unit`")),
        }
    }
}

impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Type::Number => tokens.extend(quote! { Number }),
            Type::Unit => tokens.extend(quote! { Unit }),
        }
    }
}

/// Represents a parameter provided in the `args` attribute.
#[derive(Debug)]
pub struct Param {
    /// The variable to bind the argument to.
    pub ident: Ident,

    /// The expected type of the parameter.
    pub ty: Type,

    /// An optional default value for the parameter, if the user does not provide one.
    pub default: Option<Expr>,
}

impl Parse for Param {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse::<Ident>()?;
        input.parse::<Token![:]>()?;
        let ty = input.parse::<Type>()?;
        let default = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            input.parse::<Expr>().ok()
        } else {
            None
        };

        Ok(Param {
            ident,
            ty,
            default,
        })
    }
}

/// The arguments that can be passed to the `args` attribute.
#[derive(Debug, Default)]
pub struct Args {
    /// The patterns that the arguments must match.
    pub params: Vec<Param>,
}

impl Args {
    /// Parse the next argument in the input stream and apply it to itself.
    fn parse_arg(&mut self, input: ParseStream) -> Result<()> {
        self.params.push(input.parse::<Param>()?);
        Ok(())
    }

    /// Generates the statements that check the arguments.
    pub fn generate_check_stmts(&self, func: &ItemFn) -> TokenStream2 {
        let (name, block) = (&func.sig.ident, &func.block);
        let num_patterns = self.params.len();

        // for each parameter, we generate a binding with three patterns to typecheck the arguments, and
        // possibly use the default expression if the argument is not provided
        //
        // the three patterns serve these purposes:
        //
        // - `Some(#ty(#ident))`: argument is provided, correct type
        // - `Some(_)`: argument is provided, incorrect type
        // - `None`: argument is not provided
        //
        // notice use of `Cow` to avoid cloning of the default expression; the argument is borrowed
        // if provided, and owned if not
        let type_checkers = self.params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let (ident, ty, default) = (&param.ident, &param.ty, &param.default);
                match default {
                    Some(default) => {
                        quote! {
                            let #ident = match args.get(#i) {
                                Some(#ty(#ident)) => std::borrow::Cow::Borrowed(#ident),
                                Some(_) => {
                                    return Err(BuiltinError::TypeMismatch(TypeMismatch {
                                        name: stringify!(#name).to_owned(),
                                        index: #i,
                                        expected: stringify!(#ty).to_string(),
                                        given: args[#i].to_string(),
                                    }));
                                },
                                None => std::borrow::Cow::Owned(#default),
                            };
                        }
                    },
                    None => {
                        quote! {
                            let #ident = match args.get(#i) {
                                Some(#ty(#ident)) => std::borrow::Cow::Borrowed(#ident),
                                Some(_) => {
                                    return Err(BuiltinError::TypeMismatch(TypeMismatch {
                                        name: stringify!(#name).to_owned(),
                                        index: #i,
                                        expected: stringify!(#ty).to_string(),
                                        given: args[#i].to_string(),
                                    }));
                                },
                                None => {
                                    return Err(BuiltinError::MissingArgument(MissingArgument {
                                        name: stringify!(#name).to_owned(),
                                        index: #i,
                                        expected: #num_patterns,
                                        given: args.len(),
                                    }));
                                },
                            };
                        }
                    },
                }
            });

        quote! {
            #( #type_checkers )*
            #block
        }
    }
}

impl Parse for Args {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut args = Args::default();

        while args.parse_arg(input).is_ok() {
            if input.parse::<Token![,]>().is_err() {
                break;
            }
        }

        Ok(args)
    }
}
