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
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    /// A number value. Numbers can freely coerce to [`Value::Complex`].
    Number,

    /// A complex number value. Complex numbers can coerce to [`Value::Number`] if the imaginary
    /// part is 0.
    Complex,

    /// The unit type, analogous to `()` in Rust.
    Unit,

    /// Any type.
    Any,
}

impl Parse for Type {
    fn parse(input: ParseStream) -> Result<Self> {
        match &*input.parse::<Ident>()?.to_string() {
            "Number" => Ok(Type::Number),
            "Complex" => Ok(Type::Complex),
            "Unit" => Ok(Type::Unit),
            "Any" => Ok(Type::Any),
            _ => Err(input.error("expected `Number`, `Complex`, `Unit`, or `Any`")),
        }
    }
}

impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Type::Number => tokens.extend(quote! { Number }),
            Type::Complex => tokens.extend(quote! { Complex }),
            Type::Unit => tokens.extend(quote! { Unit }),
            _ => tokens.extend(quote! {}),
        }
    }
}

/// A trigonometric unit.
#[derive(Clone, Copy, Debug)]
pub enum Unit {
    Radians,
    Degrees,
}

impl Parse for Unit {
    fn parse(input: ParseStream) -> Result<Self> {
        match &*input.parse::<Ident>()?.to_string() {
            "radians" => Ok(Unit::Radians),
            "degrees" => Ok(Unit::Degrees),
            _ => Err(input.error("expected `radians` or `degrees`")),
        }
    }
}

impl ToTokens for Unit {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Unit::Radians => tokens.extend(quote! { Radians }),
            Unit::Degrees => tokens.extend(quote! { Degrees }),
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

    /// The trigonometric unit this parameter should be in, in order to work with the `rug` crate's
    /// built-in trigonometric functions.
    pub unit: Option<Unit>,

    /// An optional default value for the parameter, if the user does not provide one.
    pub default: Option<Expr>,
}

impl Parse for Param {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident = input.parse::<Ident>()?;
        input.parse::<Token![:]>()?;
        let ty = input.parse::<Type>()?;
        let unit = if input.peek(Ident) {
            input.parse::<Unit>().ok()
        } else {
            None
        };
        let default = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            input.parse::<Expr>().ok()
        } else {
            None
        };

        Ok(Param {
            ident,
            ty,
            unit,
            default,
        })
    }
}

/// The arguments that can be passed to the `args` attribute.
#[derive(Debug, Default)]
pub struct Args {
    /// The patterns that the arguments must match.
    pub params: Vec<Param>,

    /// The output unit of the function.
    pub unit: Option<Unit>,
}

impl Args {
    /// Parse the next argument in the input stream and apply it to itself.
    fn parse_arg(&mut self, input: ParseStream) -> Result<()> {
        self.params.push(input.parse::<Param>()?);
        Ok(())
    }

    /// Generates the output struct for the `ErrorKind` derive macro.
    pub fn build_struct(&self, item: &ItemFn) -> TokenStream2 {
        let (attrs, name, return_ty) = (&item.attrs, &item.sig.ident, &item.sig.output);
        let check_stmts = self.generate_check_stmts(item);
        let output = self.generate_output_expr(item);
        let arg_count = self.params.len();

        quote! {
            #(#attrs)*
            #[allow(non_camel_case_types)]
            #[derive(Debug, Clone, PartialEq, Eq)]
            pub struct #name;

            impl Builtin for #name {
                fn num_args(&self) -> usize { #arg_count }

                fn eval(&self, ctxt: &Ctxt, args: &[Value]) #return_ty {
                    #check_stmts
                    #output
                }
            }
        }
    }

    /// Generates the statements that typecheck the arguments.
    pub fn generate_check_stmts(&self, func: &ItemFn) -> TokenStream2 {
        let name = &func.sig.ident;
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
        // the `Number` and `Complex` types are somewhat special in that they can coerce into each
        // other if:
        // - the argument given is a `Number` and the parameter is a `Complex`
        // - the argument given is a `Complex` with an imaginary part of 0 and the parameter is a
        // `Number`
        //
        // TODO: arguments are cloned, which may or may not be ideal
        let type_checkers = self.params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let (ident, ty) = (&param.ident, &param.ty);
                if *ty == Type::Any {
                    return quote! {
                        let #ident = args.get(#i).cloned()
                            .ok_or_else(|| BuiltinError::MissingArgument(MissingArgument {
                                name: stringify!(#name).to_owned(),
                                index: #i,
                                expected: #num_patterns,
                                given: args.len(),
                            }))?;
                    };
                }

                let unit_converter = {
                    let coercer = match param.ty {
                        Type::Number => quote! { inner_arg.coerce_real() },
                        Type::Complex => quote! { inner_arg.coerce_complex() },
                        _ => quote! { inner_arg },
                    };

                    match &param.unit {
                        Some(unit) => {
                            let x = match unit {
                                Unit::Radians => quote! { .to_radians() },
                                Unit::Degrees => quote! { .to_degrees() },
                            };

                            quote! {
                                if ctxt.trig_mode != TrigMode::#unit {
                                    #coercer #x
                                } else {
                                    #coercer
                                }
                            }
                        },
                        None => quote! { #coercer },
                    }
                };
                let coerce_getter_expr = quote! { args.get(#i).cloned().map(|inner_arg| #unit_converter) };
                let default_expr = match &param.default {
                    Some(expr) => quote! { #expr },
                    None => quote! {
                        return Err(BuiltinError::MissingArgument(MissingArgument {
                            name: stringify!(#name).to_owned(),
                            index: #i,
                            expected: #num_patterns,
                            given: args.len(),
                        }));
                    },
                };

                quote! {
                    let #ident = match #coerce_getter_expr {
                        Some(#ty(#ident)) => #ident,
                        Some(_) => {
                            return Err(BuiltinError::TypeMismatch(TypeMismatch {
                                name: stringify!(#name).to_owned(),
                                index: #i,
                                expected: stringify!(#ty),
                                given: args[#i].typename(),
                            }));
                        },
                        None => { #default_expr },
                    };
                }
            });

        quote! {
            if args.len() > #num_patterns {
                return Err(BuiltinError::TooManyArguments(TooManyArguments {
                    name: stringify!(#name).to_owned(),
                    expected: #num_patterns,
                    given: args.len(),
                }));
            }
            #( #type_checkers )*
        }
    }

    /// Generates the output expression for the function.
    pub fn generate_output_expr(&self, item: &ItemFn) -> TokenStream2 {
        let block = &item.block;
        let Some(unit) = self.unit else {
            return quote! { #block };
        };

        // notice that the `converter` functions are opposite of the `mode` given
        // `mode` gives us the default mode of the function. if the context is in the default mode, we
        // don't need to worry about the conversion; if not, we need to convert the output to the other
        // trig mode
        let converter = match unit {
            Unit::Radians => quote! { .to_degrees() },
            Unit::Degrees => quote! { .to_radians() },
        };

        quote! {
            { #block }.map(|value| {
                if ctxt.trig_mode != TrigMode::#unit {
                    value #converter
                } else {
                    value
                }
            })
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

        if input.parse::<Token![->]>().is_ok() {
            args.unit = input.parse::<Unit>().ok();
        }

        Ok(args)
    }
}
