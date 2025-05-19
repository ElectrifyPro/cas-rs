use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    spanned::Spanned,
    FnArg,
    Ident,
    ImplItem,
    ImplItemFn,
    ItemImpl,
    Result,
    ReturnType,
    Token,
};

/// Converts an [`Ident`] in `PascalCase` to `snake_case`.
fn pascal_to_snake_case(ident: &Ident) -> Ident {
    let mut snake = String::new();
    for (i, ch) in ident.to_string().chars().enumerate() {
        if ch.is_ascii_uppercase() {
            if i != 0 {
                snake.push('_');
            }
            snake.push(ch.to_ascii_lowercase());
        } else {
            snake.push(ch);
        }
    }
    syn::parse_str(&snake).unwrap()
}

/// Finds the `eval_static` function in the `impl` block.
fn find_eval_static_fn(block: &ItemImpl) -> Result<&ImplItemFn> {
    block.items.iter()
        .find_map(|item| match item {
            ImplItem::Fn(func) if func.sig.ident == "eval_static" => Some(func),
            _ => None,
        })
        .ok_or_else(|| syn::Error::new(block.span(), "expected `eval_static` function inside `impl` block"))
}

/// Get the identifier of a `Type`.
fn path_ident(path: &syn::Type) -> Result<&Ident> {
    match path {
        syn::Type::Path(path) => path.path.get_ident()
            .ok_or_else(|| syn::Error::new(path.span(), "expected identifier")),
        _ => Err(syn::Error::new(path.span(), "expected path")),
    }
}

/// One of the possible types for a parameter of a builtin function.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Type {
    /// Whether the entire value is optional.
    optional: bool,

    /// Whether the type should be borrowed.
    is_ref: bool,

    /// The kind of type.
    kind: TypeKind,
}

/// The kind of type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeKind {
    /// A floating-point number.
    Float,

    /// An integer.
    Integer,

    /// A complex number.
    Complex,

    /// A boolean.
    Bool,

    /// The unit type, akin to `()` in Rust.
    Unit,

    /// Any type.
    Value,
}

/// Helper function to match the first segment of a path in a type.
fn match_first_segment(ty: &syn::Type) -> Result<Type> {
    let path = match ty {
        syn::Type::Path(ty) => &ty.path,
        _ => return Err(syn::Error::new(ty.span(), "expected path")),
    };

    let Some(first) = path.segments.first() else {
        return Err(syn::Error::new(path.span(), "expected path"));
    };

    let ident_str = first.ident.to_string();
    if ident_str == "Option" {
        let args = match &first.arguments {
            syn::PathArguments::AngleBracketed(bracketed) if bracketed.args.len() == 1 => &bracketed.args,
            _ => return Err(syn::Error::new(first.ident.span(), "expected one angle-bracketed argument")),
        };

        let first_arg = args.first().unwrap();
        let ty = match first_arg {
            syn::GenericArgument::Type(ty) => ty,
            _ => return Err(syn::Error::new(first_arg.span(), "expected type as generic argument")),
        };
        Ok(Type {
            optional: true,
            is_ref: false,
            kind: match_first_segment(ty)?.kind,
        })
    } else {
        Ok(Type {
            optional: false,
            is_ref: false,
            kind: match &*ident_str {
                "Float" => TypeKind::Float,
                "Integer" => TypeKind::Integer,
                "Complex" => TypeKind::Complex,
                "bool" => TypeKind::Bool,
                "Value" => TypeKind::Value,
                _ => return Err(syn::Error::new(first.ident.span(), format!("expected `Float`, `Integer`, `Complex`, `bool`, or `Value`, found `{}`", ident_str))),
            },
        })
    }
}

impl TryFrom<ReturnType> for Type {
    type Error = syn::Error;

    fn try_from(ty: ReturnType) -> Result<Self> {
        match ty {
            ReturnType::Default => Ok(Type { optional: false, is_ref: false, kind: TypeKind::Unit }),
            ReturnType::Type(_, ty) => match_first_segment(&ty),
        }
    }
}

impl TryFrom<syn::Type> for Type {
    type Error = syn::Error;

    fn try_from(ty: syn::Type) -> Result<Self> {
        match ty {
            syn::Type::Reference(ty) => Ok(Type { optional: false, is_ref: true, kind: match_first_segment(&ty.elem)?.kind }),
            _ => match_first_segment(&ty),
        }
    }
}

impl Type {
    /// Returns the `rug` type corresponding to the type.
    pub fn rug_tokens(&self) -> TokenStream2 {
        let kind = match self.kind {
            TypeKind::Float => quote! { Float },
            TypeKind::Integer => quote! { Integer },
            TypeKind::Complex => quote! { Complex },
            TypeKind::Bool => quote! { bool },
            TypeKind::Unit => quote! { () },
            TypeKind::Value => quote! { Value },
        };

        let reffed = if self.is_ref {
            quote! { & #kind }
        } else {
            quote! { #kind }
        };

        if self.optional {
            quote! { Option<#reffed> }
        } else {
            reffed
        }
    }

    /// Returns the user-facing type name corresponding to the type.
    pub fn typename(&self) -> &'static str {
        match self.kind {
            TypeKind::Float => "Float",
            TypeKind::Integer => "Integer",
            TypeKind::Complex => "Complex",
            TypeKind::Bool => "Boolean",
            TypeKind::Unit => "Unit",
            TypeKind::Value => "Value",
        }
    }

    /// Returns the `Value` variant corresponding to the type.
    pub fn value_tokens(&self) -> TokenStream2 {
        match self.kind {
            TypeKind::Float => quote! { crate::numerical::value::Value::Float },
            TypeKind::Integer => quote! { crate::numerical::value::Value::Integer },
            TypeKind::Complex => quote! { crate::numerical::value::Value::Complex },
            TypeKind::Bool => quote! { crate::numerical::value::Value::Bool },
            TypeKind::Unit => quote! { crate::numerical::value::Value::Unit },
            TypeKind::Value => quote! { crate::numerical::value::Value::Value },
        }
    }
}

/// A parameter of a builtin function.
#[derive(Debug)]
pub struct Param {
    /// Whether the parameter is mutable.
    mutable: Option<Token![mut]>,

    /// The name of the parameter.
    ident: Ident,

    /// The type of the parameter.
    ty: Type,
}

impl TryFrom<FnArg> for Param {
    type Error = syn::Error;

    fn try_from(arg: FnArg) -> Result<Self> {
        match arg {
            FnArg::Typed(pat) => {
                let (mutable, ident) = match *pat.pat {
                    syn::Pat::Ident(pat) => (pat.mutability, pat.ident),
                    _ => return Err(syn::Error::new(pat.pat.span(), "expected identifier")),
                };
                let ty = Type::try_from(*pat.ty)?;
                Ok(Param { mutable, ident, ty })
            },
            FnArg::Receiver(_) => Err(syn::Error::new(arg.span(), "expected argument")),
        }
    }
}

impl ToTokens for Param {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self { mutable, ident, ty } = self;
        let ty = ty.rug_tokens();
        tokens.extend(quote! { #mutable #ident: #ty });
    }
}

/// Determines whether a function's input / output should be treated as a trigonometric angle.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Radian {
    /// The function takes a radian input.
    Input,

    /// The function has a radian output.
    Output,

    /// The function takes no radian input or has no radian output.
    None,
}

impl Parse for Radian {
    fn parse(input: ParseStream) -> Result<Self> {
        if let Ok(radian) = input.parse::<Ident>() {
            if radian != "radian" {
                return Ok(Radian::None);
            }
        } else {
            return Ok(Radian::None);
        }

        input.parse::<Token![=]>()?;

        let kind = input.parse::<Ident>()?;
        match kind.to_string().as_str() {
            "input" => Ok(Radian::Input),
            "output" => Ok(Radian::Output),
            _ => Err(syn::Error::new(kind.span(), "expected `input` or `output`")),
        }
    }
}

/// The function the `builtin` attribute is applied to.
#[derive(Debug)]
pub struct Builtin {
    /// The original `impl` block.
    item: ItemImpl,

    /// The lowercase name of the function.
    name: Ident,

    /// The `PascalCase` name of the function.
    pascal_name: Ident,

    /// The parameters of the function.
    params: Vec<Param>,
}

impl Builtin {
    /// Returns a string representation of the function's signature.
    pub fn signature(&self) -> String {
        let params = self.params.iter().map(|param| {
            let ty = param.ty.typename();
            if param.ty.optional {
                format!("{}: {} (optional)", param.ident, ty)
            } else {
                format!("{}: {}", param.ident, ty)
            }
        })
            .collect::<Vec<_>>()
            .join(", ");
        format!("{}({})", self.name, params)
    }

    /// Generates the statements that typecheck the arguments.
    pub fn generate_check_stmts(&self, radian: Radian) -> TokenStream2 {
        let Self { name, params, .. } = self;
        let num_params = params.len();
        let missing_arg_boundary = params.iter()
            .position(|param| param.ty.optional)
            .unwrap_or(params.len());

        // for each parameter, we generate a binding with three patterns to typecheck the arguments, and
        // possibly use the default expression if the argument is not provided
        //
        // the three patterns serve these purposes:
        //
        // - `Some(#ty(#ident))`: argument is provided, correct type
        // - `Some(_)`: argument is provided, incorrect type
        // - `None`: argument is not provided
        //
        // the `Float`, `Integer` and `Complex` types are somewhat special in that they can coerce
        // into each other if:
        // - the argument given is a `Float` / `Integer` and the parameter is a `Complex`
        // - the argument given is a `Complex` with an imaginary part of 0 and the parameter is a
        // `Float` / `Integer`
        // - the argument given is an `Integer` and the parameter is a `Float`
        // - the argument given is a `Float` with a fractional part of 0 and the parameter is an
        // `Integer`
        //
        // TODO: arguments are cloned, which may or may not be ideal
        let type_checkers = self.params
            .iter()
            .enumerate()
            .map(|(i, param)| {
                let (ident, ty) = (&param.ident, &param.ty);

                // accept argument of any type
                if ty.kind == TypeKind::Value {
                    return quote! {
                        let #ident = args.next()
                            .ok_or_else(|| crate::numerical::builtin::error::BuiltinError::MissingArgument(crate::numerical::builtin::error::check::MissingArgument {
                                name: stringify!(#name),
                                indices: #i..#missing_arg_boundary,
                                expected: #num_params,
                                given: arg_count,
                                signature: self.sig_str(),
                            }))?;
                        // TODO: previously, there was this comment throughout this function:
                        // "`cas-compiler` can verify this condition at compile time, making [the
                        // MissingArgument and TooManyArguments] errors impossible"
                        // now, with support for higher-order functions, this is no longer true;
                        // `cas-compiler` cannot verify this condition at compile time and it must
                        // be done at runtime
                    };
                }

                let base_call = quote! { args.next() };

                // coerce real / complex numbers into each other if necessary
                let type_coerce_expr = match param.ty.kind {
                    TypeKind::Float => Some(quote! { .map(|arg| arg.coerce_float()) }),
                    TypeKind::Integer => Some(quote! { .map(|arg| arg.coerce_integer()) }),
                    TypeKind::Complex => Some(quote! { .map(|arg| arg.coerce_complex()) }),
                    _ => None,
                };

                // convert input to the correct trigonometric mode if necessary
                let trig_convert_expr = if radian == Radian::Input {
                    Some(quote! {
                        .map(|arg| {
                            if trig_mode == crate::numerical::trig_mode::TrigMode::Degrees {
                                arg.into_radians()
                            } else {
                                arg
                            }
                        })
                    })
                } else {
                    None
                };

                let full_getter = quote! { #base_call #type_coerce_expr #trig_convert_expr };

                // if the parameter is optional, adjust the pattern matching
                let (received_type, none_branch) = if ty.optional {
                    (
                        quote! { Some(#ident) },
                        quote! { None },
                    )
                } else {
                    (
                        quote! { #ident },
                        quote! {
                            return Err(crate::numerical::builtin::error::BuiltinError::MissingArgument(crate::numerical::builtin::error::check::MissingArgument {
                                name: stringify!(#name),
                                indices: #i..#missing_arg_boundary,
                                expected: #num_params,
                                given: arg_count,
                                signature: self.sig_str(),
                            }));
                        },
                    )
                };
                let user_ty = ty.typename();
                let ty = ty.value_tokens();

                quote! {
                    let #ident = match #full_getter {
                        Some(#ty(#ident)) => #received_type,
                        Some(bad_value) => {
                            return Err(crate::numerical::builtin::error::BuiltinError::TypeMismatch(crate::numerical::builtin::error::check::TypeMismatch {
                                name: stringify!(#name),
                                index: #i,
                                expected: #user_ty,
                                given: bad_value.typename(),
                                signature: self.sig_str(),
                            }));
                        },
                        None => { #none_branch },
                    };
                }
            });

        quote! {
            #( #type_checkers )*
            if arg_count > #num_params {
                return Err(crate::numerical::builtin::error::BuiltinError::TooManyArguments(crate::numerical::builtin::error::check::TooManyArguments {
                    name: stringify!(#name),
                    expected: #num_params,
                    given: arg_count,
                    signature: self.sig_str(),
                }));
            }
        }
    }

    /// Generates the output expression for the function.
    pub fn generate_call(&self, radian: Radian) -> TokenStream2 {
        let pascal_name = &self.pascal_name;
        let param_idents = self.params.iter().map(|param| {
            let ident = &param.ident;
            if param.ty.is_ref {
                quote! { &#ident }
            } else {
                quote! { #ident }
            }
        });
        let make_value = quote! { crate::numerical::value::Value::from(
            #pascal_name::eval_static(#(#param_idents),*)
        ) };

        if radian == Radian::Output {
            quote! {
                if trig_mode == crate::numerical::trig_mode::TrigMode::Degrees {
                    Ok(#make_value.into_degrees())
                } else {
                    Ok(#make_value)
                }
            }
        } else {
            quote! { Ok(#make_value) }
        }
    }

    fn impl_static(&self) -> TokenStream2 {
        let Self { item, .. } = self;
        quote! { #item }
    }

    /// Generate the implementation of the `Builtin` trait for the function.
    fn impl_builtin(&self, radian: Radian) -> TokenStream2 {
        let Self { name, pascal_name, params, .. } = self;
        let type_checkers = self.generate_check_stmts(radian);
        let sig = params.iter()
            .map(|param| {
                let name = &param.ident;
                let kind = match param.ty.optional {
                    true => quote! { crate::numerical::builtin::ParamKind::Optional },
                    false => quote! { crate::numerical::builtin::ParamKind::Required },
                };
                let typename = param.ty.typename();
                quote! {
                    crate::numerical::builtin::BuiltinParam {
                        name: stringify!(#name),
                        kind: #kind,
                        typename: Some(#typename),
                    }
                }
            })
            .collect::<Vec<_>>();
        let sig_str = self.signature();
        let call = self.generate_call(radian);

        quote! {
            impl crate::numerical::builtin::Builtin for #pascal_name {
                fn name(&self) -> &'static str { stringify!(#name) }

                fn sig(&self) -> &'static [crate::numerical::builtin::BuiltinParam] { &[#( #sig ),*] }

                fn sig_str(&self) -> &'static str { #sig_str }

                fn eval(
                    &self,
                    trig_mode: crate::numerical::trig_mode::TrigMode,
                    args: std::vec::Vec<crate::numerical::value::Value>,
                ) -> Result<crate::numerical::value::Value, crate::numerical::builtin::error::BuiltinError> {
                    let arg_count = args.len();
                    let mut args = args.into_iter();
                    #type_checkers
                    #call
                }
            }
        }
    }

    /// Generate the output of the `builtin` attribute.
    pub fn generate(&self, radian: Radian) -> TokenStream2 {
        let static_impl = self.impl_static();
        let builtin_impl = self.impl_builtin(radian);
        quote! {
            #static_impl
            #builtin_impl
        }
    }
}

impl Parse for Builtin {
    fn parse(input: ParseStream) -> Result<Self> {
        let item = input.parse::<ItemImpl>()?;
        let pascal_name = path_ident(&item.self_ty)?.clone();
        let name = pascal_to_snake_case(&pascal_name);
        let eval_static_fn = find_eval_static_fn(&item)?.clone();

        let builtin = Builtin {
            item,
            pascal_name,
            name,
            params: eval_static_fn.sig.inputs.into_iter().map(Param::try_from).collect::<Result<_>>()?,
        };

        // optional parameters must be at the end
        let mut seen_optional = false;
        for param in &builtin.params {
            if param.ty.optional {
                seen_optional = true;
            } else if seen_optional {
                return Err(syn::Error::new(param.ident.span(), "optional parameters cannot be followed by non-optional parameters"));
            }
        }

        Ok(builtin)
    }
}
