use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    Expr,
    ItemFn,
    Pat,
    Result,
    Token,
};

/// Represents a parameter provided in the `args` attribute.
#[derive(Debug)]
pub struct Param {
    /// The pattern that the argument must match.
    pub pattern: Pat,

    /// An optional default value for the parameter, if the user does not provide one.
    pub default: Option<Expr>,
}

/// The arguments that can be passed to the `args` attribute.
#[derive(Debug, Default)]
pub struct Args {
    /// The patterns that the arguments must match.
    pub patterns: Vec<Param>,
}

impl Args {
    /// Parse the next argument in the input stream and apply it to itself.
    fn parse_arg(&mut self, input: ParseStream) -> Result<()> {
        let pattern = Pat::parse_single(input)?;
        let default = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            input.parse::<Expr>().ok()
        } else {
            None
        };
        self.patterns.push(Param { pattern, default });
        Ok(())
    }

    /// Generates the statements that check the arguments.
    pub fn generate_check_stmts(&self, func: &ItemFn) -> TokenStream2 {
        let (name, block) = (&func.sig.ident, &func.block);
        let num_patterns = self.patterns.len();
        let pattern_tokens = self.patterns
            .iter()
            .map(|pattern| {
                let pattern = &pattern.pattern;
                quote! { #pattern }
            })
            .collect::<Vec<_>>();

        // for all `n` patterns given, we create `n` more new patterns
        // each `i`th pattern is intended to typecheck the `i`th argument, by ignoring the
        // following arguments and using a wildcard binding for the `i`th argument
        //
        // for example, if the given patterns are [Number(n), Number(k), Number(m)], the patterns
        // generated are
        //
        // [Number(_), Number(_), _, ..] // 1st, 2nd arguments are ok, is the 3rd ok?
        // [Number(_), _, ..] // 1st argument is ok, is the 2nd ok?
        // [_, ..] // is the 1st argument ok?
        let extra_matches = {
            let mut out = Vec::new();

            for index in (0..num_patterns).rev() {
                let mut new_patterns = pattern_tokens.clone();
                new_patterns.splice(index.., [quote! { _ }, quote! { .. }]);
                let expected = &self.patterns[index].pattern;
                out.push(quote! {
                    #[allow(unused_variables)]
                    [ #(#new_patterns),* ] => {
                        Err(BuiltinError::TypeMismatch(TypeMismatch {
                            name: stringify!(#name).to_owned(),
                            index: #index,
                            expected: stringify!(#expected).to_string(),
                            given: args[#index].to_string(),
                        }))
                    }
                });
            }

            out
        };

        quote! {
            match args {
                // successful typecheck
                [ #(#pattern_tokens),* ] => {
                    #block
                },

                // catch various errors
                #[allow(unused_variables)]
                [ #(#pattern_tokens),* , .. ] => {
                    Err(BuiltinError::TooManyArguments(TooManyArguments {
                        name: stringify!(#name).to_owned(),
                        expected: #num_patterns,
                        given: args.len(),
                    }))
                },

                #(#extra_matches),*,

                [] => {
                    // TODO: for empty functions, this is not an error
                    Err(BuiltinError::MissingArgument(MissingArgument {
                        name: stringify!(#name).to_owned(),
                        index: 0,
                        expected: #num_patterns,
                        given: args.len(),
                    }))
                },
            }
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
