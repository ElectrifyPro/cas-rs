use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    Attribute,
    Expr,
    Fields,
    Ident,
    ItemStruct,
    Result,
    Token,
};

/// The arguments that can be passed to the `error` attribute.
#[derive(Debug, Default)]
pub struct ErrorArgs {
    pub message: Option<Expr>,
    pub label: Option<Expr>,
    pub help: Option<Expr>,
}

impl ErrorArgs {
    /// Parse the next argument in the input stream and applies it to itself.
    fn parse_arg(&mut self, input: ParseStream) -> Result<()> {
        let ident: Ident = input.parse()?;
        input.parse::<Token![=]>()?;

        let ident_str = ident.to_string();
        match ident_str.as_str() {
            "message" => self.message = Some(input.parse()?),
            "label" => self.label = Some(input.parse()?),
            "help" => self.help = Some(input.parse()?),
            _ => return Err(syn::Error::new_spanned(ident, format!("unknown tag `{}`", ident_str))),
        }

        Ok(())
    }
}

impl Parse for ErrorArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut args = ErrorArgs::default();

        while args.parse_arg(input).is_ok() {
            if input.parse::<Token![,]>().is_err() {
                break;
            }
        }

        Ok(args)
    }
}

/// Creates a `let` expression that destructures the given `ident` into its named fields. Returns
/// nothing if the fields are not named.
fn destructure_fields(ident: &Ident, fields: &Fields) -> TokenStream2 {
    if let Fields::Named(fields) = fields {
        let fields = fields.named.iter().map(|field| {
            let field_name = field.ident.as_ref();
            quote! { #field_name }
        });
        quote! { let #ident { #(#fields),* } = self; }
    } else {
        quote! {}
    }
}

/// The target struct to derive [`ErrorKind`] for.
#[derive(Debug)]
pub struct ErrorKindTarget {
    pub name: Ident,
    pub fields: Fields,
    pub error_args: ErrorArgs,
}

impl Parse for ErrorKindTarget {
    fn parse(input: ParseStream) -> Result<Self> {
        // parse outer attributes, including documentation and `info` attributes
        let attributes = input.call(Attribute::parse_outer)?;
        let remaining = input.parse::<ItemStruct>()?;

        let name = remaining.ident;
        let fields = remaining.fields;
        let mut error_args = ErrorArgs::default();

        for attr in &attributes {
            let attr_name = attr.path().get_ident().unwrap();
            let ident = attr_name.to_string();
            if ident.as_str() == "error" {
                error_args = attr.parse_args::<ErrorArgs>()?;
                break;
            }
        }

        Ok(ErrorKindTarget {
            name,
            fields,
            error_args,
        })
    }
}

impl ToTokens for ErrorKindTarget {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let destructure_expr = destructure_fields(&self.name, &self.fields);
        let create_function = |name: &str, body: &Expr, output_type: TokenStream2| {
            let name = Ident::new(name, Span::call_site());
            quote! {
                #[allow(unused_variables)]
                fn #name(&self) -> #output_type {
                    #destructure_expr
                    (#body).to_owned().into()
                }
            }
        };
        let (message, label, help) = (
            self.error_args.message.as_ref().map(|e| create_function("message", e, quote! { String })),
            self.error_args.label.as_ref().map(|e| create_function("label", e, quote! { String })),
            self.error_args.help.as_ref().map(|e| create_function("help", e, quote! { Option<String> })),
        );

        tokens.extend(quote! {
            #message
            #label
            #help
        });
    }
}
