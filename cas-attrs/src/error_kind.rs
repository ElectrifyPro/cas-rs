use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    Attribute,
    Expr,
    Ident,
    Item,
    Result,
    Token,
};

/// The arguments that can be passed to the `error` attribute.
#[derive(Debug, Default)]
pub struct ErrorArgs {
    pub message: Option<Expr>,
    pub labels: Option<Expr>,
    pub help: Option<Expr>,
    pub note: Option<Expr>,
}

impl ErrorArgs {
    /// Parse the next argument in the input stream and applies it to itself.
    fn parse_arg(&mut self, input: ParseStream) -> Result<()> {
        let ident: Ident = input.parse()?;
        input.parse::<Token![=]>()?;

        let ident_str = ident.to_string();
        match ident_str.as_str() {
            "message" => self.message = Some(input.parse()?),
            "labels" => self.labels = Some(input.parse()?),
            "help" => self.help = Some(input.parse()?),
            "note" => self.note = Some(input.parse()?),
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

/// The target to derive [`ErrorKind`] for.
#[derive(Debug)]
pub struct ErrorKindTarget {
    pub name: Ident,
    pub error_args: ErrorArgs,
}

impl ErrorKindTarget {
    /// Generate the code that adds the labels to the error report.
    fn generate_labels(&self) -> TokenStream2 {
        match self.error_args.labels.as_ref() {
            Some(labels) => quote! {
                #labels
                    .into_iter()
                    .enumerate()
                    .map(|(i, label_str)| {
                        let mut label = ariadne::Label::new((src_id, spans[i].clone()))
                            .with_color(cas_error::EXPR);

                        if !label_str.is_empty() {
                            label = label.with_message(label_str);
                        }

                        label
                    })
                    .collect::<Vec<_>>()
            },
            None => quote! {
                spans
                    .iter()
                    .map(|span| ariadne::Label::new((src_id, span.clone()))
                        .with_color(cas_error::EXPR))
                    .collect::<Vec<_>>()
            },
        }
    }
}

impl Parse for ErrorKindTarget {
    fn parse(input: ParseStream) -> Result<Self> {
        // parse outer attributes, including documentation and `info` attributes
        let attributes = input.call(Attribute::parse_outer)?;
        let name = match input.parse::<Item>() {
            Ok(Item::Struct(s)) => s.ident,
            Ok(Item::Enum(e)) => e.ident,
            Ok(item) => return Err(syn::Error::new_spanned(item, "expected struct or enum")),
            Err(e) => return Err(e),
        };

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
            error_args,
        })
    }
}

impl ToTokens for ErrorKindTarget {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let (name, message, help, note) = (
            &self.name,
            self.error_args.message.as_ref(),
            self.error_args.help.as_ref().map(|e| quote! { builder.set_help(#e); }),
            self.error_args.note.as_ref().map(|e| quote! { builder.set_note(#e); }),
        );
        let labels = self.generate_labels();

        tokens.extend(quote! {
            impl cas_error::ErrorKind for #name {
                fn build_report<'a>(
                    &self,
                    src_id: &'a str,
                    spans: &[std::ops::Range<usize>],
                ) -> ariadne::Report<(&'a str, std::ops::Range<usize>)> {
                    let start_span = spans.get(0).expect("ErrorKind derive: need at least one span to build a report").start;
                    let mut builder = ariadne::Report::build(ariadne::ReportKind::Error, src_id, start_span)
                        .with_message(#message)
                        .with_labels(#labels);

                    #help
                    #note
                    builder.finish()
                }
            }
        });
    }
}
