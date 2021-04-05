use syn::parse::{Parse, ParseStream};

#[derive(Debug)]
pub(crate) struct Pattern(pub(crate) String);

#[derive(Debug)]
pub(crate) struct Value(pub(crate) syn::Expr);

#[derive(Debug)]
pub(crate) struct Rule {
    pub(crate) pattern: Pattern,
    pub(crate) value: Value,
}

impl Parse for Pattern {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Pattern(input.parse::<syn::LitStr>()?.value()))
    }
}

impl Parse for Value {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(Value(input.parse()?))
    }
}

impl Parse for Rule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let pattern = input.parse()?;
        input.parse::<syn::Token![=>]>()?;
        let value = input.parse()?;
        Ok(Rule { pattern, value })
    }
}

#[derive(Debug)]
pub(crate) struct EnumDescr {
    pub(crate) enum_name: syn::Ident,
    pub(crate) variant_strings: Vec<syn::LitStr>,
}

impl Parse for EnumDescr {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let enum_name = input.parse::<syn::Ident>()?;

        input.parse::<syn::Token![,]>()?;

        let variant_strings =
            syn::punctuated::Punctuated::<syn::LitStr, syn::Token![,]>::parse_terminated(input)?
                .into_pairs()
                .map(|pair| pair.into_value())
                .collect();

        Ok(EnumDescr {
            enum_name,
            variant_strings,
        })
    }
}
