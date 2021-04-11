mod syntax;
mod tree;

use syntax::EnumDescr;

use proc_macro::TokenStream;
use quote::quote;

/// Turn a string like "some.keyword" or "some_keyword" into Rust variant/constructor
/// "SomeKeyword".
fn to_constr(s: &syn::LitStr) -> syn::Ident {
    let span = s.span();
    let str = s.value();

    let mut ident_str = String::with_capacity(str.len());

    let mut upper = true;

    for char in str.chars() {
        if char == '_' || char == '.' {
            upper = true;
        } else {
            if upper {
                for upper_char in char.to_uppercase() {
                    ident_str.push(upper_char);
                }
                upper = false;
            } else {
                ident_str.push(char);
            }
        }
    }

    syn::Ident::new(&ident_str, span)
}

#[proc_macro]
pub fn make_enum(input: TokenStream) -> TokenStream {
    let EnumDescr {
        enum_name,
        variant_strings,
    } = syn::parse_macro_input!(input as syntax::EnumDescr);

    let variant_ids: Vec<syn::Ident> = variant_strings.iter().map(to_constr).collect();

    let rules: Vec<syntax::Rule> = variant_strings
        .into_iter()
        .map(|variant| {
            let constr = to_constr(&variant);

            let mut path_segments: syn::punctuated::Punctuated<
                syn::PathSegment,
                syn::token::Colon2,
            > = syn::punctuated::Punctuated::new();

            path_segments.push(syn::PathSegment {
                ident: enum_name.clone(),
                arguments: syn::PathArguments::None,
            });

            path_segments.push(syn::PathSegment {
                ident: constr,
                arguments: syn::PathArguments::None,
            });

            syntax::Rule {
                pattern: syntax::Pattern(variant.value()),
                value: syntax::Value(syn::Expr::Path(syn::ExprPath {
                    attrs: vec![],
                    qself: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: path_segments,
                    },
                })),
            }
        })
        .collect();

    let parser_body = tree::build_decision_tree(rules);

    quote!(
        #[derive(Debug, PartialEq, Eq, Clone)]
        pub enum #enum_name {
            #(#variant_ids,)*
        }

        impl #enum_name {
            pub fn parse(chars: &mut Iterator<Item=(usize, char)>) -> Option<(usize, #enum_name)> {
                #parser_body
            }
        }
    )
    .into()
}
