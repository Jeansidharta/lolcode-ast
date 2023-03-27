use proc_macro2::{self, TokenStream, TokenTree};
use quote::quote;

#[proc_macro_derive(ToStringSlice)]
pub fn derive_to_string_slice(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let tokens = TokenStream::from(tokens);
    let mut tokens_iter = tokens
        .into_iter()
        .skip_while(
            |token| !matches!( token, TokenTree::Ident(ident) if ident.to_string() == "enum"),
        )
        .skip(1);

    let enum_name: TokenStream = tokens_iter.next().unwrap().into();

    let names_with_underscore = tokens_iter
        .find_map(|token| match token {
            TokenTree::Group(group) => Some(group.stream()),
            _ => None,
        })
        .unwrap()
        .into_iter()
        .filter_map(|token| match token {
            TokenTree::Ident(ident) => Some(ident),
            _ => None,
        });

    let names_with_spaces = names_with_underscore
        .clone()
        .map(|t| t.to_string().replace("_", " "));

    quote! {
        impl crate::to_string_slice::ToStringSlice for #enum_name {
            fn to_string_slice(&self) -> &'static str {
                match self {
                    #(#enum_name::#names_with_underscore => #names_with_spaces),*
                }
            }
    } }
    .into()
}

#[proc_macro_derive(IterableEnum)]
pub fn derive_iterable_enum(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let mut tokens_iter = TokenStream::from(tokens)
        .into_iter()
        .skip_while(|token| match token {
            TokenTree::Ident(ident) => ident.to_string() != "enum",
            _ => true,
        });

    let enum_name = tokens_iter.nth(1).unwrap();

    let idents = tokens_iter
        .find_map(|token| match token {
            TokenTree::Group(group) => Some(group.stream()),
            _ => None,
        })
        .unwrap()
        .into_iter()
        .filter_map(|token| match token {
            TokenTree::Ident(ident) => Some(ident),
            _ => None,
        });

    let idents_len = idents.clone().count();

    quote!(
    const ALL_SYMBOLS: [#enum_name; #idents_len] = [
        #(#enum_name::#idents),*
    ];

    impl #enum_name {
        pub(crate) fn iter() -> std::slice::Iter<'static, #enum_name> {
            ALL_SYMBOLS.iter()
        }
    }
        )
    .into()
}
