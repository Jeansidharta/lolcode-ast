use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(ToStringSlice)]
pub fn derive_to_string_slice(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);
    let enum_name = input.ident;
    let enum_variants = match input.data {
        syn::Data::Enum(syn::DataEnum { variants, .. }) => variants,
        _ => {
            return quote! { compile_error!("Proc macro ToStringSlice can only be implemented to enums") }.into()
        }
    }.into_iter();

    let names_with_underscore = enum_variants.map(|variant| variant.ident);

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
pub fn derive_iterable_enum(tokens: TokenStream) -> TokenStream {
    let input = parse_macro_input!(tokens as DeriveInput);
    let enum_name = input.ident;

    let enum_variants = match input.data {
        syn::Data::Enum(syn::DataEnum { variants, .. }) => variants,
        _ => {
            return quote! { compile_error!("Proc macro IterableEnum can only be implemented to enums") }.into()
        }
    }.into_iter();

    let idents = enum_variants.map(|variant| variant.ident);

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
