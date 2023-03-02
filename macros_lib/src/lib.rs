use proc_macro::{self, TokenStream, TokenTree};

#[proc_macro_derive(ToStringSlice)]
pub fn derive_to_string_slice(tokens: TokenStream) -> TokenStream {
    let mut tokens_iter =
        (tokens.clone() as TokenStream)
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

    format!(
        r#"
impl Into<&'static str> for &{} {{
    fn into(self) -> &'static str {{
        match self {{
            {}
        }}
    }}
}}
"#,
        enum_name,
        idents
            .map(|token| format!(
                r#"{}::{} => "{}","#,
                enum_name,
                token,
                token.to_string().replace("_", " ")
            ))
            .collect::<String>(),
    )
    .parse::<TokenStream>()
    .unwrap()
}

#[proc_macro_derive(IterableEnum)]
pub fn derive_iterable_enum(tokens: TokenStream) -> TokenStream {
    let mut tokens_iter =
        (tokens.clone() as TokenStream)
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
            TokenTree::Ident(ident) => Some(ident.to_string()),
            _ => None,
        })
        .collect::<Vec<String>>();

    format!(
        r#"
const ALL_SYMBOLS: [{}; {}] = [
    {}
];

impl {} {{
    pub(crate) fn iter() -> std::slice::Iter<'static, {}> {{
        ALL_SYMBOLS.iter()
    }}
}}
"#,
        enum_name,
        idents.len(),
        idents
            .into_iter()
            .map(|ident| format!("{}::{},\n", enum_name, ident))
            .collect::<String>(),
        enum_name,
        enum_name,
    )
    .parse()
    .unwrap()
}
