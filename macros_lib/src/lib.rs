use proc_macro::{self, TokenStream, TokenTree};

#[proc_macro]
pub fn symbol_enum(token_stream: TokenStream) -> TokenStream {
    let mut tokens = Vec::<Vec<String>>::new();
    tokens.push(Vec::new());

    token_stream.into_iter().for_each(|token| {
        match token {
            TokenTree::Ident(ident) => {
                let token = tokens.last_mut().unwrap();
                token.push(ident.to_string());
            }
            TokenTree::Punct(punct) => {
                if punct.as_char() == ',' {
                    tokens.push(Vec::new())
                }
            }
            _ => {}
        };
    });

    let tokens_with_space: Vec<String> = tokens.iter().map(|token| token.join(" ")).collect();
    let tokens_with_underline: Vec<String> = tokens.iter().map(|token| token.join("_")).collect();

    return format!(
        r#"
#[derive(Debug, Clone)]
pub enum Symbol {{
    {}
}}

impl Symbol {{
    pub fn to_string_slice(&self) -> &str {{
        match self {{
            {}
        }}
    }}
}}

pub const ALL_SYMBOLS: [Symbol;{}] = [
    {}
];
    "#,
        tokens_with_underline.join(",\n"),
        tokens_with_underline
            .iter()
            .zip(tokens_with_space.iter())
            .map(|(underline, space)| format!("Symbol::{} => \"{}\",", underline, space))
            .collect::<String>(),
        tokens.len(),
        tokens_with_underline
            .iter()
            .map(|token| format!("Symbol::{},", token))
            .collect::<String>()
    )
    .parse()
    .unwrap();
}

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
impl {} {{
    pub fn to_string_slice(&self) -> &str {{
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
    pub fn iter() -> std::slice::Iter<'static, {}> {{
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
