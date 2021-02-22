use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream, Result},
    parse_macro_input, Ident, LitInt, Token,
};

struct Seq {
    ident: Ident,
    start: isize,
    end: isize,
    content: TokenStream,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<LitInt>()?;
        let start = start.base10_parse::<isize>()?;
        input.parse::<Token![..]>()?;
        let end = input.parse::<LitInt>()?;
        let end = end.base10_parse::<isize>()?;

        let content;
        let _ = syn::braced!(content in input);
        let content = content.parse()?;

        Ok(Seq {
            ident,
            start,
            end,
            content,
        })
    }
}

fn replace_token_tree(tt: TokenTree, ident: &Ident, n: isize) -> TokenTree {
    match &tt {
        TokenTree::Ident(i) => {
            if *i == *ident {
                TokenTree::Literal(proc_macro2::Literal::isize_unsuffixed(n))
            } else {
                tt
            }
        }
        TokenTree::Group(grp) => {
            let mut g = proc_macro2::Group::new(
                grp.delimiter(),
                grp.stream()
                    .into_iter()
                    .map(|t| replace_token_tree(t, &ident, n))
                    .collect(),
            );
            g.set_span(grp.span());
            TokenTree::Group(g)
        }
        TokenTree::Punct(_) | TokenTree::Literal(_) => tt,
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let Seq {
        ident,
        start,
        end,
        content,
    } = parse_macro_input!(input as Seq);

    let streams: Vec<TokenStream> = (start..end)
        .map(|n| {
            content
                .clone()
                .into_iter()
                .map(|tt| replace_token_tree(tt, &ident, n))
                .collect()
        })
        .collect();

    let expanded = quote! {
        #(#streams)*
    };
    proc_macro::TokenStream::from(expanded)
}
