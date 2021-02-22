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

fn replace_token_stream(ts: TokenStream, ident: &Ident, n: isize) -> TokenStream {
    let mut tokens: Vec<TokenTree> = Vec::new();
    let mut ts_iter = ts.into_iter().peekable();

    while let Some(tt) = ts_iter.next() {
        match &tt {
            TokenTree::Ident(i) => {
                if *i == *ident {
                    tokens.push(TokenTree::Literal(proc_macro2::Literal::isize_unsuffixed(
                        n,
                    )));
                } else {
                    if let Some(peek_tt) = ts_iter.peek() {
                        match &peek_tt {
                            TokenTree::Group(_) | TokenTree::Ident(_) | TokenTree::Literal(_) => {
                                tokens.push(tt);
                            }
                            TokenTree::Punct(p) => match p.as_char() {
                                '#' => {
                                    let sharp_punct = ts_iter.next();
                                    if let Some(after_sharp_tt) = ts_iter.peek() {
                                        match after_sharp_tt {
                                            TokenTree::Ident(after_sharp_ident) => {
                                                if *after_sharp_ident == *ident {
                                                    let _ = ts_iter.next();
                                                    let new_ident = Ident::new(
                                                        &format!("{}{}", i, n),
                                                        i.span(),
                                                    );
                                                    tokens.push(TokenTree::Ident(new_ident));
                                                } else {
                                                    tokens.push(tt);
                                                    if let Some(sharp_punct) = sharp_punct {
                                                        tokens.push(sharp_punct);
                                                    }
                                                }
                                            }
                                            TokenTree::Group(_)
                                            | TokenTree::Punct(_)
                                            | TokenTree::Literal(_) => {
                                                tokens.push(tt);
                                                if let Some(sharp_punct) = sharp_punct {
                                                    tokens.push(sharp_punct);
                                                }
                                            }
                                        }
                                    } else {
                                        tokens.push(tt);
                                        if let Some(sharp_punct) = sharp_punct {
                                            tokens.push(sharp_punct);
                                        }
                                    }
                                }
                                _ => {
                                    tokens.push(tt);
                                }
                            },
                        }
                    }
                }
            }
            TokenTree::Group(grp) => {
                let mut g = proc_macro2::Group::new(
                    grp.delimiter(),
                    replace_token_stream(grp.stream(), &ident, n),
                );
                g.set_span(grp.span());
                tokens.push(TokenTree::Group(g));
            }
            TokenTree::Punct(_) | TokenTree::Literal(_) => tokens.push(tt),
        }
    }

    use std::iter::FromIterator;
    TokenStream::from_iter(tokens)
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
        .map(|n| replace_token_stream(content.clone(), &ident, n))
        .collect();

    let expanded = quote! {
        #(#streams)*
    };
    proc_macro::TokenStream::from(expanded)
}
