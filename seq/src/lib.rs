extern crate proc_macro;

use proc_macro2::{Literal, Span, TokenStream, TokenTree};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream, Result},
    parse_macro_input, Ident, LitInt, Token,
};

struct Seq {
    name: Ident,
    begin: LitInt,
    end: LitInt,
    inclusive: bool,
    content: TokenStream,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let name = input.parse()?;
        input.parse::<Token![in]>()?;
        let begin = input.parse()?;
        input.parse::<Token![..]>()?;
        let inclusive = if input.peek(Token![=]) {
            input.parse::<Token![=]>()?;
            true
        } else {
            false
        };
        let end = input.parse()?;
        let content;
        let _ = syn::braced!(content in input);
        let content = content.parse()?;
        Ok(Seq {
            name,
            begin,
            end,
            inclusive,
            content,
        })
    }
}

fn merge_tt_buffer(
    tt_buffer: &mut Vec<TokenTree>,
    merge_idents: &mut bool,
    num: usize,
    name: &Ident,
) -> Vec<TokenTree> {
    let new_tt = if *merge_idents {
        let first_span = tt_buffer
            .first()
            .map(|tt| tt.span())
            .unwrap_or_else(|| Span::call_site());
        vec![proc_macro2::TokenTree::Ident(Ident::new(
            &(tt_buffer
                .drain(..)
                .into_iter()
                .filter_map(|tt| match tt {
                    proc_macro2::TokenTree::Ident(ident) => {
                        if ident == *name {
                            Some(format!("{}", num))
                        } else {
                            Some(ident.to_string())
                        }
                    }
                    _ => None,
                })
                .collect::<Vec<_>>()
                .join("")),
            first_span,
        ))]
    } else {
        tt_buffer
            .drain(..)
            .into_iter()
            .map(|tt| replace_tt(num, &name, tt))
            .collect()
    };
    *merge_idents = false;
    new_tt
}

fn replace_stream(num: usize, name: &Ident, stream: TokenStream) -> TokenStream {
    let mut new_contents = TokenStream::new();
    let mut new_tt = Vec::new();

    let mut stream_iter = stream.into_iter();
    let mut tt_buffer = Vec::new();
    let mut merge_idents = false;
    let mut prev_hash = false;
    while let Some(tt) = stream_iter.next() {
        // dbg!(&tt);
        match &tt {
            TokenTree::Punct(punc) => {
                if punc.as_char() == '#' {
                    tt_buffer.push(tt);
                    prev_hash = true;
                } else {
                    new_tt.extend(merge_tt_buffer(
                        &mut tt_buffer,
                        &mut merge_idents,
                        num,
                        name,
                    ));
                    new_tt.push(replace_tt(num, &name, tt));
                    prev_hash = false;
                }
            }
            TokenTree::Ident(ident) => {
                if prev_hash {
                    if ident == name {
                        merge_idents = true;
                        tt_buffer.push(tt);
                    } else {
                        new_tt.extend(merge_tt_buffer(
                            &mut tt_buffer,
                            &mut merge_idents,
                            num,
                            name,
                        ));
                        new_tt.push(replace_tt(num, &name, tt));
                    }
                } else if merge_idents {
                    tt_buffer.push(tt);
                    new_tt.extend(merge_tt_buffer(
                        &mut tt_buffer,
                        &mut merge_idents,
                        num,
                        name,
                    ));
                } else if ident == name {
                    new_tt.push(replace_tt(num, &name, tt));
                } else if tt_buffer.is_empty() {
                    tt_buffer.push(tt);
                } else {
                    new_tt.extend(merge_tt_buffer(
                        &mut tt_buffer,
                        &mut merge_idents,
                        num,
                        name,
                    ));
                    tt_buffer.push(tt);
                }
                prev_hash = false;
            }
            _ => {
                new_tt.extend(merge_tt_buffer(
                    &mut tt_buffer,
                    &mut merge_idents,
                    num,
                    name,
                ));
                new_tt.push(replace_tt(num, &name, tt));
                prev_hash = false;
            }
        }
    }
    new_tt.extend(
        tt_buffer
            .drain(..)
            .into_iter()
            .map(|tt| replace_tt(num, &name, tt)),
    );
    new_contents.extend(new_tt);
    // dbg!(&new_contents);
    new_contents
}

fn replace_tt(num: usize, name: &Ident, tt: TokenTree) -> TokenTree {
    match tt {
        TokenTree::Ident(ident) => {
            if ident == *name {
                TokenTree::Literal(Literal::usize_unsuffixed(num))
            } else {
                let replacement_name = format!("#{}", name);
                let ident_string = ident.to_string();
                if ident_string.contains(&replacement_name) {
                    let x = ident_string.replace(&replacement_name, &format!("{}", num));
                    TokenTree::Ident(syn::Ident::new(&x, name.span()))
                } else {
                    TokenTree::Ident(ident)
                }
            }
        }
        TokenTree::Group(group) => {
            let delim = group.delimiter();
            let stream = replace_stream(num, name, group.stream());
            TokenTree::Group(proc_macro2::Group::new(delim, stream))
        }
        _ => tt,
    }
}

fn is_repeat_section(stream: TokenStream) -> bool {
    let mut first_char = false;
    let mut stream_iter = stream.into_iter().peekable();
    while let Some(tt) = stream_iter.next() {
        match tt {
            TokenTree::Punct(punct) => match punct.as_char() {
                '#' => first_char = true,
                _ => first_char = false,
            },
            TokenTree::Group(group) => {
                if first_char {
                    if let Some(TokenTree::Punct(punct)) = stream_iter.peek() {
                        if punct.as_char() == '*' {
                            return true;
                        }
                    }
                    first_char = false;
                }

                if is_repeat_section(group.stream()) {
                    return true;
                }
            }
            _ => first_char = false,
        }
    }
    false
}

fn repeat_section(
    begin: usize,
    end: usize,
    inclusive: bool,
    name: &Ident,
    stream: TokenStream,
) -> TokenStream {
    let mut expanded = TokenStream::new();
    let mut prev_punct = None;
    let mut stream_iter = stream.into_iter();
    while let Some(tt) = stream_iter.next() {
        match tt {
            TokenTree::Punct(punct) => match punct.as_char() {
                '#' => prev_punct = Some(punct),
                _ => {
                    if let Some(prev_punct) = prev_punct.take() {
                        expanded.extend(vec![TokenTree::Punct(prev_punct)]);
                    }
                    expanded.extend(vec![TokenTree::Punct(punct)]);
                }
            },
            TokenTree::Group(group) => {
                if let Some(prev_punct) = prev_punct.take() {
                    if prev_punct.as_char() == '#' {
                        let following_tt = stream_iter.next();
                        if let Some(following_tt) = following_tt {
                            if let TokenTree::Punct(punct) = following_tt {
                                if punct.as_char() == '*' {
                                    if inclusive {
                                        expanded.extend((begin..=end).clone().map(|n| {
                                            replace_stream(n, name, group.stream().clone())
                                        }));
                                    } else {
                                        expanded.extend((begin..end).clone().map(|n| {
                                            replace_stream(n, name, group.stream().clone())
                                        }));
                                    }
                                } else {
                                    expanded.extend(vec![TokenTree::Punct(prev_punct)]);
                                    expanded.extend(vec![TokenTree::Group(
                                        proc_macro2::Group::new(
                                            group.delimiter(),
                                            repeat_section(
                                                begin,
                                                end,
                                                inclusive,
                                                name,
                                                group.stream(),
                                            ),
                                        ),
                                    )]);
                                    expanded.extend(vec![TokenTree::Punct(punct)]);
                                }
                            } else {
                                expanded.extend(vec![TokenTree::Punct(prev_punct)]);
                                expanded.extend(vec![TokenTree::Group(proc_macro2::Group::new(
                                    group.delimiter(),
                                    repeat_section(begin, end, inclusive, name, group.stream()),
                                ))]);
                                expanded.extend(vec![following_tt]);
                            }
                        } else {
                            expanded.extend(vec![TokenTree::Punct(prev_punct)]);
                            expanded.extend(vec![TokenTree::Group(proc_macro2::Group::new(
                                group.delimiter(),
                                repeat_section(begin, end, inclusive, name, group.stream()),
                            ))]);
                        }
                    } else {
                        expanded.extend(vec![TokenTree::Punct(prev_punct)]);
                        expanded.extend(vec![TokenTree::Group(proc_macro2::Group::new(
                            group.delimiter(),
                            repeat_section(begin, end, inclusive, name, group.stream()),
                        ))]);
                    }
                } else {
                    expanded.extend(vec![TokenTree::Group(proc_macro2::Group::new(
                        group.delimiter(),
                        repeat_section(begin, end, inclusive, name, group.stream()),
                    ))]);
                }
            }
            _ => expanded.extend(vec![tt]),
        }
    }
    expanded
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let Seq {
        name,
        begin,
        end,
        inclusive,
        content,
        ..
    } = parse_macro_input!(input as Seq);

    let begin: usize = begin.base10_parse().unwrap();
    let end = end.base10_parse().unwrap();

    if is_repeat_section(content.clone()) {
        let expanded = repeat_section(begin, end, inclusive, &name, content);
        proc_macro::TokenStream::from(expanded)
    } else {
        let inv = (begin..end)
            .map(|n| replace_stream(n, &name, content.clone()))
            .collect::<Vec<_>>();
        let expanded = quote! {
            #(#inv)*
        };
        proc_macro::TokenStream::from(expanded)
    }
}
