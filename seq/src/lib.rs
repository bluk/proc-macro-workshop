use proc_macro2::{TokenStream, TokenTree};
use quote::quote;
use syn::{
    parse::{Parse, ParseStream, Result},
    parse_macro_input, Ident, LitInt, Token,
};

struct Seq {
    name_to_replace: Ident,
    begin: RepeatNumType,
    end: RepeatNumType,
    content: TokenStream,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let name_to_replace: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let begin = input.parse::<LitInt>()?;
        let begin = begin.base10_parse::<RepeatNumType>()?;
        input.parse::<Token![..]>()?;
        let end = input.parse::<LitInt>()?;
        let end = end.base10_parse::<RepeatNumType>()?;

        let content;
        let _ = syn::braced!(content in input);
        let content = content.parse()?;

        Ok(Seq {
            name_to_replace,
            begin,
            end,
            content,
        })
    }
}

type RepeatNumType = isize;

fn repeat_content(
    ts: TokenStream,
    name_to_replace: &Ident,
    begin: RepeatNumType,
    end: RepeatNumType,
) -> Vec<TokenStream> {
    (begin..end)
        .map(|n| replace_content(ts.clone(), name_to_replace, n))
        .collect()
}

fn replace_content(ts: TokenStream, name_to_replace: &Ident, num: RepeatNumType) -> TokenStream {
    let mut tokens: Vec<TokenTree> = Vec::new();
    let mut ts_iter = ts.into_iter().peekable();

    while let Some(tt) = ts_iter.next() {
        match &tt {
            TokenTree::Ident(i) => {
                if *i == *name_to_replace {
                    tokens.push(TokenTree::Literal(proc_macro2::Literal::isize_unsuffixed(
                        num,
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
                                                if *after_sharp_ident == *name_to_replace {
                                                    let _ = ts_iter.next();
                                                    let new_ident = Ident::new(
                                                        &format!("{}{}", i, num),
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
                    } else {
                        tokens.push(tt)
                    }
                }
            }
            TokenTree::Group(grp) => {
                let mut g = proc_macro2::Group::new(
                    grp.delimiter(),
                    replace_content(grp.stream(), &name_to_replace, num),
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

fn repeat_section_exists(ts: TokenStream) -> bool {
    enum DetectState {
        Sharp,
        GroupAfterSharp,
        None,
    }

    let mut detect_state = DetectState::None;
    let mut iter = ts.into_iter();
    while let Some(tt) = iter.next() {
        match tt {
            TokenTree::Punct(p) => match p.as_char() {
                '#' => detect_state = DetectState::Sharp,
                '*' => match detect_state {
                    DetectState::GroupAfterSharp => {
                        return true;
                    }
                    DetectState::Sharp => {
                        detect_state = DetectState::None;
                    }
                    DetectState::None => {}
                },
                _ => {
                    detect_state = DetectState::None;
                }
            },
            TokenTree::Group(g) => {
                match detect_state {
                    DetectState::Sharp => {
                        detect_state = DetectState::GroupAfterSharp;
                    }
                    DetectState::GroupAfterSharp => {
                        detect_state = DetectState::None;
                    }
                    DetectState::None => {}
                }
                if repeat_section_exists(g.stream()) {
                    return true;
                }
            }
            _ => {
                detect_state = DetectState::None;
            }
        }
    }
    false
}

fn repeat_section(
    ts: TokenStream,
    name_to_replace: &Ident,
    begin: RepeatNumType,
    end: RepeatNumType,
) -> TokenStream {
    enum DetectSectionState {
        Sharp(proc_macro2::Punct),
        GroupAfterSharp(proc_macro2::Punct, proc_macro2::Group),
        None,
    }

    let mut tokens: Vec<TokenTree> = Vec::new();

    let mut detect_state = DetectSectionState::None;
    let mut iter = ts.into_iter();

    while let Some(tt) = iter.next() {
        match tt {
            TokenTree::Punct(p) => {
                let mut tmp_state = DetectSectionState::None;
                std::mem::swap(&mut detect_state, &mut tmp_state);
                match tmp_state {
                    DetectSectionState::Sharp(existing_p) => {
                        tokens.push(TokenTree::Punct(existing_p));
                        match p.as_char() {
                            '#' => {
                                detect_state = DetectSectionState::Sharp(p);
                            }
                            _ => {
                                tokens.push(TokenTree::Punct(p));
                            }
                        }
                    }
                    DetectSectionState::GroupAfterSharp(existing_p, g) => match p.as_char() {
                        '*' => {
                            let streams = repeat_content(g.stream(), name_to_replace, begin, end);
                            streams.into_iter().for_each(|s| tokens.extend(s));
                        }
                        '#' => {
                            tokens.push(TokenTree::Punct(existing_p));
                            tokens.push(TokenTree::Group(g));
                            detect_state = DetectSectionState::Sharp(p);
                        }
                        _ => {
                            tokens.push(TokenTree::Punct(existing_p));
                            tokens.push(TokenTree::Group(g));
                            tokens.push(TokenTree::Punct(p));
                        }
                    },
                    DetectSectionState::None => match p.as_char() {
                        '#' => {
                            detect_state = DetectSectionState::Sharp(p);
                        }
                        _ => {
                            tokens.push(TokenTree::Punct(p));
                        }
                    },
                }
            }
            TokenTree::Group(g) => {
                let mut tmp_state = DetectSectionState::None;
                std::mem::swap(&mut detect_state, &mut tmp_state);

                let g = proc_macro2::Group::new(
                    g.delimiter(),
                    repeat_section(g.stream(), name_to_replace, begin, end),
                );
                match tmp_state {
                    DetectSectionState::Sharp(p) => {
                        detect_state = DetectSectionState::GroupAfterSharp(p, g);
                    }
                    DetectSectionState::GroupAfterSharp(p, existing_g) => {
                        tokens.push(TokenTree::Punct(p));
                        tokens.push(TokenTree::Group(existing_g));
                        tokens.push(TokenTree::Group(g));
                    }
                    DetectSectionState::None => {
                        tokens.push(TokenTree::Group(g));
                    }
                }
            }
            TokenTree::Literal(_) | TokenTree::Ident(_) => {
                let mut tmp_state = DetectSectionState::None;
                std::mem::swap(&mut detect_state, &mut tmp_state);
                match tmp_state {
                    DetectSectionState::Sharp(p) => {
                        tokens.push(TokenTree::Punct(p));
                    }
                    DetectSectionState::GroupAfterSharp(p, g) => {
                        tokens.push(TokenTree::Punct(p));
                        tokens.push(TokenTree::Group(g));
                    }
                    DetectSectionState::None => {}
                }
                tokens.push(tt);
            }
        }
    }

    use std::iter::FromIterator;
    TokenStream::from_iter(tokens)
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let Seq {
        name_to_replace,
        begin,
        end,
        content,
    } = parse_macro_input!(input as Seq);

    let expanded = if repeat_section_exists(content.clone()) {
        let stream = repeat_section(content, &name_to_replace, begin, end);
        quote! {
            #stream
        }
    } else {
        let streams = repeat_content(content, &name_to_replace, begin, end);
        quote! {
            #(#streams)*
        }
    };

    proc_macro::TokenStream::from(expanded)
}
