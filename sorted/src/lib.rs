extern crate proc_macro;

use proc_macro2::{Span, TokenStream};
use quote::quote;
use std::cmp::Ordering;
use syn::{parse_macro_input, visit_mut::VisitMut, ExprMatch, Item, ItemFn, Pat};

struct SortedArgs {}

impl syn::parse::Parse for SortedArgs {
    fn parse(_input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        Ok(SortedArgs {})
    }
}

fn verify_sorted_enum(item_enum: &syn::ItemEnum) -> syn::Result<()> {
    let variants: Vec<_> = (&item_enum.variants).into_iter().cloned().collect();
    let mut sorted_variants = variants.clone();
    sorted_variants.sort_by(|a, b| a.ident.cmp(&b.ident));

    let variants_iter = variants.into_iter();
    let sorted_variants_iter = sorted_variants.into_iter();
    variants_iter
        .zip(sorted_variants_iter)
        .find(|(a, b)| a != b)
        .map(|(a, b)| {
            Err(syn::Error::new_spanned(
                &b.ident,
                format!(
                    "{} should sort before {}",
                    &b.ident.to_string(),
                    a.ident.to_string()
                ),
            ))
        })
        .unwrap_or_else(|| Ok(()))
}

#[proc_macro_attribute]
pub fn sorted(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let args = TokenStream::from(args);
    let item = parse_macro_input!(input as Item);

    let verify_result = match &item {
        Item::Enum(item_enum) => verify_sorted_enum(item_enum),
        _ => Err(syn::Error::new_spanned(
            args,
            "expected enum or match expression",
        )),
    };

    let mut expanded = quote! { #item };
    if let Err(err) = verify_result {
        expanded.extend(err.to_compile_error());
    }
    proc_macro::TokenStream::from(expanded)
}

struct SortedVisitMut {
    sort_errs: Vec<syn::Error>,
}

impl SortedVisitMut {
    fn new() -> Self {
        Self {
            sort_errs: Vec::new(),
        }
    }

    fn contains_sorted_attr<'a, I>(attrs: I) -> bool
    where
        I: IntoIterator<Item = &'a syn::Attribute>,
    {
        attrs
            .into_iter()
            .any(|attr| SortedVisitMut::is_sorted_attr(attr))
    }

    fn is_sorted_attr(attr: &syn::Attribute) -> bool {
        attr.path.get_ident() == Some(&syn::Ident::new("sorted", Span::call_site()))
    }

    fn pat_path(pat: &Pat) -> &syn::Path {
        match pat {
            Pat::Path(pat_path) => &pat_path.path,
            Pat::Struct(pat_struct) => &pat_struct.path,
            Pat::TupleStruct(pat_tuple_struct) => &pat_tuple_struct.path,
            _ => unimplemented!("unknown pattern"),
        }
    }

    fn words_to_compare(pat: &Pat) -> Vec<String> {
        match pat {
            Pat::Ident(pat) => vec![pat.ident.to_string()],
            Pat::Path(_) | Pat::Struct(_) | Pat::TupleStruct(_) => SortedVisitMut::pat_path(pat)
                .segments
                .iter()
                .map(|segment| segment.ident.to_string())
                .collect(),
            _ => unimplemented!("unknown pattern"),
        }
    }

    fn supported_pat(pat: &Pat) -> bool {
        match pat {
            Pat::Path(_) | Pat::Struct(_) | Pat::TupleStruct(_) | Pat::Wild(_) | Pat::Ident(_) => {
                true
            }
            _ => false,
        }
    }

    fn verify_sorted_arms(&mut self, arms: &Vec<Arm>) {
        if let Some(arm) = arms
            .iter()
            .find(|arm| !SortedVisitMut::supported_pat(&arm.pat))
        {
            self.sort_errs.push(syn::Error::new_spanned(
                &arm.pat,
                "unsupported by #[sorted]",
            ));
            return;
        }

        let mut sorted_arms = arms.clone();
        sorted_arms.sort_by(|a, b| {
            match (&a.pat, &b.pat) {
                (Pat::Wild(_), Pat::Wild(_)) => {
                    return Ordering::Greater;
                }
                (Pat::Wild(_), _) => {
                    return Ordering::Greater;
                }
                (_, Pat::Wild(_)) => {
                    return Ordering::Less;
                }
                _ => {}
            }
            let a = SortedVisitMut::words_to_compare(&a.pat);
            let b = SortedVisitMut::words_to_compare(&b.pat);
            a.into_iter()
                .zip(b)
                .fold(Ordering::Equal, |acc, (a, b)| match acc {
                    Ordering::Equal => a.cmp(&b),
                    _ => acc,
                })
        });

        let arms_iter = arms.into_iter();
        let sorted_arms_iter = sorted_arms.into_iter();
        if let Some(err) = arms_iter
            .zip(sorted_arms_iter)
            .find(|(a, b)| a.pat != b.pat)
            .map(|(a, b)| {
                match (&a.pat, &b.pat) {
                    (Pat::Wild(_), _) => {
                        let b_path = SortedVisitMut::pat_path(&b.pat);
                        return syn::Error::new_spanned(&b_path, "done");
                    }
                    (_, Pat::Wild(_)) => {
                        let a_path = SortedVisitMut::pat_path(&a.pat);
                        return syn::Error::new_spanned(&a_path, "done");
                    }
                    _ => {}
                }
                let a_words = SortedVisitMut::words_to_compare(&a.pat);
                let b_words = SortedVisitMut::words_to_compare(&b.pat);

                match &b.pat {
                    Pat::Path(_) | Pat::Struct(_) | Pat::TupleStruct(_) => {
                        let b_path = SortedVisitMut::pat_path(&b.pat);
                        syn::Error::new_spanned(
                            &b_path,
                            format!(
                                "{} should sort before {}",
                                b_words.join("::"),
                                a_words.join("::"),
                            ),
                        )
                    }
                    Pat::Ident(ident) => syn::Error::new_spanned(
                        &ident,
                        format!(
                            "{} should sort before {}",
                            b_words.join("::"),
                            a_words.join("::"),
                        ),
                    ),
                    _ => unimplemented!(),
                }
            })
        {
            self.sort_errs.push(err);
        }
    }
}

use syn::Arm;

impl VisitMut for SortedVisitMut {
    fn visit_expr_match_mut(&mut self, i: &mut ExprMatch) {
        if SortedVisitMut::contains_sorted_attr(&i.attrs) {
            i.attrs
                .retain(|attr| !SortedVisitMut::is_sorted_attr(&attr));
            self.verify_sorted_arms(&i.arms);
        }
    }
}

#[proc_macro_attribute]
pub fn check(
    args: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let _args = TokenStream::from(args);
    let mut item = parse_macro_input!(input as ItemFn);

    let mut visitor = SortedVisitMut::new();
    visitor.visit_item_fn_mut(&mut item);

    let mut expanded = quote! { #item };
    expanded.extend(
        visitor
            .sort_errs
            .into_iter()
            .map(|err| err.to_compile_error()),
    );
    proc_macro::TokenStream::from(expanded)
}
