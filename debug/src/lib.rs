extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, parse_quote, Data, DeriveInput, Fields, GenericParam, Generics};

fn debug_format(field: &syn::Field) -> Option<String> {
    for attr in &field.attrs {
        if let Ok(meta) = attr.parse_meta() {
            match meta {
                syn::Meta::NameValue(name_value) => {
                    let path = &name_value.path;
                    if let Some(segment) = path.segments.first() {
                        if segment.ident == "debug" {
                            match &name_value.lit {
                                syn::Lit::Str(lit_str) => return Some(lit_str.value()),
                                _ => unimplemented!(),
                            }
                        }
                    }
                }
                syn::Meta::List(_) | syn::Meta::Path(_) => unimplemented!(),
            }
        }
    }
    None
}

fn is_generic_used(path: &syn::TypePath, generic_type_param: &syn::TypeParam) -> bool {
    for segment in &path.path.segments {
        if segment.ident == "PhantomData" {
            return false;
        }

        if segment.ident == generic_type_param.ident && path.path.segments.len() == 1 {
            return true;
        }

        match &segment.arguments {
            syn::PathArguments::AngleBracketed(angle_bracketed_params) => {
                for arg in &angle_bracketed_params.args {
                    match &arg {
                        syn::GenericArgument::Type(generic_type) => match generic_type {
                            syn::Type::Path(path) => {
                                return is_generic_used(path, generic_type_param);
                            }
                            _ => {}
                        },
                        _ => continue,
                    }
                }
            }
            _ => continue,
        }
    }
    false
}

fn should_add_param_bounds(param: &GenericParam, fields_named: &syn::FieldsNamed) -> bool {
    match &param {
        syn::GenericParam::Type(param_generic_type) => {
            for field in fields_named.named.iter() {
                match &field.ty {
                    syn::Type::Path(path) => {
                        if is_generic_used(path, param_generic_type) {
                            return true;
                        }
                    }
                    _ => continue,
                }
            }
        }
        _ => unimplemented!("blah 2"),
    }
    false
}

fn associated_value_bounds(
    generics: &Generics,
    fields_named: &syn::FieldsNamed,
) -> Vec<syn::TypePath> {
    let mut associated_bounds = Vec::new();
    for field in fields_named.named.iter() {
        match &field.ty {
            syn::Type::Path(type_path) => {
                for segment in &type_path.path.segments {
                    match &segment.arguments {
                        syn::PathArguments::AngleBracketed(angle_bracketed_params) => {
                            for arg in &angle_bracketed_params.args {
                                match &arg {
                                    syn::GenericArgument::Type(generic_type) => {
                                        match generic_type {
                                            syn::Type::Path(path) => {
                                                if let Some(first_segment) =
                                                    path.path.segments.first()
                                                {
                                                    for generic_param in &generics.params {
                                                        match generic_param {
                                                            GenericParam::Type(type_param) => {
                                                                if type_param.ident
                                                                    == first_segment.ident
                                                                    && path.path.segments.len() > 1
                                                                {
                                                                    associated_bounds
                                                                        .push(path.clone());
                                                                }
                                                            }
                                                            _ => {}
                                                        }
                                                    }
                                                }
                                            }
                                            _ => {}
                                        }
                                    }
                                    _ => continue,
                                }
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    associated_bounds
}

fn add_trait_bounds(mut generics: Generics, fields_named: &syn::FieldsNamed) -> Generics {
    for param in &mut generics.params {
        let should_add_param_bounds = should_add_param_bounds(param, fields_named);
        if let GenericParam::Type(ref mut type_param) = *param {
            if should_add_param_bounds {
                type_param.bounds.push(parse_quote!(std::fmt::Debug));
            }
        }
    }
    generics
}

fn bound_attr(attributes: &Vec<syn::Attribute>) -> Result<Option<String>, syn::Error> {
    for attr in attributes {
        if let Ok(meta) = attr.parse_meta() {
            match meta {
                syn::Meta::NameValue(_) => unimplemented!(),
                syn::Meta::Path(_) => unimplemented!(),
                syn::Meta::List(list) => {
                    let path = &list.path;
                    if let Some(segment) = path.segments.first() {
                        if segment.ident == "debug" {
                            if let Some(meta) = list.nested.first() {
                                match meta {
                                    syn::NestedMeta::Meta(meta) => match meta {
                                        syn::Meta::NameValue(name_value) => {
                                            if let Some(segment) = name_value.path.segments.first()
                                            {
                                                if segment.ident == "bound" {
                                                    match &name_value.lit {
                                                        syn::Lit::Str(lit_str) => {
                                                            return Ok(Some(lit_str.value()))
                                                        }
                                                        _ => unimplemented!(),
                                                    }
                                                }
                                            }
                                        }
                                        _ => unimplemented!(),
                                    },
                                    syn::NestedMeta::Lit(_) => unimplemented!(),
                                }
                            }

                            return Err(syn::Error::new_spanned(
                                &list,
                                r#"expected `builder(each = "...")`"#,
                            ));
                        }
                    }
                    continue;
                }
            }
        }
    }
    Ok(None)
}

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let bound_attr_str = match bound_attr(&input.attrs) {
        Ok(str) => str,
        Err(e) => return TokenStream::from(e.to_compile_error()),
    };

    let ident = input.ident;

    let fields_debug = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .map(|field| {
                    let field_ident = field.ident.as_ref().unwrap();
                    if let Some(debug_format) = debug_format(field) {
                        quote! {
                            .field(stringify!(#field_ident), &format_args!(#debug_format, &self.#field_ident))
                        }
                    } else {
                        quote! {
                            .field(stringify!(#field_ident), &self.#field_ident)
                        }
                    }
                })
                .collect::<Vec<proc_macro2::TokenStream>>(),
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    let fields_named = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => fields_named,
            _ => unimplemented!(),
        },
        _ => unimplemented!(),
    };

    let generics = if bound_attr_str.is_some() {
        input.generics
    } else {
        let generics = add_trait_bounds(input.generics, fields_named);
        generics
    };

    let associated_bounds = associated_value_bounds(&generics, fields_named);

    let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

    let where_clause = if let Some(bound_escape_str) = bound_attr_str {
        let mut where_clause = where_clause.cloned().unwrap_or_else(|| syn::WhereClause {
            where_token: syn::token::Where {
                span: proc_macro2::Span::call_site(),
            },
            predicates: syn::punctuated::Punctuated::new(),
        });
        let parsed_predicates =
            match syn::parse_str::<syn::WhereClause>(&format!("where {}", bound_escape_str)) {
                Ok(p) => p,
                Err(_) => unimplemented!(),
            };
        parsed_predicates
            .predicates
            .into_iter()
            .for_each(|p| where_clause.predicates.push(p));
        Some(where_clause)
    } else if associated_bounds.is_empty() {
        where_clause.cloned()
    } else {
        let mut where_clause = where_clause.cloned().unwrap_or_else(|| syn::WhereClause {
            where_token: syn::token::Where {
                span: proc_macro2::Span::call_site(),
            },
            predicates: syn::punctuated::Punctuated::new(),
        });
        for bound in associated_bounds {
            let mut predicate_type = syn::PredicateType {
                lifetimes: None,
                bounded_ty: syn::Type::Path(bound),
                colon_token: syn::token::Colon::default(),
                bounds: syn::punctuated::Punctuated::new(),
            };
            predicate_type
                .bounds
                .push(syn::TypeParamBound::Trait(syn::TraitBound {
                    paren_token: None,
                    modifier: syn::TraitBoundModifier::None,
                    lifetimes: None,
                    path: parse_quote!(std::fmt::Debug),
                }));
            where_clause
                .predicates
                .push(syn::WherePredicate::Type(predicate_type))
        }
        Some(where_clause)
    };

    let expanded = quote! {
        impl #impl_generics std::fmt::Debug for #ident#ty_generics #where_clause {
            fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                f.debug_struct(stringify!(#ident))
                #(#fields_debug)*
                .finish()
            }
        }

    };
    TokenStream::from(expanded)
}
