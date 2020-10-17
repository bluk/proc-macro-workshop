extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{format_ident, quote};
use syn::{parse_macro_input, Data, DeriveInput, Fields, GenericArgument, PathArguments, Type};

fn unwrapped_option_type(ty: &syn::Type) -> Option<&syn::Type> {
    match ty {
        Type::Path(type_path) => {
            let segments = &type_path.path.segments;
            if let Some(segment) = segments.first() {
                if segment.ident.to_string() == "Option" {
                    match &segment.arguments {
                        PathArguments::AngleBracketed(args) => {
                            if let Some(generic_arg) = args.args.first() {
                                match generic_arg {
                                    GenericArgument::Type(ty) => Some(ty),
                                    _ => unimplemented!(),
                                }
                            } else {
                                None
                            }
                        }
                        PathArguments::Parenthesized(_) | PathArguments::None => unimplemented!(),
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        _ => unimplemented!(),
    }
}

fn unwrapped_vec_type(ty: &syn::Type) -> Option<&syn::Type> {
    match ty {
        Type::Path(type_path) => {
            let segments = &type_path.path.segments;
            if let Some(segment) = segments.first() {
                if segment.ident.to_string() == "Vec" {
                    match &segment.arguments {
                        PathArguments::AngleBracketed(args) => {
                            if let Some(generic_arg) = args.args.first() {
                                match generic_arg {
                                    GenericArgument::Type(ty) => Some(ty),
                                    _ => unimplemented!(),
                                }
                            } else {
                                None
                            }
                        }
                        PathArguments::Parenthesized(_) | PathArguments::None => unimplemented!(),
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        _ => unimplemented!(),
    }
}

fn each_name(field: &syn::Field) -> Result<Option<String>, syn::Error> {
    for attr in &field.attrs {
        if let Ok(meta) = attr.parse_meta() {
            match meta {
                syn::Meta::NameValue(_) => unimplemented!(),
                syn::Meta::Path(_) => unimplemented!(),
                syn::Meta::List(list) => {
                    let path = &list.path;
                    if let Some(segment) = path.segments.first() {
                        if segment.ident == "builder" {
                            if let Some(meta) = list.nested.first() {
                                match meta {
                                    syn::NestedMeta::Meta(meta) => match meta {
                                        syn::Meta::NameValue(name_value) => {
                                            if let Some(segment) = name_value.path.segments.first()
                                            {
                                                if segment.ident == "each" {
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

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = input.ident;
    let builder_ident = format_ident!("{}Builder", ident);

    let fields_init = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .map(|field| {
                    let field_ident = field.ident.as_ref().unwrap();
                    let is_vec_type = unwrapped_vec_type(&field.ty).is_some();
                    if is_vec_type {
                        quote! {
                            #field_ident: Some(std::vec::Vec::new())
                        }
                    } else {
                        quote! {
                            #field_ident: None
                        }
                    }
                })
                .collect::<Vec<proc_macro2::TokenStream>>(),
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    let fields = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .map(|field| {
                    let field_ident = field.ident.as_ref().unwrap();
                    let field_ty = unwrapped_option_type(&field.ty).or(Some(&field.ty));
                    quote! {
                        #field_ident: core::option::Option<#field_ty>
                    }
                })
                .collect::<Vec<proc_macro2::TokenStream>>(),
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    let methods = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .map(|field| {
                    let field_ident = field.ident.as_ref().unwrap();
                    let field_ty = unwrapped_option_type(&field.ty).or(Some(&field.ty));
                    let each_name_result = each_name(field)?;
                    if let Some(each_name) = each_name_result {
                        let each_ident =
                            syn::Ident::new(&each_name, proc_macro2::Span::call_site());
                        if let Some(vec_type) = unwrapped_vec_type(field_ty.unwrap()) {
                            if field_ident == &each_name {
                                Ok(quote! {
                                    fn #each_ident(&mut self, #each_ident: #vec_type) -> &mut Self {
                                        self.#field_ident.as_mut().unwrap().push(#each_ident);
                                        self
                                    }
                                })
                            } else {
                                Ok(quote! {
                                    fn #each_ident(&mut self, #each_ident: #vec_type) -> &mut Self {
                                        self.#field_ident.as_mut().unwrap().push(#each_ident);
                                        self
                                    }

                                    fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                                        self.#field_ident = Some(#field_ident);
                                        self
                                    }
                                })
                            }
                        } else {
                            unimplemented!()
                        }
                    } else {
                        Ok(quote! {
                            fn #field_ident(&mut self, #field_ident: #field_ty) -> &mut Self {
                                self.#field_ident = Some(#field_ident);
                                self
                            }
                        })
                    }
                })
                .collect::<Result<Vec<proc_macro2::TokenStream>, syn::Error>>(),
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    let methods = match methods {
        Ok(methods) => methods,
        Err(err) => return TokenStream::from(err.to_compile_error()),
    };

    let builder_init = match &input.data {
        Data::Struct(data_struct) => match &data_struct.fields {
            Fields::Named(fields_named) => fields_named
                .named
                .iter()
                .map(|field| {
                    let field_ident = field.ident.as_ref().unwrap();
                    let is_option = unwrapped_option_type(&field.ty).is_some();
                    if is_option {
                        quote! {
                            #field_ident: self.#field_ident.take()
                        }
                    } else {
                        quote! {
                            #field_ident: self.#field_ident.take().ok_or_else(|| "not initialized: #field_ident")?
                        }
                    }
                })
                .collect::<Vec<proc_macro2::TokenStream>>(),
            Fields::Unnamed(_) | Fields::Unit => unimplemented!(),
        },
        Data::Enum(_) | Data::Union(_) => unimplemented!(),
    };

    let expanded = quote! {
        impl #ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#fields_init),*
                }
            }
        }

        pub struct #builder_ident {
            #(#fields),*
        }

        impl #builder_ident {
            #(#methods)*

            pub fn build(&mut self) -> core::result::Result<#ident, std::boxed::Box<dyn std::error::Error>> {
                core::result::Result::Ok(#ident {
                    #(#builder_init),*
                })
            }
        }
    };
    TokenStream::from(expanded)
}
