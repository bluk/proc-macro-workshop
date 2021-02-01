use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Data, DeriveInput, Fields, Ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the macro input as a syn::DeriveInput syntax tree.
    let input = parse_macro_input!(input as DeriveInput);

    // The name of the type which the derive macro is annotating
    let name = input.ident;

    // The name of the builder
    let builder_name = Ident::new(&format!("{}Builder", name), name.span());
    // let builder_name = quote::format_ident!("{}Builder", name);

    // The type's data (e.g. a struct's fields).
    let data = input.data;

    let builder_fields_definition = build_builder_fields_definition(&data);
    let builder_fields_init = build_builder_fields_init(&data);

    let expanded = quote! {
        pub struct #builder_name {
            #builder_fields_definition
        }

        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #builder_fields_init
                }
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

/// Define the Builder struct fields.
fn build_builder_fields_definition(data: &Data) -> TokenStream {
    match data {
        Data::Struct(data) => match data.fields {
            Fields::Named(ref fields) => {
                let fields = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    quote! {
                        #name: Option<#ty>
                    }
                });
                quote! {
                    #(#fields,)*
                }
            }
            Fields::Unit | Fields::Unnamed(_) => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

/// Build the code to init the Builder struct fields in the builder() method.
fn build_builder_fields_init(data: &Data) -> TokenStream {
    match data {
        Data::Struct(data) => match data.fields {
            Fields::Named(ref fields) => {
                let fields = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    quote! {
                        #name: None
                    }
                });
                quote! {
                    #(#fields,)*
                }
            }
            Fields::Unit | Fields::Unnamed(_) => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}
