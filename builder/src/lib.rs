use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, Data, DeriveInput, Fields, GenericArgument, Ident, PathArguments, Type,
};

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
    let builder_methods_definition = build_builder_methods_definition(&data);
    let builder_build_method = build_builder_build_method(&name, &data);

    let expanded = quote! {
        pub struct #builder_name {
            #builder_fields_definition
        }

        impl #builder_name {
            #builder_methods_definition

            #builder_build_method
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
                    let ty = &f.ty;
                    if find_option_generic_type(ty).is_some() {
                        quote! {
                            #name: Some(None)
                        }
                    } else {
                        quote! {
                            #name: None
                        }
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

/// Define the builder methods.
fn build_builder_methods_definition(data: &Data) -> TokenStream {
    match data {
        Data::Struct(data) => match data.fields {
            Fields::Named(ref fields) => {
                let methods = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    if let Some(option_generic_ty) = find_option_generic_type(ty) {
                        quote! {
                            fn #name(&mut self, #name: #option_generic_ty) -> &mut Self {
                                self.#name = Some(Some(#name));
                                self
                            }
                        }
                    } else {
                        quote! {
                            fn #name(&mut self, #name: #ty) -> &mut Self {
                                self.#name = Some(#name);
                                self
                            }
                        }
                    }
                });
                quote! {
                    #(#methods )*
                }
            }
            Fields::Unit | Fields::Unnamed(_) => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

/// Define the builder's build() method.
fn build_builder_build_method(name: &Ident, data: &Data) -> TokenStream {
    match data {
        Data::Struct(data) => match data.fields {
            Fields::Named(ref fields) => {
                let fields = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let name_str = name
                        .clone()
                        .map(|n| n.to_string())
                        .unwrap_or("(no identifier)".to_string());
                    quote! {
                        #name: self.#name.take().ok_or(format!("field {} was not set", #name_str))?
                    }
                });
                quote! {
                    pub fn build(&mut self) -> Result<#name, Box<dyn std::error::Error>> {
                        Ok(#name {
                            #(#fields,)*
                        })
                    }
                }
            }
            Fields::Unit | Fields::Unnamed(_) => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

/// Find the generic type if the `ty` argument is an `Option` type.
fn find_option_generic_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(ref ty_path) => {
            let segments = &ty_path.path.segments;
            if segments.len() == 1 {
                if let Some(segment) = segments.first() {
                    if segment.ident.to_string() == "Option" {
                        if let PathArguments::AngleBracketed(ref args) = segment.arguments {
                            if let Some(GenericArgument::Type(generic_ty)) = args.args.first() {
                                return Some(generic_ty.clone());
                            }
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}
