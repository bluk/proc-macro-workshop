use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, Data, DeriveInput, Error, Field, Fields, GenericArgument,
    Ident, Meta, PathArguments, Type,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    // Parse the macro input as a syn::DeriveInput syntax tree.
    let input = parse_macro_input!(input as DeriveInput);

    let output = derive_builder(input).unwrap_or_else(|e| e.to_compile_error());

    proc_macro::TokenStream::from(output)
}

fn derive_builder(input: DeriveInput) -> Result<TokenStream, Error> {
    // The name of the type which the derive macro is annotating
    let name = input.ident;

    // The name of the builder
    let builder_name = Ident::new(&format!("{}Builder", name), name.span());
    // let builder_name = quote::format_ident!("{}Builder", name);

    // The type's data (e.g. a struct's fields).
    let data = input.data;

    let builder_fields_definition = build_builder_fields_definition(&data);
    let builder_fields_init = build_builder_fields_init(&data)?;
    let builder_methods_definition = build_builder_methods_definition(&data)?;
    let builder_build_method = build_builder_build_method(&name, &data);

    Ok(quote! {
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
    })
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
fn build_builder_fields_init(data: &Data) -> Result<TokenStream, Error> {
    match data {
        Data::Struct(data) => match data.fields {
            Fields::Named(ref fields) => {
                let fields = fields
                    .named
                    .iter()
                    .map(|f| {
                        let name = &f.ident;
                        let ty = &f.ty;
                        Ok(if find_option_generic_type(ty).is_some() {
                            quote! {
                                #name: Some(None)
                            }
                        } else if find_builder_attribute_each_arg(f)?.is_some() {
                            quote! {
                                #name: Some(Vec::new())
                            }
                        } else {
                            quote! {
                                #name: None
                            }
                        })
                    })
                    .collect::<Result<Vec<TokenStream>, Error>>()?;
                Ok(quote! {
                    #(#fields,)*
                })
            }
            Fields::Unit | Fields::Unnamed(_) => unimplemented!(),
        },
        _ => unimplemented!(),
    }
}

/// Define the builder methods.
fn build_builder_methods_definition(data: &Data) -> Result<TokenStream, Error> {
    match data {
        Data::Struct(data) => match data.fields {
            Fields::Named(ref fields) => {
                let methods = fields.named.iter().map(|f| {
                    let name = &f.ident;
                    let ty = &f.ty;
                    if let Some(option_generic_ty) = find_option_generic_type(ty) {
                        Ok(quote! {
                            fn #name(&mut self, #name: #option_generic_ty) -> &mut Self {
                                self.#name = Some(Some(#name));
                                self
                            }
                        })
                    } else if let Some(each_arg) = find_builder_attribute_each_arg(f)? {
                        let each_arg_ident = Ident::new(&each_arg, f.span());
                        let vec_generic_ty =
                            find_vec_generic_type(ty).expect("generic type to exist");
                        let each_arg_method = quote! {
                            fn #each_arg_ident(&mut self, #each_arg_ident: #vec_generic_ty) -> &mut Self {
                                if let Some(ref mut v) = self.#name.as_mut() {
                                    v.push(#each_arg_ident);
                                } else {
                                    let mut v = Vec::new();
                                    v.push(#each_arg_ident);
                                    self.#name = Some(v);
                                }
                                self
                            }
                        };

                        let all_method = if Some(each_arg) == name.as_ref().map(|n| n.to_string()) {
                            quote! {}
                        } else {
                            quote! {
                                fn #name(&mut self, #name: #ty) -> &mut Self {
                                    self.#name = Some(#name);
                                    self
                                }
                            }
                        };

                        Ok(quote! {
                            #each_arg_method
                            #all_method
                        })
                    } else {
                        Ok(quote! {
                            fn #name(&mut self, #name: #ty) -> &mut Self {
                                self.#name = Some(#name);
                                self
                            }
                        })
                    }
                }).collect::<Result<Vec<TokenStream>, Error>>()?;
                Ok(quote! {
                    #(#methods )*
                })
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

/// Determine if there is a `builder` attribute on a field and return the `each` value if it exists.
fn find_builder_attribute_each_arg(field: &Field) -> Result<Option<String>, Error> {
    for attr in &field.attrs {
        let meta = attr.parse_meta().expect("can parse meta attributes");
        match &meta {
            Meta::List(list) => {
                let path_segments = &list.path.segments;
                if path_segments.len() == 1 {
                    if let Some(first_segment) = path_segments.first() {
                        if first_segment.ident.to_string() == "builder" {
                            for nested in &list.nested {
                                if let syn::NestedMeta::Meta(nested_meta) = nested {
                                    match nested_meta {
                                        Meta::NameValue(ref name_value) => {
                                            if let Some(segment) = name_value.path.segments.first()
                                            {
                                                if segment.ident.to_string() == "each" {
                                                    if let syn::Lit::Str(ref lit_str) =
                                                        name_value.lit
                                                    {
                                                        return Ok(Some(lit_str.value()));
                                                    }
                                                } else {
                                                    return Err(syn::Error::new_spanned(
                                                        &meta,
                                                        "expected `builder(each = \"...\")`",
                                                    ));
                                                }
                                            }
                                        }
                                        _ => unimplemented!(),
                                    }
                                }
                            }
                        }
                    }
                }
            }
            _ => unimplemented!(),
        }
    }
    Ok(None)
}

/// Find the generic type if the `ty` argument is a `Vec` type.
fn find_vec_generic_type(ty: &Type) -> Option<Type> {
    match ty {
        Type::Path(ref ty_path) => {
            let segments = &ty_path.path.segments;
            if segments.len() == 1 {
                if let Some(segment) = segments.first() {
                    if segment.ident.to_string() == "Vec" {
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
