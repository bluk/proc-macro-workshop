use proc_macro::TokenStream;

use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    // Parse the macro input as a syn::DeriveInput syntax tree.
    let _input = parse_macro_input!(input as DeriveInput);

    // Return an empty TokenStream.
    TokenStream::new()
}
