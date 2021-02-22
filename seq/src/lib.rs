use quote::quote;
use syn::{
    parse::{Parse, ParseStream, Result},
    parse_macro_input, Expr, Ident, LitInt, Token,
};

struct Seq {
    _ident: Ident,
    _start: LitInt,
    _end: LitInt,
    _expr: Expr,
}

impl Parse for Seq {
    fn parse(input: ParseStream) -> Result<Self> {
        let ident: Ident = input.parse()?;
        input.parse::<Token![in]>()?;
        let start = input.parse::<LitInt>()?;
        input.parse::<Token![..]>()?;
        let end = input.parse::<LitInt>()?;
        let expr: Expr = input.parse()?;
        Ok(Seq {
            _ident: ident,
            _start: start,
            _end: end,
            _expr: expr,
        })
    }
}

#[proc_macro]
pub fn seq(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let Seq { .. } = parse_macro_input!(input as Seq);

    proc_macro::TokenStream::from(quote! {})
}
