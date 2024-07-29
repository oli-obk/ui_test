extern crate proc_macro;

use proc_macro::TokenStream;

#[proc_macro_attribute]
pub fn passthrough(_: TokenStream, item: TokenStream) -> TokenStream {
    item
}
