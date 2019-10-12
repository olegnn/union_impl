//!
//! Implementation of the `union!` macro.
//! 
//! 

extern crate proc_macro;
extern crate proc_macro2;
extern crate proc_macro_hack;
extern crate quote;
extern crate syn;

mod expr_chain;
mod handler;
mod union;

use union::{generate_union, Union, Config};
use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;

fn union_impl(
    union: Union,
    config: Config
) -> TokenStream {
    TokenStream::from(generate_union(union, config))
}

#[proc_macro_hack]
pub fn union(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: false,
            spawn: false,
        },
    )
}

#[proc_macro_hack]
pub fn union_async(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: true,
            spawn: false,
        },
    )
}

#[proc_macro_hack]
pub fn asyncion(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: true,
            spawn: false,
        },
    )
}

#[proc_macro_hack]
pub fn union_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: false,
            spawn: true,
        },
    )
}

#[proc_macro_hack]
pub fn spawnion(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: false,
            spawn: true,
        },
    )
}

#[proc_macro_hack]
pub fn union_async_spawn(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: true,
            spawn: true,
        },
    )
}

#[proc_macro_hack]
pub fn spasyncion(input: TokenStream) -> TokenStream {
    let parsed = syn::parse_macro_input!(input as Union);

    union_impl(
        parsed,
        Config {
            is_async: true,
            spawn: true,
        },
    )
}
