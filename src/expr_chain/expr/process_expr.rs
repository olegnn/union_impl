//!
//! `ProcessExpr` used to define type of expressions in process position.
//! 

use syn::Expr;
use quote::{quote, ToTokens};
use proc_macro2::TokenStream;

use super::ExtractExpr;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum ProcessExpr {
    ///
    /// .map(Expr)
    /// 
    Map(Expr),
    ///
    /// Expr()
    /// 
    Then(Expr),
    ///
    /// .and_then(Expr)
    /// 
    AndThen(Expr),
    ///
    /// |value| { Expr(value); value }
    /// 
    Inspect(Expr),
    ///
    /// .map_err(Expr)
    /// 
    MapErr(Expr),
    ///
    /// .Expr
    /// 
    Dot(Expr),
    ///
    /// Expr
    /// 
    Initial(Expr)
}

impl ToTokens for ProcessExpr {
    fn to_tokens(&self, output: &mut TokenStream) {
        let tokens = match self {
            ProcessExpr::AndThen(expr) => {
                quote! { .and_then(#expr) }
            }
            ProcessExpr::Map(expr) => {
                quote! { .map(#expr) }
            }
            ProcessExpr::MapErr(expr) => {
                quote! { .map_err(#expr) }
            }
            ProcessExpr::Dot(expr) => {
                quote! { .#expr }
            }
            ProcessExpr::Then(expr) => {
                quote! {{ let __handler = #expr; __handler }}
            }
            //
            // Not used for now because returning closure requires bound lifetimes
            //
            ProcessExpr::Inspect(_) => {
                //quote! { __inspect(#expr) }
                unimplemented!()
            }
            ProcessExpr::Initial(expr) => {
                quote! { #expr }
            }
        };
        output.extend(tokens);
    }

    fn into_token_stream(self) -> TokenStream {
        let mut output = TokenStream::new();
        self.to_tokens(&mut output);
        output
    }
}

impl ExtractExpr for ProcessExpr {
    type InnerExpr = Expr;

    fn extract_expr(&self) -> &Expr {
        self.extract_inner_expr()
    }

    fn extract_inner_expr(&self) -> &Self::InnerExpr {
        match self {
            Self::Map(expr) => expr,
            Self::Dot(expr) => expr,
            Self::MapErr(expr) => expr,
            Self::AndThen(expr) => expr,
            Self::Then(expr) => expr,
            Self::Initial(expr) => expr,
            Self::Inspect(expr) => expr,
        }
    }
}