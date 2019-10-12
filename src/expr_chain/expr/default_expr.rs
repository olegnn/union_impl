//!
//! `DefaultExpr` used to define types of expression in default position.
//! 

use syn::Expr;
use quote::{quote, ToTokens};
use proc_macro2::TokenStream;

use super::ExtractExpr;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum DefaultExpr {
    ///
    /// .or(Expr)
    /// 
    Or(Expr),
    ///
    /// .or_else(Expr)
    /// 
    OrElse(Expr),
}

impl ToTokens for DefaultExpr {
    fn to_tokens(&self, output: &mut TokenStream) {
        let tokens = match self {
            DefaultExpr::Or(expr) => {
                quote! { .or(#expr) }
            }
            DefaultExpr::OrElse(expr) => {
                quote! { .or_else(#expr) }
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


impl ExtractExpr for DefaultExpr {
    type InnerExpr = Expr;

    fn extract_expr(&self) -> &Expr {
        self.extract_inner_expr()
    }

    fn extract_inner_expr(&self) -> &Self::InnerExpr {
        match self {
            Self::Or(expr) => expr,
            Self::OrElse(expr) => expr,
        }
    }
}