//!
//! `ActionExpr` defines two types of action: `Instant` and `Deferred`.
//! `ProcessActionExpr` is `ActionExpr` which actions are `ProcessExpr`.
//! `DefaultActionExpr` is `ActionExpr` which actions are `DefaultExpr`.
//! 

use syn::Expr;

use super::{ProcessExpr, DefaultExpr, ExtractExpr};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ActionExpr<Expr> {
    ///
    /// Action which will be applied to given value instantly.
    /// 
    Instant(Expr),
    ///
    /// Action which will be applied after all chains have finished their actions on current step.
    /// 
    Deferred(Expr),
}

pub type ProcessActionExpr = ActionExpr<ProcessExpr>;

pub type DefaultActionExpr = ActionExpr<DefaultExpr>;

impl ExtractExpr for ProcessActionExpr {
    type InnerExpr = ProcessExpr;

    fn extract_expr(&self) -> &Expr {
        self.extract_inner_expr().extract_expr()
    }

    fn extract_inner_expr(&self) -> &Self::InnerExpr {
        match self {
            ProcessActionExpr::Instant(expr) => expr,
            ProcessActionExpr::Deferred(expr) => expr,
        }
    }
}

impl ExtractExpr for DefaultActionExpr {
    type InnerExpr = DefaultExpr;
    
    fn extract_expr(&self) -> &Expr {
        self.extract_inner_expr().extract_expr()
    }

    fn extract_inner_expr(&self) -> &Self::InnerExpr {
        match self {
            DefaultActionExpr::Instant(expr) => expr,
            DefaultActionExpr::Deferred(expr) => expr,
        }
    }
}
