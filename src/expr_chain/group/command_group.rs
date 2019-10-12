//!
//! `CommandGroup` is an enum of all possible `ProcessExpr` and `DefaultExpr` operations.
//! Used to express group which was found in input `ParseStream`  
//!

use syn::Expr;

use super::super::expr::{ProcessExpr, DefaultExpr};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CommandGroup {
    /// [ProcessExpr::Map]
    Map,
    /// [ProcessExpr::Dot]
    Dot,
    /// [ProcessExpr::Inspect]
    Inspect,
    /// [ProcessExpr::Then]
    Then,
    /// [ProcessExpr::AndThen]
    AndThen,
    /// [DefaultExpr::Or]
    Or,
    /// [DefaultExpr::OrElse]
    OrElse,
    /// [ProcessExpr::MapErr]
    MapErr,
    /// [ProcessExpr::Initial]
    Initial
}

impl CommandGroup {

    ///
    /// Attempts to map expr to `ProcessExpr` with self type.
    /// Returns None if self isn't of `ProcessExpr` type
    /// 
    pub fn map_to_process_expr(&self, expr: Expr) -> Option<ProcessExpr> {
        match self {
            CommandGroup::Map => Some(ProcessExpr::Map(expr)),
            CommandGroup::AndThen => Some(ProcessExpr::AndThen(expr)),
            CommandGroup::Dot => Some(ProcessExpr::Dot(expr)),
            CommandGroup::Then => Some(ProcessExpr::Then(expr)),
            CommandGroup::MapErr => Some(ProcessExpr::MapErr(expr)),
            CommandGroup::Initial => Some(ProcessExpr::Initial(expr)),
            CommandGroup::Inspect => Some(ProcessExpr::Inspect(expr)),
            _ => None,
        }
    }

    ///
    /// Attempts to map expr to `DefaultExpr` with self type.
    /// Returns None if self isn't of `DefaultExpr` type
    /// 
    pub fn map_to_default_expr(&self, expr: Expr) -> Option<DefaultExpr> {
        match self {
            CommandGroup::Or => Some(DefaultExpr::Or(expr)),
            CommandGroup::OrElse => Some(DefaultExpr::OrElse(expr)),
            _ => None,
        }
    }
}