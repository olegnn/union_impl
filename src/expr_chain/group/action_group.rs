//!
//! `ActionGroup` represents two possible types of action: `Instant` and `Deferred`.
//! Any type could be any of `CommandGroup`.
//! 

use syn::Expr;

use super::command_group::{CommandGroup};
use super::super::expr::{ProcessActionExpr, DefaultActionExpr};

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum ActionGroup {
    Instant(CommandGroup),
    Deferred(CommandGroup),
}

impl ActionGroup {

    ///
    /// Attempts to map expr to `ProcessActionExpr` with self type.
    /// Returns None if self isn't of `ProcessExpr` type
    /// 
    pub fn map_to_process_action_expr(&self, expr: Expr) -> Option<ProcessActionExpr> {
        match self {
            ActionGroup::Instant(group) => group
                .map_to_process_expr(expr)
                .map(ProcessActionExpr::Instant),
            ActionGroup::Deferred(group) => group
                .map_to_process_expr(expr)
                .map(ProcessActionExpr::Deferred),
        }
    }

    ///
    /// Attempts to map expr to `DefaultActionExpr` with self type.
    /// Returns None if self isn't of `DefaultExpr` type
    /// 
    pub fn map_to_default_action_expr(&self, expr: Expr) -> Option<DefaultActionExpr> {
        match self {
            ActionGroup::Instant(group) => group
                .map_to_default_expr(expr)
                .map(DefaultActionExpr::Instant),
            ActionGroup::Deferred(group) => group
                .map_to_default_expr(expr)
                .map(DefaultActionExpr::Deferred),
        }
    }
}