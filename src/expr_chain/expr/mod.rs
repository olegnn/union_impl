use syn::Expr;

mod action_expr;
mod process_expr;
mod default_expr;

pub use action_expr::{ActionExpr, ProcessActionExpr, DefaultActionExpr};
pub use process_expr::ProcessExpr;
pub use default_expr::DefaultExpr;

///
/// Trait which allows to extract `Expr` and `InnerExpr` (which can be any superset of `Expr`).
/// 
pub trait ExtractExpr {
    ///
    /// Defines superset of `Expr` used by given struct.
    /// 
    type InnerExpr;

    ///
    /// Extracts `Expr` from given value.
    /// 
    fn extract_expr(&self) -> &Expr;

    ///
    /// Extracts `InnerExpr`. If `InnerExpr` is `Expr`, acts as `extract_expr`.
    /// 
    fn extract_inner_expr(&self) -> &Self::InnerExpr;
}
