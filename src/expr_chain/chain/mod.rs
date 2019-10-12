//!
//! `Chain` trait describes any chain which can be constructed from `ParseStream`
//! with members of type `Member` and optional `Pat`.
//! 

use std::cell::Ref;
use syn::Pat;
use syn::parse::ParseStream;

mod unit;

pub use unit::{UnitResult, Unit};

pub trait Chain
where
    Self: Sized,
{
    type Member;

    fn new(
        input: ParseStream,
        other_pattern_check: Box<dyn Fn(&ParseStream) -> bool>,
    ) -> syn::Result<Option<Self>>;

    fn get_members(&self) -> Ref<'_, Vec<Self::Member>>;

    fn get_pat(&self) -> Ref<'_, Option<Pat>>;
}

