use quote::ToTokens;
use std::cell::{Ref, RefCell};
use std::rc::Rc;
use syn::parse::ParseStream;
use syn::{Pat, Token};

mod utils;
mod group;
pub mod expr;
mod chain;

use group::{GroupDeterminer, CommandGroup, ActionGroup};
use expr::{ProcessActionExpr, DefaultActionExpr};
use utils::{is_block_expr, is_valid_expr};
pub use chain::Chain;
pub use expr::ExtractExpr;
use chain::{Unit, UnitResult};
use utils::parse_until;

pub struct ExprChainInternal<Member, GroupDeterminer> {
    members: Vec<Member>,
    group_determiners: Vec<GroupDeterminer>,
    pat: Option<Pat>,
}

pub struct ExprChain<Member, GroupDeterminer> {
    inner: Rc<RefCell<ExprChainInternal<Member, GroupDeterminer>>>,
}

macro_rules! instant_and_deferred_determiners {
    ($($group_type: ident => $($tokens: expr),+; $length: expr),+) => {
        vec![
            $(
                group_determiner!(
                    ActionGroup::Instant(CommandGroup::$group_type) => $($tokens),+; $length
                ),
                group_determiner!(
                    ActionGroup::Deferred(CommandGroup::$group_type) => Token![~], $($tokens),+; $length + 1
                )
            ),*
        ]
    };
}

macro_rules! tokens_checker {
    ($token1: expr, $token2:expr, $token3:expr) => {
        Box::new(|input: &ParseStream|
            input.peek($token1) && input.peek2($token2) && input.peek3($token3)
        )
    };
    ($token1: expr, $token2:expr) => {
        Box::new(|input: &ParseStream| input.peek($token1) && input.peek2($token2))
    };
    ($token: expr) => {
        Box::new(|input: &ParseStream| input.peek($token))
    };
}

macro_rules! group_determiner {
    ($group_type: expr => $($tokens: expr),+; $length: expr; $check_parsed_fn: expr) => {{
        let check_input_fn = tokens_checker!($($tokens),*);
        GroupDeterminer::new(
            $group_type,
            check_input_fn,
            Some($check_parsed_fn),
            $length
        )
    }};
    ($group_type: expr => $($tokens: expr),+; $length: expr) => {
        group_determiner!(
            $group_type => $($tokens),+; $length; Box::new(is_valid_expr)
        )
    };
}

#[derive(Debug, Clone)]
pub struct ProcessWithDefault(pub Option<ProcessActionExpr>, pub Option<DefaultActionExpr>);

pub type ExprChainWithDefault = ExprChain<ProcessWithDefault, GroupDeterminer>;

impl Chain for ExprChainWithDefault {
    type Member = ProcessWithDefault;

    fn new(
        input: ParseStream,
        other_pattern_check: Box<dyn Fn(&ParseStream) -> bool>,
    ) -> syn::Result<Option<ExprChainWithDefault>> {
        let mut group_determiners = instant_and_deferred_determiners! {
            Map => Token![|], Token![>]; 2,
            Then => Token![->]; 2,
            AndThen => Token![=>]; 2,
            Or => Token![<], Token![|]; 2,
            OrElse => Token![<=]; 2,
            Dot => Token![>], Token![.]; 2,
            MapErr => Token![!], Token![>]; 2,
            Inspect => Token![?], Token![>]; 2
        };

        group_determiners.extend(
            vec![
                GroupDeterminer::new(
                    None,
                    other_pattern_check,
                    Some(Box::new(is_valid_expr)),
                    0,
                ),
                GroupDeterminer::new(
                    None,
                    Box::new(|input| input.peek(Token![,])),
                    Some(Box::new(is_valid_expr)),
                    0,
                )
            ]
        );

        let inner = Rc::new(
            RefCell::new(
                ExprChainInternal {
                    members: Vec::new(),
                    group_determiners,
                    pat: None,
                }
            )
        );

        let expr_chain = ExprChain { inner };

        if input.is_empty() {
            Ok(None)
        } else {
            expr_chain.process(input)?;
            Ok(Some(expr_chain))
        }
    }

    fn get_members(&self) -> Ref<'_, Vec<Self::Member>> {
        Ref::map(RefCell::borrow(&self.inner), |inner| &inner.members)
    }

    fn get_pat(&self) -> Ref<'_, Option<Pat>> {
        Ref::map(RefCell::borrow(&self.inner), |inner| &inner.pat)
    }
}

impl ExprChainWithDefault {
    fn parse_unit(&self, input: ParseStream) -> UnitResult {
        let expr_chain = RefCell::borrow(&self.inner);
        parse_until(input, &expr_chain.group_determiners[..])
    }

    fn process(&self, input: ParseStream) -> syn::Result<()> {
        let mut group_type = Some(ActionGroup::Instant(CommandGroup::Initial));
        let mut is_block;
        let mut member_index = 0;

        while {
            let input = input;
            let Unit {
                mut expr,
                next_group_type,
            } = self.parse_unit(input)?;

            if member_index == 0 {
                match expr {
                    ::syn::Expr::Let(let_expr) => {
                        self.inner.borrow_mut().pat = Some(let_expr.pat);
                        expr = *let_expr.expr;
                    }
                    _ => {}
                }
            }

            let (default_expr, next_group_type) = match next_group_type {
                Some(ActionGroup::Instant(CommandGroup::Or)) => {
                    let current_group_type = next_group_type.unwrap();
                    let Unit {
                        expr,
                        next_group_type,
                    } = self.parse_unit(input)?;
                    Some((
                        current_group_type.map_to_default_action_expr(expr),
                        next_group_type,
                    ))
                }
                Some(ActionGroup::Instant(CommandGroup::OrElse)) => {
                    let current_group_type = next_group_type.unwrap();
                    let Unit {
                        expr,
                        next_group_type,
                    } = self.parse_unit(input)?;
                    Some((
                        current_group_type.map_to_default_action_expr(expr),
                        next_group_type,
                    ))
                }
                _ => None,
            }
            .unwrap_or((None, next_group_type));

            let expr = group_type
                .and_then(|action_group_type| action_group_type.map_to_process_action_expr(expr));

            is_block = 
                default_expr
                    .as_ref()
                    .map_or_else(
                        || expr.as_ref().map(|expr| expr.extract_expr().clone()), 
                        |v| Some(v.extract_expr().clone())
                    )
                    .map(|expr| is_block_expr(&expr))
                    .unwrap_or(false);

            let member = ProcessWithDefault(expr, default_expr);

            if member_index == 0
                && match &member {
                    ProcessWithDefault(expr, _) => expr
                        .as_ref()
                        .unwrap()
                        .extract_expr()
                        .into_token_stream()
                        .is_empty(),
                } {
                return Err(input.error("Chain first member can't be empty"));
            }

            if member.0.is_some() || member.1.is_some() {
                self.inner.borrow_mut().members.push(member);
            }

            member_index += 1;

            group_type = next_group_type;

            group_type.is_some()
        } {}

        let ref members = RefCell::borrow(&self.inner).members;

        if members.len() == 0 {
            Err(input.error("Chain can't be empty"))
        } else {
            if is_block {
                input.parse::<Option<Token![,]>>()?;
            } else if !input.is_empty() {
                input.parse::<Token![,]>()?;
            }
            Ok(())
        }
    }
}
