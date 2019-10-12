//!
//! `GroupDeterminer` is used to determine any `ActionGroup` or separator (for ex. `,`) in `ParseStream` 
//! 

use syn::parse::ParseStream;
use proc_macro2::{TokenStream, TokenTree};

use super::ActionGroup;

pub struct GroupDeterminer {
    group_type: Option<ActionGroup>,
    check_input_fn: Box<dyn Fn(&ParseStream) -> bool>,
    check_parsed_fn: Option<Box<dyn Fn(TokenStream) -> bool>>,
    length: usize,
}

impl GroupDeterminer {
    ///
    /// Constructs new `GroupDeterminer`.
    /// Example: 
    /// ```
    /// use quote::Token;
    /// 
    /// let first_comma_determiner = GroupDeterminer::new(
    ///    None, // Because comma is not an action group
    ///    Box::new(|input| input.peek(Token![,])),
    ///    None,
    ///    1
    /// );
    /// ```
    /// 
    pub fn new(
        group_type: impl Into<Option<ActionGroup>>, 
        check_input_fn: Box<dyn Fn(&ParseStream) -> bool>, 
        check_parsed_fn: Option<Box<dyn Fn(TokenStream) -> bool>>, 
        length: usize
    ) -> Self {
        GroupDeterminer {
            group_type: group_type.into(),
            check_input_fn,
            check_parsed_fn,
            length
        }
    }

    /// 
    /// Returns type of group of `GroupDeterminer`.
    /// 

    pub fn get_group_type(&self) -> Option<ActionGroup> {
        self.group_type
    }

    /// 
    /// Checks if input next tokens are of self group type.
    ///  

    pub fn check_input(&self, input: &ParseStream) -> bool {
        (self.check_input_fn)(input)
    }

    ///
    /// Checks already parsed tokens. In many cases it's used to check if 
    /// parsed tokens are valid expression. in this case we can say for sure that 
    /// we found separator.
    /// 
    pub fn check_parsed(&self, input: TokenStream) -> bool {
        self.check_parsed_fn.as_ref().map(|checker| checker(input)).unwrap_or(true)
    }

    /// 
    /// Used to parse `length` tokens of type `TokenTree` from input `ParseStream`.
    /// 
    pub fn erase_input<'a>(&self, input: ParseStream<'a>) -> syn::Result<ParseStream<'a>> {
        (0..self.length()).try_for_each(|_| input.parse::<TokenTree>().map(|_| ()))?;
        Ok(input)
    }

    ///
    /// Returns value of `length` field.
    /// 
    pub fn length(&self) -> usize {
        self.length
    }
}