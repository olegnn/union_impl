//!
//! `Handler` defines handler function of 3 possible types: `map`, `and_then` and `then`.
//! `map` and `and_then` will be evaluted in case of all successful results and `then` will be evaluated in any case,
//! which allows user to define its own handlers for every error.
//! 

use syn::{Expr, self};
use syn::parse::ParseStream;
use syn::Token;

pub enum Handler {
    Map(Expr),
    Then(Expr),
    AndThen(Expr),
}

mod keywords {
    syn::custom_keyword!(map);
    syn::custom_keyword!(then);
    syn::custom_keyword!(and_then);
}

impl Handler {
    ///
    /// Checks if input `ParseStream` next value is a `Handler` and then if it's true, attempts to parse it, otherwise return `None`.
    /// Will return Err if `ParseStream` must contain `Handler` but it can't be parsed.
    /// 
    pub fn new(input: ParseStream) -> syn::Result<Option<Handler>> {
        let result = if Handler::is_then_handler(&input) {
            input.parse::<keywords::then>()?;
            input.parse::<Token![=>]>()?;
            Some(Handler::Then(input.parse()?))
        } else if Handler::is_and_then_handler(&input) {
            input.parse::<keywords::and_then>()?;
            input.parse::<Token![=>]>()?;
            Some(Handler::AndThen(input.parse()?))
        } else if Handler::is_map_handler(&input) {
            input.parse::<keywords::map>()?;
            input.parse::<Token![=>]>()?;
            Some(Handler::Map(input.parse()?))
        } else {
            None
        };

        if result.is_some() {
            input.parse::<Option<Token![,]>>()?;
        }

        Ok(result)
    }


    ///
    /// Returns true if next value in input `ParseStream` is the definition of `map` `Handler`.
    /// 
    fn is_map_handler(input: &ParseStream) -> bool {
        input.peek(keywords::map)
    }


    ///
    /// Returns true if next value in input `ParseStream` is the definition of `then` `Handler`.
    /// 
    fn is_then_handler(input: &ParseStream) -> bool {
        input.peek(keywords::then)
    }

    ///
    /// Returns true if next value in input `ParseStream` is the definition of `and_then` `Handler`.
    /// 
    fn is_and_then_handler(input: &ParseStream) -> bool {
        input.peek(keywords::and_then)
    }

    ///
    /// Returns true if next value in input `ParseStream` is the definition of `Handler`.
    ///  
    pub fn is_handler(input: &ParseStream) -> bool {
        Handler::is_then_handler(input) 
            || Handler::is_and_then_handler(input) 
            || Handler::is_map_handler(input)
    }
}


