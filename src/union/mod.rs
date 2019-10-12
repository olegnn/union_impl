//!
//! Contains `Parser` trait impl for `Union` and `union!` macro code generator
//! 

use quote::ToTokens;
use proc_macro2::TokenStream;
use quote::quote;
use syn::parse::{Parse, ParseStream};

mod name_constructors;

use name_constructors::*;
use crate::expr_chain::{ExprChainWithDefault, Chain, ExtractExpr, ProcessWithDefault};
use crate::expr_chain::expr::{ProcessExpr, ProcessActionExpr, DefaultActionExpr};
use crate::handler::Handler;

pub struct Union {
    pub branches: Vec<ExprChainWithDefault>,
    pub handler: Option<Handler>,
}

///
/// Parser which takes expression chains and puts them into `branches` field,
/// and handler (one of `map`, `and_then`, `then`) and puts it into `handler` field.
/// Handler can be either defined once or not defined.
/// 
impl Parse for Union {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let mut union = Union {
            branches: Vec::new(),
            handler: None,
        };

        while !input.is_empty() {
            if Handler::is_handler(&input) {
                if union.handler.is_some() {
                    return Err(input.error("Multiple `handler` cases found, only one allowed. Please, specify one of `map`, `and_then`, `then`."));
                }
                let handler = Handler::new(input)?.expect(
                    "union: Handler `is_handler` check failed. This's a bug, please report it.",
                );
                union.handler = Some(handler);
            } else {
                let expr_chain = ExprChainWithDefault::new(
                    input,
                    Box::new(Handler::is_handler),
                )?;
                expr_chain.map(|expr_chain| union.branches.push(expr_chain));
            };
        }

        if union.branches.len() == 0 {
            Err(input.error("union must contain at least 1 branch."))
        } else {
            Ok(union)
        }
    }
}

pub struct Config {
    pub is_async: bool,
    pub spawn: bool,
    //pub is_try: bool
}

pub fn generate_union(
    Union { branches, handler }: Union,
    Config {
        is_async,
        spawn, /*, is_try*/
    }: Config
) -> TokenStream {
    let empty_stream = TokenStream::new();

    //
    // Total branch count.
    // 
    let branch_count = branches.len();

    //
    // Spawn sync threads using `std::thread` module.
    //
    let sync_spawn = spawn && !is_async;

    //
    // Spawn async threads using `::tokio::spawn` from `tokio` crate.
    // 

    let previous_result_handler: Box<dyn Fn(TokenStream) -> TokenStream> = if sync_spawn {
        //
        // In case of sync thread we must `join` std::thread's `JoinHandle` in order to get `Result`. It may panic if thread paniced.
        // 
        Box::new(|value| quote! { #value.join().unwrap() })
    } else if is_async {
        //
        // In case of async `union` we should wrap given result into a `Future`
        // 
        Box::new(|value| quote! { { async move { #value } } })
    } else {
        //
        // Otherwise it will be enough to just use previous result.
        // 
        Box::new(|value| quote! { #value })
    };
    
    let (
        //
        // Contains all chains depths. Used to calculate max length and determine if we reached chain's end and don't need to join branches anymore.
        //
        depths,
        //
        // `ProcessWithDefault` groups each of which represents chain of `Instant` actions but every next group is `Deferred` from previous.
        // [[map, or_else, map, and_then], [map, and_then]] => 
        // it will be interpreted as #expr.map().or_else.and_then() and after first step ends
        // #expr.map().and_then()
        //
        chains
    ): (
        Vec<usize>, 
        Vec<Vec<Vec<ProcessWithDefault>>>
    ) = branches
        .iter()
        .map(|expr_chain| {
            expr_chain.get_members().iter().fold(
                (1usize, vec![Vec::new()]),
                |(depth, mut chain_acc), member| 
                    match &member.0 {
                        Some(ProcessActionExpr::Deferred(_)) => match member.1 {
                            Some(DefaultActionExpr::Instant(_)) | None => {
                                chain_acc.push(vec![member.clone()]);
                                (depth + 1, chain_acc)
                            }
                            Some(DefaultActionExpr::Deferred(_)) => {
                                chain_acc.push(vec![ProcessWithDefault(member.0.clone(), None)]);
                                chain_acc.push(vec![ProcessWithDefault(None, member.1.clone())]);
                                (depth + 2, chain_acc)
                            }
                        },
                    _ => match &member.1 {
                        Some(DefaultActionExpr::Instant(_)) | None => {
                            chain_acc.last_mut().unwrap().push(member.clone());
                            (depth, chain_acc)
                        }
                        Some(DefaultActionExpr::Deferred(_)) => {
                            chain_acc
                                .last_mut()
                                .unwrap()
                                .push(ProcessWithDefault(member.0.clone(), None));
                            chain_acc.push(vec![ProcessWithDefault(None, member.1.clone())]);
                            (depth + 1, chain_acc)
                        }
                    },
                },
            )
        })
        .unzip();

    //
    // Returns `Pat` name if exists, otherwise generate default chain result name.
    // 
    let get_chain_result_name = |chain_index: usize| -> TokenStream {
        branches[chain_index]
            .get_pat()
            .as_ref()
            .map(|pat| quote! { #pat })
            .unwrap_or_else(|| {
                let name = construct_result_name(chain_index);
                name.into_token_stream()
            })
    };
    
    //
    // Calculates max chain depth.
    // 
    let max_depth = *depths.iter().max().unwrap_or(&0) + (sync_spawn as usize);

    //
    // Contains all generated code to be executed step by step before final results.
    // 
    let mut results_by_step = Vec::new();

    for step_number in 0..max_depth {
        if step_number > 0 {
            //
            // If we already have tuple of results, deconstruct them before use in order to every result could be captured by its chain closure correclty.
            // 
            let previous_step_result_name = construct_step_result_name(step_number - 1);
            let results = (0..branch_count).map(|chain_index| {
                let result_name = get_chain_result_name(chain_index);
                quote! { #result_name }
            });
            results_by_step.push(quote! {
                let (#( #results ),*) = #previous_step_result_name;
            });
        }

        let step_actions = chains.iter().map(|chain| chain.get(step_number as usize));

        let step_results = step_actions
            .enumerate()
            .map(|(chain_index, chain_step_actions)| match chain_step_actions {
                Some(chain) => chain
                    .into_iter()
                    .fold(None, |acc, ProcessWithDefault(expr, default_expr)| {
                        let or_clause = 
                            default_expr
                                .as_ref()
                                .map(ExtractExpr::extract_inner_expr)
                                .map(|expr| expr.into_token_stream())
                                .unwrap_or_else(|| empty_stream.clone());
                        acc.map(|previous_result| {
                            match &expr {
                                Some(ProcessActionExpr::Instant(expr)) => {
                                    match expr {
                                        ProcessExpr::Then(_) => {
                                            let handler = expr.into_token_stream();
                                            quote! { #handler(#previous_result)#or_clause }
                                        }
                                        ProcessExpr::Inspect(expr) => {
                                            //
                                            // Define custom `into_token_stream` converter because `__inspect` function signature accepts two params.
                                            //  
                                            quote! { __inspect(#expr, #previous_result)#or_clause }
                                        }
                                        _ => {
                                            quote! { #previous_result#expr#or_clause } 
                                        }
                                    }
                                }
                                None => {
                                    quote! { #previous_result#or_clause } 
                                }
                                _ => panic!("union: Unexpected expression type. This is a bug, please report it."),
                            }
                        })
                        .or_else(|| {
                            Some(match expr {
                                Some(ProcessActionExpr::Deferred(deferred)) => {
                                    let previous_result_name = get_chain_result_name(chain_index);
                                    let previous_result = previous_result_handler(quote! { #previous_result_name });
                                    match deferred {
                                        ProcessExpr::Then(_) => {
                                            quote! { #deferred(#previous_result)#or_clause }
                                        }
                                        _ => {
                                            quote! { #previous_result#deferred#or_clause } 
                                        }
                                    }
                                }
                                Some(ProcessActionExpr::Instant(expr)) => quote! { #expr#or_clause },
                                None => {
                                    let previous_result_name = get_chain_result_name(chain_index);
                                    let previous_result = previous_result_handler(quote! { #previous_result_name });
                                    quote! { #previous_result#or_clause }
                                }
                            })
                        })
                    })
                    .map(|chain| {
                        if spawn {
                            if is_async {
                                quote! {
                                    __spawn_tokio(#chain)
                                }
                            } else {
                                let thread_builder_name = construct_thread_builder_name(chain_index);
                                quote! {
                                    #thread_builder_name.spawn(|| #chain ).unwrap() 
                                }
                            }
                        } else {
                            chain
                        }
                    })
                    .unwrap_or_else(|| empty_stream.clone()),
                None => {
                    let previous_result_name = get_chain_result_name(chain_index);
                    if sync_spawn && step_number <= depths[chain_index] || is_async {
                        previous_result_handler(quote! { #previous_result_name })
                    } else {
                        quote! { #previous_result_name }
                    }
                }
            });
        //
        // Name of variable which contains tuple of current step results.
        //
        let step_result_name = construct_step_result_name(step_number);

        results_by_step.push(
            if is_async {
                quote! { 
                    let #step_result_name = ::futures::join!(#( #step_results ),*); 
                }
            } else {
                //
                // In case of sync spawn generate thread builder for every chain.
                //
                let thread_builders = if spawn { 
                    (0..branch_count).map(|chain_index| {
                        let thread_name = construct_thread_name(chain_index).to_string();
                        let thread_builder_name = construct_thread_builder_name(chain_index);
                        quote! {
                            let #thread_builder_name = ::std::thread::Builder::new();
                            let #thread_builder_name = #thread_builder_name.name(
                                ::std::thread::current().name()
                                    .map(
                                        |current_thread_name| 
                                            format!("{current_thread_name}_{new_thread_name}", 
                                                current_thread_name=current_thread_name, 
                                                new_thread_name=#thread_name
                                            )
                                    )
                                    .unwrap_or(#thread_name.to_owned())

                            );
                        }
                    }).collect()
                } else {
                    Vec::new()
                };
                quote! {
                    #( #thread_builders );*
                    let #step_result_name = (#( #step_results ),*);
                }
            }
        );
    }

    let last_step_results = construct_step_result_name(max_depth - 1);

    //
    // Return last step results at the end of expression.
    // 
    let results = quote! { { #( #results_by_step )* #last_step_results } };

    //
    // Define variable names to be used when destructuring results.
    // 
    let result_vars: Vec<_> = (0..branch_count)
        .map(|index| {
            let result_name = construct_result_name(index);
            quote! { #result_name }
        })
        .collect();

    //
    // Will transpose tuple of results in result of tuple. 
    // 
    // (Result<A, Error>, Result<B, Error>, Result<C, Error>) => Result<(A, B, C), Error>
    // 
    // ```
    // result0.and_then(|value0| result1.and_then(|value1| result2.map(|value2| (value0, value1, value2))))
    // ```
    // 
    // 
    let generate_results_unwrapper = || {
        (0..branch_count).fold(None, |acc, index| {
            let index = branch_count - index - 1;
            let value_var_name = construct_var_name(index);
            let result_var_name = construct_result_name(index);
            acc
                .and_then(|acc|
                    Some(quote! { #result_var_name.and_then(|#value_var_name| #acc ) })
                )
                .or_else(|| {
                    // 
                    // Generates final tuple of unwrapped results.
                    // 
                    let tuple_values = (0..branch_count).map(|index| {
                        let value_var_name = construct_var_name(index);
                        quote! { #value_var_name }
                    });
                    Some(quote! { #result_var_name.map(|#value_var_name| (#( #tuple_values ),*) ) })
                })
        })
    };

    let results_wrapper = if is_async {
        quote! { async move { __results } }
    } else {
        quote! { __results }
    };

    //
    // Defines handler based on user input or returns result of tuple of values 
    // [or single result in case of one branch].
    // 
    let handle_results = handler.as_ref().map_or_else(
        || {
            let unwrap_results = generate_results_unwrapper();
            if branch_count == 1 && is_async  {
                //
                // Return first tuple element if have one branch
                //
                quote! {
                    __results.0
                }
            } else {
               //
               // Transform tuple of results in result of tuple
               //
               quote! {
                    let (#( #result_vars ),*) = __results;
                    let __results = #unwrap_results;
                    __results
                }
            }            
        },
        |handler| match handler {
            Handler::Then(handler) => {
                //
                // Don't unwrap results because handler accepts results.
                // 
                quote! {
                    let __handler = #handler;
                    let (#( #result_vars ),*) = __results;
                    __handler(#( #result_vars ),*)
                }
            }
            Handler::Map(handler) => {
                //
                // Unwrap results and pass them to handler if all of them are `Ok` (`Some`). Otherwise return `Err` (`None`).
                // 
                let unwrap_results = generate_results_unwrapper();

                quote! { 
                    let (#( #result_vars ),*) = __results;
                    let __results = #unwrap_results;
                    #results_wrapper.map(|__results| { 
                        let __handler = #handler; 
                        let (#( #result_vars ),*) = __results; 
                        __handler(#( #result_vars ),*) 
                    })
                }
            }
            Handler::AndThen(handler) => {
                //
                // Unwrap results and pass them to handler if all of them are `Ok` (`Some`). Otherwise return `Err` (`None`).
                // 
                let unwrap_results = generate_results_unwrapper();

                quote! {
                    let (#( #result_vars ),*) = __results;
                    let __results = #unwrap_results;
                    #results_wrapper.and_then(|__results| { 
                        let __handler = #handler; 
                        let (#( #result_vars ),*) = __results; 
                        __handler(#( #result_vars ),*) 
                    })
                }
            }
        },
    );

    let inspect_definition = quote! {
        fn __inspect<I>(handler: impl Fn(&I) -> (), input: I) -> I {
            handler(&input);
            input
        }
    };
    
    if is_async {
        let async_spawn_fn_definition = if spawn {
            quote! {
                async fn __spawn_tokio<T, F: ::futures::future::Future<Output = T>>(future: F) -> T
                where
                    F: Send + Sync + 'static,
                    T: Send + Sync + 'static,
                {
                    let (tx, rx) = ::futures::channel::oneshot::channel();

                    ::tokio::spawn(async move {
                        let value = future.await;
                        tx.send(value).unwrap_or_else(|_| panic!("Unexpected ::futures::channel::oneshot::channel panic"));
                    });

                    rx.await.unwrap_or_else(|_| panic!("Unexpected ::futures::channel::oneshot::channel panic"))
                }
            }
        } else {
            empty_stream.clone()
        };
        let await_handler = if handler.is_some() {
            quote! { .await }
        } else {
            empty_stream.clone()
        };
        quote! { 
            async move {
                use ::futures::{FutureExt, TryFutureExt};
                use ::futures::{StreamExt, TryStreamExt};
                #async_spawn_fn_definition
                #inspect_definition
                let __results = #results;
                #handle_results#await_handler
            }
        }
    } else {
        quote! {{
            #inspect_definition
            let __results = #results;
            #handle_results
        }}
    }
}