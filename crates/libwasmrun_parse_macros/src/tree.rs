use crate::syntax::*;

use proc_macro2::TokenStream;
use quote::quote;
use quote::TokenStreamExt;

use std::collections::HashMap;
use std::str::Chars;

struct Node {
    value: Option<syn::Expr>,
    next: HashMap<char, Node>,
}

impl Node {
    fn new() -> Self {
        Node {
            value: None,
            next: HashMap::new(),
        }
    }

    fn to_token_stream(&self) -> TokenStream {
        let Node { value, next } = self;

        let node_value = match value {
            None => quote!(None),
            Some(expr) => quote!(Some(#expr)),
        };

        // Optimize the case when the node is a leaf. Not necessary for correctness, but makes the
        // generated code smaller.
        if next.is_empty() {
            return quote!(#node_value);
        }

        let mut match_arms = vec![];
        for (char, next) in next.iter() {
            let next_tokens = next.to_token_stream();
            match_arms.push(quote!(
                #char => {
                    #next_tokens
                }
            ));
        }
        match_arms.push(quote!(_ => #node_value));

        quote!(
            match chars.next() {
                None => #node_value,
                Some(char) => {
                    match char {
                        #(#match_arms,)*
                    }
                }
            }
        )
    }

    fn add_rule(&mut self, rule: Rule) {
        let Rule { pattern, value } = rule;
        let pattern: String = pattern.0;
        let value: syn::Expr = value.0;

        let mut chars = pattern.chars();

        let first = chars.next().expect("empty pattern");

        match self.next.get_mut(&first) {
            None => {
                let mut node = Node::new();
                node.add_rule_(&mut chars, value);
                self.next.insert(first, node);
            }
            Some(node) => {
                node.add_rule_(&mut chars, value);
            }
        }
    }

    fn add_rule_(&mut self, chars: &mut Chars, value: syn::Expr) {
        match chars.next() {
            None => {
                assert!(self.value.is_none()); // TODO: improve the err msg
                self.value = Some(value);
            }
            Some(char) => match self.next.get_mut(&char) {
                None => {
                    let mut node = Node::new();
                    node.add_rule_(chars, value);
                    self.next.insert(char, node);
                }
                Some(node) => {
                    node.add_rule_(chars, value);
                }
            },
        }
    }
}

pub(crate) fn build_decision_tree(rules: Vec<Rule>) -> TokenStream {
    let mut stream = TokenStream::new();
    let mut tree = Node::new();
    for rule in rules {
        tree.add_rule(rule);
    }

    stream.append_all(tree.to_token_stream());

    stream
}
