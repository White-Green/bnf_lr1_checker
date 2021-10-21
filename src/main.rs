use clap::{crate_authors, crate_description, crate_name, crate_version, Arg};
use parser::enum_index::EnumIndex;
use parser::{LR1Parser, Rule, Syntax};
use std::collections::HashMap;
use std::error::Error;
use std::fs;
use std::hash::Hash;
use syn::parse::{Parse, ParseStream};
use syn::{bracketed, token, Expr, Ident, LitStr, Pat, Token};

mod kw {
    syn::custom_keyword!(ERROR);
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
enum TokenKey {
    Named(String),
    Raw(String),
}

impl Parse for TokenKey {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(LitStr) {
            let s: LitStr = input.parse()?;
            Ok(TokenKey::Raw(s.value()))
        } else {
            let s: Ident = input.parse()?;
            Ok(TokenKey::Named(s.to_string()))
        }
    }
}

enum BNFTerminalSymbol {
    Named { _bracket: token::Bracket, name: Ident },
    Raw(LitStr),
}

impl Parse for BNFTerminalSymbol {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(token::Bracket) {
            let name;
            Ok(BNFTerminalSymbol::Named { _bracket: bracketed!(name in input), name: name.parse()? })
        } else {
            Ok(BNFTerminalSymbol::Raw(input.parse()?))
        }
    }
}

enum BNFSymbol {
    Terminal(BNFTerminalSymbol),
    NonTerminal { _open: Token![<], name: Ident, _close: Token![>] },
    Error(kw::ERROR),
}

impl Parse for BNFSymbol {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![<]) {
            Ok(BNFSymbol::NonTerminal { _open: input.parse()?, name: input.parse()?, _close: input.parse()? })
        } else if input.peek(kw::ERROR) {
            Ok(BNFSymbol::Error(input.parse()?))
        } else {
            Ok(BNFSymbol::Terminal(input.parse()?))
        }
    }
}

enum BNFRuleKey {
    Named { _open: Token![<], name: Ident, _close: Token![>], _eq1: Token![::], _eq2: Token![=] },
    Follow(Token![|]),
}

impl Parse for BNFRuleKey {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(Token![<]) {
            Ok(BNFRuleKey::Named {
                _open: input.parse()?,
                name: input.parse()?,
                _close: input.parse()?,
                _eq1: input.parse()?,
                _eq2: input.parse()?,
            })
        } else {
            Ok(BNFRuleKey::Follow(input.parse()?))
        }
    }
}

struct BNFRule {
    key: BNFRuleKey,
    rule: Vec<BNFSymbol>,
    _sep: Token![:],
    _pattern: Pat,
    _arrow: Token![=>],
    _generator: Expr,
    _end: Token![;],
}

impl Parse for BNFRule {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(BNFRule {
            key: input.parse()?,
            rule: {
                let mut vec = Vec::new();
                while !input.peek(Token![:]) {
                    vec.push(input.parse()?);
                }
                vec
            },
            _sep: input.parse()?,
            _pattern: input.parse()?,
            _arrow: input.parse()?,
            _generator: input.parse()?,
            _end: input.parse()?,
        })
    }
}

struct BNFRules(Vec<BNFRule>);

impl Parse for BNFRules {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut rules = Vec::new();
        while !input.is_empty() {
            rules.push(input.parse()?);
        }
        Ok(BNFRules(rules))
    }
}

fn main() -> Result<(), Box<dyn Error>> {
    let matches = clap::app_from_crate!().arg(Arg::with_name("bnf").help("path/to/bnf_file").required(true).takes_value(true)).get_matches();
    let bnf_path = matches.value_of("bnf").unwrap();
    let bnf = fs::read_to_string(bnf_path).expect("failed to open bnf file");
    let BNFRules(rules) = syn::parse_str(&bnf)?;

    struct AnonymousSymbol(usize);
    impl EnumIndex for AnonymousSymbol {
        fn enum_index(&self) -> usize {
            self.0
        }
    }
    fn map_getter<K: Hash + Eq + Clone>(map: &mut HashMap<K, usize>) -> impl FnMut(&K) -> usize + '_ {
        move |s| {
            if let Some(result) = map.get(s) {
                *result
            } else {
                let result = map.len();
                map.insert(s.clone(), result);
                result
            }
        }
    }
    let mut symbol_map = HashMap::new();
    let mut token_map = HashMap::new();
    let mut get_symbol = map_getter(&mut symbol_map);
    let mut get_token = map_getter(&mut token_map);
    let mut builder = Syntax::<_, _, ()>::builder();
    let mut follow_key = None;
    for BNFRule { key, rule, .. } in &rules {
        let key = match key {
            BNFRuleKey::Named { name, .. } => get_symbol(&name.to_string()),
            BNFRuleKey::Follow(_) => follow_key.expect("above rule is not found"),
        };
        follow_key = Some(key);
        let start = AnonymousSymbol(key);
        let mut rule_item = Vec::with_capacity(rule.len());
        for item in rule {
            rule_item.push(match item {
                BNFSymbol::Terminal(BNFTerminalSymbol::Named { name, .. }) => parser::Symbol::Terminal(AnonymousSymbol(get_token(&TokenKey::Named(name.to_string())))),
                BNFSymbol::Terminal(BNFTerminalSymbol::Raw(name)) => parser::Symbol::Terminal(AnonymousSymbol(get_token(&TokenKey::Raw(name.value())))),
                BNFSymbol::NonTerminal { name, .. } => parser::Symbol::NonTerminal(AnonymousSymbol(get_symbol(&name.to_string()))),
                BNFSymbol::Error(_) => parser::Symbol::Error(()),
            })
        }
        builder = builder.rule(Rule::new(start, &rule_item, |_| unreachable!()));
    }
    drop(get_symbol);
    drop(get_token);
    let syntax = builder.build(AnonymousSymbol(0));
    let (_, warnings) = LR1Parser::new(syntax);
    match warnings.as_slice() {
        [] => println!("there is no warnings"),
        [warning] => {
            println!("there is a warning");
            println!("{:?}", warning);
        }
        warnings => {
            println!("there are {} warnings", warnings.len());
            for (i, warning) in warnings.iter().enumerate() {
                println!("{}:\t {:?}", i, warning);
            }
        }
    }
    Ok(())
}
