use clap::{crate_authors, crate_description, crate_name, crate_version, Arg};
use parser::enum_index::EnumIndex;
use parser::enum_index_derive::EnumIndex;
use parser::{enum_index, LR1Parser, Parse, Rule, Syntax};
use parser_generator::parser;
use std::collections::BTreeMap;
use std::error::Error;
use std::fmt::{Display, Formatter};
use std::{fs, mem};
use tokenizer::Tokenize;
use tokenizer_generator::tokenizer;

#[derive(Debug, EnumIndex)]
pub enum Token {
    Token(String),
    Symbol(String),
    Or,
    Eq,
    Line,
    Error,
}

#[derive(Debug)]
pub enum RuleItem {
    Token(String),
    Symbol(String),
    Error,
}

impl<'a> From<&'a Token> for RuleItem {
    fn from(token: &Token) -> Self {
        match token {
            Token::Token(s) => RuleItem::Token(s.clone()),
            Token::Symbol(s) => RuleItem::Symbol(s.clone()),
            Token::Error => RuleItem::Error,
            _ => panic!(),
        }
    }
}

tokenizer! {
    character char;
    token Option<Token>;
    "<\\w+>": (|s, _| Some(Token::Symbol(s[1..s.len() - 1].to_string())));
    "\\[\\w+\\]": (|s, _| Some(Token::Token(s[1..s.len() - 1].to_string())));
    "\"(\\\\(n|t|x[0-9a-fA-F]{2}|u\\{[0-9a-fA-F]{1,6}\\}|\\\\|\")|[^\\\\\"\n])*\"": (|s, _| Some(Token::Token(parse_string_literal(s).unwrap())));
    "\\|": (|_, _| Some(Token::Or));
    "::=": (|_, _| Some(Token::Eq));
    "\\n": (|_, _| Some(Token::Line));
    "[eE][rR][rR][oO][rR]": (|_, _| Some(Token::Error));
    "\\s": (|_, _| None);
    // ".|\n": (|s, _|{dbg!(s); None})
}

fn parse_string_literal(s: &str) -> Option<String> {
    let mut iter = s[1..s.len() - 1].chars();
    let mut result = String::with_capacity(s.len());
    while let Some(c) = iter.next() {
        if c != '\\' {
            result.push(c);
            continue;
        }
        match iter.next() {
            Some('n') => result.push('\n'),
            Some('t') => result.push('\t'),
            Some('\\') => result.push('\\'),
            Some('\"') => result.push('\"'),
            Some('x') => {
                let c = iter.by_ref().take(2).fold(0, |acc, c| {
                    acc << 4
                        | c.to_digit(16)
                            .expect("事前にとーくないざの正規表現で確認してるのであんりーちゃぶる")
                });
                result.push(char::from_u32(c).expect("16進2桁なのであんりーちゃぶる"));
            }
            Some('u') => {
                let c = iter
                    .by_ref()
                    .skip(1)
                    .take_while(|c| *c != '}')
                    .fold(0, |acc, c| {
                        acc << 4
                            | c.to_digit(16).expect(
                                "事前にとーくないざの正規表現で確認してるのであんりーちゃぶる",
                            )
                    });
                result.push(char::from_u32(c)?);
            }
            _ => unreachable!("事前にとーくないざの正規表現で確認してるのであんりーちゃぶる"),
        }
    }
    result.shrink_to_fit();
    Some(result)
}

#[derive(Debug, EnumIndex)]
pub enum Symbol {
    Rules(Vec<(String, Vec<RuleItem>)>),
    Rule(String, Vec<Vec<RuleItem>>),
    RuleItem(Vec<RuleItem>),
}

#[derive(Debug)]
pub enum NoneError {}

impl Display for NoneError {
    fn fmt(&self, _: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            _ => unreachable!(),
        }
    }
}

impl Error for NoneError {}

parser! {
    token Token {
        Token(Default::default(),),
        Symbol(Default::default(),),
        Or,
        Eq,
        Line,
        Error,
    }
    symbol Symbol {
        Rules(Default::default(),),
        Rule(Default::default(), Default::default(),),
        RuleItem(Default::default(),)
    }
    Rules
    (NoneError)
    <Rules>::=()
        (|list| if let [] = list {
            Ok(Symbol::Rules(Default::default()))
        } else { unreachable!() })
    <Rules>::=(<Rules> <Rule>)
        (|list| if let [parser::Symbol::NonTerminal(Symbol::Rules(rules)), parser::Symbol::NonTerminal(Symbol::Rule(symbol, rule))] = list {
            rules.extend(mem::take(rule).into_iter().map(|rule|(symbol.clone(), rule)));
            Ok(Symbol::Rules(mem::take(rules)))
        } else { unreachable!() })
    <Rule>::=([Symbol] [Eq] <RuleItem> [Line])
        (|list| if let [parser::Symbol::Terminal(Token::Symbol(symbol)), _, parser::Symbol::NonTerminal(Symbol::RuleItem(rule)), _] = list {
            Ok(Symbol::Rule(symbol.clone(), vec![mem::take(rule)]))
        } else { unreachable!() })
    <Rule>::=(<Rule> [Or] <RuleItem> [Line])
        (|list| if let [parser::Symbol::NonTerminal(Symbol::Rule(symbol, rules)), _, parser::Symbol::NonTerminal(Symbol::RuleItem(rule)), _] = list {
            rules.push(mem::take(rule));
            Ok(Symbol::Rule(mem::take(symbol), mem::take(rules)))
        } else { unreachable!() })
    <RuleItem>::=()
        (|list| if let [] = list {
            Ok(Symbol::RuleItem(vec![]))
        } else { unreachable!() })
    <RuleItem>::=(<RuleItem> [Token])
        (|list| if let [parser::Symbol::NonTerminal(Symbol::RuleItem(items)), parser::Symbol::Terminal(token)] = list {
            items.push(token.clone().into());
            Ok(Symbol::RuleItem(mem::take(items)))
        } else { unreachable!() })
    <RuleItem>::=(<RuleItem> [Symbol])
        (|list| if let [parser::Symbol::NonTerminal(Symbol::RuleItem(items)), parser::Symbol::Terminal(symbol)] = list {
            items.push(symbol.clone().into());
            Ok(Symbol::RuleItem(mem::take(items)))
        } else { unreachable!() })
    <RuleItem>::=(<RuleItem> [Error])
        (|list| if let [parser::Symbol::NonTerminal(Symbol::RuleItem(items)), parser::Symbol::Terminal(error)] = list {
            items.push(error.clone().into());
            Ok(Symbol::RuleItem(mem::take(items)))
        } else { unreachable!() })
}

fn main() -> Result<(), Box<dyn Error>> {
    let matches = clap::app_from_crate!()
        .arg(
            Arg::with_name("bnf")
                .help("path/to/bnf_file")
                .required(true)
                .takes_value(true),
        )
        .get_matches();
    let bnf_path = matches.value_of("bnf").unwrap();
    let bnf = fs::read_to_string(bnf_path).expect("failed to open bnf file");
    let rules = bnf
        .chars()
        .tokenize(get_tokenizer())
        .flatten()
        .parse(&get_parser())?;
    let rules = if let Symbol::Rules(rules) = rules {
        rules
    } else {
        unreachable!()
    };
    struct AnonymousSymbol(usize);
    impl EnumIndex for AnonymousSymbol {
        fn enum_index(&self) -> usize {
            self.0
        }
    }
    fn map_getter(map: &mut BTreeMap<String, usize>) -> impl FnMut(&String) -> usize + '_ {
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
    let mut symbol_map = BTreeMap::new();
    let mut token_map = BTreeMap::new();
    let mut get_symbol = map_getter(&mut symbol_map);
    let mut get_token = map_getter(&mut token_map);
    let mut builder = Syntax::<_, _, ()>::builder();
    for (symbol, rule) in &rules {
        let start = AnonymousSymbol(get_symbol(symbol));
        let mut rule_item = Vec::with_capacity(rule.len());
        for item in rule {
            rule_item.push(match item {
                RuleItem::Token(token) => {
                    parser::Symbol::Terminal(AnonymousSymbol(get_token(token)))
                }
                RuleItem::Symbol(symbol) => {
                    parser::Symbol::NonTerminal(AnonymousSymbol(get_symbol(symbol)))
                }
                RuleItem::Error => parser::Symbol::Error(()),
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
