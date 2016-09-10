use types::{Term, Variable, RcTerm};
use builtin::BuiltinType;
use combine::{many, string, many1, try, digit, letter, char, between, spaces, parser, Parser,
              ParserExt};
use combine::primitives::{State, Stream, ParseResult};
use super::TermParser;
use std::iter::Iterator;

pub struct CombineParser;

impl TermParser for CombineParser {
    fn parse(s: &str) -> Option<RcTerm> {
        match parser(term_parser).parse(s) {
            Ok((e, _)) => Some(e),
            _ => None,
        }
    }
}

fn term_parser<I>(input: State<I>) -> ParseResult<RcTerm, I>
    where I: Stream<Item = char>
{
    let lex_char = |c| char(c).skip(spaces());
    let lex_string = |s| spaces().with(string(s)).skip(spaces());
    let sub_term = || between(lex_char('('), lex_char(')'), parser(term_parser));
    let num_lit = || {
        spaces().with(many1(digit()).map(|v: Vec<char>| {
            Term::num_lit_rc(v.into_iter()
                .fold(0, |acc, c| {
                    acc * 10 + c.to_digit(10).unwrap_or(0)
                }) as isize)
        }))
    };
    let bool_lit = || {
        spaces().with(string("true")
            .map(|_| Term::bool_lit_rc(true))
            .or(string("false").map(|_| Term::bool_lit_rc(false))))
    };
    let builtin_term = || {
        lex_char('+')
            .map(|_| Term::builtin_rc(BuiltinType::Add, vec![]))
            .or(lex_char('-').map(|_| Term::builtin_rc(BuiltinType::Sub, vec![])))
            .or(lex_char('*').map(|_| Term::builtin_rc(BuiltinType::Mul, vec![])))
            .or(lex_char('/').map(|_| Term::builtin_rc(BuiltinType::Div, vec![])))
            .or(lex_char('=').map(|_| Term::builtin_rc(BuiltinType::Eq, vec![])))
            .or(lex_char('?').map(|_| Term::builtin_rc(BuiltinType::If, vec![])))
    };
    let var_term = || {
        letter()
            .skip(spaces())
            .map(|t| Term::var_rc(Variable::new(vec![t])))
    };
    let abs_parser = || {
        (lex_char('\\'), letter(), spaces(), lex_char('.'), parser(term_parser))
            .map(|t| Term::abs_rc(Variable::new(vec![t.1]), t.4))
    };
    let without_if_appl = || {
        spaces().with(abs_parser()
            .or(builtin_term())
            .or(try(bool_lit()))
            .or(num_lit())
            .or(var_term())
            .or(sub_term()))
    };
    let if_term = || {
        (lex_string("if"),
         without_if_appl(),
         lex_string("then"),
         without_if_appl(),
         lex_string("else"),
         without_if_appl())
            .map(|v| Term::if_rc(v.1, v.3, v.5))
    };
    let without_appl = || try(if_term()).or(without_if_appl());
    let mut appl_parser = (without_appl(), many(without_appl()))
        .map(|v: (RcTerm, Vec<RcTerm>)| {
            if v.1.is_empty() {
                v.0
            } else {
                v.1.into_iter().fold(v.0, Term::appl_rc)
            }
        });
    appl_parser.parse_state(input)
}
