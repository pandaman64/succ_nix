use crate::ast;
use lalrpop_util::{lexer::Token, ParseError};

#[allow(clippy::all)]
mod term;

pub fn parse_term(input: &str) -> Result<ast::Term, ParseError<usize, Token, &'static str>> {
    term::TermParser::new().parse(input)
}
