use crate::ast;
use lalrpop_util::{lexer::Token, ParseError};

#[allow(clippy::all)]
mod term;

pub fn parse_term(input: &str) -> Result<ast::Term, ParseError<usize, Token, &'static str>> {
    term::TermParser::new().parse(input)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_if_assoc() {
        use ast::Term::*;

        assert_eq!(
            parse_term("if x then y else z w").unwrap(),
            If(
                Var("x".into()).into(),
                Var("y".into()).into(),
                App(Var("z".into()).into(), Var("w".into()).into()).into()
            )
        );
    }

    #[test]
    fn test_parse_let() {
        let t = parse_term("let x = true; y = let z = false; in x; in y").unwrap();
        assert_eq!(
            t.to_string(),
            "let
  x = tt;
  y = let
    z = ff;
  in x;
in y"
        );
    }
}
