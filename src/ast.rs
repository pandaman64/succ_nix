use std::fmt;

#[derive(Debug, Clone)]
pub enum Term {
    True,
    False,
    Var(String),
    Lam(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Term::*;

        match self {
            True => f.write_str("tt"),
            False => f.write_str("ff"),
            Var(v) => f.write_str(v),
            Lam(x, t) => write!(f, "({}: {})", x, t),
            App(t1, t2) => write!(f, "{} {}", t1, t2),
            If(c, t, e) => write!(f, "if {} then {} else {}", c, t, e),
        }
    }
}
