// #[derive(Debug, Clone)]
// pub enum Term {
//     True,
//     False,
//     Var(String),
//     Lam(String, Box<Term>),
//     App(Box<Term>, Box<Term>),
//     If(Box<Term>, Box<Term>, Box<Term>),
// }
pub type Term = crate::Term;
