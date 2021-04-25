use std::{collections::HashMap, fmt};

use crate::ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.0)
    }
}

impl Id {
    pub fn new() -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static CURRENT_ID: AtomicUsize = AtomicUsize::new(0);

        let current = CURRENT_ID.fetch_add(1, Ordering::Relaxed);
        Self(current)
    }
}

// alpha-converted term
pub type Term<'a> = &'a TermData<'a>;
#[derive(Debug, Clone, Copy)]
pub enum TermData<'a> {
    True,
    False,
    Var(Id),
    Lam(Id, Term<'a>),
    App(Term<'a>, Term<'a>),
    If(Term<'a>, Term<'a>, Term<'a>),
}

impl fmt::Display for TermData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TermData::*;

        match self {
            True => f.write_str("tt"),
            False => f.write_str("ff"),
            Var(v) => write!(f, "{}", v),
            Lam(x, t) => write!(f, "({}: {})", x, t),
            App(t1, t2) => write!(f, "{} {}", t1, t2),
            If(c, t, e) => write!(f, "if {} then {} else {}", c, t, e),
        }
    }
}

pub fn from_ast<'a>(
    ast: &ast::Term,
    terms: &'a typed_arena::Arena<TermData<'a>>,
    env: &HashMap<String, Id>,
) -> Term<'a> {
    match ast {
        ast::Term::True => terms.alloc(TermData::True),
        ast::Term::False => terms.alloc(TermData::False),
        ast::Term::Var(v) => terms.alloc(TermData::Var(*env.get(v).unwrap())),
        ast::Term::Lam(v, t) => {
            let id = Id::new();
            let t = {
                let mut env = env.clone();
                env.insert(v.clone(), id);
                from_ast(t, terms, &env)
            };

            terms.alloc(TermData::Lam(id, t))
        }
        ast::Term::App(t1, t2) => {
            let t1 = from_ast(t1, terms, env);
            let t2 = from_ast(t2, terms, env);

            terms.alloc(TermData::App(t1, t2))
        }
        ast::Term::If(c, t, e) => {
            let c = from_ast(c, terms, env);
            let t = from_ast(t, terms, env);
            let e = from_ast(e, terms, env);

            terms.alloc(TermData::If(c, t, e))
        }
    }
}
