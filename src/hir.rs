use std::{
    collections::{BTreeMap, HashMap},
    fmt,
};

use crate::ast;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(usize);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.0)
    }
}

impl Default for Id {
    fn default() -> Self {
        Id::new()
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
#[derive(Debug, Clone)]
pub enum TermData<'a> {
    True,
    False,
    Var(Id),
    Lam(Id, Term<'a>),
    App(Term<'a>, Term<'a>),
    If(Term<'a>, Term<'a>, Term<'a>),
    Let(Vec<(Id, Term<'a>)>, Term<'a>),
    AttrSet(BTreeMap<String, Term<'a>>),
    Path(Term<'a>, String),
}

impl fmt::Display for TermData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt(f, 1)
    }
}

fn indent(f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
    for _ in 0..level {
        f.write_str("  ")?;
    }

    Ok(())
}

impl TermData<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        use TermData::*;

        match self {
            True => f.write_str("tt"),
            False => f.write_str("ff"),
            Var(v) => write!(f, "{}", v),
            Lam(x, t) => {
                write!(f, "({}: ", x)?;
                t.fmt(f, level)?;
                f.write_str(")")
            }
            App(t1, t2) => {
                t1.fmt(f, level)?;
                f.write_str(" ")?;
                t2.fmt(f, level)
            }
            If(c, t, e) => {
                f.write_str("if ")?;
                c.fmt(f, level)?;
                f.write_str(" then ")?;
                t.fmt(f, level)?;
                f.write_str(" else ")?;
                e.fmt(f, level)
            }
            Let(assignments, e) => {
                f.write_str("let\n")?;
                for (v, t) in assignments.iter() {
                    indent(f, level)?;
                    write!(f, "{} = ", v)?;
                    t.fmt(f, level + 1)?;
                    f.write_str(";\n")?;
                }
                indent(f, level.saturating_sub(1))?;
                f.write_str("in ")?;
                e.fmt(f, level)
            }
            AttrSet(assignments) => {
                f.write_str("{\n")?;
                for (v, t) in assignments.iter() {
                    indent(f, level)?;
                    write!(f, "{} = ", v)?;
                    t.fmt(f, level + 1)?;
                    f.write_str(";\n")?;
                }
                indent(f, level.saturating_sub(1))?;
                f.write_str("}")
            }
            Path(t, field) => {
                t.fmt(f, level)?;
                write!(f, ".{}", field)
            }
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
        ast::Term::Let(assignments, e) => {
            let mut env = env.clone();
            let ids: Vec<_> = assignments.iter().map(|_| Id::new()).collect();
            for ((v, _), id) in assignments.iter().zip(ids.iter()) {
                env.insert(v.clone(), *id);
            }
            let assignments = assignments
                .iter()
                .zip(ids.iter())
                .map(|((_, t), id)| (*id, from_ast(t, terms, &env)))
                .collect();

            terms.alloc(TermData::Let(assignments, from_ast(e, terms, &env)))
        }
        ast::Term::AttrSet(assignments) => {
            let attrs = assignments
                .iter()
                .map(|(v, t)| (v.clone(), from_ast(t, terms, env)))
                .collect();

            terms.alloc(TermData::AttrSet(attrs))
        }
        ast::Term::Path(t, f) => {
            let t = from_ast(t, terms, env);

            terms.alloc(TermData::Path(t, f.clone()))
        }
    }
}
