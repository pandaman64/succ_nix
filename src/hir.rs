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
    Integer,
    List,
    Path,
    String,
    Var(Id),
    Lam(Id, Term<'a>),
    App(Term<'a>, Term<'a>),
    If(Term<'a>, Term<'a>, Term<'a>),
    Let(Vec<(Id, Term<'a>)>, Term<'a>),
    AttrSet(BTreeMap<String, Term<'a>>),
    Select(Term<'a>, String),
    Or(Term<'a>, Term<'a>),
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
            Integer => f.write_str("integer"),
            List => f.write_str("list"),
            Path => f.write_str("path"),
            String => f.write_str("string"),
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
            Select(t, field) => {
                t.fmt(f, level)?;
                write!(f, ".{}", field)
            }
            Or(t1, t2) => {
                t1.fmt(f, level)?;
                f.write_str(" or ")?;
                t2.fmt(f, level)
            }
        }
    }
}

pub type AlphaEnv<'a> = HashMap<String, Term<'a>>;

pub fn from_ast<'a>(
    ast: &ast::Term,
    terms: &'a typed_arena::Arena<TermData<'a>>,
    env: &AlphaEnv<'a>,
) -> Term<'a> {
    match ast {
        ast::Term::True => terms.alloc(TermData::True),
        ast::Term::False => terms.alloc(TermData::False),
        ast::Term::Var(v) => env.get(v).unwrap(),
        ast::Term::Lam(arg, t) => {
            match arg {
                ast::Argument::Var(v) => {
                    let arg_id = Id::new();

                    let mut env = env.clone();
                    env.insert(v.clone(), terms.alloc(TermData::Var(arg_id)));

                    let t = from_ast(t, terms, &env);

                    terms.alloc(TermData::Lam(arg_id, t))
                }
                ast::Argument::AttrSet(args_str, attr_set) => {
                    // Given `args @ { v1 ? t1, ... }: t`, we desugar it to
                    // ```
                    // args:
                    // let
                    //   vn = args.vn or tn;
                    //   ...
                    // in t
                    // ```
                    // Nix allows referring names in the same argument attribute set. e.g. `{ a, b ? a } ...`.
                    // And the desugaring reproduces this behavior.
                    let args_id = Id::new();
                    let args_expr = &*terms.alloc(TermData::Var(args_id));
                    let vn_ids: Vec<_> = attr_set.iter().map(|_| Id::new()).collect();

                    let mut env = env.clone();
                    if let Some(args_str) = args_str {
                        env.insert(args_str.clone(), args_expr);
                    }
                    for ((v, _), id) in attr_set.iter().zip(vn_ids.iter()) {
                        env.insert(v.clone(), terms.alloc(TermData::Var(*id)));
                    }

                    let assignments = vn_ids
                        .iter()
                        .zip(attr_set.iter())
                        .map(|(id, (v, t))| {
                            let select_expr = terms.alloc(TermData::Select(args_expr, v.clone()));
                            let t = match t {
                                Some(t) => {
                                    terms.alloc(TermData::Or(select_expr, from_ast(t, terms, &env)))
                                }
                                None => select_expr,
                            };
                            (*id, &*t)
                        })
                        .collect();
                    let t = from_ast(t, terms, &env);

                    terms.alloc(TermData::Lam(
                        args_id,
                        terms.alloc(TermData::Let(assignments, t)),
                    ))
                }
            }
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
            let ids: Vec<_> = assignments.iter().map(|_| Id::new()).collect();

            let mut env = env.clone();
            for ((v, _), id) in assignments.iter().zip(ids.iter()) {
                env.insert(v.clone(), terms.alloc(TermData::Var(*id)));
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
        ast::Term::Select(t, f) => {
            let t = from_ast(t, terms, env);

            terms.alloc(TermData::Select(t, f.clone()))
        }
    }
}
