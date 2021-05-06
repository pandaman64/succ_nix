use std::{
    collections::{BTreeMap, HashMap},
    fmt,
};

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

#[derive(Debug, Clone)]
pub enum AttrSetDescriptor<'a> {
    Leaf(Term<'a>),
    Internal(BTreeMap<String, AttrSetDescriptor<'a>>),
}

impl Default for AttrSetDescriptor<'_> {
    fn default() -> Self {
        Self::Internal(BTreeMap::new())
    }
}

impl<'a> AttrSetDescriptor<'a> {
    fn push<I: Iterator<Item = String>>(&mut self, mut iter: I, t: Term<'a>) {
        use AttrSetDescriptor::*;

        match iter.next() {
            Some(x) => match self {
                Internal(attrs) => {
                    let entry = attrs.entry(x).or_default();
                    entry.push(iter, t);
                }
                _ => unreachable!(),
            },
            None => *self = Leaf(t),
        }
    }

    fn fmt(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        use AttrSetDescriptor::*;

        match self {
            Leaf(t) => t.fmt(f, level),
            Internal(attrs) => {
                f.write_str("{\n")?;
                for (v, desc) in attrs.iter() {
                    indent(f, level)?;
                    write!(f, "{} = ", v)?;
                    desc.fmt(f, level + 1)?;
                    f.write_str(";\n")?;
                }
                indent(f, level.saturating_sub(1))?;
                f.write_str("}")
            }
        }
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
    AttrSet(AttrSetDescriptor<'a>),
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
            AttrSet(attrs) => attrs.fmt(f, level),
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

pub fn from_rnix<'a>(
    ast: rnix::SyntaxNode,
    terms: &'a typed_arena::Arena<TermData<'a>>,
    env: &AlphaEnv<'a>,
) -> Term<'a> {
    use rnix::types::*;
    use rnix::SyntaxKind::*;

    match ast.kind() {
        NODE_ROOT => {
            let root = Root::cast(ast).unwrap();
            from_rnix(root.inner().unwrap(), terms, env)
        }
        NODE_PAREN => {
            let paren = Paren::cast(ast).unwrap();
            from_rnix(paren.inner().unwrap(), terms, env)
        }
        NODE_LITERAL => {
            // the child node must be one of float, integer, path, and uri
            let literal_token = ast.children().next().unwrap();
            match literal_token.kind() {
                TOKEN_FLOAT => todo!(),
                TOKEN_INTEGER => terms.alloc(TermData::Integer),
                TOKEN_PATH => terms.alloc(TermData::Path),
                TOKEN_URI => todo!(),
                _ => unreachable!(),
            }
        }
        NODE_IDENT => {
            let ident = Ident::cast(ast).unwrap();
            env.get(ident.as_str()).unwrap()
        }
        NODE_LAMBDA => {
            let lambda = Lambda::cast(ast).unwrap();
            let arg = lambda.arg().unwrap();
            let body = lambda.body().unwrap();

            match arg.kind() {
                NODE_IDENT => {
                    let arg = Ident::cast(arg).unwrap();
                    let arg_id = Id::new();

                    let mut env = env.clone();
                    env.insert(arg.as_str().into(), terms.alloc(TermData::Var(arg_id)));

                    let t = from_rnix(body, terms, &env);
                    terms.alloc(TermData::Lam(arg_id, t))
                }
                NODE_PATTERN => {
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
                    let pattern = Pattern::cast(arg).unwrap();

                    let args_id = Id::new();
                    let args_term = &*terms.alloc(TermData::Var(args_id));

                    let mut env = env.clone();
                    if let Some(at) = pattern.at() {
                        env.insert(at.as_str().into(), args_term);
                    }
                    let assignments: Vec<_> = pattern
                        .entries()
                        .map(|pat| {
                            let id = Id::new();
                            let name = pat.name().unwrap();

                            env.insert(name.as_str().into(), terms.alloc(TermData::Var(id)));

                            (id, String::from(name.as_str()), pat.default())
                        })
                        .collect();
                    let assignments = assignments
                        .into_iter()
                        .map(|(id, name, def)| {
                            let select_term = terms.alloc(TermData::Select(args_term, name));
                            let t = match def {
                                Some(t) => terms
                                    .alloc(TermData::Or(select_term, from_rnix(t, terms, &env))),
                                None => select_term,
                            };

                            (id, &*t)
                        })
                        .collect();
                    let t = from_rnix(body, terms, &env);

                    terms.alloc(TermData::Lam(
                        args_id,
                        terms.alloc(TermData::Let(assignments, t)),
                    ))
                }
                _ => unreachable!(),
            }
        }
        NODE_APPLY => {
            let apply = Apply::cast(ast).unwrap();

            let t1 = from_rnix(apply.lambda().unwrap(), terms, env);
            let t2 = from_rnix(apply.value().unwrap(), terms, env);

            terms.alloc(TermData::App(t1, t2))
        }
        NODE_IF_ELSE => {
            let ifelse = IfElse::cast(ast).unwrap();

            let c = from_rnix(ifelse.condition().unwrap(), terms, env);
            let t = from_rnix(ifelse.body().unwrap(), terms, env);
            let e = from_rnix(ifelse.else_body().unwrap(), terms, env);

            terms.alloc(TermData::If(c, t, e))
        }
        NODE_LET_IN => {
            fn assert_single<T, I: Iterator<Item = T>>(mut iter: I) -> T {
                let ret = iter.next().unwrap();
                assert!(iter.next().is_none());
                ret
            }

            let mut env = env.clone();
            let letin = LetIn::cast(ast).unwrap();
            let assignments: Vec<_> = letin
                .entries()
                .map(|kv| {
                    let id = Id::new();
                    let v = Ident::cast(assert_single(kv.key().unwrap().path())).unwrap();
                    env.insert(v.as_str().into(), terms.alloc(TermData::Var(id)));

                    let t = kv.value().unwrap();

                    (id, t)
                })
                .collect();

            let assignments = assignments
                .into_iter()
                .map(|(id, t)| {
                    let t = from_rnix(t, terms, &env);
                    (id, t)
                })
                .collect();

            terms.alloc(TermData::Let(
                assignments,
                from_rnix(letin.body().unwrap(), terms, &env),
            ))
        }
        NODE_LEGACY_LET => todo!(),
        NODE_ATTR_SET => {
            let attrs = AttrSet::cast(ast).unwrap();

            if attrs.recursive() {
                todo!()
            }

            let mut descriptor = AttrSetDescriptor::default();
            for attr in attrs.entries() {
                let key = attr.key().unwrap();
                let value = attr.value().unwrap();

                let t = from_rnix(value, terms, env);
                descriptor.push(
                    key.path().map(|x| Ident::cast(x).unwrap().as_str().into()),
                    t,
                );
            }

            terms.alloc(TermData::AttrSet(descriptor))
        }
        NODE_SELECT => {
            let select = Select::cast(ast).unwrap();

            let t = from_rnix(select.set().unwrap(), terms, env);
            // TODO: non-ident selections
            let f = Ident::cast(select.index().unwrap()).unwrap();

            terms.alloc(TermData::Select(t, f.as_str().into()))
        }
        k => todo!("unsupported node: {:?}", k),
    }
}
