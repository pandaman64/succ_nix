use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    convert::TryFrom,
    fmt,
};

use crate::context::{Context, Interned};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Id(pub(crate) usize);

impl fmt::Display for Id {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "_{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AttrPath<'a> {
    Static(String),
    Dynamic(Term<'a>),
}

impl AttrPath<'_> {
    fn fmt(&self, f: &mut fmt::Formatter, level: usize) -> fmt::Result {
        use AttrPath::*;

        match self {
            Static(s) => f.write_str(s),
            Dynamic(t) => t.fmt(f, level),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum KeyValueDescriptor<'a> {
    Leaf(Term<'a>),
    Internal(BTreeMap<AttrPath<'a>, KeyValueDescriptor<'a>>),
}

impl Default for KeyValueDescriptor<'_> {
    fn default() -> Self {
        Self::Internal(BTreeMap::new())
    }
}

impl<'a> KeyValueDescriptor<'a> {
    fn push<I: Iterator<Item = AttrPath<'a>>>(&mut self, mut iter: I, t: Term<'a>) {
        use KeyValueDescriptor::*;

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
        use KeyValueDescriptor::*;

        match self {
            Leaf(t) => t.fmt(f, level),
            Internal(attrs) => {
                f.write_str("{\n")?;
                for (v, desc) in attrs.iter() {
                    indent(f, level)?;
                    v.fmt(f, level)?;
                    f.write_str(" = ")?;
                    desc.fmt(f, level + 1)?;
                    f.write_str(";\n")?;
                }
                indent(f, level.saturating_sub(1))?;
                f.write_str("}")
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum UnaryOpKind {
    // !: boolean -> boolean
    BooleanNeg,
    // -: integer -> integer
    IntegerNeg,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BinOpKind {
    // arithmetics: integer -> integer -> integer
    Add,
    Sub,
    Mul,
    Div,

    // logical operators: boolean -> any -> boolean
    And,
    Or,
    Implication,

    // equalities: any -> any -> boolean
    Equal,
    NotEqual,

    // comparisons: any -> any -> boolean
    // it is possible to limit argument types like (integer | string)
    Less,
    LessOrEq,
    Greater,
    GreaterOrEq,

    // list concatnation: list -> list -> list
    Concat,

    // attrset update: any -> any -> any
    Update,
    // other binary operators have distinct HIR variants
}

// alpha-converted term
pub type Term<'a> = Interned<'a, TermData<'a>>;
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TermData<'a> {
    True,
    False,
    Integer,
    // CR pandaman: include elements
    List(Vec<Term<'a>>),
    Null,
    Path,
    String,
    Var(Id),
    Lam(Id, Term<'a>),
    App(Term<'a>, Term<'a>),
    Assert(Term<'a>, Term<'a>),
    If(Term<'a>, Term<'a>, Term<'a>),
    Let(BTreeMap<String, Id>, KeyValueDescriptor<'a>, Term<'a>),
    AttrSet(KeyValueDescriptor<'a>),
    UnaryOp(UnaryOpKind, Term<'a>),
    BinOp(BinOpKind, Term<'a>, Term<'a>),
    Select(Term<'a>, AttrPath<'a>),
    Or(Term<'a>, Term<'a>),
    // CR pandaman: handle attrpath
    HasAttr(Term<'a>),
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
            List(_items) => f.write_str("list"),
            Null => f.write_str("null"),
            Path => f.write_str("path"),
            String => f.write_str("string"),
            Var(v) => write!(f, "{}", v),
            Lam(x, t) => {
                write!(f, "({}: ", x)?;
                t.fmt(f, level)?;
                f.write_str(")")
            }
            App(t1, t2) => {
                f.write_str("(")?;
                t1.fmt(f, level)?;
                f.write_str(" ")?;
                t2.fmt(f, level)?;
                f.write_str(")")
            }
            Assert(t1, t2) => {
                f.write_str("assert ")?;
                t1.fmt(f, level)?;
                f.write_str("; ")?;
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
            Let(names, attrs, e) => {
                f.write_str("let\n")?;
                match attrs {
                    KeyValueDescriptor::Internal(attrs) => {
                        for name in names.keys() {
                            write!(f, "{} = ", name)?;
                            // let cannot have dynamic attrpath at the first level
                            attrs
                                .get(&AttrPath::Static(name.into()))
                                .unwrap()
                                .fmt(f, level + 1)?;
                        }
                    }
                    _ => unreachable!(),
                }
                indent(f, level.saturating_sub(1))?;
                f.write_str("in ")?;
                e.fmt(f, level)
            }
            AttrSet(attrs) => attrs.fmt(f, level),
            UnaryOp(kind, t) => {
                use UnaryOpKind::*;

                let op = match kind {
                    BooleanNeg => "!",
                    IntegerNeg => "-",
                };

                f.write_str(op)?;
                t.fmt(f, level)
            }
            BinOp(kind, t1, t2) => {
                use BinOpKind::*;

                let op = match kind {
                    Add => "+",
                    Sub => "-",
                    Mul => "*",
                    Div => "/",
                    And => "&&",
                    Or => "||",
                    Implication => "->",
                    Equal => "==",
                    NotEqual => "!=",
                    Less => "<",
                    LessOrEq => "<=",
                    Greater => ">",
                    GreaterOrEq => ">=",
                    Concat => "++",
                    Update => "//",
                };

                t1.fmt(f, level)?;
                write!(f, " {} ", op)?;
                t2.fmt(f, level)
            }
            Select(t, field) => {
                t.fmt(f, level)?;
                f.write_str(".")?;
                field.fmt(f, level)
            }
            Or(t1, t2) => {
                t1.fmt(f, level)?;
                f.write_str(" or ")?;
                t2.fmt(f, level)
            }
            HasAttr(t) => {
                t.fmt(f, level)?;
                f.write_str(" ? ...")
            }
        }
    }
}

pub type AlphaEnv<'a> = HashMap<String, Term<'a>>;

fn to_attr_path<'a>(
    ast: rnix::SyntaxNode,
    ctx: &'a Context<'a>,
    env: &AlphaEnv<'a>,
    with_stack: &[Term<'a>],
) -> AttrPath<'a> {
    use rnix::types::*;

    match match ParsedType::try_from(ast) {
        Ok(a) => a,
        _ => unreachable!(),
    } {
        ParsedType::Ident(ident) => AttrPath::Static(ident.as_str().into()),
        ParsedType::Str(string) => {
            let parts: Option<Vec<_>> = string
                .parts()
                .into_iter()
                .map(|part| match part {
                    rnix::StrPart::Literal(s) => Some(s),
                    rnix::StrPart::Ast(_) => None,
                })
                .collect();

            match parts {
                Some(parts) => AttrPath::Static(parts.concat()),
                // the string contains an interpolation, hence dynamic
                None => AttrPath::Dynamic(ctx.mk_term(TermData::String)),
            }
        }
        ParsedType::Dynamic(dynamic) => {
            AttrPath::Dynamic(from_rnix(dynamic.inner().unwrap(), ctx, env, with_stack))
        }
        _ => unreachable!(),
    }
}

fn lookup<'a>(
    ident: &str,
    ctx: &'a Context<'a>,
    env: &AlphaEnv<'a>,
    with_stack: &[Term<'a>],
) -> Term<'a> {
    match env.get(ident) {
        Some(t) => *t,
        None => with_stack
            .iter()
            .map(|t| ctx.mk_term(TermData::Select(*t, AttrPath::Static(ident.into()))))
            .reduce(|t1, t2| ctx.mk_term(TermData::Or(t1, t2)))
            .unwrap_or_else(|| panic!("{} not found", ident)),
    }
}

pub fn from_rnix<'a>(
    ast: rnix::SyntaxNode,
    ctx: &'a Context<'a>,
    env: &AlphaEnv<'a>,
    with_stack: &[Term<'a>],
) -> Term<'a> {
    use rnix::types::*;
    use rnix::SyntaxKind::*;

    match match ParsedType::try_from(ast) {
        Ok(a) => a,
        _ => unreachable!(),
    } {
        ParsedType::Apply(apply) => {
            let t1 = from_rnix(apply.lambda().unwrap(), ctx, env, with_stack);
            let t2 = from_rnix(apply.value().unwrap(), ctx, env, with_stack);

            ctx.mk_term(TermData::App(t1, t2))
        }
        ParsedType::Assert(assert) => {
            let cond = from_rnix(assert.condition().unwrap(), ctx, env, with_stack);
            let body = from_rnix(assert.body().unwrap(), ctx, env, with_stack);

            ctx.mk_term(TermData::Assert(cond, body))
        }
        ParsedType::Ident(ident) => lookup(ident.as_str(), ctx, env, with_stack),
        ParsedType::IfElse(ifelse) => {
            let c = from_rnix(ifelse.condition().unwrap(), ctx, env, with_stack);
            let t = from_rnix(ifelse.body().unwrap(), ctx, env, with_stack);
            let e = from_rnix(ifelse.else_body().unwrap(), ctx, env, with_stack);

            ctx.mk_term(TermData::If(c, t, e))
        }
        ParsedType::Select(select) => {
            let t = from_rnix(select.set().unwrap(), ctx, env, with_stack);
            let f = to_attr_path(select.index().unwrap(), ctx, env, with_stack);

            ctx.mk_term(TermData::Select(t, f))
        }
        ParsedType::Lambda(lambda) => {
            let arg = lambda.arg().unwrap();
            let body = lambda.body().unwrap();

            match arg.kind() {
                NODE_IDENT => {
                    let arg = Ident::cast(arg).unwrap();
                    let arg_id = ctx.new_hir_id();

                    let mut env = env.clone();
                    env.insert(arg.as_str().into(), ctx.mk_term(TermData::Var(arg_id)));

                    let t = from_rnix(body, ctx, &env, with_stack);
                    ctx.mk_term(TermData::Lam(arg_id, t))
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

                    let args_id = ctx.new_hir_id();
                    let args_term = ctx.mk_term(TermData::Var(args_id));
                    let vn_ids: BTreeMap<String, Id> = pattern
                        .entries()
                        .map(|pat| {
                            let id = ctx.new_hir_id();
                            let name = pat.name().unwrap().as_str().into();

                            (name, id)
                        })
                        .collect();

                    let mut env = env.clone();
                    if let Some(at) = pattern.at() {
                        env.insert(at.as_str().into(), args_term);
                    }
                    for (name, id) in vn_ids.iter() {
                        env.insert(name.clone(), ctx.mk_term(TermData::Var(*id)));
                    }

                    let mut descriptor = KeyValueDescriptor::default();
                    for pat in pattern.entries() {
                        let name = String::from(pat.name().unwrap().as_str());
                        let select_term = ctx
                            .mk_term(TermData::Select(args_term, AttrPath::Static(name.clone())));

                        let t = match pat.default() {
                            Some(t) => ctx.mk_term(TermData::Or(
                                select_term,
                                from_rnix(t, ctx, &env, with_stack),
                            )),
                            None => select_term,
                        };
                        descriptor.push(std::iter::once(AttrPath::Static(name)), t);
                    }
                    let t = from_rnix(body, ctx, &env, with_stack);

                    ctx.mk_term(TermData::Lam(
                        args_id,
                        ctx.mk_term(TermData::Let(vn_ids, descriptor, t)),
                    ))
                }
                _ => unreachable!(),
            }
        }
        ParsedType::LegacyLet(_) => todo!(),
        ParsedType::LetIn(letin) => {
            let names: BTreeSet<_> = letin
                .entries()
                .map(|kv| {
                    let key = kv.key().unwrap();
                    // dynamic attributes not allowed in let, so first element in
                    // the path must be static
                    let first = key.path().next().unwrap();
                    match to_attr_path(first, ctx, env, with_stack) {
                        AttrPath::Static(name) => name,
                        _ => unreachable!(),
                    }
                })
                .chain(letin.inherits().flat_map(|inherit| {
                    // CR pandaman: check for duplicates?
                    // inherits (foo) bar baz; -> bar baz
                    inherit.idents().map(|ident| String::from(ident.as_str()))
                }))
                .collect();
            let ids: BTreeMap<_, _> = names
                .into_iter()
                .map(|name| (name, ctx.new_hir_id()))
                .collect();

            let mut env = env.clone();
            for (name, id) in ids.iter() {
                env.insert(name.clone(), ctx.mk_term(TermData::Var(*id)));
            }

            let mut descriptor = KeyValueDescriptor::default();
            for kv in letin.entries() {
                let key = kv.key().unwrap();
                let value = kv.value().unwrap();

                let t = from_rnix(value, ctx, &env, with_stack);
                descriptor.push(
                    key.path().map(|x| {
                        // dynamic attributes in non-first attrpath can refer to
                        // the names introduced by the same let. e.g.
                        // `let foo = "bar"; baz."${"foo"}" = 42; in baz` evaluates to
                        // `{ foo = 42; }`
                        to_attr_path(x, ctx, &env, with_stack)
                    }),
                    t,
                );
            }
            for inherit in letin.inherits() {
                match inherit.from() {
                    Some(from) => {
                        let from = from_rnix(from.inner().unwrap(), ctx, &env, with_stack);

                        for ident in inherit.idents() {
                            descriptor.push(
                                std::iter::once(AttrPath::Static(ident.as_str().into())),
                                ctx.mk_term(TermData::Select(
                                    from,
                                    AttrPath::Static(ident.as_str().into()),
                                )),
                            );
                        }
                    }
                    None => {
                        for ident in inherit.idents() {
                            descriptor.push(
                                std::iter::once(AttrPath::Static(ident.as_str().into())),
                                lookup(ident.as_str(), ctx, &env, with_stack),
                            );
                        }
                    }
                }
            }
            let t = from_rnix(letin.body().unwrap(), ctx, &env, with_stack);

            ctx.mk_term(TermData::Let(ids, descriptor, t))
        }
        ParsedType::List(list) => {
            // CR pandaman: support polymorphic lists
            let items = list
                .items()
                .map(|item| from_rnix(item, ctx, env, with_stack))
                .collect();

            ctx.mk_term(TermData::List(items))
        }
        ParsedType::OrDefault(ordefault) => {
            let t1 = from_rnix(
                ordefault.index().unwrap().node().clone(),
                ctx,
                env,
                with_stack,
            );
            let t2 = from_rnix(ordefault.default().unwrap(), ctx, env, with_stack);

            ctx.mk_term(TermData::Or(t1, t2))
        }
        ParsedType::Paren(paren) => from_rnix(paren.inner().unwrap(), ctx, env, with_stack),
        ParsedType::Root(root) => from_rnix(root.inner().unwrap(), ctx, env, with_stack),
        ParsedType::AttrSet(attrs) => {
            if attrs.recursive() {
                // given `rec { x = foo; y = x; inherit (t) bar; "${dynamic}" = d; }`, we desugar it to
                // ```
                // let
                //   x = foo;
                //   y = x;
                //   bar = t.bar;
                // in
                // {
                //   x = x;
                //   y = y;
                //   "${dynamic}" = d;
                // }
                // ```
                // Note that dynamic attribute names can refer to other names
                // introduced by the same attrset. For example,
                // `rec { foo = "bar"; "${foo}" = "baz"; }` evaluates to
                // `{ bar = "baz"; foo = "bar"; }`
                let names: BTreeSet<_> = attrs
                    .entries()
                    .filter_map(|kv| {
                        let key = kv.key().unwrap();
                        let first = key.path().next().unwrap();

                        // Nix does not allow us to refer dynamic attribute names
                        // in recursive attrsets. In other words,
                        // `{ "${"foo"}" = "bar"; baz = foo }` is disallowed.
                        // Therefore, we filter out dynamic names here.
                        match to_attr_path(first, ctx, env, with_stack) {
                            AttrPath::Static(name) => Some(name),
                            AttrPath::Dynamic(_) => None,
                        }
                    })
                    .chain(attrs.inherits().flat_map(|inherit| {
                        // CR pandaman: check for duplicates?
                        // inherits (foo) bar baz; -> bar baz
                        inherit.idents().map(|ident| String::from(ident.as_str()))
                    }))
                    .collect();
                let ids: BTreeMap<_, _> = names
                    .into_iter()
                    .map(|name| (name, ctx.new_hir_id()))
                    .collect();

                let mut env = env.clone();
                for (name, id) in ids.iter() {
                    env.insert(name.clone(), ctx.mk_term(TermData::Var(*id)));
                }

                let mut let_descriptor = KeyValueDescriptor::default();
                for kv in attrs.entries() {
                    let key = kv.key().unwrap();
                    let first = key.path().next().unwrap();
                    // if the key starts with dynamic attrpath, skip it because
                    // they are not in the new environment.
                    if let AttrPath::Dynamic(_) = to_attr_path(first, ctx, &env, with_stack) {
                        continue;
                    }

                    let value = kv.value().unwrap();
                    let t = from_rnix(value, ctx, &env, with_stack);

                    let_descriptor.push(
                        key.path().map(|x| to_attr_path(x, ctx, &env, with_stack)),
                        t,
                    );
                }
                for inherit in attrs.inherits() {
                    match inherit.from() {
                        Some(from) => {
                            let from = from_rnix(from.inner().unwrap(), ctx, &env, with_stack);

                            for ident in inherit.idents() {
                                let_descriptor.push(
                                    std::iter::once(AttrPath::Static(ident.as_str().into())),
                                    ctx.mk_term(TermData::Select(
                                        from,
                                        AttrPath::Static(ident.as_str().into()),
                                    )),
                                );
                            }
                        }
                        None => {
                            for ident in inherit.idents() {
                                let_descriptor.push(
                                    std::iter::once(AttrPath::Static(ident.as_str().into())),
                                    lookup(ident.as_str(), ctx, &env, with_stack),
                                );
                            }
                        }
                    }
                }

                let mut attr_set_descriptor = KeyValueDescriptor::default();
                for (name, id) in ids.iter() {
                    attr_set_descriptor.push(
                        std::iter::once(AttrPath::Static(name.into())),
                        ctx.mk_term(TermData::Var(*id)),
                    );
                }
                for kv in attrs.entries() {
                    let key = kv.key().unwrap();
                    let first = key.path().next().unwrap();
                    if let AttrPath::Dynamic(_) = to_attr_path(first, ctx, &env, with_stack) {
                        let t = from_rnix(kv.value().unwrap(), ctx, &env, with_stack);
                        attr_set_descriptor.push(
                            key.path().map(|k| to_attr_path(k, ctx, &env, with_stack)),
                            t,
                        )
                    }
                }

                ctx.mk_term(TermData::Let(
                    ids,
                    let_descriptor,
                    ctx.mk_term(TermData::AttrSet(attr_set_descriptor)),
                ))
            } else {
                let mut descriptor = KeyValueDescriptor::default();
                for attr in attrs.entries() {
                    let key = attr.key().unwrap();
                    let value = attr.value().unwrap();

                    let t = from_rnix(value, ctx, env, with_stack);
                    descriptor.push(key.path().map(|k| to_attr_path(k, ctx, env, with_stack)), t);
                }
                for inherit in attrs.inherits() {
                    match inherit.from() {
                        Some(from) => {
                            let from = from_rnix(from.inner().unwrap(), ctx, &env, with_stack);

                            for ident in inherit.idents() {
                                descriptor.push(
                                    std::iter::once(AttrPath::Static(ident.as_str().into())),
                                    ctx.mk_term(TermData::Select(
                                        from,
                                        AttrPath::Static(ident.as_str().into()),
                                    )),
                                );
                            }
                        }
                        None => {
                            for ident in inherit.idents() {
                                descriptor.push(
                                    std::iter::once(AttrPath::Static(ident.as_str().into())),
                                    lookup(ident.as_str(), ctx, &env, with_stack),
                                );
                            }
                        }
                    }
                }

                ctx.mk_term(TermData::AttrSet(descriptor))
            }
        }
        ParsedType::UnaryOp(op) => {
            let kind = match op.operator() {
                UnaryOpKind::Invert => self::UnaryOpKind::BooleanNeg,
                UnaryOpKind::Negate => self::UnaryOpKind::IntegerNeg,
            };
            let value = from_rnix(op.value().unwrap(), ctx, env, with_stack);

            ctx.mk_term(TermData::UnaryOp(kind, value))
        }
        ParsedType::BinOp(op) => {
            let kind = match op.operator() {
                BinOpKind::Concat => self::BinOpKind::Concat,
                BinOpKind::IsSet => {
                    let t = from_rnix(op.lhs().unwrap(), ctx, env, with_stack);

                    return ctx.mk_term(TermData::HasAttr(t));
                }
                BinOpKind::Update => self::BinOpKind::Update,
                BinOpKind::Add => self::BinOpKind::Add,
                BinOpKind::Sub => self::BinOpKind::Sub,
                BinOpKind::Mul => self::BinOpKind::Mul,
                BinOpKind::Div => self::BinOpKind::Div,
                BinOpKind::And => self::BinOpKind::And,
                BinOpKind::Equal => self::BinOpKind::Equal,
                BinOpKind::Implication => self::BinOpKind::Implication,
                BinOpKind::Less => self::BinOpKind::Less,
                BinOpKind::LessOrEq => self::BinOpKind::LessOrEq,
                BinOpKind::More => self::BinOpKind::Greater,
                BinOpKind::MoreOrEq => self::BinOpKind::GreaterOrEq,
                BinOpKind::NotEqual => self::BinOpKind::NotEqual,
                BinOpKind::Or => self::BinOpKind::Or,
            };

            let t1 = from_rnix(op.lhs().unwrap(), ctx, env, with_stack);
            let t2 = from_rnix(op.rhs().unwrap(), ctx, env, with_stack);

            ctx.mk_term(TermData::BinOp(kind, t1, t2))
        }
        // CR pandaman: check terms inside string interpolations
        ParsedType::Str(_) => ctx.mk_term(TermData::String),
        ParsedType::Value(value) => {
            use rnix::value::Value;

            match value.to_value().unwrap() {
                Value::Float(_) => todo!(),
                Value::Integer(_) => ctx.mk_term(TermData::Integer),
                Value::String(_) => ctx.mk_term(TermData::String),
                Value::Path(_, _) => ctx.mk_term(TermData::Path),
            }
        }
        ParsedType::With(with) => {
            let namespace = from_rnix(with.namespace().unwrap(), ctx, env, with_stack);
            let mut with_stack = with_stack.to_vec();
            with_stack.push(namespace);

            from_rnix(with.body().unwrap(), ctx, env, &with_stack)
        }
        ParsedType::Error(_) => todo!(),
        ParsedType::Key(_) => unreachable!(),
        ParsedType::Dynamic(_) => unreachable!(),
        ParsedType::KeyValue(_) => unreachable!(),
        ParsedType::PatBind(_) => unreachable!(),
        ParsedType::PatEntry(_) => unreachable!(),
        ParsedType::Pattern(_) => unreachable!(),
        ParsedType::Inherit(_) => unreachable!(),
        ParsedType::InheritFrom(_) => unreachable!(),
    }
}
