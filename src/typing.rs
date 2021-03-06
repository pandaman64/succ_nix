use crate::{
    domain::*,
    hir::{self, KeyValueDescriptor},
    span::Span,
};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt::{self, Debug},
    sync::atomic::{self, AtomicUsize},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Any,
    Union {
        boolean: BoolDomain,
        integer: IntDomain,
        list: ListDomain,
        null: NullDomain,
        path: PathDomain,
        string: StringDomain,
        vars: VarDomain,
        fun: FunDomain,
        attrs: AttrSetDomain,
    },
    // Bottom,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;

        match self {
            Any => f.write_str("any"),
            Union { .. } if self.is_bottom() => f.write_str("∅"),
            Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } => {
                let mut first = true;
                if !boolean.is_bottom() {
                    write!(f, "{}", boolean)?;
                    first = false;
                }
                if !integer.is_bottom() {
                    if !first {
                        f.write_str(" ∪ ")?;
                    }
                    write!(f, "{}", integer)?;
                    first = false;
                }
                if !list.is_bottom() {
                    if !first {
                        f.write_str(" ∪ ")?;
                    }
                    write!(f, "{}", list)?;
                    first = false;
                }
                if !null.is_bottom() {
                    if !first {
                        f.write_str(" ∪ ")?;
                    }
                    write!(f, "{}", null)?;
                    first = false;
                }
                if !path.is_bottom() {
                    if !first {
                        f.write_str(" ∪ ")?;
                    }
                    write!(f, "{}", path)?;
                    first = false;
                }
                if !string.is_bottom() {
                    if !first {
                        f.write_str(" ∪ ")?;
                    }
                    write!(f, "{}", string)?;
                    first = false;
                }
                if !vars.is_bottom() {
                    vars.fmt(f, first)?;
                    first = false;
                }
                if !fun.is_bottom() {
                    fun.fmt(f, first)?;
                    first = false;
                }
                if !attrs.is_bottom() {
                    attrs.fmt(f, first)?;
                }
                Ok(())
            }
        }
    }
}

impl Type {
    pub fn tt() -> Self {
        Type::Union {
            boolean: BoolDomain {
                tt: true,
                ff: false,
            },
            integer: Default::default(),
            list: Default::default(),
            null: Default::default(),
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: Default::default(),
        }
    }

    pub fn ff() -> Self {
        Type::Union {
            boolean: BoolDomain {
                tt: false,
                ff: true,
            },
            integer: Default::default(),
            list: Default::default(),
            null: Default::default(),
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: Default::default(),
        }
    }

    pub fn boolean() -> Self {
        Type::Union {
            boolean: BoolDomain { tt: true, ff: true },
            integer: Default::default(),
            list: Default::default(),
            null: Default::default(),
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: Default::default(),
        }
    }

    pub fn integer() -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: IntDomain { is_all: true },
            list: Default::default(),
            null: Default::default(),
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: Default::default(),
        }
    }

    pub fn list() -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: Default::default(),
            list: ListDomain { is_all: true },
            null: Default::default(),
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: Default::default(),
        }
    }

    pub fn null() -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: Default::default(),
            list: Default::default(),
            null: NullDomain { is_all: true },
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: Default::default(),
        }
    }

    pub fn path() -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: Default::default(),
            list: Default::default(),
            null: Default::default(),
            path: PathDomain { is_all: true },
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: Default::default(),
        }
    }

    pub fn string() -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: Default::default(),
            list: Default::default(),
            null: Default::default(),
            path: Default::default(),
            string: StringDomain { is_all: true },
            vars: Default::default(),
            fun: Default::default(),
            attrs: Default::default(),
        }
    }

    fn var(v: String) -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: Default::default(),
            list: Default::default(),
            null: Default::default(),
            path: Default::default(),
            string: Default::default(),
            vars: VarDomain {
                vars: {
                    let mut vars = BTreeSet::new();
                    vars.insert(v);
                    vars
                },
            },
            fun: Default::default(),
            attrs: Default::default(),
        }
    }

    pub fn fun(arg: Type, ret: Type) -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: Default::default(),
            list: Default::default(),
            null: Default::default(),
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: FunDomain::new(arg, ret),
            attrs: Default::default(),
        }
    }

    pub fn attr_set(attrs: AttrSetType) -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: Default::default(),
            list: Default::default(),
            null: Default::default(),
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: AttrSetDomain::new(attrs),
        }
    }

    pub fn any_attr_set() -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: Default::default(),
            list: Default::default(),
            null: Default::default(),
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: AttrSetDomain::new(AttrSetType {
                attrs: Default::default(),
                rest: Type::any().into(),
            }),
        }
    }

    pub fn none() -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: Default::default(),
            list: Default::default(),
            null: Default::default(),
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: Default::default(),
        }
    }

    pub fn any() -> Self {
        Type::Any
    }

    // CR pandaman: as_xxx may sound like just extracing xxx domain without checking bottomness of others
    pub fn as_boolean(&self) -> Option<&BoolDomain> {
        match self {
            Type::Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } if integer.is_bottom()
                && list.is_bottom()
                && null.is_bottom()
                && path.is_bottom()
                && string.is_bottom()
                && vars.is_bottom()
                && fun.is_bottom()
                && attrs.is_bottom() =>
            {
                Some(boolean)
            }
            _ => None,
        }
    }

    pub fn as_integer(&self) -> Option<&IntDomain> {
        match self {
            Type::Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && list.is_bottom()
                && null.is_bottom()
                && path.is_bottom()
                && string.is_bottom()
                && vars.is_bottom()
                && fun.is_bottom()
                && attrs.is_bottom() =>
            {
                Some(integer)
            }
            _ => None,
        }
    }

    pub fn as_list(&self) -> Option<&ListDomain> {
        match self {
            Type::Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && integer.is_bottom()
                && null.is_bottom()
                && path.is_bottom()
                && string.is_bottom()
                && vars.is_bottom()
                && fun.is_bottom()
                && attrs.is_bottom() =>
            {
                Some(list)
            }
            _ => None,
        }
    }

    pub fn as_null(&self) -> Option<&NullDomain> {
        match self {
            Type::Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && integer.is_bottom()
                && list.is_bottom()
                && path.is_bottom()
                && string.is_bottom()
                && vars.is_bottom()
                && fun.is_bottom()
                && attrs.is_bottom() =>
            {
                Some(null)
            }
            _ => None,
        }
    }

    pub fn as_path(&self) -> Option<&PathDomain> {
        match self {
            Type::Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && integer.is_bottom()
                && list.is_bottom()
                && null.is_bottom()
                && string.is_bottom()
                && vars.is_bottom()
                && fun.is_bottom()
                && attrs.is_bottom() =>
            {
                Some(path)
            }
            _ => None,
        }
    }

    pub fn as_vars(&self) -> Option<&VarDomain> {
        match self {
            Type::Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && list.is_bottom()
                && null.is_bottom()
                && path.is_bottom()
                && integer.is_bottom()
                && string.is_bottom()
                && fun.is_bottom()
                && attrs.is_bottom() =>
            {
                Some(vars)
            }
            _ => None,
        }
    }

    pub fn as_fun(&self) -> Option<&FunType> {
        match self {
            Type::Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && list.is_bottom()
                && null.is_bottom()
                && path.is_bottom()
                && integer.is_bottom()
                && string.is_bottom()
                && vars.is_bottom()
                && attrs.is_bottom() =>
            {
                fun.as_fun()
            }
            _ => None,
        }
    }

    pub fn as_attrs(&self) -> Option<&AttrSetType> {
        match self {
            Type::Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && list.is_bottom()
                && null.is_bottom()
                && path.is_bottom()
                && integer.is_bottom()
                && string.is_bottom()
                && vars.is_bottom()
                && fun.is_bottom() =>
            {
                attrs.as_attrs()
            }
            _ => None,
        }
    }

    pub fn is_bottom(&self) -> bool {
        match self {
            Type::Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && list.is_bottom()
                && null.is_bottom()
                && path.is_bottom()
                && integer.is_bottom()
                && string.is_bottom()
                && vars.is_bottom()
                && fun.is_bottom()
                && attrs.is_bottom() =>
            {
                true
            }
            _ => false,
        }
    }

    fn is_subtype(&self, other: &Type) -> bool {
        // CR pandaman: 同じ型でも表現が違うケースいっぱいあるが
        // e.g. { a = ∅, ... = ∅; } = { ... = ∅; }. (これ本当に同じ型？)
        tracing::trace!("subtype? {} ⊆ {}, {}", self, other, self.inf(other));
        &self.inf(other) == self
    }

    // T1 ∪ T2
    pub fn sup(&self, other: &Type) -> Type {
        use Type::*;

        match (self, other) {
            (Any, _) | (_, Any) => Any,
            (
                Union {
                    boolean: b1,
                    integer: i1,
                    list: l1,
                    null: n1,
                    path: p1,
                    string: s1,
                    vars: v1,
                    fun: f1,
                    attrs: a1,
                },
                Union {
                    boolean: b2,
                    integer: i2,
                    list: l2,
                    null: n2,
                    path: p2,
                    string: s2,
                    vars: v2,
                    fun: f2,
                    attrs: a2,
                },
            ) => {
                let boolean = b1.sup(b2);
                let integer = i1.sup(i2);
                let list = l1.sup(l2);
                let null = n1.sup(n2);
                let path = p1.sup(p2);
                let string = s1.sup(s2);
                let vars = v1.sup(v2);
                let fun = f1.sup(f2);
                let attrs = a1.sup(a2);

                Union {
                    boolean,
                    integer,
                    list,
                    null,
                    path,
                    string,
                    vars,
                    fun,
                    attrs,
                }
            }
        }
    }

    // T1 ∩ T2
    pub fn inf(&self, other: &Type) -> Type {
        use Type::*;

        match (self, other) {
            (Any, ty) | (ty, Any) => ty.clone(),
            (_, ty) if self.is_bottom() => ty.clone(),
            (ty, _) if other.is_bottom() => ty.clone(),
            (
                Union {
                    boolean: b1,
                    integer: i1,
                    list: l1,
                    null: n1,
                    path: p1,
                    string: s1,
                    vars: v1,
                    fun: f1,
                    attrs: a1,
                },
                Union {
                    boolean: b2,
                    integer: i2,
                    list: l2,
                    null: n2,
                    path: p2,
                    string: s2,
                    vars: v2,
                    fun: f2,
                    attrs: a2,
                },
            ) => {
                assert!(v1.is_bottom());
                assert!(v2.is_bottom());

                let boolean = b1.inf(b2);
                let integer = i1.inf(i2);
                let list = l1.inf(l2);
                let null = n1.inf(n2);
                let path = p1.inf(p2);
                let string = s1.inf(s2);
                let vars = Default::default();
                let fun = f1.inf(f2);
                let attrs = a1.inf(a2);

                Union {
                    boolean,
                    integer,
                    list,
                    null,
                    path,
                    string,
                    vars,
                    fun,
                    attrs,
                }
            }
        }
    }

    fn ftv(&self) -> impl Iterator<Item = &String> {
        match self {
            Type::Any => vec![].into_iter(),
            Type::Union {
                vars, fun, attrs, ..
            } => {
                let mut vars: Vec<_> = vars.vars.iter().collect();
                if let Some(fun) = fun.as_fun() {
                    vars.extend(fun.arg.ftv());
                    vars.extend(fun.ret.ftv());
                }
                if let Some(attrs) = attrs.as_attrs() {
                    vars.extend(attrs.attrs.iter().flat_map(|(_, ty)| ty.ftv()));
                    vars.extend(attrs.rest.ftv());
                }
                vars.into_iter()
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Constraint {
    pub kind: ConstraintKind,
    pub span: Span,
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.kind, f)
    }
}

impl Constraint {
    fn top(span: Span) -> Self {
        Self {
            kind: ConstraintKind::Conj(vec![]),
            span,
        }
    }

    fn eq(ty1: Type, ty2: Type, span: Span) -> Self {
        Self {
            kind: ConstraintKind::Equal(ty1.into(), ty2.into()),
            span,
        }
    }

    fn subset(ty1: Type, ty2: Type, span: Span) -> Self {
        Self {
            kind: ConstraintKind::Subset(ty1.into(), ty2.into()),
            span,
        }
    }

    fn conj(cs: Vec<Constraint>, span: Span) -> Self {
        // CR pandaman: normalize?
        Self {
            kind: ConstraintKind::Conj(cs),
            span,
        }
    }

    fn disj(cs: Vec<Constraint>, span: Span) -> Self {
        // CR pandaman: normalize?
        Self {
            kind: ConstraintKind::Disj(cs),
            span,
        }
    }
}

#[derive(Debug, Clone)]
pub enum ConstraintKind {
    // special case of conjunction of subset rules
    Equal(Box<Type>, Box<Type>),
    Subset(Box<Type>, Box<Type>),
    Conj(Vec<Constraint>),
    Disj(Vec<Constraint>),
}

impl fmt::Display for ConstraintKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ConstraintKind::*;

        match self {
            Equal(lhs, rhs) => write!(f, "{} = {}", lhs, rhs),
            Subset(lhs, rhs) => write!(f, "{} ⊆ {}", lhs, rhs),
            Conj(cs) => match cs.as_slice() {
                [] => f.write_str("⊤"),
                [head, tail @ ..] => {
                    write!(f, "({}", head)?;
                    for c in tail {
                        write!(f, " ∧ {}", c)?;
                    }
                    write!(f, ")")
                }
            },
            Disj(cs) => match cs.as_slice() {
                [] => f.write_str("⊥"),
                [head, tail @ ..] => {
                    write!(f, "({}", head)?;
                    for c in tail {
                        write!(f, " ∨ {}", c)?;
                    }
                    write!(f, ")")
                }
            },
        }
    }
}

pub type Environment = HashMap<hir::Id, Type>;

fn fresh_tvar() -> Type {
    static COUNT: AtomicUsize = AtomicUsize::new(0);

    let current = COUNT.fetch_add(1, atomic::Ordering::Relaxed);
    Type::var(format!("α{}", current))
}

fn type_descriptor(
    env: &mut Environment,
    attrs: &hir::KeyValueDescriptor,
    span: Span,
) -> (Type, Constraint) {
    use hir::KeyValueDescriptor::*;

    match attrs {
        Leaf(t) => success_type(env, t),
        Internal(descriptors) => {
            let mut attrs = BTreeMap::new();
            let mut rest = Type::none();
            let mut constraints = vec![];

            for (name, child) in descriptors.iter() {
                use hir::AttrPath::*;

                let (desc_ty, desc_c) = type_descriptor(env, child, span);
                constraints.push(desc_c);

                match name {
                    Static(name) => {
                        attrs.insert(name.clone(), desc_ty);
                    }
                    Dynamic(name) => {
                        let (name_ty, name_c) = success_type(env, name);
                        constraints.push(Constraint::subset(name_ty, Type::string(), name.span));
                        constraints.push(name_c);

                        // merge types of dynamic attributes
                        rest = rest.sup(&desc_ty);
                    }
                }
            }

            let result_ty = Type::attr_set(AttrSetType {
                attrs,
                rest: rest.into(),
            });

            (result_ty, Constraint::conj(constraints, span))
        }
    }
}

pub fn success_type(env: &mut Environment, term: &hir::Term) -> (Type, Constraint) {
    use hir::TermKind::*;

    match &term.kind {
        True => (Type::tt(), Constraint::top(term.span)),
        False => (Type::ff(), Constraint::top(term.span)),
        Integer => (Type::integer(), Constraint::top(term.span)),
        List(items) => {
            let constraints = items
                .iter()
                .map(|item| {
                    let (_, c) = success_type(env, item);

                    c
                })
                .collect();
            (Type::list(), Constraint::conj(constraints, term.span))
        }
        Null => (Type::null(), Constraint::top(term.span)),
        Path => (Type::path(), Constraint::top(term.span)),
        String => (Type::string(), Constraint::top(term.span)),
        // assumes every name is in the environment already
        Var(v) => (env.get(v).unwrap().clone(), Constraint::top(term.span)),
        Lam(x, t) => {
            let arg_ty = fresh_tvar();
            env.insert(*x, arg_ty.clone());
            let (ret_ty, c) = success_type(env, t);
            let fun_ty = fresh_tvar();

            (
                fun_ty.clone(),
                Constraint::conj(
                    vec![
                        Constraint::subset(fun_ty, Type::fun(arg_ty, ret_ty), term.span),
                        c,
                    ],
                    term.span,
                ),
            )
        }
        App(t1, t2) => {
            let (ty1, c1) = success_type(env, t1);
            let (ty2, c2) = success_type(env, t2);

            let app_ty = fresh_tvar();
            let arg_ty = fresh_tvar();
            let ret_ty = fresh_tvar();
            let app_constraint = Constraint::conj(
                vec![
                    // CR pandaman: I'm not sure that we should introduce equality here in the presence of function unions.
                    // Consider a case where `Γ ⊢ not: true -> false ∪ false -> true, t: τ`.
                    // In this case, `not t` generates an equality constraint `true -> false ∪ false -> true = α -> β`.
                    // Since this equation does not have an answer, we instead solve an inprecise equation of `boolean -> boolean = α -> β`.
                    // However, we can just admit it does not have an answer, and generate constraint of `α -> β ⊂ true -> false ∪ false -> true`.
                    // The subset constraint diverges from the Success Typing paper, but we think this version is more correct.
                    // (Probably they are not considering function unions, and just taking the `merge` of them)
                    Constraint::eq(ty1, Type::fun(arg_ty.clone(), ret_ty.clone()), t1.span),
                    Constraint::subset(ty2, arg_ty, t2.span),
                    Constraint::subset(app_ty, ret_ty.clone(), term.span),
                    c1,
                    c2,
                ],
                term.span,
            );

            (ret_ty, app_constraint)
        }
        Assert(t1, t2) => {
            let (cond_ty, cond_c) = success_type(env, t1);
            let (body_ty, body_c) = success_type(env, t2);

            let constraints = Constraint::conj(
                vec![
                    // CR pandaman: I'm not sure that the return type should be true or boolean
                    Constraint::subset(cond_ty, Type::tt(), t1.span),
                    cond_c,
                    body_c,
                ],
                term.span,
            );

            (body_ty, constraints)
        }
        If(c, t, e) => {
            let (c_ty, c_c) = success_type(env, c);
            let (t_ty, t_c) = success_type(env, t);
            let (e_ty, e_c) = success_type(env, e);

            let result_ty = fresh_tvar();
            let result_c = Constraint::conj(
                vec![
                    c_c,
                    Constraint::disj(
                        vec![
                            // then
                            Constraint::conj(
                                vec![
                                    Constraint::eq(c_ty.clone(), Type::tt(), c.span),
                                    Constraint::eq(result_ty.clone(), t_ty, t.span),
                                    t_c,
                                ],
                                term.span,
                            ),
                            // else
                            Constraint::conj(
                                vec![
                                    Constraint::eq(c_ty, Type::ff(), c.span),
                                    Constraint::eq(result_ty.clone(), e_ty, e.span),
                                    e_c,
                                ],
                                term.span,
                            ),
                        ],
                        term.span,
                    ),
                ],
                term.span,
            );

            (result_ty, result_c)
        }
        Let(names, descriptor, e) => {
            let tvs: Vec<_> = names.iter().map(|_| fresh_tvar()).collect();
            for ((_, id), t) in names.iter().zip(tvs.iter()) {
                env.insert(*id, t.clone());
            }

            match descriptor {
                KeyValueDescriptor::Internal(descriptors) => {
                    let (tys, cs): (Vec<(&str, Type)>, Vec<Constraint>) = descriptors
                        .iter()
                        .map(|(name, child)| {
                            let (ty, c) = type_descriptor(env, child, term.span);

                            // let cannot have descriptors that start with
                            // dynamic attribute names.
                            match name {
                                hir::AttrPath::Static(name) => ((name.as_str(), ty), c),
                                _ => unreachable!(),
                            }
                        })
                        .unzip();
                    let (result_ty, result_c) = success_type(env, e);

                    let constraints = tys
                        .iter()
                        .map(|(name, ty)| {
                            let tv = env.get(names.get(*name).unwrap()).unwrap().clone();
                            Constraint::eq(tv, ty.clone(), term.span)
                        })
                        .chain(cs)
                        .chain(std::iter::once(result_c))
                        .collect();

                    (result_ty, Constraint::conj(constraints, term.span))
                }
                _ => unreachable!(),
            }
        }
        AttrSet(attrs) => type_descriptor(env, attrs, term.span),
        UnaryOp(kind, t) => {
            use hir::UnaryOpKind::*;

            match kind {
                BooleanNeg => {
                    let (t_ty, t_c) = success_type(env, t);
                    let ret_ty = fresh_tvar();

                    let constraints = Constraint::conj(
                        vec![
                            // CR pandaman: consider constructing a constraint like
                            // `(τ_t ⊂ tt ∧ τ_ret ⊂ ff) ∨ (τ_t ⊂ ff ∧ τ_ret ⊂ tt)`
                            Constraint::subset(t_ty, Type::boolean(), t.span),
                            Constraint::subset(ret_ty.clone(), Type::boolean(), term.span),
                            t_c,
                        ],
                        term.span,
                    );

                    (ret_ty, constraints)
                }
                IntegerNeg => {
                    let (t_ty, t_c) = success_type(env, t);
                    let ret_ty = fresh_tvar();

                    let constraints = Constraint::conj(
                        vec![
                            Constraint::subset(t_ty, Type::integer(), t.span),
                            Constraint::subset(ret_ty.clone(), Type::integer(), term.span),
                            t_c,
                        ],
                        term.span,
                    );

                    (ret_ty, constraints)
                }
            }
        }
        BinOp(kind, t1, t2) => {
            use hir::BinOpKind::*;

            let (t1_ty, t1_c) = success_type(env, t1);
            let (t2_ty, t2_c) = success_type(env, t2);
            let ret_ty = fresh_tvar();

            let mut constraints = match kind {
                Add | Sub | Mul | Div => {
                    vec![
                        Constraint::subset(t1_ty, Type::integer(), t1.span),
                        Constraint::subset(t2_ty, Type::integer(), t2.span),
                        Constraint::subset(ret_ty.clone(), Type::integer(), term.span),
                    ]
                }
                And | Or | Implication => {
                    vec![
                        Constraint::subset(t1_ty, Type::boolean(), t1.span),
                        // Due to short-curcuiting, the second argument can be any type
                        // e.g. `false && 100 = false`
                        Constraint::subset(t2_ty, Type::any(), t2.span),
                        Constraint::subset(ret_ty.clone(), Type::boolean(), term.span),
                    ]
                }
                Equal | NotEqual => {
                    vec![
                        Constraint::subset(t1_ty, Type::any(), t1.span),
                        Constraint::subset(t2_ty, Type::any(), t2.span),
                        Constraint::subset(ret_ty.clone(), Type::boolean(), term.span),
                    ]
                }
                Less | LessOrEq | Greater | GreaterOrEq => {
                    vec![
                        Constraint::subset(t1_ty, Type::any(), t1.span),
                        Constraint::subset(t2_ty, Type::any(), t2.span),
                        Constraint::subset(ret_ty.clone(), Type::boolean(), term.span),
                    ]
                }
                Concat => {
                    vec![
                        Constraint::subset(t1_ty, Type::list(), t1.span),
                        Constraint::subset(t2_ty, Type::list(), t2.span),
                        Constraint::subset(ret_ty.clone(), Type::list(), term.span),
                    ]
                }
                Update => {
                    vec![
                        // arguments must be attrsets
                        Constraint::subset(t1_ty, Type::any_attr_set(), t1.span),
                        Constraint::subset(t2_ty, Type::any_attr_set(), t2.span),
                        Constraint::subset(ret_ty.clone(), Type::any_attr_set(), term.span),
                    ]
                }
            };

            constraints.push(t1_c);
            constraints.push(t2_c);

            (ret_ty, Constraint::conj(constraints, term.span))
        }
        Select(t, x) => {
            use hir::AttrPath::*;

            let (t_ty, t_c) = success_type(env, t);
            let tx_ty = fresh_tvar();

            let mut constraints = match x {
                Static(name) => vec![
                    // When `t.x` evaluates to a value, `τ_t` must include the set
                    // `{ x = τ_x, ... = none }`.
                    Constraint::subset(
                        Type::attr_set(AttrSetType {
                            attrs: {
                                let mut attrs = BTreeMap::new();
                                attrs.insert(name.clone(), tx_ty.clone());
                                attrs
                            },
                            rest: Type::none().into(),
                        }),
                        t_ty.clone(),
                        term.span,
                    ),
                    // In order to `t.x` evaluates to a value, `t` must be in the set of
                    // all attribute set with `x` field, i.e. `{ x = τ_x, ... = any }`.
                    Constraint::subset(
                        t_ty,
                        Type::attr_set(AttrSetType {
                            attrs: {
                                let mut attrs = BTreeMap::new();
                                attrs.insert(name.clone(), tx_ty.clone());
                                attrs
                            },
                            rest: Type::any().into(),
                        }),
                        term.span,
                    ),
                ],
                Dynamic(name) => {
                    // we have not discovered the constraint for the result type
                    // ideally, we'd like to conclude that
                    // type_of({ a = 100; }.${...}) ⊂ integer
                    // for example.

                    let (name_ty, name_c) = success_type(env, name);

                    vec![
                        // dynamic name evaluates to a string
                        Constraint::subset(name_ty, Type::string(), name.span),
                        name_c,
                        // `t` must be an attribute set
                        Constraint::subset(t_ty, Type::any_attr_set(), term.span),
                    ]
                }
            };
            constraints.push(t_c);

            (tx_ty, Constraint::conj(constraints, term.span))
        }
        Or(t1, t2) => {
            // precisely, `or` operator introduces an implication, like
            // if `x.e` exists, `x.e or t ⊆ type(x.e)`, and otherwise, `x.e or t ⊆ type(t)`.
            let (t1_ty, t1_c) = success_type(env, t1);
            let (t2_ty, t2_c) = success_type(env, t2);
            let ret_ty = fresh_tvar();

            (
                ret_ty.clone(),
                Constraint::disj(
                    vec![
                        Constraint::conj(
                            vec![Constraint::subset(ret_ty.clone(), t1_ty, term.span), t1_c],
                            term.span,
                        ),
                        Constraint::conj(
                            vec![Constraint::subset(ret_ty, t2_ty, term.span), t2_c],
                            term.span,
                        ),
                    ],
                    term.span,
                ),
            )
        }
        HasAttr(t) => {
            let (t_ty, t_c) = success_type(env, t);
            let ret_ty = fresh_tvar();

            (
                ret_ty.clone(),
                Constraint::conj(
                    vec![
                        Constraint::subset(t_ty, Type::any_attr_set(), term.span),
                        Constraint::subset(ret_ty, Type::boolean(), term.span),
                        t_c,
                    ],
                    term.span,
                ),
            )
        }
    }
}

#[derive(Debug, Clone)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TypeErrorKind {
    Subset(Type, Type),
    Disj,
    Limit,
}

#[derive(Debug, Default, Clone)]
pub struct TypeErrorSink {
    errors: Vec<TypeError>,
}

impl TypeErrorSink {
    fn push_subset_error(&mut self, smaller: Type, larger: Type, span: Span) {
        self.errors.push(TypeError {
            kind: TypeErrorKind::Subset(smaller, larger),
            span,
        });
    }

    fn push_disj_error(&mut self, span: Span) {
        self.errors.push(TypeError {
            kind: TypeErrorKind::Disj,
            span,
        });
    }

    fn push_limit_error(&mut self, span: Span) {
        self.errors.push(TypeError {
            kind: TypeErrorKind::Limit,
            span,
        })
    }

    pub fn is_error(&self) -> bool {
        !self.errors.is_empty()
    }

    pub fn errors(&self) -> impl Iterator<Item = &TypeError> {
        self.errors.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Solution {
    // tvar -> ty
    pub(crate) map: HashMap<String, Type>,
}

impl fmt::Display for Solution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (v, t) in self.map.iter() {
            write!(f, "{} --> {}", v, t)?;
        }

        Ok(())
    }
}

impl Solution {
    fn map_tvar(&self, tvar: &str) -> Type {
        // unconstrained types will be any
        self.map.get(tvar).cloned().unwrap_or_else(Type::any)
    }

    fn map_fun(&self, fun: &FunType) -> FunType {
        FunType {
            arg: self.map_ty(&fun.arg).into(),
            ret: self.map_ty(&fun.ret).into(),
        }
    }

    fn map_attrs(&self, ty: &AttrSetType) -> AttrSetType {
        let attrs = ty
            .attrs
            .iter()
            .map(|(v, t)| {
                let t = self.map_ty(t);
                (v.clone(), t)
            })
            .collect();
        let rest = self.map_ty(&ty.rest).into();

        AttrSetType { attrs, rest }
    }

    pub fn map_ty(&self, ty: &Type) -> Type {
        use Type::*;

        match ty {
            Union {
                boolean,
                integer,
                list,
                null,
                path,
                string,
                vars,
                fun,
                attrs,
            } => {
                // (A ∪ B) ∪ (C ∪ D) = A ∪ B ∪ C ∪ D
                let mut ty = Type::Union {
                    boolean: *boolean,
                    integer: *integer,
                    list: *list,
                    null: *null,
                    path: *path,
                    string: *string,
                    vars: Default::default(),
                    fun: Default::default(),
                    attrs: Default::default(),
                };

                for v in vars.vars.iter() {
                    match self.map_tvar(v) {
                        Any => return Any,
                        t => ty = ty.sup(&t),
                    }
                }

                if let Some(fun) = fun.as_fun() {
                    let fun = self.map_fun(fun);
                    ty = ty.sup(&Type::fun(*fun.arg, *fun.ret));
                }

                if let Some(attrs) = attrs.as_attrs() {
                    let attrs = self.map_attrs(attrs);
                    ty = ty.sup(&Type::attr_set(attrs));
                }

                ty
            }
            Any => Any,
        }
    }

    // extract syntactic matching between t1 (non-substituted) and inf (substituted),
    // and update the solution
    fn update(&mut self, t1: &Type, inf: &Type) {
        tracing::trace!("update check: {} ⊆ {}", t1, inf);

        if let Some(vars) = t1.as_vars() {
            // A ∪ B ⊂ X --> A ⊂ X ∧ B ⊂ X
            for v in vars.vars.iter() {
                tracing::trace!("update {} --> {}", v, inf);
                self.map.insert(v.clone(), inf.clone());
            }
        }

        if let Some(fun) = t1.as_fun() {
            // CR pandaman: for inf, just extracting the corresponding domain is enough?
            if let Some(inf_fun) = inf.as_fun() {
                // A -> B ⊂ C -> D implies A ⊂ C ∧ B ⊂ D
                self.update(&fun.arg, &inf_fun.arg);
                self.update(&fun.ret, &inf_fun.ret);
            }
        }

        if let Some(attrs) = t1.as_attrs() {
            if let Some(inf_attrs) = inf.as_attrs() {
                // since we take infimum, the attribute sets on the right hand
                // side contains all attribute names on the left hand side.
                for (v, t1) in attrs.attrs.iter() {
                    let inf = inf_attrs.attrs.get(v).unwrap();
                    self.update(t1, inf);
                }
            }
        }
    }

    fn refine_subset(&mut self, t1: &Type, t2: &Type, sink: &mut TypeErrorSink, span: Span) {
        tracing::trace!("refine subset: {} ⊆ {}", t1, t2);

        let cur1 = self.map_ty(t1);
        let cur2 = self.map_ty(t2);

        // if the current solution does not satisfy the constraints, we refine
        // the assignments of variables.
        if !cur1.is_subtype(&cur2) {
            let inf = cur1.inf(&cur2);
            // type clash, or the constraint cannot be met
            if inf.is_bottom() {
                sink.push_subset_error(cur1, cur2, span);
            }
            self.update(t1, &inf);
        }
    }

    fn refine(&mut self, c: &Constraint, sink: &mut TypeErrorSink, limit: &mut usize) {
        while *limit > 0 {
            *limit = limit.saturating_sub(1);

            tracing::debug!("refine: {}", c);
            use ConstraintKind::*;

            let old = self.clone();
            match &c.kind {
                Equal(t1, t2) => {
                    self.refine_subset(t1, t2, sink, c.span);
                    self.refine_subset(t2, t1, sink, c.span);
                }
                Subset(t1, t2) => self.refine_subset(t1, t2, sink, c.span),
                Conj(cs) => cs.iter().for_each(|c| self.refine(c, sink, limit)),
                Disj(cs) => {
                    // prevent interleaving mutable access to self
                    let disj_sol = cs
                        .iter()
                        .filter_map(|c| {
                            let mut sol = self.clone();
                            let mut sink = TypeErrorSink::default();
                            sol.refine(c, &mut sink, limit);
                            if sink.is_error() {
                                None
                            } else {
                                Some(sol)
                            }
                        })
                        .reduce(|mut s1, s2| {
                            for (var, t2) in s2.map.into_iter() {
                                let ty = s1.map.get(&var).unwrap().sup(&t2);
                                s1.map.insert(var, ty);
                            }
                            s1
                        });

                    match disj_sol {
                        Some(disj_sol) => {
                            *self = disj_sol;
                        }
                        None => {
                            // at least one branch must have a satisfying solution
                            sink.push_disj_error(c.span);
                        }
                    }
                }
            };

            if self == &old {
                return;
            } else {
                let diffs: Vec<_> = old
                    .map
                    .iter()
                    .filter_map(|(v, old_ty)| {
                        let new_ty = self.map.get(v).unwrap();
                        if old_ty != new_ty {
                            Some(format!("{}: {} ==> {}", v, old_ty, new_ty))
                        } else {
                            None
                        }
                    })
                    .collect();
                tracing::debug!(?diffs);
            }
        }

        // failed to find a solution within a determined limit
        sink.push_limit_error(c.span);
    }

    fn init_ftv(&mut self, c: &Constraint) {
        use ConstraintKind::*;

        match &c.kind {
            Equal(t1, t2) | Subset(t1, t2) => {
                for tv in t1.ftv() {
                    self.map.insert(tv.clone(), Type::any());
                }
                for tv in t2.ftv() {
                    self.map.insert(tv.clone(), Type::any());
                }
            }
            Conj(cs) | Disj(cs) => {
                for c in cs.iter() {
                    self.init_ftv(c);
                }
            }
        }
    }

    pub fn solve(c: &Constraint, sink: &mut TypeErrorSink, mut limit: usize) -> Self {
        let mut sol = Self {
            map: Default::default(),
        };
        sol.init_ftv(c);
        sol.refine(c, sink, &mut limit);
        sol
    }
}
