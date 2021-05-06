use crate::{
    domain::*,
    hir::{self, AttrSetDescriptor},
};
use std::{
    collections::{BTreeMap, BTreeSet, HashMap},
    fmt,
    sync::atomic::{self, AtomicUsize},
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Any,
    Union {
        boolean: BoolDomain,
        integer: IntDomain,
        list: ListDomain,
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
            path: Default::default(),
            string: Default::default(),
            vars: Default::default(),
            fun: Default::default(),
            attrs: AttrSetDomain::new(attrs),
        }
    }

    pub fn none() -> Self {
        Type::Union {
            boolean: Default::default(),
            integer: Default::default(),
            list: Default::default(),
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
                path,
                string,
                vars,
                fun,
                attrs,
            } if integer.is_bottom()
                && list.is_bottom()
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
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && list.is_bottom()
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
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && integer.is_bottom()
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

    pub fn as_path(&self) -> Option<&PathDomain> {
        match self {
            Type::Union {
                boolean,
                integer,
                list,
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && integer.is_bottom()
                && list.is_bottom()
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
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && list.is_bottom()
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
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && list.is_bottom()
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
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && list.is_bottom()
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
                path,
                string,
                vars,
                fun,
                attrs,
            } if boolean.is_bottom()
                && list.is_bottom()
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
        eprintln!("subtype? {} ⊆ {}, {}", self, other, self.inf(other));
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
                let path = p1.sup(p2);
                let string = s1.sup(s2);
                let vars = v1.sup(v2);
                let fun = f1.sup(f2);
                let attrs = a1.sup(a2);

                Union {
                    boolean,
                    integer,
                    list,
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
                let path = p1.inf(p2);
                let string = s1.inf(s2);
                let vars = Default::default();
                let fun = f1.inf(f2);
                let attrs = a1.inf(a2);

                Union {
                    boolean,
                    integer,
                    list,
                    path,
                    string,
                    vars,
                    fun,
                    attrs,
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constraint {
    // special case of conjunction of subset rules
    Equal(Box<Type>, Box<Type>),
    Subset(Box<Type>, Box<Type>),
    Conj(Vec<Constraint>),
    Disj(Vec<Constraint>),
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Constraint::*;

        match self {
            Equal(lhs, rhs) => write!(f, "{} = {}", lhs, rhs),
            Subset(lhs, rhs) => write!(f, "{} ⊆ {}", lhs, rhs),
            Conj(cs) => match cs.as_slice() {
                [] => f.write_str("⊤"),
                [head, tail @ ..] => {
                    write!(f, "{}", head)?;
                    for c in tail {
                        write!(f, " ∧ {}", c)?;
                    }
                    Ok(())
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

impl Constraint {
    fn top() -> Self {
        Constraint::Conj(vec![])
    }

    fn eq(ty1: Type, ty2: Type) -> Self {
        Constraint::Equal(ty1.into(), ty2.into())
    }

    fn subset(ty1: Type, ty2: Type) -> Self {
        Constraint::Subset(ty1.into(), ty2.into())
    }

    fn conj(cs: Vec<Constraint>) -> Self {
        // CR pandaman: normalize?
        Constraint::Conj(cs)
    }

    fn disj(cs: Vec<Constraint>) -> Self {
        // CR pandaman: normalize?
        Constraint::Disj(cs)
    }
}

pub type Environment = HashMap<hir::Id, Type>;

fn fresh_tvar() -> Type {
    static COUNT: AtomicUsize = AtomicUsize::new(0);

    let current = COUNT.fetch_add(1, atomic::Ordering::Relaxed);
    Type::var(format!("α{}", current))
}

fn type_descriptor(env: &mut Environment, attrs: &hir::AttrSetDescriptor) -> (Type, Constraint) {
    use hir::AttrSetDescriptor::*;

    match attrs {
        Leaf(t) => success_type(env, t),
        Internal(attrs) => {
            let (tys, cs): (Vec<Type>, Vec<Constraint>) = attrs
                .iter()
                .map(|(_, attrs)| type_descriptor(env, attrs))
                .unzip();
            let result_ty = Type::attr_set(AttrSetType {
                attrs: attrs.keys().cloned().zip(tys.iter().cloned()).collect(),
                rest: Type::none().into(),
            });
            (result_ty, Constraint::conj(cs))
        }
    }
}

pub fn success_type(env: &mut Environment, term: &hir::Term) -> (Type, Constraint) {
    use hir::TermData::*;

    match term {
        True => (Type::tt(), Constraint::top()),
        False => (Type::ff(), Constraint::top()),
        Integer => (Type::integer(), Constraint::top()),
        List => (Type::list(), Constraint::top()),
        Path => (Type::list(), Constraint::top()),
        String => (Type::string(), Constraint::top()),
        // assumes every name is in the environment already
        Var(v) => (env.get(v).unwrap().clone(), Constraint::top()),
        Lam(x, t) => {
            let arg_ty = fresh_tvar();
            env.insert(*x, arg_ty.clone());
            let (ret_ty, c) = success_type(env, t);

            (Type::fun(arg_ty, ret_ty), c)
        }
        App(t1, t2) => {
            let (ty1, c1) = success_type(env, t1);
            let (ty2, c2) = success_type(env, t2);

            let app_ty = fresh_tvar();
            let arg_ty = fresh_tvar();
            let ret_ty = fresh_tvar();
            let app_constraint = Constraint::conj(vec![
                // CR pandaman: I'm not sure that we should introduce equality here in the presence of function unions.
                // Consider a case where `Γ ⊢ not: true -> false ∪ false -> true, t: τ`.
                // In this case, `not t` generates an equality constraint `true -> false ∪ false -> true = α -> β`.
                // Since this equation does not have an answer, we instead solve an inprecise equation of `boolean -> boolean = α -> β`.
                // However, we can just admit it does not have an answer, and generate constraint of `α -> β ⊂ true -> false ∪ false -> true`.
                // The subset constraint diverges from the Success Typing paper, but we think this version is more correct.
                // (Probably they are not considering function unions, and just taking the `merge` of them)
                Constraint::eq(ty1, Type::fun(arg_ty.clone(), ret_ty.clone())),
                Constraint::subset(ty2, arg_ty),
                Constraint::subset(app_ty, ret_ty.clone()),
                c1,
                c2,
            ]);

            (ret_ty, app_constraint)
        }
        If(c, t, e) => {
            let (c_ty, c_c) = success_type(env, c);
            let (t_ty, t_c) = success_type(env, t);
            let (e_ty, e_c) = success_type(env, e);

            let result_ty = fresh_tvar();
            let result_c = Constraint::conj(vec![
                c_c,
                Constraint::disj(vec![
                    // then
                    Constraint::conj(vec![
                        Constraint::eq(c_ty.clone(), Type::tt()),
                        Constraint::eq(result_ty.clone(), t_ty),
                        t_c,
                    ]),
                    // else
                    Constraint::conj(vec![
                        Constraint::eq(c_ty, Type::ff()),
                        Constraint::eq(result_ty.clone(), e_ty),
                        e_c,
                    ]),
                ]),
            ]);

            (result_ty, result_c)
        }
        Let(names, descriptor, e) => {
            let tvs: Vec<_> = names.iter().map(|_| fresh_tvar()).collect();
            for ((_, id), t) in names.iter().zip(tvs.iter()) {
                env.insert(*id, t.clone());
            }

            match descriptor {
                AttrSetDescriptor::Internal(attrs) => {
                    let (tys, cs): (Vec<(&str, Type)>, Vec<Constraint>) = attrs
                        .iter()
                        .map(|(name, child)| {
                            let (ty, c) = type_descriptor(env, child);
                            ((name.as_str(), ty), c)
                        })
                        .unzip();
                    let (result_ty, result_c) = success_type(env, e);

                    let constraints = tys
                        .iter()
                        .map(|(name, ty)| {
                            let tv = env.get(names.get(*name).unwrap()).unwrap().clone();
                            Constraint::eq(tv, ty.clone())
                        })
                        .chain(cs)
                        .chain(std::iter::once(result_c))
                        .collect();

                    (result_ty, Constraint::conj(constraints))
                }
                _ => unreachable!(),
            }
        }
        AttrSet(attrs) => type_descriptor(env, attrs),
        Select(t, x) => {
            let (t_ty, t_c) = success_type(env, t);
            let tx_ty = fresh_tvar();
            let constraints = Constraint::conj(vec![
                // When `t.x` evaluates to a value, `τ_t` must include the set
                // `{ x = τ_x, ... = none }`.
                Constraint::subset(
                    Type::attr_set(AttrSetType {
                        attrs: {
                            let mut attrs = BTreeMap::new();
                            attrs.insert(x.clone(), tx_ty.clone());
                            attrs
                        },
                        rest: Type::none().into(),
                    }),
                    t_ty.clone(),
                ),
                // In order to `t.x` evaluates to a value, `t` must be in the set of
                // all attribute set with `x` field, i.e. `{ x = τ_x, ... = any }`.
                Constraint::subset(
                    t_ty,
                    Type::attr_set(AttrSetType {
                        attrs: {
                            let mut attrs = BTreeMap::new();
                            attrs.insert(x.clone(), tx_ty.clone());
                            attrs
                        },
                        rest: Type::any().into(),
                    }),
                ),
                t_c,
            ]);

            (tx_ty, constraints)
        }
        Or(t1, t2) => {
            // precisely, `or` operator introduces an implication, like
            // if `x.e` exists, `x.e or t ⊆ type(x.e)`, and otherwise, `x.e or t ⊆ type(t)`.
            // here, we just over-approximate these sets via `type(x.e) ∪ type(t)`.
            let (t1_ty, t1_c) = success_type(env, t1);
            let (t2_ty, t2_c) = success_type(env, t2);

            (t1_ty.sup(&t2_ty), Constraint::conj(vec![t1_c, t2_c]))
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct TypeErrorSink {
    is_error: bool,
}

impl TypeErrorSink {
    pub fn is_error(&self) -> bool {
        self.is_error
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
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
        eprintln!("update check: {} ⊆ {}", t1, inf);

        if let Some(vars) = t1.as_vars() {
            // A ∪ B ⊂ X --> A ⊂ X ∧ B ⊂ X
            for v in vars.vars.iter() {
                eprintln!("update {} --> {}", v, inf);
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

    fn refine_subset(&mut self, t1: &Type, t2: &Type, sink: &mut TypeErrorSink) {
        eprintln!("refine subset: {} ⊆ {}", t1, t2);

        let cur1 = self.map_ty(t1);
        let cur2 = self.map_ty(t2);

        // if the current solution does not satisfy the constraints, we refine
        // the assignments of variables.
        if !cur1.is_subtype(&cur2) {
            let inf = cur1.inf(&cur2);
            // type clash, or the constraint cannot be met
            if inf.is_bottom() {
                // TODO: more useful error reporting e.g. file names and lines
                eprintln!("type clash between {} and {}", cur1, cur2);
                sink.is_error = true;
            }
            self.update(t1, &inf);
        }
    }

    pub fn refine(&mut self, c: &Constraint, sink: &mut TypeErrorSink) {
        eprintln!("refine: {}", c);
        use Constraint::*;

        let old = self.clone();
        match c {
            Equal(t1, t2) => {
                self.refine_subset(t1, t2, sink);
                self.refine_subset(t2, t1, sink);
            }
            Subset(t1, t2) => self.refine_subset(t1, t2, sink),
            Conj(cs) => cs.iter().for_each(|c| self.refine(c, sink)),
            Disj(cs) => {
                // prevent interleaving mutable access to self
                let sols: Vec<_> = cs
                    .iter()
                    .filter_map(|c| {
                        let mut sol = self.clone();
                        let mut sink = TypeErrorSink::default();
                        sol.refine(c, &mut sink);
                        if sink.is_error() {
                            None
                        } else {
                            Some(sol)
                        }
                    })
                    .collect();

                let mut ok = false;

                for sol in sols.iter() {
                    // at least one branch must have a satisfying solution
                    ok = true;

                    for (v, t) in sol.map.iter() {
                        use std::collections::hash_map::Entry;

                        match self.map.entry(v.clone()) {
                            Entry::Vacant(v) => {
                                v.insert(t.clone());
                            }
                            Entry::Occupied(mut o) => {
                                // widen the assigned type
                                let t = o.get().sup(t);
                                o.insert(t);
                            }
                        }
                    }
                }

                if !ok {
                    sink.is_error = true;
                }
            }
        };

        if self != &old {
            self.refine(c, sink)
        }
    }
}
