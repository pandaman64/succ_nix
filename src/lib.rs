// 型は単相だと思って全ての名前に対して単相の型を与える感じ
use std::{
    collections::{BTreeSet, HashMap},
    fmt,
    sync::atomic::{self, AtomicUsize},
};

mod ast;
pub mod hir;
mod parser;

pub use parser::parse_term;

// possible boolean values
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BoolDomain {
    tt: bool,
    ff: bool,
}

impl fmt::Display for BoolDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match (self.tt, self.ff) {
            (true, true) => f.write_str("bool"),
            (true, false) => f.write_str("tt"),
            (false, true) => f.write_str("ff"),
            (false, false) => Ok(()),
        }
    }
}

impl Default for BoolDomain {
    fn default() -> Self {
        Self {
            tt: false,
            ff: false,
        }
    }
}

impl BoolDomain {
    fn is_bottom(&self) -> bool {
        !self.tt && !self.ff
    }

    fn sup(&self, other: &Self) -> Self {
        Self {
            tt: self.tt || other.tt,
            ff: self.ff || other.ff,
        }
    }

    fn inf(&self, other: &Self) -> Self {
        Self {
            tt: self.tt && other.tt,
            ff: self.ff && other.ff,
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarDomain {
    vars: BTreeSet<String>,
}

impl VarDomain {
    fn is_bottom(&self) -> bool {
        self.vars.is_empty()
    }

    fn sup(&self, other: &Self) -> Self {
        let vars = self.vars.union(&other.vars).cloned().collect();
        Self { vars }
    }

    fn fmt(&self, f: &mut fmt::Formatter, mut first: bool) -> fmt::Result {
        for v in self.vars.iter() {
            if first {
                first = false;
                write!(f, "{}", v)?;
            } else {
                write!(f, " ∪ {}", v)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunType {
    arg: Box<Type>,
    ret: Box<Type>,
}

impl fmt::Display for FunType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.arg, self.ret)
    }
}

impl FunType {
    fn inf(&self, other: &Self) -> Option<Self> {
        // since f: A1 -> R1 and f: A2 -> R2,
        // f x ≠ ⊥ implies x ∈ A1 ∧ x ∈ A2 ∧ f x ∈ R1 ∧ f x ∈ R2.
        // therefore, f: A1 ∩ A2 -> R1 ∩ R2.
        let arg = self.arg.inf(&other.arg);
        let ret = self.ret.inf(&other.ret);

        // coalesce ⊥ -> R and A -> ⊥ into ⊥
        if arg.is_bottom() || ret.is_bottom() {
            None
        } else {
            Some(Self {
                arg: Box::new(arg),
                ret: Box::new(ret),
            })
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunDomain {
    funs: BTreeSet<FunType>,
}

impl FunDomain {
    fn is_bottom(&self) -> bool {
        self.funs.is_empty()
    }

    fn sup(&self, other: &Self) -> Self {
        // CR pandaman: minimization/normalization is needed here
        let funs = self.funs.union(&other.funs).cloned().collect();
        Self { funs }
    }

    fn inf(&self, other: &Self) -> Self {
        // (A ∪ B) ∩ (C ∪ D) = (A ∩ C) ∪ (A ∩ D) ∪ (B ∩ C) ∪ (B ∩ D)
        let mut funs = BTreeSet::new();
        for fun1 in self.funs.iter() {
            for fun2 in other.funs.iter() {
                if let Some(fun) = fun1.inf(fun2) {
                    funs.insert(fun);
                }
            }
        }
        Self { funs }
    }

    fn merge(&self) -> FunType {
        // (A -> B) `merge` (C -> D) = (A ∪ C) -> (B ∪ D)
        // Note that `merge` does NOT give a supremum in general.
        // e.g. (true -> false) `merge` (false -> true) = boolean -> boolean
        // but sup { true -> false, false -> true } = { true -> false, false -> true }
        self.funs.iter().fold(FunType { arg: Type::none().into(), ret: Type::none().into() }, |t1, t2| FunType {
            arg: t1.arg.sup(&t2.arg).into(),
            ret: t1.ret.sup(&t2.ret).into(),
        })
    }

    fn fmt(&self, f: &mut fmt::Formatter, mut first: bool) -> fmt::Result {
        for fun in self.funs.iter() {
            if first {
                first = false;
                write!(f, "{}", fun)?;
            } else {
                write!(f, " ∪ {}", fun)?;
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Any,
    Union {
        boolean: BoolDomain,
        vars: VarDomain,
        funs: FunDomain,
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
                vars,
                funs,
            } => {
                if !boolean.is_bottom() {
                    write!(f, "{}", boolean)?;
                }
                if !vars.is_bottom() {
                    vars.fmt(f, boolean.is_bottom())?;
                }
                if !funs.is_bottom() {
                    funs.fmt(f, boolean.is_bottom() && vars.is_bottom())?;
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
            vars: Default::default(),
            funs: Default::default(),
        }
    }

    pub fn ff() -> Self {
        Type::Union {
            boolean: BoolDomain {
                tt: false,
                ff: true,
            },
            vars: Default::default(),
            funs: Default::default(),
        }
    }

    pub fn boolean() -> Self {
        Type::Union {
            boolean: BoolDomain { tt: true, ff: true },
            vars: Default::default(),
            funs: Default::default(),
        }
    }

    fn var(v: String) -> Self {
        Type::Union {
            boolean: Default::default(),
            vars: VarDomain {
                vars: {
                    let mut vars = BTreeSet::new();
                    vars.insert(v);
                    vars
                },
            },
            funs: Default::default(),
        }
    }

    pub fn fun(arg: Type, ret: Type) -> Self {
        Type::Union {
            boolean: Default::default(),
            vars: Default::default(),
            funs: FunDomain {
                funs: {
                    let mut funs = BTreeSet::new();
                    funs.insert(FunType {
                        arg: arg.into(),
                        ret: ret.into(),
                    });
                    funs
                },
            },
        }
    }

    pub fn none() -> Self {
        Type::Union {
            boolean: Default::default(),
            vars: Default::default(),
            funs: Default::default(),
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
                vars,
                funs,
            } if vars.is_bottom() && funs.is_bottom() => Some(boolean),
            _ => None,
        }
    }

    pub fn as_vars(&self) -> Option<&VarDomain> {
        match self {
            Type::Union {
                boolean,
                vars,
                funs,
            } if boolean.is_bottom() && funs.is_bottom() => Some(vars),
            _ => None,
        }
    }

    pub fn as_funs(&self) -> Option<&FunDomain> {
        match self {
            Type::Union {
                boolean,
                vars,
                funs,
            } if boolean.is_bottom() && vars.is_bottom() => Some(funs),
            _ => None,
        }
    }

    fn is_bottom(&self) -> bool {
        matches!(self, Type::Union { boolean, vars, funs } if boolean.is_bottom() && vars.is_bottom() && funs.is_bottom())
    }

    fn is_subtype(&self, other: &Type) -> bool {
        // CR pandaman: 同じ型でも表現が違うケースいっぱいあるが
        eprintln!("subtype? {} ⊆ {}, {}", self, other, self.inf(other));
        &self.inf(other) == self
    }

    // T1 ∪ T2
    fn sup(&self, other: &Type) -> Type {
        use Type::*;

        match (self, other) {
            (Any, _) | (_, Any) => Any,
            (
                Union {
                    boolean: b1,
                    vars: v1,
                    funs: f1,
                },
                Union {
                    boolean: b2,
                    vars: v2,
                    funs: f2,
                },
            ) => {
                let boolean = b1.sup(b2);
                let vars = v1.sup(v2);
                let funs = f1.sup(f2);

                Union {
                    boolean,
                    vars,
                    funs,
                }
            }
        }
    }

    // T1 ∩ T2
    fn inf(&self, other: &Type) -> Type {
        use Type::*;

        match (self, other) {
            (Any, ty) | (ty, Any) => ty.clone(),
            (_, ty) if self.is_bottom() => ty.clone(),
            (ty, _) if other.is_bottom() => ty.clone(),
            (
                Union {
                    boolean: b1,
                    vars: v1,
                    funs: f1,
                },
                Union {
                    boolean: b2,
                    vars: v2,
                    funs: f2,
                },
            ) => {
                assert!(v1.is_bottom());
                assert!(v2.is_bottom());

                let boolean = b1.inf(b2);
                let vars = Default::default();
                let funs = f1.inf(f2);

                Union {
                    boolean,
                    vars,
                    funs,
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

type Environment = HashMap<hir::Id, Type>;

fn fresh_tvar() -> Type {
    static COUNT: AtomicUsize = AtomicUsize::new(0);

    let current = COUNT.fetch_add(1, atomic::Ordering::Relaxed);
    Type::var(format!("α{}", current))
}

pub fn success_type(env: &mut Environment, term: &hir::Term) -> (Type, Constraint) {
    use hir::TermData::*;

    match term {
        True => (Type::tt(), Constraint::top()),
        False => (Type::ff(), Constraint::top()),
        // assumes every name is in the environment already
        Var(v) => (env.get(v).unwrap().clone(), Constraint::top()),
        Lam(x, t) => {
            let arg_ty = fresh_tvar();
            env.insert(*x, arg_ty.clone());
            let (ret_ty, c) = success_type(env, t);

            // let fun_constraint = Constraint::Equal(Box::new(fun_ty.clone()), Box::new(Type::When(Box::new(Type::Fun(Box::new(arg_ty), Box::new(ty))), c)));
            (Type::fun(arg_ty, ret_ty), c)
        }
        App(t1, t2) => {
            let (ty1, c1) = success_type(env, t1);
            let (ty2, c2) = success_type(env, t2);

            let app_ty = fresh_tvar();
            let arg_ty = fresh_tvar();
            let ret_ty = fresh_tvar();
            let app_constraint = Constraint::conj(vec![
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
            let result_c = Constraint::Conj(vec![
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
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Solution {
    // tvar -> ty
    map: HashMap<String, Type>,
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

    fn map_ty(&self, ty: &Type) -> Type {
        use Type::*;

        match ty {
            Union {
                boolean,
                vars,
                funs,
            } => {
                // (A ∪ B) ∪ (C ∪ D) = A ∪ B ∪ C ∪ D
                let mut ty = Type::Union {
                    boolean: *boolean,
                    vars: Default::default(),
                    funs: Default::default(),
                };

                for v in vars.vars.iter() {
                    match self.map_tvar(v) {
                        Any => return Any,
                        t => ty = ty.sup(&t),
                    }
                }

                for fun in funs.funs.iter() {
                    let fun = self.map_fun(fun);
                    ty = ty.sup(&Type::fun(*fun.arg, *fun.ret));
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
            // A ∪ B ⊂ X --> A ⊂ X ∧ B ⊂ X
            for v in vars.vars.iter() {
                eprintln!("update {} --> {}", v, inf);
                self.map.insert(v.clone(), inf.clone());
            }
        }

        if let Some(funs) = t1.as_funs() {
            if let Some(inf_funs) = inf.as_funs() {
                // this merge introduces inpreciseness
                let inf = inf_funs.merge();
                // A ∪ B ⊂ X --> A ⊂ X ∧ B ⊂ X
                for f in funs.funs.iter() {
                    // A -> B ⊂ C -> D implies A ⊂ C ∧ B ⊂ D
                    self.update(&f.arg, &inf.arg);
                    self.update(&f.ret, &inf.ret);
                }
            }
        }
    }

    fn refine_subset(&mut self, t1: &Type, t2: &Type) -> bool {
        eprintln!("refine subset: {} ⊆ {}", t1, t2);

        let cur1 = self.map_ty(t1);
        let cur2 = self.map_ty(t2);

        if cur1.is_subtype(&cur2) {
            true
        } else {
            let inf = cur1.inf(&cur2);
            // type clash
            if inf.is_bottom() {
                eprintln!("type clash between {} and {}", cur1, cur2);
                false
            } else {
                self.update(t1, &inf);
                true
            }
        }
    }

    pub fn refine(&mut self, c: &Constraint) -> bool {
        eprintln!("refine: {}", c);
        use Constraint::*;

        let old = self.clone();
        let result = match c {
            Equal(t1, t2) => self.refine_subset(t1, t2) && self.refine_subset(t2, t1),
            Subset(t1, t2) => self.refine_subset(t1, t2),
            Conj(cs) => cs.iter().all(|c| self.refine(c)),
            Disj(cs) => {
                // prevent interleaving mutable access to self
                let sols: Vec<_> = cs
                    .iter()
                    .filter_map(|c| {
                        let mut sol = self.clone();
                        if sol.refine(c) {
                            Some(sol)
                        } else {
                            None
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

                ok
            }
        };
        if result {
            if self != &old {
                self.refine(c)
            } else {
                true
            }
        } else {
            false
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn env() -> (HashMap<String, hir::Id>, Environment) {
        let not_id = hir::Id::new();
        (
            vec![(String::from("not"), not_id)].into_iter().collect(),
            vec![(not_id, Type::fun(Type::boolean(), Type::boolean()))]
                .into_iter()
                .collect(),
        )
    }

    fn run(
        input: &str,
        (name_env, mut ty_env): (HashMap<String, hir::Id>, Environment),
    ) -> (bool, Type) {
        let ast = parse_term(input).unwrap();
        let arena = typed_arena::Arena::new();
        let hir = hir::from_ast(&ast, &arena, &name_env);
        eprintln!("hir = {}", hir);

        let (t, c) = success_type(&mut ty_env, &hir);
        eprintln!("type = {}\nconstraint = {}", t, c);
        eprintln!("env:");
        for (v, id) in name_env.iter() {
            eprintln!("{} --> {}", v, id);
        }
        for (v, t) in ty_env.iter() {
            eprintln!("{} --> {}", v, t);
        }
        let mut sol = Solution::default();
        let result = sol.refine(&c);
        eprintln!("sol[{}]:", result);
        for (v, t) in sol.map.iter() {
            eprintln!("{} --> {}", v, t);
        }

        (result, sol.map_ty(&t))
    }

    fn success(input: &str, env: (HashMap<String, hir::Id>, Environment), expected: Type) {
        let (result, actual) = run(input, env);

        assert!(result);
        assert_eq!(actual, expected);
    }

    fn fail(input: &str, env: (HashMap<String, hir::Id>, Environment)) {
        let (result, _actual) = run(input, env);

        assert!(!result);
    }

    #[test]
    fn test_not_first() {
        success(
            "x: y: not x",
            env(),
            Type::fun(Type::boolean(), Type::fun(Type::any(), Type::boolean())),
        );
    }

    #[test]
    fn test_id() {
        success("x: x", env(), Type::fun(Type::any(), Type::any()));
    }

    #[test]
    fn test_xx() {
        success(
            "x: x x",
            env(),
            Type::fun(Type::fun(Type::any(), Type::any()), Type::any()),
        );
    }

    #[test]
    fn test_not_id() {
        fail("not (x: x)", env())
    }

    #[test]
    fn test_branch() {
        success(
            "x: if x then not x else x",
            env(),
            Type::fun(Type::boolean(), Type::boolean()),
        );
    }

    #[test]
    fn test_branch_half_succ() {
        // let F be this lambda abstaction.
        // If (F x) terminates, x = ff and F x = ff, which means F is typable with ff -> ff.
        success(
            "x: if x then x true else x",
            env(),
            Type::fun(Type::ff(), Type::ff()),
        );
    }
}
