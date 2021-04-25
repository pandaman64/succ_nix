use std::{
    collections::HashMap,
    fmt,
    sync::atomic::{self, AtomicUsize},
};

#[derive(Debug, Clone)]
enum Term {
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
            Lam(x, t) => write!(f, "(λ{}. {})", x, t),
            App(t1, t2) => write!(f, "{} {}", t1, t2),
            If(c, t, e) => write!(f, "if {} then {} else {}", c, t, e),
        }
    }
}

macro_rules! term {
    (true) => { Term::True };
    (false) => { Term::False };
    ($v:literal) => { Term::Var(String::from($v)) };
    (lam $x:literal. $($t:tt)+) => { Term::Lam(String::from($x), Box::new(term!($($t)+))) };
    (($($t1:tt)+) $($t2:tt)+) => { Term::App(Box::new(term!($($t1)+)), Box::new(term!($($t2)+))) };
    (if ( $($c:tt)+ ) { $($t:tt)+ } else { $($e:tt)+ }) => { Term::If(Box::new(term!($($c)+)), Box::new(term!($($t)+)), Box::new(term!($($e)+))) };
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimType {
    True,
    False,
    Var(String),
    Fun(Box<Type>, Box<Type>),
}

impl fmt::Display for PrimType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PrimType::*;

        match self {
            True => f.write_str("tt"),
            False => f.write_str("ff"),
            Var(v) => f.write_str(v),
            Fun(t1, t2) => write!(f, "{} -> {}", t1, t2),
        }
    }
}

impl PrimType {
    fn inf(&self, other: &PrimType) -> Option<PrimType> {
        use PrimType::*;

        match (self, other) {
            (Var(_), _) | (_, Var(_)) => panic!("no var must occur in inf"),
            (_, _) if self == other => Some(self.clone()),
            (Fun(arg1, ret1), Fun(arg2, ret2)) => {
                // since f: A1 -> R1 and f: A2 -> R2,
                // f x ≠ ⊥ implies x ∈ A1 ∧ x ∈ A2 ∧ f x ∈ R1 ∧ f x ∈ R2.
                // therefore, f: A1 ∩ A2 -> R1 ∩ R2.
                let arg = arg1.inf(arg2);
                let ret = ret1.inf(ret2);

                // coalesce ⊥ -> R and A -> ⊥ into ⊥
                if arg.is_bottom() || ret.is_bottom() {
                    None
                } else {
                    Some(Fun(arg.into(), ret.into()))
                }
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Union(Vec<PrimType>),
    Any,
    // Bottom,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Type::*;

        match self {
            // When(t, c) => write!(f, "({} when {})", t, c),
            Union(ts) => match ts.as_slice() {
                [] => f.write_str("∅"),
                [head, tail @ ..] => {
                    write!(f, "{}", head)?;
                    for t in tail {
                        write!(f, " ∪ {}", t)?;
                    }
                    Ok(())
                }
            },
            Any => f.write_str("any"),
        }
    }
}

impl Type {
    fn tt() -> Self {
        Type::Union(vec![PrimType::True])
    }

    fn ff() -> Self {
        Type::Union(vec![PrimType::False])
    }

    fn var(v: String) -> Self {
        Type::Union(vec![PrimType::Var(v)])
    }

    fn fun(arg: Type, ret: Type) -> Self {
        Type::Union(vec![PrimType::Fun(arg.into(), ret.into())])
    }

    fn boolean() -> Self {
        Type::Union(vec![PrimType::True, PrimType::False])
    }

    fn none() -> Self {
        Type::Union(vec![])
    }

    fn any() -> Self {
        Type::Any
    }

    fn is_bottom(&self) -> bool {
        matches!(self, Type::Union(ts) if ts.is_empty())
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
            (Union(this), Union(other)) => {
                // suppress infinite growth
                if this.len() + other.len() > 10 {
                    Any
                } else {
                    // CR pandaman: opportunity for optimization (PrimTypeのdiscriminantが同じやつをまとめられる)
                    // Union自体をDomainごとに分割してそれぞれでsupとりたい
                    let mut this = this.clone();
                    for o in other.iter() {
                        if this.iter().all(|t| t != o) {
                            this.push(o.clone());
                        }
                    }
                    Union(this)
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
            // (A ∪ B) ∩ (C ∪ D) = (A ∩ C) ∪ (A ∩ D) ∪ (B ∩ C) ∪ (B ∩ D)
            (Union(this), Union(other)) => {
                let mut ts = vec![];
                for t1 in this.iter() {
                    for t2 in other.iter() {
                        if let Some(t) = t1.inf(t2) {
                            ts.push(t);
                        }
                    }
                }
                Union(ts)
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
}

type Environment = HashMap<String, Type>;

fn fresh_tvar() -> Type {
    static COUNT: AtomicUsize = AtomicUsize::new(0);

    let current = COUNT.fetch_add(1, atomic::Ordering::Relaxed);
    Type::var(format!("α{}", current))
}

fn success_type(env: &mut Environment, term: &Term) -> (Type, Constraint) {
    match term {
        Term::True => (Type::tt(), Constraint::top()),
        Term::False => (Type::ff(), Constraint::top()),
        // assumes every name is in the environment already
        Term::Var(v) => (env.get(v).unwrap().clone(), Constraint::top()),
        Term::Lam(x, t) => {
            let arg_ty = fresh_tvar();
            env.insert(x.clone(), arg_ty.clone());
            let (ret_ty, c) = success_type(env, t);

            // let fun_constraint = Constraint::Equal(Box::new(fun_ty.clone()), Box::new(Type::When(Box::new(Type::Fun(Box::new(arg_ty), Box::new(ty))), c)));
            (Type::fun(arg_ty, ret_ty), c)
        }
        Term::App(t1, t2) => {
            let (ty1, c1) = success_type(env, t1);
            let (ty2, c2) = success_type(env, t2);

            let app_ty = fresh_tvar();
            let arg_ty = fresh_tvar();
            let ret_ty = fresh_tvar();
            let app_constraint = Constraint::Conj(vec![
                Constraint::Equal(
                    Box::new(ty1),
                    Box::new(Type::fun(arg_ty.clone(), ret_ty.clone())),
                ),
                Constraint::Subset(Box::new(ty2), Box::new(arg_ty)),
                Constraint::Subset(Box::new(app_ty), Box::new(ret_ty.clone())),
                c1,
                c2,
            ]);

            (ret_ty, app_constraint)
        }
        Term::If(c, t, e) => {
            let (c_ty, c_c) = success_type(env, c);
            let (t_ty, t_c) = success_type(env, t);
            let (e_ty, e_c) = success_type(env, e);

            let result_ty = fresh_tvar();
            let result_c = Constraint::Conj(vec![
                c_c,
                Constraint::Disj(vec![
                    // then
                    Constraint::Conj(vec![
                        Constraint::Equal(Box::new(c_ty.clone()), Box::new(Type::tt())),
                        Constraint::Equal(Box::new(result_ty.clone()), Box::new(t_ty)),
                        t_c,
                    ]),
                    // else
                    Constraint::Conj(vec![
                        Constraint::Equal(Box::new(c_ty), Box::new(Type::ff())),
                        Constraint::Equal(Box::new(result_ty.clone()), Box::new(e_ty)),
                        e_c,
                    ]),
                ]),
            ]);

            (result_ty, result_c)
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
struct Solution {
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
        self.map.get(tvar).cloned().unwrap_or(Type::any())
    }

    fn map_primty(&self, ty: &PrimType) -> Type {
        use PrimType::*;

        match ty {
            True => Type::tt(),
            False => Type::ff(),
            Var(v) => self.map_tvar(v),
            Fun(arg, ret) => Type::fun(self.map_ty(arg), self.map_ty(ret)),
        }
    }

    fn map_ty(&self, ty: &Type) -> Type {
        use Type::*;

        match ty {
            Union(ts) => {
                // (A ∪ B) ∪ (C ∪ D) = A ∪ B ∪ C ∪ D
                let mut mapped = vec![];
                for t in ts.iter() {
                    match self.map_primty(t) {
                        // A ∪ any = any
                        Any => return Any,
                        Union(us) => mapped.extend(us.into_iter()),
                    }
                }
                Union(mapped)
            }
            Any => Any,
            // Intersection(ts) => Intersection(ts.iter().map(|t| self.map_ty(t)).collect()),
        }
    }

    // extract syntactic matching between t1 (non-substituted) and inf (substituted),
    // and update the solution
    fn update(&mut self, t1: &Type, inf: &Type) {
        eprintln!("update check: {} ⊆ {}", t1, inf);
        use PrimType::*;
        use Type::*;

        if let Union(t1) = t1 {
            if let [t1] = t1.as_slice() {
                match t1 {
                    Var(v) => {
                        eprintln!("update {} --> {}", v, inf);
                        self.map.insert(v.clone(), inf.clone());
                    }
                    Fun(arg1, ret1) => {
                        if let Union(inf) = inf {
                            if let [inf] = inf.as_slice() {
                                match inf {
                                    Var(_) => unreachable!(),
                                    Fun(arg2, ret2) => {
                                        self.update(arg1, arg2);
                                        self.update(ret1, ret2);
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    _ => {}
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

    fn refine(&mut self, c: &Constraint) -> bool {
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

// 型は単相だと思って全ての名前に対して単相の型を与える感じ
fn main() {
    println!("{}", term!(true));
    println!("{}", term!(false));
    println!("{}", term!("x"));
    println!("{}", term!(lam "x". "x"));
    println!("{}", term!((lam "x". ("x") "x") lam "x". "x"));
    println!("{}", term!(lam "x". ("not") ("not") "x"));
    println!("{}", term!(lam "x". if ("x") { ("not") "x" } else { "x" }));

    // let (t, c) = success_type(&mut Environment::default(), &term!((lam "x". "x") true));
    // println!("{}\n{}\n", t, c);

    // println!();
    // let mut env = vec![(String::from("not"), Type::fun(Type::boolean(), Type::boolean()))].into_iter().collect();
    // let (t, c) = success_type(&mut env, &term!(lam "x". lam "y". ("not") "x"));
    // println!("{}\n{}", t, c);
    // println!("env:");
    // for (v, t) in env.iter() {
    //     println!("{} --> {}", v, t);
    // }
    // let mut sol = Solution::default();
    // let result = sol.refine(&c);
    // println!("sol[{}]:", result);
    // for (v, t) in sol.map.iter() {
    //     println!("{} --> {}", v, t);
    // }

    // println!();
    // let mut env = Environment::default();
    // let (t, c) = success_type(&mut env, &term!(lam "x". ("x") "x"));
    // println!("{}\n{}", t, c);
    // println!("env:");
    // for (v, t) in env.iter() {
    //     println!("{} --> {}", v, t);
    // }
    // let mut sol = Solution::default();
    // let result = sol.refine(&c);
    // println!("sol[{}]:", result);
    // for (v, t) in sol.map.iter() {
    //     println!("{} --> {}", v, t);
    // }

    // println!();
    // let mut env = vec![(String::from("not"), Type::fun(Type::boolean(), Type::boolean()))].into_iter().collect();
    // let (t, c) = success_type(&mut env, &term!(("not") lam "x". "x"));
    // println!("{}\n{}", t, c);
    // println!("env:");
    // for (v, t) in env.iter() {
    //     println!("{} --> {}", v, t);
    // }
    // let mut sol = Solution::default();
    // let result = sol.refine(&c);
    // println!("sol[{}]:", result);
    // for (v, t) in sol.map.iter() {
    //     println!("{} --> {}", v, t);
    // }

    // println!();
    // let mut env = vec![(String::from("not"), Type::fun(Type::boolean(), Type::boolean()))].into_iter().collect();
    // let (t, c) = success_type(&mut env, &term!(lam "x". if ("x") { ("not") "x" } else { "x" }));
    // println!("{}\n{}", t, c);
    // println!("env:");
    // for (v, t) in env.iter() {
    //     println!("{} --> {}", v, t);
    // }
    // let mut sol = Solution::default();
    // let result = sol.refine(&c);
    // println!("sol[{}]:", result);
    // for (v, t) in sol.map.iter() {
    //     println!("{} --> {}", v, t);
    // }

    println!();
    let mut env = vec![(
        String::from("not"),
        Type::fun(Type::boolean(), Type::boolean()),
    )]
    .into_iter()
    .collect();
    let (t, c) = success_type(
        &mut env,
        &term!(lam "x". if ("x") { ("x") true } else { ("x") false }),
    );
    println!("{}\n{}", t, c);
    println!("env:");
    for (v, t) in env.iter() {
        println!("{} --> {}", v, t);
    }
    let mut sol = Solution::default();
    let result = sol.refine(&c);
    println!("sol[{}]:", result);
    for (v, t) in sol.map.iter() {
        println!("{} --> {}", v, t);
    }
}
