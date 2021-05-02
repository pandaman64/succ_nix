use crate::typing::Type;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunType {
    pub arg: Box<Type>,
    pub ret: Box<Type>,
}

impl fmt::Display for FunType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} -> {}", self.arg, self.ret)
    }
}

impl FunType {
    fn sup(&self, other: &Self) -> Self {
        // this calculation is imprecise in a sense.
        // See notes/function_union.md
        let arg = self.arg.sup(&other.arg);
        let ret = self.ret.sup(&other.ret);

        Self {
            arg: arg.into(),
            ret: ret.into(),
        }
    }

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
                arg: arg.into(),
                ret: ret.into(),
            })
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunDomain {
    fun: Option<FunType>,
}

impl FunDomain {
    pub fn new(arg: Type, ret: Type) -> Self {
        Self {
            fun: Some(FunType {
                arg: arg.into(),
                ret: ret.into(),
            }),
        }
    }

    pub fn is_bottom(&self) -> bool {
        self.fun.is_none()
    }

    pub fn sup(&self, other: &Self) -> Self {
        let fun = match (&self.fun, &other.fun) {
            (Some(f1), Some(f2)) => Some(f1.sup(f2)),
            (Some(f), _) | (_, Some(f)) => Some(f.clone()),
            (None, None) => None,
        };

        Self { fun }
    }

    pub fn inf(&self, other: &Self) -> Self {
        let fun = match (&self.fun, &other.fun) {
            (Some(f1), Some(f2)) => f1.inf(f2),
            (None, _) | (_, None) => None,
        };

        Self { fun }
    }

    pub fn fmt(&self, f: &mut fmt::Formatter, first: bool) -> fmt::Result {
        match &self.fun {
            None => Ok(()),
            Some(fun) => {
                if !first {
                    f.write_str(" ∪ ")?;
                }
                write!(f, "{}", fun)
            }
        }
    }

    pub fn as_fun(&self) -> Option<&FunType> {
        self.fun.as_ref()
    }
}
