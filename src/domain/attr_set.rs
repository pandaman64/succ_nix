use crate::typing::Type;
use std::{
    collections::{BTreeMap, BTreeSet},
    fmt,
};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AttrSetType {
    pub attrs: BTreeMap<String, Type>,
    pub rest: Box<Type>,
}

impl fmt::Display for AttrSetType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str("{ ")?;
        for (name, ty) in self.attrs.iter() {
            write!(f, "{} = {}; ", name, ty)?;
        }
        write!(f, "... = {}; ", self.rest)?;
        f.write_str("}")
    }
}

impl AttrSetType {
    pub fn union_names<'a>(&'a self, other: &'a Self) -> BTreeSet<&'a String> {
        self.attrs.keys().chain(other.attrs.keys()).collect()
    }

    fn sup(&self, other: &Self) -> Self {
        let names = self.union_names(other);

        let attrs = names
            .iter()
            .map(|&name| {
                let t = match (self.attrs.get(name), other.attrs.get(name)) {
                    (Some(t1), Some(t2)) => t1.sup(t2),
                    (Some(t1), None) => t1.sup(&other.rest),
                    (None, Some(t2)) => t2.sup(&self.rest),
                    (None, None) => unreachable!(),
                };
                (name.clone(), t)
            })
            .collect();

        Self {
            attrs,
            rest: self.rest.sup(&other.rest).into(),
        }
    }

    fn inf(&self, other: &Self) -> Self {
        let names = self.union_names(other);

        let attrs = names
            .iter()
            .map(|&name| {
                let t = match (self.attrs.get(name), other.attrs.get(name)) {
                    (Some(t1), Some(t2)) => t1.inf(t2),
                    (Some(t1), None) => t1.inf(&other.rest),
                    (None, Some(t2)) => t2.inf(&self.rest),
                    (None, None) => unreachable!(),
                };
                (name.clone(), t)
            })
            .collect();

        Self {
            attrs,
            rest: self.rest.inf(&other.rest).into(),
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct AttrSetDomain {
    attrs: Option<AttrSetType>,
}

impl AttrSetDomain {
    pub fn new(attrs: AttrSetType) -> Self {
        Self { attrs: Some(attrs) }
    }

    pub fn is_bottom(&self) -> bool {
        self.attrs.is_none()
    }

    pub fn sup(&self, other: &Self) -> Self {
        let attrs = match (&self.attrs, &other.attrs) {
            (Some(a1), Some(a2)) => Some(a1.sup(a2)),
            (Some(a), _) | (_, Some(a)) => Some(a.clone()),
            (None, None) => None,
        };

        Self { attrs }
    }

    pub fn inf(&self, other: &Self) -> Self {
        let attrs = match (&self.attrs, &other.attrs) {
            (Some(a1), Some(a2)) => Some(a1.inf(a2)),
            (None, _) | (_, None) => None,
        };

        Self { attrs }
    }

    pub fn fmt(&self, f: &mut fmt::Formatter, first: bool) -> fmt::Result {
        match &self.attrs {
            None => Ok(()),
            Some(attrs) => {
                if !first {
                    f.write_str(" ∪ ")?;
                }
                write!(f, "{}", attrs)
            }
        }
    }

    pub fn as_attrs(&self) -> Option<&AttrSetType> {
        self.attrs.as_ref()
    }
}
