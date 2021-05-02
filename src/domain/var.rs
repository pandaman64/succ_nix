use std::{collections::BTreeSet, fmt};

#[derive(Debug, Default, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarDomain {
    pub vars: BTreeSet<String>,
}

impl VarDomain {
    pub fn is_bottom(&self) -> bool {
        self.vars.is_empty()
    }

    pub fn sup(&self, other: &Self) -> Self {
        let vars = self.vars.union(&other.vars).cloned().collect();
        Self { vars }
    }

    pub fn fmt(&self, f: &mut fmt::Formatter, mut first: bool) -> fmt::Result {
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
