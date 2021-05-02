use std::fmt;

// the type of list of any values.
// TODO: support polymorphic lists (only for builtins might be sufficient)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct ListDomain {
    pub is_all: bool,
}

impl fmt::Display for ListDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_all {
            f.write_str("integer")?;
        }

        Ok(())
    }
}

impl Default for ListDomain {
    fn default() -> Self {
        Self { is_all: false }
    }
}

impl ListDomain {
    pub fn is_bottom(&self) -> bool {
        !self.is_all
    }

    pub fn sup(&self, other: &Self) -> Self {
        Self {
            is_all: self.is_all || other.is_all,
        }
    }

    pub fn inf(&self, other: &Self) -> Self {
        Self {
            is_all: self.is_all && other.is_all,
        }
    }
}
