use std::fmt;

// very imprecise representation, i.e. all integers or none.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct IntDomain {
    pub is_all: bool,
}

impl fmt::Display for IntDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_all {
            f.write_str("integer")?;
        }

        Ok(())
    }
}

impl Default for IntDomain {
    fn default() -> Self {
        Self { is_all: false }
    }
}

impl IntDomain {
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
