use std::fmt;

// can be null or not
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct NullDomain {
    pub is_all: bool,
}

impl fmt::Display for NullDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_all {
            f.write_str("null")?;
        }

        Ok(())
    }
}

impl Default for NullDomain {
    fn default() -> Self {
        Self { is_all: false }
    }
}

impl NullDomain {
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
