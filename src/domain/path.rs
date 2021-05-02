use std::fmt;

// very imprecise representation, i.e. all paths or none.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct PathDomain {
    pub is_all: bool,
}

impl fmt::Display for PathDomain {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_all {
            f.write_str("path")?;
        }

        Ok(())
    }
}

impl Default for PathDomain {
    fn default() -> Self {
        Self { is_all: false }
    }
}

impl PathDomain {
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
