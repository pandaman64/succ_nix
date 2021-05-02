use std::fmt;

// possible boolean values
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct BoolDomain {
    pub tt: bool,
    pub ff: bool,
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
    pub fn is_bottom(&self) -> bool {
        !self.tt && !self.ff
    }

    pub fn sup(&self, other: &Self) -> Self {
        Self {
            tt: self.tt || other.tt,
            ff: self.ff || other.ff,
        }
    }

    pub fn inf(&self, other: &Self) -> Self {
        Self {
            tt: self.tt && other.tt,
            ff: self.ff && other.ff,
        }
    }
}
