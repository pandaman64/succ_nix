use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Term {
    True,
    False,
    Var(String),
    Lam(String, Box<Term>),
    App(Box<Term>, Box<Term>),
    If(Box<Term>, Box<Term>, Box<Term>),
    Let(Vec<(String, Term)>, Box<Term>),
    AttrSet(Vec<(String, Term)>),
    Path(Box<Term>, String),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt(f, 1)
    }
}

fn indent(f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
    for _ in 0..level {
        f.write_str("  ")?;
    }

    Ok(())
}

impl Term {
    fn fmt(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        use Term::*;

        match self {
            True => f.write_str("tt"),
            False => f.write_str("ff"),
            Var(v) => f.write_str(v),
            Lam(x, t) => {
                write!(f, "({}: ", x)?;
                t.fmt(f, level)?;
                f.write_str(")")
            }
            App(t1, t2) => {
                t1.fmt(f, level)?;
                f.write_str(" ")?;
                t2.fmt(f, level)
            }
            If(c, t, e) => {
                f.write_str("if ")?;
                c.fmt(f, level)?;
                f.write_str(" then ")?;
                t.fmt(f, level)?;
                f.write_str(" else ")?;
                e.fmt(f, level)
            }
            Let(assignments, e) => {
                f.write_str("let\n")?;
                for (v, t) in assignments.iter() {
                    indent(f, level)?;
                    write!(f, "{} = ", v)?;
                    t.fmt(f, level + 1)?;
                    f.write_str(";\n")?;
                }
                indent(f, level.saturating_sub(1))?;
                f.write_str("in ")?;
                e.fmt(f, level)
            }
            AttrSet(assignments) => {
                f.write_str("{\n")?;
                for (v, t) in assignments.iter() {
                    indent(f, level)?;
                    write!(f, "{} = ", v)?;
                    t.fmt(f, level + 1)?;
                    f.write_str(";\n")?;
                }
                indent(f, level.saturating_sub(1))?;
                f.write_str("}")
            }
            Path(t, field) => {
                t.fmt(f, level)?;
                write!(f, ".{}", field)
            }
        }
    }
}
