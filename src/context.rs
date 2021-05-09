use std::{
    cell::RefCell,
    collections::HashSet,
    fmt,
    hash::{Hash, Hasher},
    ops::Deref,
};

use typed_arena::Arena;

use crate::hir::{Term, TermData};

/// the type of interned (hash-consed) things, where equality and hash values are
/// determined by its memory address
pub struct Interned<'a, T>(&'a T);

impl<'a, T> fmt::Debug for Interned<'a, T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a, T> fmt::Display for Interned<'a, T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<'a, T> Clone for Interned<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Copy for Interned<'a, T> {}

impl<'a, T> PartialEq for Interned<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        // pointer comparison
        let this: *const T = self.0;
        let other: *const T = other.0;
        std::ptr::eq(this, other)
    }
}

impl<'a, T> Eq for Interned<'a, T> {}

impl<'a, T> PartialOrd for Interned<'a, T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a, T> Ord for Interned<'a, T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // pointer comparison
        let this: *const T = self.0;
        let other: *const T = other.0;
        this.cmp(&other)
    }
}

impl<'a, T> Hash for Interned<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        // pointer hashing
        let this: *const T = self.0;
        this.hash(state)
    }
}

impl<'a, T> Deref for Interned<'a, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Default)]
pub struct Context<'a> {
    terms_arena: Arena<TermData<'a>>,
    terms: RefCell<HashSet<&'a TermData<'a>>>,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            terms_arena: Arena::new(),
            terms: RefCell::new(HashSet::new()),
        }
    }

    pub fn mk_term(&'a self, t: TermData<'a>) -> Term<'a> {
        let mut terms = self.terms.borrow_mut();
        match terms.get(&t) {
            Some(t) => Interned(t),
            None => {
                let t: &'a TermData<'a> = self.terms_arena.alloc(t);
                terms.insert(t);
                Interned(t)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::hir::BinOpKind;

    use super::*;

    #[test]
    fn test_mk_term() {
        let ctx = Context::new();

        // (a + a) + (a + a)
        let a1 = ctx.mk_term(TermData::Integer);
        let a2 = ctx.mk_term(TermData::Integer);
        let a1a2 = ctx.mk_term(TermData::BinOp(BinOpKind::Add, a1, a2));

        let b1 = ctx.mk_term(TermData::Integer);
        let b2 = ctx.mk_term(TermData::Integer);
        let b1b2 = ctx.mk_term(TermData::BinOp(BinOpKind::Add, b1, b2));

        let t = ctx.mk_term(TermData::BinOp(BinOpKind::Add, a1a2, b1b2));

        eprintln!("{}", t);
        assert_eq!(ctx.terms_arena.len(), 3);
    }
}
