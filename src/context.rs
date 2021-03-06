use std::{
    cell::RefCell,
    collections::HashSet,
    fmt,
    hash::{Hash, Hasher},
    ops::Deref,
    sync::atomic::{AtomicUsize, Ordering},
};

use typed_arena::Arena;

use crate::{
    hir::{Id, Term, TermData, TermKind},
    span::Span,
};

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
    current_hir_id: AtomicUsize,
}

impl<'a> Context<'a> {
    pub fn new() -> Self {
        Self {
            terms_arena: Arena::new(),
            terms: RefCell::new(HashSet::new()),
            current_hir_id: AtomicUsize::new(0),
        }
    }

    pub fn new_hir_id(&'a self) -> Id {
        let current = self.current_hir_id.fetch_add(1, Ordering::Relaxed);
        Id(current)
    }

    pub fn mk_term(&'a self, kind: TermKind<'a>, span: Span) -> Term<'a> {
        let t = TermData { kind, span };
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
