#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<rnix::TextRange> for Span {
    fn from(range: rnix::TextRange) -> Self {
        Span {
            start: range.start().into(),
            end: range.end().into(),
        }
    }
}
