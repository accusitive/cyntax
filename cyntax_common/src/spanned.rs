use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub range: Range<usize>,
}

impl<T> Spanned<T> {
    pub fn new(range: Range<usize>, value: T) -> Spanned<T> {
        Spanned { value: value, range }
    }
    pub fn map<U: Clone, F: Fn(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            range: self.range.start..self.range.end,
            value: f(self.value),
        }
    }
    pub fn map_ref<U: Clone, F: Fn(&T) -> U>(&self, f: F) -> Spanned<U> {
        Spanned {
            range: self.range.start..self.range.end,
            value: f(&self.value),
        }
    }
    pub fn with_offset(self, offset: usize) -> Spanned<T> {
        Spanned {
            value: self.value,
            range: (self.range.start + offset)..(self.range.end + offset),
        }
    }
}
impl<T: PartialEq + Clone> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
pub trait SpanRef {
    fn span_ref(&self) -> Option<&Range<usize>>;
    fn span_ref_fallback(&self, fallback: Range<usize>) -> &Range<usize>;
}
pub trait Span {
    fn span(&self) -> Option<Range<usize>>;
    fn span_fallback(&self, fallback: Range<usize>) -> Range<usize>;
}

impl<T> SpanRef for Spanned<T> {
    fn span_ref(&self) -> Option<&Range<usize>> {
        Some(&self.range)
    }
    fn span_ref_fallback(&self, fallback: Range<usize>) -> &Range<usize> {
        &self.range
    }
}
impl<T: SpanRef> Span for Vec<T> {
    fn span(&self) -> Option<Range<usize>> {
        Some(self.first()?.span_ref()?.start..self.last()?.span_ref()?.end)
    }
    fn span_fallback(&self, fallback: Range<usize>) -> Range<usize> {
        self.span().unwrap_or(fallback)
    }
}
impl<T: SpanRef> From<Vec<T>> for Spanned<Vec<T>> {
    fn from(value: Vec<T>) -> Self {
        let span = value.span().unwrap_or(0..0);
        Spanned::new(span, value)
    }
}
