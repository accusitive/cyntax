use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Spanned<T: Clone> {
    pub value: T,
    pub range: Range<usize>,
}

impl<T: Clone> Spanned<T> {
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
