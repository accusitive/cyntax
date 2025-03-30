use std::ops::Range;

#[derive(Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub range: Range<usize>,
}

impl<T> Spanned<T> {
    pub fn new(range: Range<usize>, value: T) -> Spanned<T> {
        Spanned {
            value: value,
            range,
        }
    }
    pub fn augment<U, F: Fn(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            range: self.range.start..self.range.end,
            value: f(self.value),
        }
    }
}
impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
