use std::ops::Range;

#[derive(Debug)]
pub struct Spanned<T> {
    pub value: T,
    pub range: Range<usize>
}

impl<T> Spanned<T> {
    pub fn new(range: Range<usize>, value: T) -> Spanned<T>{
        Spanned { value: value, range }
    }
}
impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}