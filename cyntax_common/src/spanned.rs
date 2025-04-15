use std::ops::Range;

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    pub range: Range<usize>,
    pub file_id: usize
}
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub value: T,
    pub location: Location
}

impl Location {
    pub fn new() -> Self {
        Self {
            range: 0..0,
            file_id: 0,
        }
    }
    pub fn until(&self, other: &Self) -> Self {
        assert_eq!(self.file_id, other.file_id);
        Self {
            range: self.range.start..other.range.end,
            file_id: self.file_id,
        }
    }
    pub fn as_fallback_for_vec<T>(&self, other: &Vec<Spanned<T>>) -> Self {
        let first = other.first().map(|s|s.location.clone()).unwrap_or(self.clone());
        let last = other.first().map(|s|s.location.clone()).unwrap_or(self.clone());

        first.until(&last)
    }
}

impl<T> Spanned<T> {
    pub fn new(location: Location, value: T) -> Spanned<T> {
        Spanned { value: value, location }
    }
    pub fn map<U: Clone, F: FnMut(T) -> U>(self, mut f: F) -> Spanned<U> {
        Spanned {
            location: self.location.clone(),
            value: f(self.value),
        }
    }
    pub fn map_ref<U: Clone, F: Fn(&T) -> U>(&self, f: F) -> Spanned<U> {
        Spanned {
            location: self.location.clone(),
            value: f(&self.value),
        }
    }
    pub fn with_offset(self, offset: usize) -> Spanned<T> {
        Spanned {
            value: self.value,
            location: Location { range: (self.location.range.start + offset)..(self.location.range.end + offset), file_id: self.location.file_id }
            // location: 
        }
    }
    pub fn start(&self) -> usize {
        self.location.range.start
    }
    pub fn end(&self) -> usize {
        self.location.range.end
    }
}
impl<T: PartialEq + Clone> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
// pub trait SpanRef {
//     fn span_ref(&self) -> Option<&location<usize>>;
//     fn span_ref_fallback(&self, fallback: location<usize>) -> &location<usize>;
// }
// pub trait Span {
//     fn span(&self) -> Option<location<usize>>;
//     fn span_fallback(&self, fallback: location<usize>) -> location<usize>;
// }

// impl<T> SpanRef for Spanned<T> {
//     fn span_ref(&self) -> Option<&location<usize>> {
//         Some(&self.location)
//     }
//     fn span_ref_fallback(&self, fallback: location<usize>) -> &location<usize> {
//         &self.location
//     }
// }
// impl<T: SpanRef> Span for Vec<T> {
//     fn span(&self) -> Option<location<usize>> {
//         Some(self.first()?.span_ref()?.start..self.last()?.span_ref()?.end)
//     }
//     fn span_fallback(&self, fallback: location<usize>) -> location<usize> {
//         self.span().unwrap_or(fallback)
//     }
// }
// impl<T: SpanRef> From<Vec<T>> for Spanned<Vec<T>> {
//     fn from(value: Vec<T>) -> Self {
//         let span = value.span().unwrap_or(0..0);
//         Spanned::new(span, value)
//     }
// }
