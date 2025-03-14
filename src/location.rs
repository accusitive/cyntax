use std::{fmt::{Debug, Display}, ops::{Deref, Range}};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

#[derive(Debug, Clone)]
pub struct Located<T> {
    pub(crate) value: T,
    // TODO rework this to use Region
    pub(crate) start: Location,
    pub(crate) end: Location,
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Location {
    // Human facing line (ie. starts at 1)
    pub(crate) line: usize,
    // Column within it's line
    pub(crate) col: usize,
    // byte wise offset from start of source string
    pub(crate) offset: usize,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Region {
    pub(crate) start: Location,
    pub(crate) end: Location,
    pub file_id: usize,
}

/// A wrapper for data that contains information about where it appeared
#[derive(Clone)]
pub struct LocationHistory<T> {
    pub(crate) value: T,
    /// The location of the macro that corresponded to this token
    pub origin: Vec<Region>,
    /// The location of the invocation of the macro that created this token
    pub location: Region,
}
impl<T> Into<Range<usize>> for Located<T> {
    fn into(self) -> Range<usize> {
        self.start.offset..self.end.offset
    }
}
impl<T: Eq> PartialEq for Located<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl<T: Eq> PartialEq for LocationHistory<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}
impl<T: Eq> Eq for LocationHistory<T> {}
impl Location {
    pub(crate) fn located_until<T>(&self, end: Self, value: T) -> Located<T> {
        Located { value, start: self.clone(), end }
    }
}

impl Location {
    #[must_use]
    pub(crate) fn bump(&self, n: usize) -> Self {
        Self {
            line: self.line,
            col: self.col + n,
            offset: self.offset + n,
        }
    }
    #[must_use]
    pub(crate) fn newline(&self) -> Self {
        Self {
            line: self.line + 1,
            col: 0,
            offset: self.offset + 1,
        }
    }
}

impl<T> Located<T> {
    pub fn same<V>(&self, v: V) -> Located<V> {
        Located::<V> {
            value: v,
            start: self.start,
            end: self.end,
        }
    }
    pub fn shell(&self) -> Located<()> {
        Located {
            value: (),
            start: self.start,
            end: self.end,
        }
    }
    pub fn doubled<V>(&self, file_id: usize, v: V) -> LocationHistory<V> {
        LocationHistory::<V> {
            value: v,
            location: Region {
                start: self.start,
                end: self.end,
                file_id,
            },
            origin: vec![],
        }
    }
    pub fn double(self, file_id: usize) -> LocationHistory<T> {
        LocationHistory::<T> {
            value: self.value,
            location: Region {
                start: self.start,
                end: self.end,
                file_id,
            },
            origin: vec![],
        }
    }
}

impl<T> LocationHistory<T> {
    pub fn same<V>(&self, v: V) -> LocationHistory<V> {
        LocationHistory::<V> {
            value: v,
            location: self.location.clone(),
            origin: self.origin.clone(),
        }
    }

    pub fn shell(&self) -> LocationHistory<()> {
        LocationHistory::<()> {
            value: (),
            location: self.location.clone(),
            origin: self.origin.clone(),
        }
    }
    pub fn until<V>(self, other: &LocationHistory<V>) -> LocationHistory<T> {
        LocationHistory {
            value: self.value,
            origin: other.origin.clone(),
            location: Region {
                start: self.location.start,
                end: other.location.end,
                file_id: self.location.file_id,
            },
        }
    }
    pub fn until_last<V>(self, other: &[LocationHistory<V>]) -> LocationHistory<T> {
        let other_location = other.last().map(|o| &o.location).unwrap_or(&self.location);
        let other_origin = other.last().map(|o| &o.origin).unwrap_or(&self.origin);

        LocationHistory {
            value: self.value,
            origin: other_origin.clone(),
            location: Region {
                start: self.location.start,
                end: other_location.end,
                file_id: self.location.file_id,
            },
        }
    }
    /// Used for testing. a: LocationHistory<T> == b: LocationHistory<V> is the same as `a == b`, the location information is not considered
    pub fn x(v: T) -> LocationHistory<T> {
        LocationHistory {
            value: v,
            origin: vec![Region {
                start: Location { line: 0, col: 0, offset: 0 },
                end: Location { line: 0, col: 0, offset: 0 },
                file_id: 0,
            }],
            location: Region {
                start: Location { line: 0, col: 0, offset: 0 },
                end: Location { line: 0, col: 0, offset: 0 },
                file_id: 0,
            },
        }
    }
    pub fn location_range(&self) -> Range<usize> {
        self.location.start.offset..self.location.end.offset
    }
    pub fn file_id(&self) -> usize {
        // assert_eq!(self.location.file_id, self.origin.file_id);
        self.location.file_id
    }
    pub fn generate_location_labels(&self) -> Vec<codespan_reporting::diagnostic::Label<usize>> {
        let mut labels = vec![];
        labels.push(Label::primary(self.location.file_id, self.location_range()));
        let mut depth = 0;
        for region in self.origin.iter().rev() {
            let message = if depth > 0 {
                "which also originates from a macro expansion"
            } else {
                "originates from a macro expansion"
            };
            labels.push(Label::secondary(region.file_id, region.start.offset..region.end.offset).with_message(message));
            depth += 1;
        }

        labels
    }
}
impl<T: Debug> Debug for LocationHistory<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}

impl<T> LocationHistory<Option<T>> {
    pub fn unwrap_diag(self, files: &SimpleFiles<String, String>) -> T {
        match self.value {
            Some(val) => val,
            None => {
                let d = Diagnostic::<usize>::bug().with_message("Tried to unwrap None").with_labels(self.generate_location_labels());
                let writer: StandardStream = StandardStream::stderr(ColorChoice::Always);
                let config = codespan_reporting::term::Config::default();
                term::emit(&mut writer.lock(), &config, files, &d).unwrap();

                panic!();
            }
        }
    }
}

impl<T> Deref for LocationHistory<T> {
    type Target =T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}
impl<T: Display> Display for LocationHistory<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}