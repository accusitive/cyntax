use std::ops::Range;

use cyntax_common::{ast::Token, spanned::Spanned};

use crate::{Diagnostic, DiagnosticSeverity, Label};
#[derive(Debug)]

pub struct UnmatchedDelimiter {
    pub opening_delimiter_location: Range<usize>,
    pub potential_closing_delimiter_location: usize,
    pub closing_delimiter: String,
}
impl Diagnostic for UnmatchedDelimiter {
    fn title<'a>(&self) -> &'a str {
        "Unmatched delimiter"
    }
    fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::Error
    }
    fn labels(&self) -> Vec<crate::Label> {
        vec![
            Label {
                kind: crate::LabelKind::Primary,
                range: self.opening_delimiter_location.start..self.opening_delimiter_location.end,
                message: "Unmatched delimiter".to_string(),
            },
            Label {
                kind: crate::LabelKind::Secondary,
                range: self.potential_closing_delimiter_location..self.potential_closing_delimiter_location,
                message: "Potential location for a closing delimiter".to_string(),
            },
        ]
    }
}
#[derive(Debug)]
pub struct UnterminatedTreeNode {
    pub opening_token: Range<usize>,
}
impl Diagnostic for UnterminatedTreeNode {
    fn title<'a>(&self) -> &'a str {
        "Unterminated tree node"
    }
    fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::Error
    }
    fn labels(&self) -> Vec<crate::Label> {
        vec![Label {
            kind: crate::LabelKind::Primary,
            range: self.opening_token.start..self.opening_token.end,
            message: "No closer for this group".to_string(),
        }]
    }
}

pub struct UnknownDirective(pub Range<usize>);
impl Diagnostic for UnknownDirective {
    fn title<'a>(&self) -> &'a str {
        "Unknown directive"
    }

    fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::Error
    }
    fn labels(&self) -> Vec<Label> {
        vec![Label {
            kind: crate::LabelKind::Primary,
            range: self.0.start..self.0.end,
            message: "".to_string(),
        }]
    }
}

pub struct DanglingEndif(pub Range<usize>);
impl Diagnostic for DanglingEndif {
    fn title<'a>(&self) -> &'a str {
        "Dangling end if"
    }

    fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::Error
    }
    fn labels(&self) -> Vec<Label> {
        vec![Label {
            kind: crate::LabelKind::Primary,
            range: self.0.start..self.0.end,
            message: "This endif directive has no matching opening directive".to_string(),
        }]
    }
}
pub struct ExpectedButFound {
    pub location: Range<usize>,
    pub expected: String,
    pub found: String,
}
impl Diagnostic for ExpectedButFound {
    fn title<'a>(&self) -> &'a str {
        "Encountered wrong token"
    }

    fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::Error
    }
    fn labels(&self) -> Vec<Label> {
        vec![
            Label {
                kind: crate::LabelKind::Primary,
                range: self.location.start..self.location.end,
                message: self.expected.clone(),
            },
            Label {
                kind: crate::LabelKind::Secondary,
                range: self.location.start..self.location.end,
                message: self.found.clone(),
            },
        ]
    }
}

pub struct ErrorDirective(pub Range<usize>, pub Option<Spanned<Token>>);

impl Diagnostic for ErrorDirective {
    fn title<'a>(&self) -> &'a str {
        "Encountered error directive"
    }

    fn severity(&self) -> DiagnosticSeverity {
        DiagnosticSeverity::Error
    }
    fn labels(&self) -> Vec<Label> {
        let msg = match &self.1 {
            Some(msg) => Some(Label {
                kind: crate::LabelKind::Secondary,
                range: msg.range.clone(),
                message: "Error message provided".into()
            }),
            None => None,
        };
        match msg {
            Some(msg_label) => {
                vec![
                    Label {
                        kind: crate::LabelKind::Primary,
                        range: self.0.clone(),
                        message: "".into(),
                    },
                    msg_label,
                ]
            }
            None => {
                vec![Label {
                    kind: crate::LabelKind::Primary,
                    range: self.0.clone(),
                    message: "".into(),
                }]
            }
        }
    }
}
