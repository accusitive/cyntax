use std::ops::Range;

use cyntax_common::{ast::Token, spanned::Spanned};

use crate::{Diagnostic, DiagnosticSeverity, Label};
#[derive(Debug)]

pub struct UnmatchedDelimiter {
    pub opening_delimiter_location: Range<usize>,
    pub potential_closing_delimiter_location: usize,
    pub closing_delimiter: char,
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
                range: self.potential_closing_delimiter_location
                    ..self.potential_closing_delimiter_location,
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

pub struct DanglingEndif(Spanned<Token>);