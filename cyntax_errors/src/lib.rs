use std::ops::Range;

pub use cab_why as why;
use cab_why::Report;
use codespan_reporting::{files::SimpleFiles, term::termcolor::Ansi};
pub enum DiagnosticSeverity {
    Error,
    Warning,
}
pub enum LabelKind {
    Primary,
    Secondary,
    Help,
}
pub struct Label {
    kind: LabelKind,
    range: Range<usize>,
    // file_id: usize,
    message: String,
}
pub trait Diagnostic: Sized {
    fn title<'a>(&self) -> &'a str;
    fn severity(&self) -> DiagnosticSeverity;
    fn labels(&self) -> Vec<Label> {
        vec![]
    }
    fn into_why_report(&self) -> cab_why::Report {
        let severity = match self.severity() {
            DiagnosticSeverity::Error => cab_why::ReportSeverity::Error,
            DiagnosticSeverity::Warning => cab_why::ReportSeverity::Warn,
        };
        let mut report = cab_why::Report::new(severity, self.title());
        for label in self.labels() {
            let label_severity = match label.kind {
                LabelKind::Primary => cab_why::LabelSeverity::Primary,
                LabelKind::Secondary => cab_why::LabelSeverity::Secondary,
                LabelKind::Help => todo!(),
            };

            report.push_label(cab_why::Label {
                span: label.range.into(),
                severity: label_severity,
                text: label.message.into(),
            });
        }
        report
    }
    fn into_codespan_report(&self) -> codespan_reporting::diagnostic::Diagnostic<usize> {
        let diag = match self.severity() {
            DiagnosticSeverity::Error => codespan_reporting::diagnostic::Diagnostic::error(),
            DiagnosticSeverity::Warning => codespan_reporting::diagnostic::Diagnostic::warning(),
        };
        let mut diag = diag.with_message(self.title());
        for label in self.labels() {
            let style = match label.kind {
                LabelKind::Primary => codespan_reporting::diagnostic::LabelStyle::Primary,
                LabelKind::Secondary => codespan_reporting::diagnostic::LabelStyle::Secondary,
                LabelKind::Help => unimplemented!(),
            };
            diag.labels.push(codespan_reporting::diagnostic::Label::new(style, 0, label.range).with_message(label.message));
        }

        diag
    }
}

pub fn write_codespan_report(diag: codespan_reporting::diagnostic::Diagnostic<usize>, file_name: &str, file_source: &str) -> String {
    let config = codespan_reporting::term::Config::default();
    let mut output_buffer = Vec::new();
    let mut ansi_writer = Ansi::new(&mut output_buffer);
    let mut files = SimpleFiles::new();
    files.add(file_name, file_source);

    codespan_reporting::term::emit(&mut ansi_writer, &config, &files, &diag).unwrap();

    String::from_utf8(output_buffer).unwrap()
}

pub mod errors;
pub trait UnwrapDiagnostic<T> {
    fn unwrap_diagnostic(self, file_name: &str, file_source: &str) -> T;
}
impl<T> UnwrapDiagnostic<T> for Result<T, Report> {
    fn unwrap_diagnostic(self, file_name: &str, file_source: &str) -> T {
        match self {
            Ok(value) => value,
            Err(e) => panic!("{}", e.with(file_name, file_source)),
        }
    }
}
