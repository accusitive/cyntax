use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
        Chars, DisplayStyle,
    },
};
use lexer::Lexer;
use location::LocationHistory;
use parser::Parser;
use peekmore::PeekMore;

pub mod lexer;
pub mod location;
pub mod parser;
pub mod preprocess;
fn main() {
    let source = std::fs::read_to_string("./input.c").unwrap();
    let source = source.as_str();
    let mut lexer = Lexer::from(source);

    let lexer_tokens = lexer.tokenize().iter().cloned().map(|tok| tok.double(0)).collect::<Vec<_>>();
    dbg!(&lexer_tokens);
    let mut pp = preprocess::Preprocessor::new();
    let groups = pp.create_token_stretches(&mut lexer_tokens.iter().peekmore()).unwrap();
    dbg!(&groups);
    let groups = pp.parse_all_groups(&mut groups.iter().peekmore());
    dbg!(&groups);
    let mut expanded_tokens = vec![];
    for group in &groups {
        expanded_tokens.extend(pp.expand_group(group));
    }
    dbg!(&expanded_tokens);

    // dbg!(&pp.expand_group(group));

    // let global = pp.parse_global(&mut groups.iter().peekmore()).unwrap();
    // dbg!(&global);
    // let unit = &pp.parse_translation_unit(&mut lexer_tokens.iter().peekmore()).unwrap();
    // dbg!(&unit);
    // let s = pp.s(&unit);
    // println!("{}", s);
    // println!("{}", &s);
    // // let source = include_str!("../input.c");
    // let source = std::fs::read_to_string("./input.c").unwrap();
    // let source = source.as_str();
    // let mut lexer = Lexer::from(source);
    // let tokens = lexer.tokenize();

    // let mut files = SimpleFiles::new();
    // let f = files.add("tcc.c".to_string(), source.to_string());

    // for token in &tokens {
    //     let d = Diagnostic::note()
    //         .with_message(format!("Token {:?}", token))
    //         .with_labels(vec![Label::primary(f, token.start.offset..token.end.offset)]);
    //     let writer: StandardStream = StandardStream::stderr(ColorChoice::Always);
    //     let config = codespan_reporting::term::Config::default();
    //     term::emit(&mut writer.lock(), &config, &files, &d).unwrap();
    // }

    // let mut pp = PreProcessor {
    //     files,
    //     file_id: f,
    //     macros: PreProcessor::default_macros(),
    //     cursor: LocationHistory::x(()),
    //     conditions: vec![],
    //     ignore_conditions: false,
    // };

    // let preprocessed_tokens = pp.process(tokens);
    // preprocess::print::print(&preprocessed_tokens);
    // for token in &preprocessed_tokens {
    //     let d = Diagnostic::note().with_message(format!("Token {:?}", token)).with_labels(token.generate_location_labels());
    //     // .with_labels(vec![Label::primary(f, token.location.start.offset..token.location.start.offset)]);
    //     let writer: StandardStream = StandardStream::stderr(ColorChoice::Always);
    //     let config = codespan_reporting::term::Config::default();
    //     term::emit(&mut writer.lock(), &config, &pp.files, &d).unwrap();
    // }

    // assert!(pp.conditions.len() == 0);
    // // dbg!(&preprocessed_tokens);

    // let mut parser = Parser {
    //     tokens: preprocessed_tokens.iter().peekmore(),
    //     files: pp.files.clone(),
    //     symbol_stack: Parser::default_symbol_stack(),
    //     location: LocationHistory::x(()),
    // };

    // let unit = &parser.parse();
    // // assert!(parser.symbol_stack.len() == 1);

    // for declaration in &unit.declarations {
    //     // if declaration.location.file_id != f {
    //     //     continue;
    //     // }
    //     let writer: StandardStream = StandardStream::stderr(ColorChoice::Always);
    //     let mut config = codespan_reporting::term::Config::default();
    //     config.display_style = DisplayStyle::Rich;
    //     config.chars = Chars::ascii();
    //     let diagnostic = Diagnostic::note().with_message("decl").with_labels(vec![
    //         Label::primary(declaration.location.file_id, declaration.location_range()).with_message("declaration span"),
    //         // Label::secondary(declaration.value.declarator.location.file_id, declaration.value.declarator.location_range()).with_message("declarator span"),
    //     ]);

    //     // term::emit(&mut writer.lock(), &config, &pp.files, &diagnostic).unwrap();
    //     // println!("{:#?}", declaration);
    // }
    // println!("{}", unit.declarations.len());
}
