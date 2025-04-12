#[cfg(test)]
mod tests {
    use cyntax_common::ast::{Punctuator, Token};
    use cyntax_lexer::lexer::Lexer;

    use crate::Preprocessor;
    fn test_helper(source: &str) -> Vec<Token> {
        let tokens = Lexer::new("test.c", source).collect::<Vec<_>>();
        let pp = Preprocessor::new("test.c", source, &tokens).expand();
        let despanned = pp.into_iter().map(|token| token.value).filter(|token| !matches!(token, Token::Whitespace(_))).collect::<Vec<_>>();

        despanned
    }
    macro_rules! pp {
        ($s: literal) => {
            Token::PPNumber($s.to_string())
        };
    }
    macro_rules! id {
        ($s: literal) => {
            Token::Identifier($s.to_string())
        };
    }
    macro_rules! bi {
        ($s: literal) => {
            Token::BlueIdentifier($s.to_string())
        };
    }
    macro_rules! lp {
        () => {
            Token::Punctuator(Punctuator::LeftParen)
        };
    }
    macro_rules! rp {
        () => {
            Token::Punctuator(Punctuator::RightParen)
        };
    }
    macro_rules! plus {
        () => {
            Token::Punctuator(Punctuator::Plus)
        };
    }
    #[test]
    fn test_01() {
        let toks = test_helper(include_str!("../tests/01.c"));
        assert_eq!(toks, vec![pp!("5")]);
    }
    #[test]
    fn test_02() {
        let toks = test_helper(include_str!("../tests/02.c"));
        assert_eq!(toks, vec![bi!("a")]);
    }
    #[test]
    fn test_03() {
        let toks = test_helper(include_str!("../tests/03.c"));
        assert_eq!(toks, vec![bi!("X")]);
    }
    #[test]
    fn test_04() {
        let toks = test_helper(include_str!("../tests/04.c"));
        assert_eq!(toks, vec![lp!(), lp!(), pp!("5"), rp!(), plus!(), lp!(), pp!("5"), rp!(), rp!()]);
    }
}
