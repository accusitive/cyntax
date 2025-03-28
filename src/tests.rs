use super::*;

#[test]
fn test_trigraphs() {
    let input = "??=";
    let mut iter = SkipEscapedNewlinesIter::new(input);
    assert_eq!(iter.next(), Some((0..3, '#')));

    let input = "Hello??(World";
    let mut iter = SkipEscapedNewlinesIter::new(input);
    assert_eq!(iter.next(), Some((0..1, 'H')));
    assert_eq!(iter.next(), Some((1..2, 'e')));
    assert_eq!(iter.next(), Some((2..3, 'l')));
    assert_eq!(iter.next(), Some((3..4, 'l')));
    assert_eq!(iter.next(), Some((4..5, 'o')));
    assert_eq!(iter.next(), Some((5..8, '[')));
    assert_eq!(iter.next(), Some((8..9, 'W')));
    assert_eq!(iter.next(), Some((9..10, 'o')));
    assert_eq!(iter.next(), Some((10..11, 'r')));
    assert_eq!(iter.next(), Some((11..12, 'l')));
    assert_eq!(iter.next(), Some((12..13, 'd')));
}

#[test]
fn test_newlines() {
    let input = "test\\\nX";
    let mut iter = SkipEscapedNewlinesIter::new(input);
    assert_eq!(iter.next(), Some((0..1, 't')));
    assert_eq!(iter.next(), Some((1..2, 'e')));
    assert_eq!(iter.next(), Some((2..3, 's')));
    assert_eq!(iter.next(), Some((3..4, 't')));
    assert_eq!(iter.next(), Some((6..7, 'X')));
}


use super::*;

#[test]
fn test_trigraph_edge_cases() {
    let input = "??-??/??'";
    let mut iter = SkipEscapedNewlinesIter::new(input);
    assert_eq!(iter.next(), Some((0..3, '~')));
    assert_eq!(iter.next(), Some((3..6, '\\')));
    assert_eq!(iter.next(), Some((6..9, '^')));
}

#[test]
fn test_newline_edge_cases() {
    let input = "\\\n\\\nA";
    let mut iter = SkipEscapedNewlinesIter::new(input);
    assert_eq!(iter.next(), Some((4..5, 'A')));
}

#[test]
fn test_mixed_trigraphs_and_newlines() {
    let input = "??<test\\\n??>ing";
    let mut iter = SkipEscapedNewlinesIter::new(input);
    assert_eq!(iter.next(), Some((0..3, '{')));
    assert_eq!(iter.next(), Some((3..4, 't')));
    assert_eq!(iter.next(), Some((4..5, 'e')));
    assert_eq!(iter.next(), Some((5..6, 's')));
    assert_eq!(iter.next(), Some((6..7, 't')));
    assert_eq!(iter.next(), Some((10..13, '}')));
    assert_eq!(iter.next(), Some((13..14, 'i')));
    assert_eq!(iter.next(), Some((14..15, 'n')));
    assert_eq!(iter.next(), Some((15..16, 'g')));
}

#[test]
fn test_escaped_newline_with_trigraph() {
    let input = "Hello\\\n??=world";
    let mut iter = SkipEscapedNewlinesIter::new(input);
    assert_eq!(iter.next(), Some((0..1, 'H')));
    assert_eq!(iter.next(), Some((1..2, 'e')));
    assert_eq!(iter.next(), Some((2..3, 'l')));
    assert_eq!(iter.next(), Some((3..4, 'l')));
    assert_eq!(iter.next(), Some((4..5, 'o')));
    assert_eq!(iter.next(), Some((7..10, '#')));
    assert_eq!(iter.next(), Some((10..11, 'w')));
    assert_eq!(iter.next(), Some((11..12, 'o')));
    assert_eq!(iter.next(), Some((12..13, 'r')));
    assert_eq!(iter.next(), Some((13..14, 'l')));
    assert_eq!(iter.next(), Some((14..15, 'd')));
}
