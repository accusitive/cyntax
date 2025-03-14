/// TU Contains groups
struct TranslationUnit {
    groups: Vec<Group>
}
/// Groups contain their kind, and their children, example:
/// -----------Group
/// | #ifdef a  -- GroupKind::If(IfGroupKind::IfDef)
/// | int a = 5 -- lots of GroupChild::Token(..) 
/// | #endif
/// ----------
struct Group {
    kind: GroupKind,
    children: Vec<GroupChild>
}
enum GroupChild {
    Group(Group),
    Directive,
    Token(PreprocessingToken)
}
enum GroupKind {
    If(IfGroupKind),
    Else(Expression),
    Elif(IfGroupKind, Expression),
}
enum IfGroupKind {
    Ifdef,
    Ifndef,
    Constant(Expression)
}