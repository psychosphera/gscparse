use gscparse::parse_script;

fn main() {
    let source = std::fs::read_to_string("./utility.gsc").unwrap();
    let source = comment_strip::strip_comments(source, comment_strip::CommentStyle::C, true).unwrap();
    let source = source.chars().filter(|c| *c != '\t').collect::<String>();
    //dbg!(&source);

    ////dbg!(parse_expression::<()>(test).unwrap());
    ////dbg!(parse_statement::<()>(test).unwrap());
    ////dbg!(parse_function_def::<()>(test).unwrap());
    dbg!(parse_script::<()>(&source).unwrap());
}
