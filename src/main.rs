#![allow(dead_code)]

use std::usize;

use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_till, take_until};
use nom::bytes::streaming::is_not;
use nom::character::complete::{digit1, hex_digit1, multispace0, oct_digit1};
use nom::character::streaming::{char, multispace1};
use nom::combinator::{map, opt, value, verify};
use nom::error::{ErrorKind, ParseError};
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, preceded};
use nom::{AsChar, Err, IResult, InputTakeAtPosition, Parser};

/// Parse an escaped character: \n, \t, \r, \u{00AC}, etc.
fn parse_escaped_char<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
where
  E: ParseError<&'a str>
{
  preceded(
    char('\\'),
    // `alt` tries each parser in sequence, returning the result of
    // the first successful match
    alt((
      // The `value` parser returns a fixed value (the first argument) if its
      // parser (the second argument) succeeds. In these cases, it looks for
      // the marker characters (n, r, t, etc) and returns the matching
      // character (\n, \r, \t, etc).
      value('\n', char('n')),
      value('\r', char('r')),
      value('\t', char('t')),
      value('\\', char('\\')),
      value('/', char('/')),
      value('"', char('"')),
    )),
  )
  .parse(input)
}

/// Parse a backslash, followed by any amount of whitespace. This is used later
/// to discard any escaped whitespace.
fn parse_escaped_whitespace<'a, E: ParseError<&'a str>>(
  input: &'a str,
) -> IResult<&'a str, &'a str, E> {
  preceded(char('\\'), multispace1).parse(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
fn parse_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
  // `is_not` parses a string of 0 or more characters that aren't one of the
  // given characters.
  let not_quote_slash = is_not("\"\\");

  // `verify` runs a parser, then runs a verification function on the output of
  // the parser. The verification function accepts out output only if it
  // returns true. In this case, we want to ensure that the output of is_not
  // is non-empty.
  verify(not_quote_slash, |s: &str| !s.is_empty()).parse(input)
}

/// A string fragment contains a fragment of a string being parsed: either
/// a non-empty Literal (a series of non-escaped characters), a single
/// parsed escaped character, or a block of escaped whitespace.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum StringFragment<'a> {
  Literal(&'a str),
  EscapedChar(char),
  EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
fn parse_fragment<'a, E>(input: &'a str) -> IResult<&'a str, StringFragment<'a>, E>
where
  E: ParseError<&'a str>
{
  alt((
    // The `map` combinator runs a parser, then applies a function to the output
    // of that parser.
    map(parse_literal, StringFragment::Literal),
    map(parse_escaped_char, StringFragment::EscapedChar),
    value(StringFragment::EscapedWS, parse_escaped_whitespace),
  ))
  .parse(input)
}

/// Parse a string. Use a loop of parse_fragment and push all of the fragments
/// into an output string.
fn parse_string_literal<'a, E>(input: &'a str) -> IResult<&'a str, String, E>
where
  E: ParseError<&'a str>,
{
  // fold is the equivalent of iterator::fold. It runs a parser in a loop,
  // and for each output value, calls a folding function on each output value.
  let build_string = fold_many0(
    // Our parser function – parses a single string fragment
    parse_fragment,
    // Our init value, an empty string
    String::new,
    // Our folding function. For each fragment, append the fragment to the
    // string.
    |mut string, fragment| {
      match fragment {
        StringFragment::Literal(s) => string.push_str(s),
        StringFragment::EscapedChar(c) => string.push(c),
        StringFragment::EscapedWS => {}
      }
      string
    },
  );

  // Finally, parse the string. Note that, if `build_string` could accept a raw
  // " character, the closing delimiter " would never match. When using
  // `delimited` with a looping parser (like fold), be sure that the
  // loop won't accidentally match your closing delimiter!
  delimited(char('"'), build_string, char('"')).parse(input)
}

fn parse_hex_literal<'a, E>(input: &'a str) -> IResult<&'a str, u32, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = tag("0x")(input)?;
    let (input, hex) = hex_digit1(input)?;

    Ok((input, u32::from_str_radix(hex, 16).unwrap()))
}

fn parse_oct_literal<'a, E>(input: &'a str) -> IResult<&'a str, u32, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = char('0')(input)?;
    let (input, hex) = oct_digit1(input)?;

    Ok((input, u32::from_str_radix(hex, 8).unwrap()))
}

fn parse_dec_literal<'a, E>(input: &'a str) -> IResult<&'a str, u32, E>
where
  E: ParseError<&'a str>,
{
    let (input, hex) = digit1(input)?;

    Ok((input, u32::from_str_radix(hex, 10).unwrap()))
}

fn parse_int_literal<'a, E>(input: &'a str) -> IResult<&'a str, u32, E>
where
  E: ParseError<&'a str>,
{
    alt((parse_hex_literal, parse_oct_literal, parse_dec_literal)).parse(input)
}

fn take_until_balanced<'a, E>(
    begin: impl AsRef<str> + 'a,
    end: impl AsRef<str> + 'a,
) -> impl Fn(&'a str) -> IResult<&'a str, &'a str, E>
where
    E: ParseError<&'a str>,
{
    move |input: &'a str| {
        let begin = begin.as_ref();
        let end = end.as_ref();
        if begin == end || !input.starts_with(begin) {
            return Err(Err::Error(E::from_error_kind(input, ErrorKind::TakeUntil)));
        }
        
        let mut index = 0;
        let mut nesting = 0;
        loop {
            let next_begin = input[index..].find(begin).unwrap_or(usize::MAX);
            let next_end = input[index..].find(end).unwrap_or(usize::MAX);

            if next_begin == usize::MAX && next_end == usize::MAX {
                if nesting == 0 {
                    return Ok((&input[index..], &input[0..index]));
                } else {
                    return Err(Err::Error(E::from_error_kind(input, ErrorKind::TakeUntil)));
                }
            }
            
            if next_begin < next_end {
                nesting += 1;
                index += next_begin + begin.len();
            } else {
                nesting -= 1;
                index += next_end + end.len();

                if nesting == 0 {
                    return Ok((&input[index..], &input[..index]));
                }
            }
        }
    }
}

fn parse_comma_separated_list_item<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
  E: ParseError<&'a str>,
{
    if input.is_empty() {
        return Err(Err::Error(E::from_error_kind(input, ErrorKind::TakeUntil)));
    }
    let (input, _) = multispace0(input)?;
    let (input, s) = opt(take_until(","))(input)?;
    let (input, s) = if let Some(s) = s {
        (input, s)
    } else {
        ("", input)
    };
    // take the comma, unless it's the last arg so there isn't one
    let (input, _) = opt(take(1_usize))(input)?;
    // take whatever whitespace follows the comma too
    let (input, _) = multispace0(input)?;
    Ok((input, s))
}

fn parse_comma_separated_list<'a, E>(input: &'a str) -> IResult<&'a str, Vec<&'a str>, E>
where
  E: ParseError<&'a str>,
{
    let (input, v) = many0(parse_comma_separated_list_item)(input)?;
    Ok((input, v))
}

static RESERVED_KEYWORDS: [&str; 9] = [
    "if",
    "for",
    "while",
    "return",
    "break",
    "continue",
    "undefined",
    "true",
    "false",
];

#[derive(Clone, Debug)]
#[repr(transparent)]
struct Ident(String);

impl Ident {
    fn try_from_str(s: impl AsRef<str>) -> Option<Self> {
        let s = s.as_ref();

        // valid idents can't be empty
        if s.is_empty() {
            return None;
        } 

        if RESERVED_KEYWORDS.contains(&s) {
            return None;
        }
        
        let mut chars = s.chars();
        let first = chars.next().unwrap();

        // valid idents can only start with a letter or underscore
        if !first.is_alphabetic() && first != '_' && first != ':' {
            return None;
        }
        
        // the rest of a valid ident can have letters, numbers, underscores, and colons
        if !chars.all(|c| c.is_ascii_alphanumeric() || c == '_' || c == ':') {
            return None;
        }

        // but it can't only consist of underscores
        if s.chars().all(|c| c == '_') {
            return None;
        }

        Some(Self(s.to_string()))
    }
}

fn underscore1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: InputTakeAtPosition,
  <T as InputTakeAtPosition>::Item: AsChar,
{
    input.split_at_position1_complete(|item| item.as_char() == '_', ErrorKind::AlphaNumeric)
}

fn ident1<T, E: ParseError<T>>(input: T) -> IResult<T, T, E>
where
  T: InputTakeAtPosition,
  <T as InputTakeAtPosition>::Item: AsChar + Clone,
{
    input.split_at_position1_complete(|item| item.clone().as_char() == '_' || item.as_char().is_ascii_alphanumeric(), ErrorKind::AlphaNumeric)
}

fn parse_ident<'a, E>(input: &'a str) -> IResult<&'a str, Ident, E>
where
  E: ParseError<&'a str>,
{
    let (input, ident) = take_till(|c: char| c != ':' && c != '_' && !c.is_ascii_alphanumeric())(input)?;
    let Some(ident) = Ident::try_from_str(ident) else {
        return Err(Err::Error(E::from_error_kind(ident, ErrorKind::AlphaNumeric)));
    };

    Ok((input, ident))
}

#[derive(Clone, Debug)]
enum Value {
    Name(Ident),
    IntLiteral(u32),
    StringLiteral(String),
    MemberAccess {
        parent: Box<Expression>,
        child: Box<Expression>,
    },
    ArrayMember {
        name: Ident,
        index: Box<Expression>,
    },
    ArrayList(Vec<Expression>),
    Undefined,
    Bool(bool),
}

fn parse_value<'a, E>(input: &'a str) -> IResult<&'a str, Value, E>
where
  E: ParseError<&'a str>,
{
    if let Ok((input, i)) = parse_int_literal::<()>(input) {
        return Ok((input, Value::IntLiteral(i)));
    }

    if input.starts_with("[") {
        if input.starts_with("[]") {
            let (input, _) = take("[]".len())(input)?;
            return Ok((input, Value::ArrayList(vec![])));
        }
        let (input, list) = take_until_balanced("[", "]")(input)?;
        let (list, _) = take(1_usize)(list)?;
        let (_, values) = parse_comma_separated_list(list)?;
        let values = values.into_iter().map(|v| parse_expression(v).map(|(_, v)| v)).collect::<Result<Vec<_>, _>>()?;
        return Ok((input, Value::ArrayList(values)));
    }

    if input.contains(".") && input.find(".").unwrap() < input.find("[").unwrap_or(usize::MAX) {
        let (input, slhs) = take_until(".")(input)?;
        let slhs = slhs.trim();
        let (input, _) = take(1_usize)(input)?;
        let (input, _) = multispace0(input)?;

        let (_, lhs) = parse_expression(slhs)?;
        let (input, rhs) = parse_value(input)?;

        let assignment = Value::MemberAccess { parent: Box::new(lhs), child: Box::new(Expression::Value(rhs)) };
        return Ok((input, assignment))
    }

    if let Ok((input, name)) = parse_ident::<()>(input) {
        let (input, _) = multispace0(input)?;
        if input.starts_with("[") && !input.starts_with("[[") {
            let (input, expr) = parse_array_subscript(input)?;
            //dbg!(input);
            return Ok((input, Value::ArrayMember { name, index: Box::new(expr) }));
        }

        return Ok((input, Value::Name(name)));
    }
    

    let v = if input.starts_with("undefined") {
        Value::Undefined
    } else if input.starts_with("true") {
        Value::Bool(true)
    } else if input.starts_with("false") {
        Value::Bool(false)
    } else {
        return Err(Err::Error(E::from_error_kind(input, ErrorKind::Fail)));
    };

    Ok((input, v))
}

fn parse_array_subscript<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    //dbg!(input);
    let (input, subscript) = take_until_balanced("[", "]")(input)?;
    let (subscript, _) = take(1_usize)(subscript)?;
    let (subscript, _) = multispace0(subscript)?;
    let (_, expr) = parse_expression(subscript)?;
    //dbg!(input, &expr);
    Ok((input, expr))
}

#[derive(Clone, Debug)]
enum Statement {
    Simple(Expression),
    Compound(CompoundStatement),
}

fn parse_statement<'a, E>(input: &'a str) -> IResult<&'a str, Statement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = multispace0(input)?;
    if input.starts_with("if") && input.contains("(") {
        let pos = input.find("(").unwrap();
        let space = &input[2..pos];
        if space.is_empty() || space.chars().all(|c| c.is_ascii_whitespace()) {
            let (input, compound) = parse_if_statement(input)?;
            return Ok((input, Statement::Compound(compound)));
        }
    } 

    if input.starts_with("else") {
        let (input, compound) = parse_else_statement(input)?;
        return Ok((input, Statement::Compound(compound)));
    }

    if input.starts_with("for") && input.contains("(") {
        let pos = input.find("(").unwrap();
        let space = &input[3..pos];
        if space.is_empty() || space.chars().all(|c| c.is_ascii_whitespace()) {
            let (input, compound) = parse_for_statement(input)?;
            return Ok((input, Statement::Compound(compound)));
        }
    }

    if input.starts_with("while") && input.contains("(") {
        let pos = input.find("(").unwrap();
        let space = &input[5..pos];
        if space.is_empty() || space.chars().all(|c| c.is_ascii_whitespace()) {
            let (input, compound) = parse_while_statement(input)?;
            return Ok((input, Statement::Compound(compound)));
        }
    }

    if input.starts_with("/#") {
        let (input, compound) = parse_dev_block(input)?;
        return Ok((input, Statement::Compound(compound)));
    }
    
    let (input, statement) = take_until(";")(input)?;
    let (_, expr) = parse_expression(statement)?;
    let (input, _) = take(1_usize)(input)?;

    Ok((input, Statement::Simple(expr)))
}

#[derive(Clone, Debug)]
enum CompoundStatement {
    If {
        cond: Box<Expression>,
        body: Vec<Statement>,
    }, 
    Else {
        body: Vec<Statement>,
    },
    While {
        cond: Box<Expression>,
        body: Vec<Statement>,
    },
    For {
        init: Box<Expression>,
        cond: Box<Expression>,
        inc: Box<Expression>,
        body: Vec<Statement>,
    },
    RangedFor {
        var: Ident,
        container: Value,
        body: Vec<Statement>,
    },
    DevBlock {
        body: Vec<Statement>,
    }
}

fn parse_compound_statement<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    if input.starts_with("if") {
        parse_if_statement(input)
    } else if input.starts_with("else") {
        parse_else_statement(input)
    } else if input.starts_with("for") {
        parse_for_statement(input)
    } else if input.starts_with("while") {
        parse_while_statement(input)
    } else if input.starts_with("/#") {
        parse_dev_block(input)
    } else {
        unreachable!()
    }
}

fn parse_if_statement<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take_until("(")(input)?;
    let (input, scond) = take_until_balanced("(", ")")(input)?;
    let (scond, _) = take(1_usize)(scond)?;
    let (scond, _) = multispace0(scond)?;
    
    let (_, cond) = parse_expression(scond)?;

    let (input, _) = opt(multispace0)(input)?;

    if input.starts_with("{") {
        let (input, block) = parse_block(input)?;
        Ok((input, CompoundStatement::If { cond: Box::new(cond), body: block.statements }))
    } else {
        let (input, body) = take_until(";")(input)?;
        let (input, _) = take(1_usize)(input)?;
        let input = input.trim_start();
        let body = body.trim();
        if body.is_empty() {
            return Ok((input, CompoundStatement::If { cond: Box::new(cond), body: vec![] }));
        }
        let (_, expr) = parse_expression(body)?;
        Ok((input, CompoundStatement::If { cond: Box::new(cond), body: vec![ Statement::Simple(expr) ] }))
    }
}

fn parse_else_statement<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take("else ".len())(input)?;

    let (input, _) = opt(multispace0)(input)?;

    if input.starts_with("{") {
        let (input, block) = parse_block(input)?;
        Ok((input, CompoundStatement::Else { body: block.statements }))
    } else {
        //dbg!(input);
        let (input, statement) = parse_statement(input)?;
        Ok((input, CompoundStatement::Else { body: vec![ statement ] }))
    }
}

fn parse_for_statement<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take_until("(")(input)?;
    let (input, control) = take_until_balanced("(", ")")(input)?;
    let (control, _) = take(1_usize)(control)?;

    let (control, sinit) = take_until(";")(control)?;
    let (control, _) = take(1_usize)(control)?;
    let sinit = sinit.trim();
    let (_, init) = parse_expression(sinit)?;
    
    let (control, scond) = take_until(";")(control)?;
    let (control, _) = take(1_usize)(control)?;
    let scond = scond.trim();
    let (_, cond) = parse_expression(scond)?;
    
    let (_, sinc) = take_until(")")(control)?;
    let sinc = sinc.trim();
    let (_, inc) = parse_expression(sinc)?;

    let (input, _) = opt(multispace0)(input)?;

    if input.starts_with("{") {
        let (input, block) = parse_block(input)?;
        Ok((input, CompoundStatement::For { init: Box::new(init), cond: Box::new(cond), inc: Box::new(inc), body: block.statements }))
    } else {
        let (input, body) = take_until(";")(input)?;
        let (input, _) = take(1_usize)(input)?;
        let input = input.trim_start();
        let body = body.trim();
        if body.is_empty() {
            return Ok((input, CompoundStatement::For { init: Box::new(init), cond: Box::new(cond), inc: Box::new(inc), body: vec![] }));
        }
        let (_, expr) = parse_expression(body)?;
        Ok((input, CompoundStatement::For { init: Box::new(init), cond: Box::new(cond), inc: Box::new(inc), body: vec![ Statement::Simple(expr) ] }))
    }
}

fn parse_while_statement<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take_until("(")(input)?;
    let (input, scond) = take_until_balanced("(", ")")(input)?;
    let (scond, _) = take(1_usize)(scond)?;
    let (scond, _) = multispace0(scond)?;
    
    let (_, cond) = parse_expression(scond)?;

    let (input, _) = opt(multispace0)(input)?;

    if input.starts_with("{") {
        let (input, block) = parse_block(input)?;
        Ok((input, CompoundStatement::If { cond: Box::new(cond), body: block.statements }))
    } else {
        let (input, body) = take_until(";")(input)?;
        let (input, _) = take(1_usize)(input)?;
        let input = input.trim_start();
        let body = body.trim();
        if body.is_empty() {
            return Ok((input, CompoundStatement::If { cond: Box::new(cond), body: vec![] }));
        }
        let (_, expr) = parse_expression(body)?;
        Ok((input, CompoundStatement::While { cond: Box::new(cond), body: vec![ Statement::Simple(expr) ] }))
    }
}

#[derive(Clone, Debug)]
enum Expression {
    Null,

    Parenthetized(Box<Expression>),

    Iteration {
        ident: Ident,
        args: Vec<Expression>,
    },
    SelfIteration {
        self_: Box<Expression>,
        iteration: Box<Expression>,
    },

    LogicalNot(Box<Expression>),
    
    PrefixIncrement(Box<Expression>),
    PostfixIncrement(Box<Expression>),
    PrefixDecrement(Box<Expression>),
    PostfixDecrement(Box<Expression>),

    Assignment {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    LogicalOr {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    LogicalAnd {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    BitwiseOr {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    BitwiseAnd {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    Equal {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    NotEqual {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    LessThan {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    LessThanOrEqual {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    GreaterThan {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    GreaterThanOrEqual {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    Addition {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Subtraction {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Multiplication {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Division {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Modulo {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    AdditionAssignment {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    SubtractionAssignment {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    MultiplicationAssignment {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    DivisionAssignment {
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },

    Call {
        thread: bool,
        name: Ident,
        args: Vec<Expression>,
    },
    SelfCall {
        self_: Box<Expression>,
        call: Box<Expression>,
    },
    Return(Option<Box<Expression>>),
    Break,
    Continue,
    Value(Value),
}

fn parse_expression<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = multispace0(input)?;

    if input.starts_with("return") {
        let (input, ret) = parse_return(input)?;
        return Ok((input, ret));
    }

    if input.starts_with("(") {
        let (input, expr) = parse_parenthetized_expression(input)?;
        return Ok((input, expr));
    }

    if input.starts_with("[[") {
        let (input, expr) = parse_iteration_expression(input)?;
        return Ok((input, expr));
    }

    if input.starts_with("\"") {
        let (input, s) = parse_string_literal(input)?;
        return Ok((input, Expression::Value(Value::StringLiteral(s))));
    }

    if input.starts_with("#\"") {
        let (input, _) = take(1_usize)(input)?;
        let (input, s) = parse_string_literal(input)?;
        return Ok((input, Expression::Value(Value::StringLiteral(s))));
    }

    if let Ok((input2, _)) = parse_ident::<(&str, ErrorKind)>(input) {
        let (input2, _) = multispace0(input2)?;
        //dbg!(input2);
        if input2.starts_with("(") {
            let (input, call) = parse_call(input)?;
            return Ok((input, call));
        }
    }

    if input.contains("||") {
        let (input, or) = parse_logical_or(input)?;
        return Ok((input, or));
    } else if input.contains("&&") {
        let (input, or) = parse_logical_and(input)?;
        return Ok((input, or));
    } else if input.contains("|") {
        let (input, or) = parse_bitwise_or(input)?;
        return Ok((input, or));
    } else if input.contains("&") {
        let (input, or) = parse_bitwise_and(input)?;
        return Ok((input, or));
    } else if input.contains("==") {
        let (input, lt) = parse_equal(input)?;
        return Ok((input, lt));
    } else if input.contains("!=") {
        let (input, lt) = parse_not_equal(input)?;
        return Ok((input, lt));
    } else if input.contains("<=") {
        let (input, lt) = parse_less_than_or_equal(input)?;
        return Ok((input, lt));
    } else if input.contains("<") {
        let (input, lt) = parse_less_than(input)?;
        return Ok((input, lt));
    } else if input.contains(">=") {
        let (input, lt) = parse_greater_than_or_equal(input)?;
        return Ok((input, lt));
    } else if input.contains(">") {
        let (input, lt) = parse_greater_than(input)?;
        return Ok((input, lt));
    } else if input.contains("+=") {
        let (input, lt) = parse_addition_assignment(input)?;
        return Ok((input, lt));
    } else if input.contains("-=") {
        let (input, lt) = parse_subtraction_assignment(input)?;
        return Ok((input, lt));
    } else if input.contains("*=") {
        let (input, lt) = parse_multiplication_assignment(input)?;
        return Ok((input, lt));
    } else if input.contains("/=") {
        let (input, lt) = parse_division_assignment(input)?;
        return Ok((input, lt));
    } else if input.contains("=") {
        let (input, assignment) = parse_assignment(input)?;
        return Ok((input, assignment));
    } else if input.contains("+") && input.chars().nth(input.find("+").unwrap() + 1).unwrap() != '+'  {
        let (input, lt) = parse_addition(input)?;
        return Ok((input, lt));
    } else if input.contains("-") && input.chars().nth(input.find("-").unwrap() + 1).unwrap() != '-' {
        let (input, lt) = parse_subtraction(input)?;
        return Ok((input, lt));
    } else if input.contains("*") {
        let (input, lt) = parse_multiplication(input)?;
        return Ok((input, lt));
    } else if input.contains("/") {
        let (input, lt) = parse_division(input)?;
        return Ok((input, lt));
    } else if input.contains("%") {
        let (input, lt) = parse_modulo(input)?;
        return Ok((input, lt));
    } 

    if input.starts_with("thread ") {
        let (input, call) = parse_call(input)?;
        return Ok((input, call));
    }

    if let Ok((input2, _)) = parse_ident::<(&str, ErrorKind)>(input) {
        let (input2, _) = multispace0(input2)?;
        //dbg!(input2);
        if input2.starts_with("(") {
            let (input, call) = parse_call(input)?;
            return Ok((input, call));
        } else if input2.starts_with("++") {
            let (input, inc) = parse_postfix_increment(input)?;
            return Ok((input, inc));
        } else if input2.starts_with("--") {
            let (input, dec) = parse_postfix_decrement(input)?;
            return Ok((input, dec));
        }
    }

    if input.starts_with("++") {
        let (input, inc) = parse_prefix_increment(input)?;
        return Ok((input, inc));
    } else if input.starts_with("--") {
        let (input, inc) = parse_prefix_decrement(input)?;
        return Ok((input, inc));
    }  else if input.starts_with("break") {
        let (input, _) = take("break".len())(input)?;
        return Ok((input, Expression::Break));
    } else if input.starts_with("continue") {
        let (input, _) = take("continue".len())(input)?;
        return Ok((input, Expression::Continue));
    } else if input.starts_with("!") {
        let (input, not) = parse_logical_not(input)?;
        return Ok((input, not));
    } 
        
    //dbg!(input);
    let (input, value) = parse_value(input)?;
    if input.starts_with("++") {
        let (input, _) = take("++".len())(input)?;
        return Ok((input, Expression::PostfixIncrement(Box::new(Expression::Value(value)))))
    } else if input.starts_with("--") {
        let (input, _) = take("--".len())(input)?;
        return Ok((input, Expression::PostfixDecrement(Box::new(Expression::Value(value)))))
    }

    let value = Expression::Value(value);
    //dbg!(input);
    
    let (input, _) = multispace0(input)?;

    //dbg!(input);
    if let Ok((input, call)) = parse_call::<(&str, ErrorKind)>(input) {
        return Ok((input, Expression::SelfCall { self_: Box::new(value), call: Box::new(call) }));
    } else if let Ok((input, iteration)) = parse_iteration_expression::<(&str, ErrorKind)>(input) {
        return Ok((input, Expression::SelfIteration { self_: Box::new(value), iteration: Box::new(iteration) }));
    }
    
    Ok((input, value))
}

fn parse_parenthetized_expression<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    //dbg!(input);
    let (input, sexpr) = take_until_balanced("(", ")")(input)?;
    let (sexpr, _) = take(1_usize)(sexpr)?;
    let (sexpr, _) = multispace0(sexpr)?;
    //dbg!(input, sexpr);
    let (_, expr) = parse_expression(sexpr)?;
    let expr = Expression::Parenthetized(Box::new(expr));
    Ok((input, expr))
}

fn parse_iteration_expression<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    //dbg!(input);
    let (input, sfunc) = take_until_balanced("[[", "]]")(input)?;
    let (sfunc, _) = take(2_usize)(sfunc)?;
    let (sfunc, _) = multispace0(sfunc)?;
    //dbg!(input, sfunc);
    let (_, func) = parse_ident(sfunc)?;
    let (input, args) = take_until_balanced("(", ")")(input)?;
    let (args, _) = take(1_usize)(args)?;
    let (_, args) = parse_comma_separated_list(args)?;
    let args = args.into_iter().map(|a| parse_expression(a).map(|(_, a)| a)).collect::<Result<Vec<_>, _>>()?;
    let expr = Expression::Iteration {
        ident: func,
        args,
    };
    Ok((input, expr))
}

fn parse_logical_not<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, expr) = parse_prefix_unary_expression(input, "!")?;
    Ok((input, Expression::LogicalNot(Box::new(expr))))
}

fn parse_prefix_increment<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, expr) = parse_prefix_unary_expression(input, "++")?;
    Ok((input, Expression::PrefixIncrement(Box::new(expr))))
}

fn parse_prefix_decrement<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, expr) = parse_prefix_unary_expression(input, "--")?;
    Ok((input, Expression::PrefixDecrement(Box::new(expr))))
}

fn parse_prefix_unary_expression<'a, E>(input: &'a str, op: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take(op.len())(input)?;
    let (input, expr) = parse_expression(input)?;
    Ok((input, expr))
}

fn parse_postfix_increment<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, expr) = parse_postfix_unary_expression(input, "++")?;
    Ok((input, Expression::PostfixIncrement(Box::new(expr))))
}

fn parse_postfix_decrement<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, expr) = parse_postfix_unary_expression(input, "--")?;
    Ok((input, Expression::PostfixDecrement(Box::new(expr))))
}

fn parse_postfix_unary_expression<'a, E>(input: &'a str, op: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, expr) = take_until(op)(input)?;
    let (_, expr) = parse_expression(expr)?;
    let (input, _) = take(op.len())(input)?;
    Ok((input, expr))
}

fn parse_logical_or<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "||")?;

    Ok((input, Expression::LogicalOr { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_logical_and<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "&&")?;

    Ok((input, Expression::LogicalAnd { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_bitwise_or<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "|")?;

    Ok((input, Expression::BitwiseOr { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_bitwise_and<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "&")?;

    Ok((input, Expression::BitwiseAnd { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_assignment<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "=")?;

    Ok((input, Expression::Assignment { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_equal<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "==")?;

    Ok((input, Expression::Equal { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_not_equal<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "!=")?;

    Ok((input, Expression::NotEqual { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_less_than<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "<")?;

    Ok((input, Expression::LessThan { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_less_than_or_equal<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "<=")?;

    Ok((input, Expression::LessThanOrEqual { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_greater_than<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, ">")?;

    Ok((input, Expression::GreaterThan { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_greater_than_or_equal<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, ">=")?;

    Ok((input, Expression::GreaterThanOrEqual { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_addition<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "+")?;

    Ok((input, Expression::Addition { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_addition_assignment<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "+=")?;

    Ok((input, Expression::AdditionAssignment { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_subtraction<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "-")?;

    Ok((input, Expression::Subtraction { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_subtraction_assignment<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "-=")?;

    Ok((input, Expression::SubtractionAssignment { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_multiplication<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "*")?;

    Ok((input, Expression::Multiplication { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_multiplication_assignment<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "*=")?;

    Ok((input, Expression::MultiplicationAssignment { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_division<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "/")?;

    Ok((input, Expression::Division { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_division_assignment<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "/=")?;

    Ok((input, Expression::DivisionAssignment { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_modulo<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, (lhs, rhs)) = parse_binary_expression(input, "%")?;

    Ok((input, Expression::Modulo { lhs: Box::new(lhs), rhs: Box::new(rhs) }))
}

fn parse_binary_expression<'a, E>(input: &'a str, op: &'a str) -> IResult<&'a str, (Expression, Expression), E>
where
  E: ParseError<&'a str>,
{
    let (input, slhs) = take_until(op)(input)?;
    let slhs = slhs.trim();
    let (input, _) = take(op.len())(input)?;
    let (input, _) = multispace0(input)?;

    let (_, lhs) = parse_expression(slhs)?;
    let (input, rhs) = parse_expression(input)?;

    Ok((input, (lhs, rhs)))
}

fn parse_call<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, thread) = if input.starts_with("thread ") {
        let (input, _) = take("thread ".len())(input)?;
        let (input, _) = multispace0(input)?;
        (input, true)
    } else {
        (input, false)
    };
    //dbg!(input);
    let (input, ident) = parse_ident(input)?;
    //dbg!(input, &ident);
    let (input, _) = opt(multispace0)(input)?;
    let (input, args) = take_until_balanced("(", ")")(input)?;
    //dbg!(args);
    let (args, _) = take(1_usize)(args)?;
    //dbg!(args);
    let (_, args) = many0(parse_arg)(args)?;
    //dbg!(&args);
    let call = Expression::Call { thread, name: ident, args };
    Ok((input, call))
}

fn parse_return<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take("return".len())(input)?;
    let (input, _) = multispace0(input)?;
    if input.is_empty() {
        return Ok((input, Expression::Return(None)));
    }
    let (input, expr) = parse_expression(input)?;
    Ok((input, Expression::Return(Some(Box::new(expr)))))
}

#[derive(Clone, Debug)]
struct Ast {
    directives: Vec<Directive>,
    functions: Vec<FunctionDef>,
}

fn parse_script<'a, E>(input: &'a str) -> IResult<&'a str, Ast, E>
where
  E: ParseError<&'a str>,
{
    let (_, directives) = parse_directives(input)?;
    let include = input.rfind("\n#include");
    let using_animtree = input.rfind("\n#using_animtree");
    let off = include.unwrap_or(0).max(using_animtree.unwrap_or(0));
    let off = if off == 0 {
        if input.starts_with("#include") || input.starts_with("#using_animtree") {
            input.find("\n").unwrap_or(0)
        } else {
            0
        }
    } else {
        off
    };
    let (input, _) = take(off)(input)?;
    let (_, functions) = many0(parse_function_def)(&input)?;
    Ok(("", Ast { directives, functions })) 
}

#[derive(Clone, Debug)]
enum Directive {
    Include(String),
    UsingAnimtree(String),
}

fn parse_directives<'a, E>(input: &'a str) -> IResult<&'a str, Vec<Directive>, E>
where
  E: ParseError<&'a str>,
{
    let mut directives = Vec::new();
    for line in input.lines() {
        if line.starts_with("#include ") {
            let (line, _) = take("#include ".len())(line)?;
            let (line, _) = multispace0(line)?;
            let (_, include) = take_until(";")(line)?;
            let include = Directive::Include(include.to_string());
            directives.push(include);
        } else if line.starts_with("#using_animtree") {
            let (line, _) = take("#using_animtree".len())(line)?;
            let (line, _) = multispace0(line)?;
            let (line, _) = take_until("(")(line)?;
            let (line, _) = multispace0(line)?;
            let (line, _) = take_until("\"")(line)?;
            let (line, _) = take(1_usize)(line)?;
            let (_, animtree) = take_until("\"")(line)?;
            let animtree = Directive::UsingAnimtree(animtree.to_string());
            directives.push(animtree);
        }
    }
    Ok((input, directives)) 
}

#[derive(Clone, Debug)]
struct FunctionDef {
    name: Ident,
    args: ArgsList,
    body: Block,
}

fn parse_function_def<'a, E>(input: &'a str) -> IResult<&'a str, FunctionDef, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_ident(input)?;
    //dbg!(&name);
    let (input, args) = parse_args_list(input)?;
    let (input, body) = parse_block(input)?;

    Ok((input, FunctionDef { name, args, body }))
}

#[derive(Clone, Debug)]
struct ArgsList {
    args: Vec<Ident>,
}

fn parse_args_list<'a, E>(input: &'a str) -> IResult<&'a str, ArgsList, E>
where
  E: ParseError<&'a str>,
{
    let (input, list) = take_until_balanced("(", ")")(input)?;
    let (list, _) = take(1_usize)(list)?;
    let (_, args) = many0(parse_arg_decl)(list)?;

    Ok((input, ArgsList { args }))
}

fn parse_arg_decl<'a, E>(input: &'a str) -> IResult<&'a str, Ident, E>
where
  E: ParseError<&'a str>,
{
    let (input, arg) = parse_arg(input)?;
    let ident = match arg {
        Expression::Value(Value::Name(i)) => i,
        _ => unreachable!(),
    };

    Ok((input, ident))
}

fn parse_arg<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, arg) = parse_comma_separated_list_item(input)?; 
    let (_, expr) = parse_expression(arg)?;
    Ok((input, expr))
}

#[derive(Clone, Debug)]
struct Block {
    statements: Vec<Statement>,
}

fn parse_block<'a, E>(input: &'a str) -> IResult<&'a str, Block, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = multispace0(input)?;
    let (input, body) = take_until_balanced("{", "}")(input)?;
    let (body, _) = take(1_usize)(body)?;
    //dbg!(body);
    let (_, statements) = many0(parse_statement)(body)?;

    Ok((input, Block { statements }))
}

fn parse_dev_block<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = multispace0(input)?;
    let (input, _) = take_until_balanced("/#", "#/")(input)?;
    //let (body, _) = take(2_usize)(body)?;
    //let body = &body[..body.len() - 2];
    //dbg!(body);
    //let (_, statements) = many0(parse_statement)(body)?;

    //Ok((input, CompoundStatement::DevBlock { body: statements }))
    Ok((input, CompoundStatement::DevBlock { body: vec![] }))
}

fn main() {
    let source = std::fs::read_to_string("./utility.gsc").unwrap();
    let source = comment_strip::strip_comments(source, comment_strip::CommentStyle::C, true).unwrap();
    let source = source.lines().filter(|l| !l.is_empty()).collect::<String>();
    let source = source.chars().filter(|c| *c != '\t').collect::<String>();
    //dbg!(&source);
    
    ////dbg!(parse_expression::<()>(test).unwrap());
    ////dbg!(parse_statement::<()>(test).unwrap());
    ////dbg!(parse_function_def::<()>(test).unwrap());
    dbg!(parse_script::<()>(&source).unwrap());
}
