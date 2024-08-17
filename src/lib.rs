#![allow(dead_code)]

// TODO
//
// fix parsing of dev blocks
//
// probably other fixes
// 
// tidy up
//
// docs

use std::usize;

use nom::branch::alt;
use nom::bytes::complete::{tag, take, take_till, take_until};
use nom::bytes::streaming::is_not;
use nom::character::complete::{digit0, digit1, hex_digit1, multispace0, oct_digit1};
use nom::character::streaming::{char, multispace1};
use nom::combinator::{map, opt, value, verify};
use nom::error::{ErrorKind, ParseError};
use nom::multi::{fold_many0, many0};
use nom::sequence::{delimited, preceded};
use nom::{Err, IResult, Parser};

#[cfg(feature = "serde")]
use serde_derive::{Serialize, Deserialize};

/// Parse an escaped character: \n, \t, \r, etc.
pub fn parse_escaped_char<'a, E>(input: &'a str) -> IResult<&'a str, char, E>
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
pub fn parse_escaped_whitespace<'a, E: ParseError<&'a str>>(
  input: &'a str,
) -> IResult<&'a str, &'a str, E> {
  preceded(char('\\'), multispace1).parse(input)
}

/// Parse a non-empty block of text that doesn't include \ or "
pub fn parse_literal<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
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
pub enum StringFragment<'a> {
  Literal(&'a str),
  EscapedChar(char),
  EscapedWS,
}

/// Combine parse_literal, parse_escaped_whitespace, and parse_escaped_char
/// into a StringFragment.
pub fn parse_fragment<'a, E>(input: &'a str) -> IResult<&'a str, StringFragment<'a>, E>
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
pub fn parse_string_literal<'a, E>(input: &'a str) -> IResult<&'a str, String, E>
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

pub fn parse_hex_literal<'a, E>(input: &'a str) -> IResult<&'a str, i32, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = tag("0x")(input)?;
    let (input, hex) = hex_digit1(input)?;

    Ok((input, i32::from_str_radix(hex, 16).unwrap()))
}

pub fn parse_oct_literal<'a, E>(input: &'a str) -> IResult<&'a str, i32, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = char('0')(input)?;
    let (input, hex) = oct_digit1(input)?;

    Ok((input, i32::from_str_radix(hex, 8).unwrap()))
}

pub fn parse_dec_literal<'a, E>(input: &'a str) -> IResult<&'a str, i32, E>
where
  E: ParseError<&'a str>,
{
    let (input, hex) = digit1(input)?;

    Ok((input, i32::from_str_radix(hex, 10).unwrap()))
}

pub fn parse_int_literal<'a, E>(input: &'a str) -> IResult<&'a str, i32, E>
where
  E: ParseError<&'a str>,
{
    alt((parse_hex_literal, parse_oct_literal, parse_dec_literal)).parse(input)
}

pub fn parse_float_literal<'a, E>(input: &'a str) -> IResult<&'a str, f32, E>
where
  E: ParseError<&'a str>,
{
    let (input, flt) = take_till(|c: char| c.is_ascii_whitespace() || c == ')' || c == ']')(input)?;

    let flt = if flt.contains(".") {
        let parts = flt.split(".").collect::<Vec<_>>();
        if parts.len() > 2 {
            return Err(Err::Error(E::from_error_kind(input, ErrorKind::TakeUntil)));
        }

        // one can be empty (i.e. .300 and 300. are valid), but both can't
        if parts[0].is_empty() && parts[1].is_empty() {
            return Err(Err::Error(E::from_error_kind(input, ErrorKind::TakeUntil)));
        }

        let (rem, _) = digit0(parts[0])?;
        if !rem.is_empty() {
            return Err(Err::Error(E::from_error_kind(input, ErrorKind::TakeUntil)));
        }

        let (rem, _) = digit0(parts[1])?; {
            if !rem.is_empty() {
                return Err(Err::Error(E::from_error_kind(input, ErrorKind::TakeUntil)));
            }   
        }
        flt
    } else {
        let (rem, _) = digit1(flt)?;
        if !rem.is_empty() {
            return Err(Err::Error(E::from_error_kind(input, ErrorKind::TakeUntil)));
        }
        flt
    };

    //dbg!(flt);
    Ok((input, flt.parse().unwrap()))
}

pub fn take_until_balanced<'a, E>(
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

pub fn parse_comma_separated_list_item<'a, E>(input: &'a str) -> IResult<&'a str, &'a str, E>
where
  E: ParseError<&'a str>,
{
    if input.is_empty() {
        return Err(Err::Error(E::from_error_kind(input, ErrorKind::TakeUntil)));
    }
    let (input, _) = multispace0(input)?;
    let (input, s) = if input.starts_with("(") {
        let pos = input.find(")").ok_or(Err::Error(E::from_error_kind(input, ErrorKind::TakeUntil)))?;
        if let Some(comma) = input[pos..].find(",") {
            (&input[..comma], Some(&input[comma + 1..]))
        } else {
            (input, None)
        }
    } else {
        opt(take_until(","))(input)?
    };
    
    let (input, s) = if let Some(s) = s {
        (input, s)
    } else {
        ("", &input[..input.len() - 1])
    };
    // take the comma, unless it's the last arg so there isn't one
    let (input, _) = opt(take(1_usize))(input)?;
    // take whatever whitespace follows the comma too
    let (input, _) = multispace0(input)?;
    Ok((input, s))
}

pub fn parse_comma_separated_list<'a, E>(input: &'a str) -> IResult<&'a str, Vec<&'a str>, E>
where
  E: ParseError<&'a str>,
{
    let (input, v) = many0(parse_comma_separated_list_item)(input)?;
    let v = v.into_iter().filter(|s| !s.is_empty()).collect();
    Ok((input, v))
}

static RESERVED_KEYWORDS: [&str; 11] = [
    "if",
    "for",
    "while",
    "return",
    "break",
    "continue",
    "undefined",
    "true",
    "false",
    "wait",
    "thread",
];

/// An identifier.
/// 
/// Essentially just wraps a [`String`] and ensures the identifier is valid.
/// Valid identifiers can only contain alphanumeric characters and underscore,
/// can only begin with a letter or underscore, and cannot consist solely of 
/// underscores.
/// 
/// Function names and variable names are both identifiers.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct Ident(String);

impl Ident {
    pub fn try_from_str(s: impl AsRef<str>) -> Option<Self> {
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
        if !first.is_alphabetic() && first != '_' {
            return None;
        }
        
        // the rest of a valid ident can have letters, numbers, and underscores
        if !chars.all(|c| c.is_ascii_alphanumeric() || c == '_') {
            return None;
        }

        // but it can't only consist of underscores
        if s.chars().all(|c| c == '_') {
            return None;
        }

        Some(Self(s.to_string()))
    }
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct ExternalIdent(String);

impl ExternalIdent {
    pub fn try_from_str(s: impl AsRef<str>) -> Option<Self> {
        let s = s.as_ref();

        let idents = s.split("::").collect::<Vec<_>>();
        // can't be more than 1 namespace separator
        if idents.len() > 2 {
            return None;
        }

        // if there is no namespace separator, parse it as a normal ident
        if idents.len() == 1 {
            if Ident::try_from_str(idents[0]).is_none() {
                return None;
            } else {
                return Some(Self(s.to_string()));
            }
        } 
        
        // if it's the global namespace specifier (::some_func), just parse the ident
        if idents[0].is_empty() {
            if Ident::try_from_str(idents[1]).is_none() {
                return None;
            } else {
                return Some(Self(s.to_string()));
            }
        }

        // strip the backslashes and try to parse it as an ident
        let module = idents[0].chars().filter(|c| *c != '\\').collect::<String>();
        if Ident::try_from_str(&module).is_none() {
            return None;
        }

        // can't start or end on a backslash
        if idents[0].starts_with("\\") || idents[0].ends_with("\\") {
            return None;
        }

        // can't be only backslashes
        if idents[0].chars().all(|c| c == '\\') {
            return None;
        }

        Some(Self(s.to_string()))
    }

    pub fn module(&self) -> &str {
        let idents = self.0.split("::").collect::<Vec<_>>();
        idents[0]
    }

    pub fn ident(&self) -> &str {
        let idents = self.0.split("::").collect::<Vec<_>>();
        idents[1]
    }

    pub fn is_global_namespace(&self) -> bool {
        self.0.starts_with("::")
    }
}

pub fn parse_ident<'a, E>(input: &'a str) -> IResult<&'a str, Ident, E>
where
  E: ParseError<&'a str>,
{
    let (input, ident) = take_till(|c: char| c != '_' && !c.is_ascii_alphanumeric())(input)?;
    let Some(ident) = Ident::try_from_str(ident) else {
        return Err(Err::Error(E::from_error_kind(ident, ErrorKind::AlphaNumeric)));
    };

    Ok((input, ident))
}

pub fn parse_external_ident<'a, E>(input: &'a str) -> IResult<&'a str, ExternalIdent, E>
where
  E: ParseError<&'a str>,
{
    let (input, ident) = take_till(|c: char| c != ':' && c != '_' && !c.is_ascii_alphanumeric())(input)?;
    let Some(ident) = ExternalIdent::try_from_str(ident) else {
        return Err(Err::Error(E::from_error_kind(ident, ErrorKind::AlphaNumeric)));
    };

    Ok((input, ident))
}

/// A simple value.
/// 
/// Can be an identifier, a string or int literal, a boolean value, an array 
/// list, an array or struct access, etc.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Value {
    Name(Ident),
    ExternalName(ExternalIdent),
    IntLiteral(i32),
    FloatLiteral(f32),
    StringLiteral(String),
    Undefined,
    Bool(bool),
}

pub fn parse_value<'a, E>(input: &'a str) -> IResult<&'a str, Value, E>
where
  E: ParseError<&'a str>,
{
    if let Ok((input, i)) = parse_int_literal::<()>(input) {
        return Ok((input, Value::IntLiteral(i)));
    }

    if let Ok((input, i)) = parse_float_literal::<()>(input) {
        return Ok((input, Value::FloatLiteral(i)));
    }

    if let Ok((input, name)) = parse_ident::<()>(input) {
        return Ok((input, Value::Name(name)));
    }

    if let Ok((input, name)) = parse_external_ident::<()>(input) {
        return Ok((input, Value::ExternalName(name)));
    }

    if input.starts_with("undefined") {
        let (input, _) = take("undefined".len())(input)?;
        Ok((input, Value::Undefined))
    } else if input.starts_with("true") {
        let (input, _) = take("true".len())(input)?;
        Ok((input, Value::Bool(true)))
    } else if input.starts_with("false") {
        let (input, _) = take("false".len())(input)?;
        Ok((input, Value::Bool(false)))
    } else {
        Err(Err::Error(E::from_error_kind(input, ErrorKind::Fail)))
    }
}

/// A semicolon-terminated [`Expression`], which may contain one or more 
/// sub-[`Expression`]s or [`Value`]s, or a [`CompoundStatement`] (if, for, while, etc.),
/// which may contain one or more sub-[`Statement`]s.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Statement {
    Simple(Expression),
    Compound(CompoundStatement),
}

pub fn parse_statement<'a, E>(input: &'a str) -> IResult<&'a str, Statement, E>
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
    
    let (input, statement) = opt(take_until(";"))(input)?;
    let statement = if let Some(statement) = statement {
        statement
    } else if input.ends_with(")") {
        &input[..input.len() - 1]
    } else {
        input
    };

    let (statement, mut expr) = parse_expression(statement)?;
    
    let (mut statement, _) = multispace0(statement)?;
    while !statement.is_empty() {
        //dbg!(statement);
        let (statement2, mut expr2) = parse_partial_expression(statement, &expr)?;
        if let Expression::Call { .. } = expr2 {
            expr2 = Expression::SelfCall { self_: Box::new(expr), call: Box::new(expr2) };
        }
 
        (statement, _) = multispace0(statement2)?;
        expr = expr2;
    }

    let (input, _) = take(1_usize)(input)?;

    Ok((input, Statement::Simple(expr)))
}

/// An if, else, while, or for loop, or a dev block.
/// 
/// They are "compound" in the sense that they are statements in themselves,
/// but also may contain multiple statements, including other [`CompoundStatement`]s
/// (e.g., nested if-statements).
/// 
/// `else if` gets parsed as two [`CompoundStatement`]s (else, then if).
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum CompoundStatement {
    If {
        cond: Box<Statement>,
        body: Vec<Statement>,
    }, 
    Else {
        body: Vec<Statement>,
    },
    While {
        cond: Box<Statement>,
        body: Vec<Statement>,
    },
    For {
        init: Box<Statement>,
        cond: Box<Statement>,
        inc: Box<Statement>,
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

pub fn parse_compound_statement<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
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

pub fn parse_if_statement<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take_until("(")(input)?;
    let (input, scond) = take_until_balanced("(", ")")(input)?;
    let (scond, _) = take(1_usize)(scond)?;
    let (scond, _) = multispace0(scond)?;
    
    let (_, cond) = parse_statement(scond)?;

    let (input, _) = opt(multispace0)(input)?;

    if input.starts_with("{") {
        let (input, block) = parse_block(input)?;
        Ok((input, CompoundStatement::If { cond: Box::new(cond), body: block.statements }))
    } else {
        let semicolon = input.find(";").ok_or(Err::Error(E::from_error_kind(input, ErrorKind::Fail)))?;
        let (input, body) = (&input[semicolon + 1..], &input[..=semicolon]);
        let input = input.trim_start();
        let body = body.trim();
        //dbg!(input, body);
        if body.is_empty() {
            return Ok((input, CompoundStatement::If { cond: Box::new(cond), body: vec![] }));
        }
        let (_, statement) = parse_statement(body)?;
        Ok((input, CompoundStatement::If { cond: Box::new(cond), body: vec![ statement ] }))
    }
}

pub fn parse_else_statement<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take("else".len())(input)?;

    let (input, _) = opt(multispace0)(input)?;

    if input.starts_with("{") {
        let (input, block) = parse_block(input)?;
        Ok((input, CompoundStatement::Else { body: block.statements }))
    } else {
        ////dbg!(input);
        let (input, statement) = parse_statement(input)?;
        Ok((input, CompoundStatement::Else { body: vec![ statement ] }))
    }
}

pub fn parse_for_statement<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take_until("(")(input)?;
    let (input, control) = take_until_balanced("(", ")")(input)?;
    let (control, _) = take(1_usize)(control)?;

    let (control, sinit) = take_until(";")(control)?;
    let (control, _) = take(1_usize)(control)?;
    let sinit = sinit.trim();
    let (_, init) = parse_statement(sinit)?;
    
    let (control, scond) = take_until(";")(control)?;
    let (control, _) = take(1_usize)(control)?;
    let scond = scond.trim();
    let (_, cond) = parse_statement(scond)?;
    
    let (_, sinc) = take_until(")")(control)?;
    let sinc = sinc.trim();
    let (_, inc) = parse_statement(sinc)?;

    let (input, _) = opt(multispace0)(input)?;

    if input.starts_with("{") {
        let (input, block) = parse_block(input)?;
        Ok((input, CompoundStatement::For { init: Box::new(init), cond: Box::new(cond), inc: Box::new(inc), body: block.statements }))
    } else {
        let semicolon = input.find(";").ok_or(Err::Error(E::from_error_kind(input, ErrorKind::Fail)))?;
        let (input, body) = (&input[..=semicolon], &input[semicolon + 1..]);
        let input = input.trim_start();
        let body = body.trim();
        if body.is_empty() {
            return Ok((input, CompoundStatement::For { init: Box::new(init), cond: Box::new(cond), inc: Box::new(inc), body: vec![] }));
        }
        let (_, statement) = parse_statement(body)?;
        Ok((input, CompoundStatement::For { init: Box::new(init), cond: Box::new(cond), inc: Box::new(inc), body: vec![ statement ] }))
    }
}

pub fn parse_while_statement<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take_until("(")(input)?;
    let (input, scond) = take_until_balanced("(", ")")(input)?;
    let (scond, _) = take(1_usize)(scond)?;
    let (scond, _) = multispace0(scond)?;
    
    let (_, cond) = parse_statement(scond)?;

    let (input, _) = opt(multispace0)(input)?;

    if input.starts_with("{") {
        let (input, block) = parse_block(input)?;
        Ok((input, CompoundStatement::While { cond: Box::new(cond), body: block.statements }))
    } else {
        let semicolon = input.find(";").ok_or(Err::Error(E::from_error_kind(input, ErrorKind::Fail)))?;
        let (input, body) = (&input[semicolon + 1..], &input[..=semicolon]);
        let input = input.trim_start();
        let body = body.trim();
        //dbg!(input, body);
        if body.is_empty() {
            return Ok((input, CompoundStatement::While { cond: Box::new(cond), body: vec![] }));
        }
        let (_, statement) = parse_statement(body)?;
        Ok((input, CompoundStatement::While { cond: Box::new(cond), body: vec![ statement ] }))
    }
}

/// An expression following the format `<lhs> <op> <rhs>` (assignment, 
/// addition, etc.).
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct BinaryExpression {
    lhs: Box<Expression>,
    rhs: Box<Expression>,
}

/// The building block of a program.
/// 
/// Can be a unary or binary operation, a function call, a `break` or 
/// `continue`, etc.
/// 
/// An expression may (and often will) contain sub-expressions.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Expression {
    Parenthetized(Box<Expression>),
    Tuple(Vec<Expression>),

    StructAccess {
        parent: Box<Expression>,
        child: Box<Expression>,
    },

    ArrayAccess {
        name: Box<Expression>,
        index: Box<Expression>,
    },

    ArrayList(Vec<Expression>),

    Iteration {
        thread: bool,
        ident: ExternalIdent,
        args: Vec<Expression>,
    },
    SelfIteration {
        self_: Box<Expression>,
        iteration: Box<Expression>,
    },

    LogicalNot(Box<Expression>),
    Negation(Box<Expression>),
    
    PrefixIncrement(Box<Expression>),
    PostfixIncrement(Box<Expression>),
    PrefixDecrement(Box<Expression>),
    PostfixDecrement(Box<Expression>),

    Assignment(BinaryExpression),

    LogicalOr(BinaryExpression),
    LogicalAnd(BinaryExpression),
    BitwiseOr(BinaryExpression),
    BitwiseAnd(BinaryExpression),

    Equal(BinaryExpression),
    NotEqual(BinaryExpression),
    LessThan(BinaryExpression),
    LessThanOrEqual(BinaryExpression),
    GreaterThan(BinaryExpression),
    GreaterThanOrEqual(BinaryExpression),

    Addition(BinaryExpression),
    Subtraction(BinaryExpression),
    Multiplication(BinaryExpression),
    Division(BinaryExpression),
    Modulo(BinaryExpression),

    AdditionAssignment(BinaryExpression),
    SubtractionAssignment(BinaryExpression),
    MultiplicationAssignment(BinaryExpression),
    DivisionAssignment(BinaryExpression),

    Call {
        thread: bool,
        name: ExternalIdent,
        args: Vec<Expression>,
    },
    SelfCall {
        self_: Box<Expression>,
        call: Box<Expression>,
    },
    Return(Option<Box<Expression>>),
    Wait(Box<Expression>),
    Break,
    Continue,
    Value(Value),
}

pub fn parse_expression<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = multispace0(input)?;
    //dbg!(input);

    if input.starts_with("return") {
        return parse_return(input);
    }

    if input.starts_with("wait") {
        return parse_wait(input);
    }

    if input.starts_with("!") {
        return parse_logical_not(input);
    }

    if input.starts_with("(") {
        return parse_parenthetized_expression(input);
    }

    if input.starts_with("[[") {
        return parse_iteration_expression(input);
    }

    if input.starts_with("[") {
        return parse_array_list::<E>(input).or(parse_array_subscript(input));
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
        if input2.starts_with("(") {
            if let Ok((input, call)) = parse_call::<E>(input) {
                let (input, call1) = if let Ok((input, subscript)) = parse_array_subscript::<E>(input) {
                    (input, Expression::ArrayAccess { name: Box::new(call), index: Box::new(subscript) })
                } else {
                    (input, call)
                };
                //dbg!(&call1);

                let input = if let Ok((input2, _)) = multispace1::<&str, E>(input) {
                    input2
                } else {
                    return Ok((input, call1));
                };

                if let Ok((input, call2)) = parse_call::<E>(input) {
                    return Ok((input, Expression::SelfCall { self_: Box::new(call1), call: Box::new(call2) }));
                }

                if input.starts_with("thread ") {
                    let (input, _) = take("thread ".len())(input)?;
                    let (input, _) = multispace0(input)?;

                    let (input, call2) = parse_call(input)?;
                    return Ok((input, Expression::SelfCall { self_: Box::new(call1), call: Box::new(call2) }));
                }

                return Ok((input, call1));
            }
        }
    } else if let Ok((input2, _)) = parse_external_ident::<(&str, ErrorKind)>(input) {
        let (input2, _) = multispace0(input2)?;
        ////dbg!(input2);
        if input2.starts_with("(") {
            if let Ok((input, call)) = parse_call::<E>(input) {
                let (input, call1) = if let Ok((input, subscript)) = parse_array_subscript::<E>(input) {
                    (input, Expression::ArrayAccess { name: Box::new(call), index: Box::new(subscript) })
                } else {
                    (input, call)
                };

                let input = if let Ok((input2, _)) = multispace1::<&str, E>(input) {
                    input2
                } else {
                    return Ok((input, call1));
                };

                if let Ok((input, call2)) = parse_call::<E>(input) {
                    return Ok((input, Expression::SelfCall { self_: Box::new(call1), call: Box::new(call2) }));
                }

                if input.starts_with("thread ") {
                    let (input, _) = take("thread ".len())(input)?;
                    let (input, _) = multispace0(input)?;

                    let (input, call2) = parse_call(input)?;
                    return Ok((input, Expression::SelfCall { self_: Box::new(call1), call: Box::new(call2) }));
                }
            }
        }
    }

    let binary_expression_handlers = [
        ("||", parse_logical_or as fn(&'a str) -> IResult<&'a str, Expression, E>),
        ("&&", parse_logical_and),
        ("|", parse_bitwise_or),
        ("&", parse_bitwise_and),
        ("==", parse_equal),
        ("!=", parse_not_equal),
        ("<=", parse_less_than_or_equal),
        ("<", parse_less_than),
        (">=", parse_greater_than_or_equal),
        (">", parse_greater_than),
        ("+=", parse_addition_assignment),
        ("-=", parse_subtraction_assignment),
        ("*=", parse_multiplication_assignment),
        ("/=", parse_division_assignment),
        ("=", parse_assignment),
        ("*", parse_multiplication),
        ("/", parse_division),
        ("%", parse_modulo),
    ];

    for (op, handler) in binary_expression_handlers.iter() {
        if input.contains(op) {
            return handler(input);
        }
    }
    
    if input.contains("+") && input.chars().nth(input.find("+").unwrap() + 1).unwrap() != '+'  {
        return parse_addition(input);
    } else if input.contains("-") && input.chars().nth(input.find("-").unwrap() + 1).unwrap() != '-' && !input.starts_with("-") {
        return parse_subtraction(input);
    }

    if let Ok((input2, _)) = parse_ident::<(&str, ErrorKind)>(input) {
        let (input2, _) = multispace0(input2)?;
        ////dbg!(input2);
        if input2.starts_with("(") {
            return parse_call(input);
        }
    }

    if let Ok((input2, _)) = parse_external_ident::<(&str, ErrorKind)>(input) {
        let (input2, _) = multispace0(input2)?;
        ////dbg!(input2);
        if input2.starts_with("(") {
            return parse_call(input);
        }
    }

    if input.contains(".") && 
        (input.chars().nth(0).unwrap().is_ascii_alphabetic() || input.chars().nth(0).unwrap() == '_') && 
        input.find(".").unwrap() < input.find("[").unwrap_or(usize::MAX)
    {
        if let Ok((input, flt)) = parse_float_literal::<E>(input) {
            return Ok((input, Expression::Value(Value::FloatLiteral(flt))));
        }

        let (input, slhs) = take_until(".")(input)?;
        let slhs = slhs.trim();
        let (input, _) = take(1_usize)(input)?;
        let (input, _) = multispace0(input)?;

        let (_, lhs) = parse_expression(slhs)?;
        let (input, rhs) = parse_value(input)?;

        let assignment = Expression::StructAccess { parent: Box::new(lhs), child: Box::new(Expression::Value(rhs)) };
        //dbg!(&assignment);
        return Ok((input, assignment))
    }

    if input.starts_with("thread ") {
        return parse_call::<E>(input).or(parse_iteration_expression(input));
    }

    if let Ok((input2, _)) = parse_ident::<(&str, ErrorKind)>(input) {
        let (input2, _) = multispace0(input2)?;
        ////dbg!(input2);
        if input2.starts_with("++") {
            return parse_postfix_increment(input);
        } else if input2.starts_with("--") {
            return parse_postfix_decrement(input);
        }
    }

    if input.starts_with("++") {
        return parse_prefix_increment(input);
    } else if input.starts_with("--") {
        return parse_prefix_decrement(input);
    } else if input.starts_with("-") {
        return parse_negation(input);
    } else if input.starts_with("break ") || input.starts_with("break;") {
        let (input, _) = take("break ".len())(input)?;
        return Ok((input, Expression::Break));
    } else if input == "break" {
        let (input, _) = take("break".len())(input)?;
        return Ok((input, Expression::Break));
    } else if input.starts_with("continue ") || input.starts_with("continue;") {
        let (input, _) = take("continue ".len())(input)?;
        return Ok((input, Expression::Continue));
    } else if input == "continue" {
        let (input, _) = take("continue".len())(input)?;
        return Ok((input, Expression::Continue));
    } 
        
    ////dbg!(input);
    let (input, value) = parse_value(input)?;
    if input.starts_with("++") {
        let (input, _) = take("++".len())(input)?;
        return Ok((input, Expression::PostfixIncrement(Box::new(Expression::Value(value)))))
    } else if input.starts_with("--") {
        let (input, _) = take("--".len())(input)?;
        return Ok((input, Expression::PostfixDecrement(Box::new(Expression::Value(value)))))
    } else if input.starts_with("[") {
        let (input, subscript) = parse_array_subscript(input)?;
        return Ok((input, Expression::ArrayAccess { name: Box::new(Expression::Value(value)), index: Box::new(subscript) }))
    }

    let value = Expression::Value(value);
    ////dbg!(input);
    
    let (input, _) = multispace0(input)?;

    ////dbg!(input);
    if let Ok((input, call)) = parse_call::<(&str, ErrorKind)>(input) {
        return Ok((input, Expression::SelfCall { self_: Box::new(value), call: Box::new(call) }));
    } else if let Ok((input, iteration)) = parse_iteration_expression::<(&str, ErrorKind)>(input) {
        return Ok((input, Expression::SelfIteration { self_: Box::new(value), iteration: Box::new(iteration) }));
    }
    
    Ok((input, value))
}

pub fn parse_partial_expression<'a, E>(input: &'a str, prev_expression: &Expression) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = multispace0(input)?;
    //dbg!(input);

    if input.starts_with("(") {
        return parse_parenthetized_expression(input);
    }

    if input.starts_with("[") {
        let (input, subscript) = parse_array_subscript(input)?;
        return Ok((input, Expression::ArrayAccess { name: Box::new(prev_expression.clone()), index: Box::new(subscript) }));
    }

    let binary_expression_handlers = [
        ("||", parse_partial_logical_or as fn(&'a str, &Expression) -> IResult<&'a str, Expression, E>),
        ("&&", parse_partial_logical_and),
        ("|",  parse_partial_bitwise_or),
        ("&",  parse_partial_bitwise_and),
        ("==", parse_partial_equal),
        ("!=", parse_partial_not_equal),
        ("<=", parse_partial_less_than_or_equal),
        ("<",  parse_partial_less_than),
        (">=", parse_partial_greater_than_or_equal),
        (">",  parse_partial_greater_than),
        ("+=", parse_partial_addition_assignment),
        ("-=", parse_partial_subtraction_assignment),
        ("*=", parse_partial_multiplication_assignment),
        ("/=", parse_partial_division_assignment),
        ("=",  parse_partial_assignment),
        ("*",  parse_partial_multiplication),
        ("/",  parse_partial_division),
        ("%",  parse_partial_modulo),
    ];

    for (op, handler) in binary_expression_handlers.iter() {
        if input.starts_with(op) {
            //dbg!(input);
            return handler(input, prev_expression);
        }
    }
    
    if input.starts_with("+") && !input.starts_with("++")  {
        return parse_partial_addition(input, prev_expression);
    } else if input.starts_with("++") {
        let (input, _) = take("++".len())(input)?;
        return Ok((input, Expression::PostfixIncrement(Box::new(prev_expression.clone()))));
    } else if input.contains("-") && input.chars().nth(input.find("-").unwrap() + 1).unwrap() != '-' {
        return parse_partial_subtraction(input, prev_expression);
    } else if input.starts_with("--") {
        let (input, _) = take("--".len())(input)?;
        return Ok((input, Expression::PostfixDecrement(Box::new(prev_expression.clone()))));
    } else if let Ok((input, call)) = parse_call::<E>(input) {
        return Ok((input, call));
    } else {
        return Err(Err::Error(E::from_error_kind(input, ErrorKind::Fail)));
    }

}

pub fn parse_parenthetized_expression<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    //dbg!(input);

    let (input, sexpr) = take_until_balanced("(", ")")(input)?;
    let (sexpr, _) = take(1_usize)(sexpr)?;

    let (sexpr, _) = multispace0(sexpr)?;
    //dbg!(input, sexpr);
    let (sexpr2, expr) = parse_expression(sexpr)?;
    if !sexpr2.starts_with(",") {
        return Ok((input, Expression::Parenthetized(Box::new(expr))));
    }
    
    let (_, values) = parse_comma_separated_list(sexpr)?;
    let exprs = values.into_iter().map(|s| parse_expression(s).map(|(_, e)| e)).collect::<Result<Vec<_>, _>>()?;
    Ok((input, Expression::Tuple(exprs)))
}

pub fn parse_iteration_expression<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, thread) = if input.starts_with("thread ") {
        (&input["thread ".len()..], true)
    } else {
        (input, false)
    };
    let (input, _) = multispace0(input)?;
    ////dbg!(input);
    let (input, sfunc) = take_until_balanced("[[", "]]")(input)?;
    let (sfunc, _) = take(2_usize)(sfunc)?;
    let (sfunc, _) = multispace0(sfunc)?;
    ////dbg!(input, sfunc);
    let (_, func) = parse_external_ident(sfunc)?;
    let (input, args) = take_until_balanced("(", ")")(input)?;
    let (args, _) = take(1_usize)(args)?;
    let (_, args) = parse_comma_separated_list(args)?;
    let args = args.into_iter().map(|a| parse_expression(a).map(|(_, a)| a)).collect::<Result<Vec<_>, _>>()?;
    let expr = Expression::Iteration {
        thread,
        ident: func,
        args,
    };
    Ok((input, expr))
}

pub fn parse_array_list<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    if input.starts_with("[]") {
        let (input, _) = take("[]".len())(input)?;
        return Ok((input, Expression::ArrayList(vec![])));
    }
    let (input, list) = take_until_balanced("[", "]")(input)?;
    let (list, _) = take("[".len())(list)?;
    let (_, values) = parse_comma_separated_list(list)?;
    let values = values.into_iter().map(|v| parse_expression(v).map(|(_, v)| v)).collect::<Result<Vec<_>, _>>()?;
    Ok((input, Expression::ArrayList(values)))
}

pub fn parse_array_subscript<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    ////dbg!(input);
    let (input, subscript) = take_until_balanced("[", "]")(input)?;
    let (subscript, _) = take(1_usize)(subscript)?;
    let (subscript, _) = multispace0(subscript)?;
    let (_, expr) = parse_expression(subscript)?;
    ////dbg!(input, &expr);
    Ok((input, expr))
}

macro_rules! parse_prefix_unary_expression {
    ($name:ident, $op:literal, $ex:expr) => {
        pub fn $name<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
        where
          E: ParseError<&'a str>,
        {
            let (input, expr) = parse_prefix_unary_expression(input, $op)?;
            Ok((input, $ex(Box::new(expr))))
        }
    };
}

parse_prefix_unary_expression!(parse_logical_not, "!", Expression::LogicalNot);
parse_prefix_unary_expression!(parse_prefix_increment, "++", Expression::PrefixIncrement);
parse_prefix_unary_expression!(parse_prefix_decrement, "--", Expression::PrefixDecrement);
parse_prefix_unary_expression!(parse_negation, "-", Expression::Negation);

pub fn parse_prefix_unary_expression<'a, E>(input: &'a str, op: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take(op.len())(input)?;
    let (input, expr) = parse_expression(input)?;
    Ok((input, expr))
}

macro_rules! parse_postfix_unary_expression {
    ($name:ident, $op:literal, $ex:expr) => {
        pub fn $name<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
        where
          E: ParseError<&'a str>,
        {
            let (input, expr) = parse_postfix_unary_expression(input, $op)?;
            Ok((input, $ex(Box::new(expr))))
        }
    };
}

parse_postfix_unary_expression!(parse_postfix_increment, "++", Expression::PostfixIncrement);
parse_postfix_unary_expression!(parse_postfix_decrement, "--", Expression::PostfixDecrement);

pub fn parse_postfix_unary_expression<'a, E>(input: &'a str, op: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, expr) = take_until(op)(input)?;
    let (_, expr) = parse_expression(expr)?;
    let (input, _) = take(op.len())(input)?;
    Ok((input, expr))
}

macro_rules! parse_binary_expression {
    ($name:ident, $op:literal, $ex:ident::$var:ident) => {
        pub fn $name<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
        where
          E: ParseError<&'a str>,
        {
            let (input, (lhs, rhs)) = parse_binary_expression(input, $op)?;
            Ok((input, $ex::$var(BinaryExpression { lhs: Box::new(lhs), rhs: Box::new(rhs) } )))
        }
    };
}

parse_binary_expression!(parse_logical_or, "||", Expression::LogicalOr);
parse_binary_expression!(parse_logical_and, "&&", Expression::LogicalAnd);
parse_binary_expression!(parse_bitwise_or, "|", Expression::BitwiseOr);
parse_binary_expression!(parse_bitwise_and, "&", Expression::BitwiseAnd);
parse_binary_expression!(parse_assignment, "=", Expression::Assignment);
parse_binary_expression!(parse_equal, "==", Expression::Equal);
parse_binary_expression!(parse_not_equal, "!=", Expression::NotEqual);
parse_binary_expression!(parse_less_than, "<", Expression::LessThan);
parse_binary_expression!(parse_less_than_or_equal, "<=", Expression::LessThanOrEqual);
parse_binary_expression!(parse_greater_than, ">", Expression::GreaterThan);
parse_binary_expression!(parse_greater_than_or_equal, ">=", Expression::GreaterThanOrEqual);
parse_binary_expression!(parse_addition, "+", Expression::Addition);
parse_binary_expression!(parse_addition_assignment, "+=", Expression::AdditionAssignment);
parse_binary_expression!(parse_subtraction, "-", Expression::Subtraction);
parse_binary_expression!(parse_subtraction_assignment, "-=", Expression::SubtractionAssignment);
parse_binary_expression!(parse_multiplication, "*", Expression::Multiplication);
parse_binary_expression!(parse_multiplication_assignment, "*=", Expression::MultiplicationAssignment);
parse_binary_expression!(parse_division, "/", Expression::Division);
parse_binary_expression!(parse_division_assignment, "/=", Expression::DivisionAssignment);
parse_binary_expression!(parse_modulo, "%", Expression::Modulo);

pub fn parse_binary_expression<'a, E>(input: &'a str, op: &'a str) -> IResult<&'a str, (Expression, Expression), E>
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

macro_rules! parse_partial_binary_expression {
    ($name:ident, $op:literal, $ex:ident::$var:ident) => {
        pub fn $name<'a, E>(input: &'a str, prev_expression: &Expression) -> IResult<&'a str, Expression, E>
        where
          E: ParseError<&'a str>,
        {
            let (input, (lhs, rhs)) = parse_partial_binary_expression(input, $op, prev_expression)?;
            Ok((input, $ex::$var(BinaryExpression { lhs: Box::new(lhs), rhs: Box::new(rhs) } )))
        }
    };
}

parse_partial_binary_expression!(parse_partial_logical_or, "||", Expression::LogicalOr);
parse_partial_binary_expression!(parse_partial_logical_and, "&&", Expression::LogicalAnd);
parse_partial_binary_expression!(parse_partial_bitwise_or, "|", Expression::BitwiseOr);
parse_partial_binary_expression!(parse_partial_bitwise_and, "&", Expression::BitwiseAnd);
parse_partial_binary_expression!(parse_partial_assignment, "=", Expression::Assignment);
parse_partial_binary_expression!(parse_partial_equal, "==", Expression::Equal);
parse_partial_binary_expression!(parse_partial_not_equal, "!=", Expression::NotEqual);
parse_partial_binary_expression!(parse_partial_less_than, "<", Expression::LessThan);
parse_partial_binary_expression!(parse_partial_less_than_or_equal, "<=", Expression::LessThanOrEqual);
parse_partial_binary_expression!(parse_partial_greater_than, ">", Expression::GreaterThan);
parse_partial_binary_expression!(parse_partial_greater_than_or_equal, ">=", Expression::GreaterThanOrEqual);
parse_partial_binary_expression!(parse_partial_addition, "+", Expression::Addition);
parse_partial_binary_expression!(parse_partial_addition_assignment, "+=", Expression::AdditionAssignment);
parse_partial_binary_expression!(parse_partial_subtraction, "-", Expression::Subtraction);
parse_partial_binary_expression!(parse_partial_subtraction_assignment, "-=", Expression::SubtractionAssignment);
parse_partial_binary_expression!(parse_partial_multiplication, "*", Expression::Multiplication);
parse_partial_binary_expression!(parse_partial_multiplication_assignment, "*=", Expression::MultiplicationAssignment);
parse_partial_binary_expression!(parse_partial_division, "/", Expression::Division);
parse_partial_binary_expression!(parse_partial_division_assignment, "/=", Expression::DivisionAssignment);
parse_partial_binary_expression!(parse_partial_modulo, "%", Expression::Modulo);

pub fn parse_partial_binary_expression<'a, E>(input: &'a str, op: &'a str, prev_expression: &Expression) -> IResult<&'a str, (Expression, Expression), E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take(op.len())(input)?;
    let (input, _) = multispace0(input)?;

    //dbg!(input);
    let (input, rhs) = parse_expression(input)?;

    //dbg!(input);
    Ok((input, (prev_expression.clone(), rhs)))
}

pub fn parse_call<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
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
    ////dbg!(input);
    let (input, ident) = parse_external_ident(input)?;
    ////dbg!(input, &ident);
    let (input, _) = opt(multispace0)(input)?;
    let (input, args) = take_until_balanced("(", ")")(input)?;
    ////dbg!(args);
    let (args, _) = take(1_usize)(args)?;
    ////dbg!(args);
    let (_, args) = many0(parse_arg)(args)?;
    ////dbg!(&args);
    let call = Expression::Call { thread, name: ident, args };
    Ok((input, call))
}

pub fn parse_return<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
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

pub fn parse_wait<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = take("wait".len())(input)?;
    let (input, _) = multispace0(input)?;
    let (input, expr) = parse_expression(input)?;
    Ok((input, Expression::Wait(Box::new(expr))))
}

/// An abstract syntax tree for `GSC`.
/// 
/// Thankfully, it's very simple in structure. At top-level, `GSC` only has 
/// pseudo-preprocessor directives (`#include`, `#using_animtree`), which 
/// resemble C preprocessor directives but are really just part of the syntax,
/// and function definitions. There are no structure definitions, typedefs,
/// function declarations, etc.
/// 
/// Function bodies may contain one or more [`Statement`]s, which in turn may
/// contain one or more sub-[`Statement`]s or [`Expression`]s, which in turn may 
/// contain one or more sub-[`Expression`]s or [`Value`]s.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Ast {
    directives: Vec<Directive>,
    functions: Vec<FunctionDef>,
}

/// Parses a script (`.gsc`, `.csc`, etc.) and returns an [`Ast`].
pub fn parse_script(input: &str) -> Option<Ast> {
    let input = comment_strip::strip_comments(input.to_string(), comment_strip::CommentStyle::C, true).unwrap();
    let input = input.chars().filter(|c| *c != '\t').collect::<String>();
    parse_script_internal::<()>(&input).map(|(_, a)| a).ok()
}

pub fn parse_script_internal<'a, E>(input: &'a str) -> IResult<&'a str, Ast, E>
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

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub enum Directive {
    Include(String),
    UsingAnimtree(String),
}

pub fn parse_directives<'a, E>(input: &'a str) -> IResult<&'a str, Vec<Directive>, E>
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

/// A function definition. 
/// 
/// Contains the name of the function, its arguments list, and its body.
/// 
/// Function bodies may contain one or more [`Statement`]s, which in turn may
/// contain one or more sub-[`Statement`]s or [`Expression`]s, which in turn may 
/// contain one or more sub-[`Expression`]s or [`Value`]s.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct FunctionDef {
    name: Ident,
    args: ArgsList,
    body: Block,
}

pub fn parse_function_def<'a, E>(input: &'a str) -> IResult<&'a str, FunctionDef, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = multispace0(input)?;
    let (input, name) = parse_ident(input)?;
    ////dbg!(&name);
    let (input, args) = parse_args_list(input)?;
    let (input, body) = parse_block(input)?;

    Ok((input, FunctionDef { name, args, body }))
}

/// A list of arguments from a function declaration.
/// 
/// Can be empty.
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct ArgsList {
    pub args: Vec<Ident>,
}

pub fn parse_args_list<'a, E>(input: &'a str) -> IResult<&'a str, ArgsList, E>
where
  E: ParseError<&'a str>,
{
    let (input, list) = take_until_balanced("(", ")")(input)?;
    let (list, _) = take(1_usize)(list)?;
    let (_, args) = many0(parse_arg_decl)(list)?;

    Ok((input, ArgsList { args }))
}

pub fn parse_arg_decl<'a, E>(input: &'a str) -> IResult<&'a str, Ident, E>
where
  E: ParseError<&'a str>,
{
    let (input, arg) = parse_arg(input)?;
    //dbg!(&arg);
    let ident = match arg {
        Expression::Value(Value::Name(i)) => i,
        _ => unreachable!(),
    };

    Ok((input, ident))
}

pub fn parse_arg<'a, E>(input: &'a str) -> IResult<&'a str, Expression, E>
where
  E: ParseError<&'a str>,
{
    let (input, arg) = parse_comma_separated_list_item(input)?; 
    let (_, expr) = parse_expression(arg)?;
    Ok((input, expr))
}

/// A curly-brace-enclosed block of [`Statement`]s (function body, loop body, etc.).
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[derive(Clone, Debug)]
pub struct Block {
    statements: Vec<Statement>,
}

pub fn parse_block<'a, E>(input: &'a str) -> IResult<&'a str, Block, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = multispace0(input)?;
    let (input, body) = take_until_balanced("{", "}")(input)?;
    let body = &body[1..body.len() - 1];
    //dbg!(input, body);
    let (body, _) = take(1_usize)(body)?;
    //dbg!(body);
    let (_, statements) = many0(parse_statement)(body)?;

    Ok((input, Block { statements }))
}

pub fn parse_dev_block<'a, E>(input: &'a str) -> IResult<&'a str, CompoundStatement, E>
where
  E: ParseError<&'a str>,
{
    let (input, _) = multispace0(input)?;
    let (input, _) = take_until("/#")(input)?;
    let (input, _) = take("/#".len())(input)?;
    //let (input, body) = take_until("#/")(input)?;
    let (input, _) = take_until("#/")(input)?;
    let (input, _) = take("#/".len())(input)?;
    ////dbg!(body);
    //let (_, statements) = many0(parse_statement)(body)?;

    //Ok((input, CompoundStatement::DevBlock { body: statements }))
    Ok((input, CompoundStatement::DevBlock { body: vec![] }))
}