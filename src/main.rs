use std::ffi::OsString;

use gscparse::{parse_expression, parse_function_def, parse_script, parse_statement};

fn main() {
    let path = std::env::args_os().nth(1).unwrap_or(OsString::from("./utility.gsc"));
    let source = std::fs::read_to_string(path).unwrap();
    //dbg!(&source);

    let test = "flag_wait( msg )
{
	while( !level.flag[ msg ] )
		level waittill( msg );
}";
    //let test = "line (start, end, (1,1,0.5));";
    //dbg!(parse_expression::<()>(test).unwrap());
    //dbg!(parse_statement::<(&str, nom::error::ErrorKind)>(test).unwrap());
    //dbg!(parse_function_def::<()>(test).unwrap());
    dbg!(parse_script(&source).unwrap());
}
