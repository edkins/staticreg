use nom::{
    IResult,
    AsChar,
    error::ErrorKind,
    branch::alt,
    bytes::complete::{tag,take_while_m_n,take_while,take_while1},
    character::complete::{multispace0},
    combinator::{all_consuming,not,cut,opt,map},
    multi::{many0,separated_list},
};
use crate::ast::{Decl,Type,FieldDecl,ArgDecl};

fn alphanumunder(c: char) -> bool {
    c.is_alphanum() || c == '_'
}

fn kw<'a>(word: &'a str) -> impl Fn(&str) -> IResult<&str, ()>+'a {
    move|input| {
        let (input, _) = tag(word)(input)?;
        let (input, _) = not(take_while_m_n(1,1,alphanumunder))(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input,()))
    }
}

fn sym<'a>(word: &'a str) -> impl Fn(&str) -> IResult<&str, ()>+'a {
    move|input| {
        let (input, _) = tag(word)(input)?;
        let (input, _) = multispace0(input)?;
        Ok((input,()))
    }
}

fn name(input: &str) -> IResult<&str, String> {
    let (input,head) = take_while_m_n(1,1,AsChar::is_alpha)(input)?;
    let (input,tail) = take_while(alphanumunder)(input)?;
    let (input,_) = multispace0(input)?;
    let mut result = String::with_capacity(head.len() + tail.len());
    result.push_str(head);
    result.push_str(tail);
    Ok((input,result))
}

fn numeroid(input: &str) -> IResult<&str, (usize,&str)> {
    let (input,head) = take_while1(AsChar::is_dec_digit)(input)?;
    let (input,tail) = take_while(alphanumunder)(input)?;
    let (input,_) = multispace0(input)?;
    match head.parse() {
        Ok(n) => Ok((input,(n,tail))),
        Err(_) => Err(nom::Err::Failure((input,ErrorKind::AlphaNumeric)))
    }
}

fn open(input: &str) -> IResult<&str, ()> {
    sym("{")(input)
}

fn close(input: &str) -> IResult<&str, ()> {
    sym("}")(input)
}

fn openp(input: &str) -> IResult<&str, ()> {
    sym("(")(input)
}

fn closep(input: &str) -> IResult<&str, ()> {
    sym(")")(input)
}

fn comma(input: &str) -> IResult<&str, ()> {
    sym(",")(input)
}

fn colon(input: &str) -> IResult<&str, ()> {
    sym(":")(input)
}

fn numeroid_type(input: &str) -> IResult<&str, Type> {
    let (input,(num,suffix)) = numeroid(input)?;
    match suffix {
        "t" => Ok((input,Type::BoundedUint(num))),
        _ => Err(nom::Err::Failure((input, ErrorKind::Alt)))
    }
}

fn named_type(input: &str) -> IResult<&str, Type> {
    map(name, Type::Named)(input)
}

fn typ(input: &str) -> IResult<&str, Type> {
    alt((numeroid_type,named_type))(input)
}

fn field_decl(input: &str) -> IResult<&str, FieldDecl> {
    let (input,n) = name(input)?;
    let (input,_) = colon(input)?;
    let (input,t) = typ(input)?;
    let f = FieldDecl::new(n,t);
    Ok((input,f))
}

fn arg_decl(input: &str) -> IResult<&str, ArgDecl> {
    let (input,n) = name(input)?;
    let (input,_) = colon(input)?;
    let (input,t) = typ(input)?;
    let a = ArgDecl::new(n,t);
    Ok((input,a))
}

fn arg_list(input: &str) -> IResult<&str, Vec<ArgDecl>> {
    let (input,_) = openp(input)?;
    let (input,args) = separated_list(comma, cut(arg_decl))(input)?;
    let (input,_) = closep(input)?;
    Ok((input,args))
}

fn struct_decl(input: &str) -> IResult<&str, Decl> {
    let (input,_) = kw("struct")(input)?;
    cut(|input|{
        let (input,n) = name(input)?;
        let (input,args) = opt(arg_list)(input)?;
        let (input,_) = open(input)?;
        let (input,fields) = separated_list(comma, cut(field_decl))(input)?;
        let (input,_) = close(input)?;
        let d = Decl::Struct(n, args, fields);
        Ok((input,d))
    })(input)
}

fn trait_decl(input: &str) -> IResult<&str, Decl> {
    let (input,_) = kw("trait")(input)?;
    let (input,n) = name(input)?;
    let (input,_) = open(input)?;
    let (input,_) = close(input)?;
    let d = Decl::Trait(n);
    Ok((input,d))
}

fn decl(input: &str) -> IResult<&str, Decl> {
    alt((struct_decl,trait_decl))(input)
}

pub fn parse_program(input: &str) -> Result<Vec<Decl>,nom::Err<(&str,ErrorKind)>> {
    Ok(all_consuming(|input| {
        let (input,_) = multispace0(input)?;
        many0(decl)(input)
    })(input)?.1)
}
