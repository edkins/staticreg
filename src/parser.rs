use nom::{
    IResult,
    AsChar,
    error::ErrorKind,
    branch::alt,
    bytes::complete::{tag,take_while_m_n,take_while,take_while1},
    character::complete::{multispace0},
    combinator::{all_consuming,not,opt,map,value},
    multi::{many0,separated_list,fold_many0},
    sequence::preceded,
};
use crate::ast::{Decl,Type,FieldDecl,ArgDecl,FuncArg,FuncImpl,Expr,Statement,AssignOp};

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

fn semicolon(input: &str) -> IResult<&str, ()> {
    sym(";")(input)
}

fn arrow(input: &str) -> IResult<&str, ()> {
    sym("->")(input)
}

fn dot(input: &str) -> IResult<&str, ()> {
    sym(".")(input)
}

fn numeroid_type(input: &str) -> IResult<&str, Type> {
    let (input,(num,suffix)) = numeroid(input)?;
    match suffix {
        "t" => Ok((input,Type::BoundedUint(num))),
        _ => Err(nom::Err::Failure((input, ErrorKind::Alt)))
    }
}

fn type_list(input: &str) -> IResult<&str, Vec<Type>> {
    let (input,_) = openp(input)?;
    let (input,ts) = many0(typ)(input)?;
    let (input,_) = closep(input)?;
    Ok((input,ts))
}

fn named_type(input: &str) -> IResult<&str, Type> {
    let (input,n) = name(input)?;
    let (input,args) = opt(type_list)(input)?;
    Ok((input,Type::Named(n,args)))
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

fn func_arg(input: &str) -> IResult<&str, FuncArg> {
    let (input,n) = name(input)?;
    if &n == "self" {
        let (input,t) = opt(preceded(arrow, typ))(input)?;
        if t.is_some() {
            let a = FuncArg::SelfTo(t.unwrap());
            Ok((input,a))
        } else {
            let a = FuncArg::JustSelf;
            Ok((input,a))
        }
    } else {
        let (input,_) = colon(input)?;
        let (input,t) = typ(input)?;
        let a = FuncArg::Arg(n,t);
        Ok((input,a))
    }
}

fn arg_list(input: &str) -> IResult<&str, Vec<ArgDecl>> {
    let (input,_) = openp(input)?;
    let (input,args) = separated_list(comma, arg_decl)(input)?;
    let (input,_) = closep(input)?;
    Ok((input,args))
}

fn func_arg_list(input: &str) -> IResult<&str, Vec<FuncArg>> {
    let (input,_) = openp(input)?;
    let (input,args) = separated_list(comma, func_arg)(input)?;
    let (input,_) = closep(input)?;
    Ok((input,args))
}

fn struct_decl(input: &str) -> IResult<&str, Decl> {
    let (input,_) = kw("struct")(input)?;
    let (input,n) = name(input)?;
    let (input,args) = opt(arg_list)(input)?;
    let (input,_) = open(input)?;
    let (input,fields) = separated_list(comma, field_decl)(input)?;
    let (input,_) = close(input)?;
    let d = Decl::Struct(n, args, fields);
    Ok((input,d))
}

fn trait_decl(input: &str) -> IResult<&str, Decl> {
    let (input,_) = kw("trait")(input)?;
    let (input,n) = name(input)?;
    let (input,_) = open(input)?;
    let (input,_) = close(input)?;
    let d = Decl::Trait(n);
    Ok((input,d))
}

fn func_body(input: &str) -> IResult<&str, Vec<Statement>> {
    let (input,_) = open(input)?;
    let (input,stmts) = many0(statement)(input)?;
    let (input,_) = close(input)?;
    Ok((input,stmts))
}

fn wheres(input: &str) -> IResult<&str, Vec<Expr>> {
    let (input,_) = kw("where")(input)?;
    let (input,exprs) = separated_list(comma,expr)(input)?;
    Ok((input,exprs))
}

fn func_impl(input: &str) -> IResult<&str, FuncImpl> {
    let (input,_) = kw("fn")(input)?;
    let (input,n) = name(input)?;
    let (input,args) = func_arg_list(input)?;
    let (input,_) = arrow(input)?;
    let (input,ret) = typ(input)?;
    let (input,wheres) = opt(wheres)(input)?;
    let (input,body) = func_body(input)?;
    let f = FuncImpl::new(n,args,ret,wheres,body);
    Ok((input,f))
}

fn impl_decl(input: &str) -> IResult<&str, Decl> {
    let (input,_) = kw("impl")(input)?;
    let (input,args) = opt(arg_list)(input)?;
    let (input,t) = typ(input)?;
    let (input,_) = open(input)?;
    let (input,funcs) = many0(func_impl)(input)?;
    let (input,_) = close(input)?;
    let d = Decl::Impl(args,t,funcs);
    Ok((input,d))
}

fn decl(input: &str) -> IResult<&str, Decl> {
    alt((struct_decl,trait_decl,impl_decl))(input)
}

fn assignment_op(input: &str) -> IResult<&str, AssignOp> {
    alt((
        value(AssignOp::PlusAssign, sym("+=")),
        value(AssignOp::Assign, sym("=")),
    ))(input)
}

fn statement(input: &str) -> IResult<&str, Statement> {
    let (input,e) = expr(input)?;
    let (input,op) = assignment_op(input)?;
    let (input,e2) = expr(input)?;
    let (input,_) = semicolon(input)?;
    Ok((input,Statement::Assign(e,op,e2)))
}

fn struct_initializer(input: &str) -> IResult<&str, (String, Box<Expr>)> {
    let (input,n) = name(input)?;
    let (input,_) = colon(input)?;
    let (input,e) = expr(input)?;
    Ok((input,(n,Box::new(e))))
}

fn struct_expr(input: &str) -> IResult<&str, Expr> {
    let (input,n) = name(input)?;
    let (input,_) = open(input)?;
    let (input,inits) = separated_list(comma, struct_initializer)(input)?;
    let (input,_) = close(input)?;
    Ok((input,Expr::Struct(n,inits)))
}

fn name_expr(input: &str) -> IResult<&str, Expr> {
    map(name, Expr::Name)(input)
}

fn numeroid_expr(input: &str) -> IResult<&str, Expr> {
    let (input,(num,suffix)) = numeroid(input)?;
    match suffix {
        "" => Ok((input,Expr::Num(num))),
        _ => Err(nom::Err::Failure((input, ErrorKind::Alt)))
    }
}

pub enum Suffix {
    Field(String),
    Equal(Expr),
}

fn suffix_expr(e: Expr, suffix: Suffix) -> Expr {
    match suffix {
        Suffix::Field(x) => Expr::Field(Box::new(e),x),
        Suffix::Equal(e1) => Expr::Equal(Box::new(e),Box::new(e1)),
    }
}

fn suffix_dot(input: &str) -> IResult<&str, Suffix> {
    let (input,_) = dot(input)?;
    let (input,n) = name(input)?;
    Ok((input,Suffix::Field(n)))
}

fn suffix_equals(input: &str) -> IResult<&str, Suffix> {
    let (input,_) = sym("==")(input)?;
    let (input,e) = expr(input)?;
    Ok((input,Suffix::Equal(e)))
}

fn suffix(input: &str) -> IResult<&str, Suffix> {
    alt((suffix_dot, suffix_equals))(input)
}

fn expr(input: &str) -> IResult<&str, Expr> {
    let (input,e) = alt((struct_expr, name_expr, numeroid_expr))(input)?;
    fold_many0(suffix, e, suffix_expr)(input)
}

pub fn parse_program(input: &str) -> Result<Vec<Decl>,nom::Err<(&str,ErrorKind)>> {
    Ok(all_consuming(|input| {
        let (input,_) = multispace0(input)?;
        many0(decl)(input)
    })(input)?.1)
}
