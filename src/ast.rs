#[derive(Debug)]
pub enum Decl {
    Struct(String, Option<Vec<ArgDecl>>, Vec<FieldDecl>),
    Trait(String),
    Impl(Option<Vec<ArgDecl>>, Type, Vec<FuncImpl>),
}

#[derive(Debug)]
pub struct ArgDecl {
    name: String,
    typ: Type
}

#[derive(Debug)]
pub struct FieldDecl {
    name: String,
    typ: Type
}

#[derive(Debug)]
pub enum Type {
    Named(String,Option<Vec<Type>>),
    BoundedUint(usize),
}

#[derive(Debug)]
pub enum FuncArg {
    Arg(String,Type),
    JustSelf,
    SelfTo(Type),
}

#[derive(Debug)]
pub struct FuncImpl {
    name: String,
    args: Vec<FuncArg>,
    ret: Type,
    wheres: Option<Vec<Expr>>,
    body: Vec<Statement>
}

#[derive(Debug)]
pub enum Statement {
    Assign(Expr,AssignOp,Expr)
}

#[derive(Debug,Clone)]
pub enum AssignOp {
    Assign,
    PlusAssign,
}

#[derive(Debug,Clone)]
pub enum Expr {
    Struct(String,Vec<(String,Box<Expr>)>),
    Field(Box<Expr>,String),
    Name(String),
    Num(usize),
    Equal(Box<Expr>,Box<Expr>),
}

impl FuncImpl {
    pub fn new(name: String, args: Vec<FuncArg>, ret: Type, wheres: Option<Vec<Expr>>, body: Vec<Statement>) -> Self {
        FuncImpl{name,args,ret,wheres,body}
    }
}

impl ArgDecl {
    pub fn new(name: String, typ: Type) -> Self {
        ArgDecl{
            name,
            typ
        }
    }
}

impl FieldDecl {
    pub fn new(name: String, typ: Type) -> Self {
        FieldDecl{
            name,
            typ
        }
    }
}
