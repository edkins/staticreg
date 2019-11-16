#[derive(Debug)]
pub enum Decl {
    Struct(String, Option<Vec<ArgDecl>>, Vec<FieldDecl>),
    Trait(String),
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
    Named(String),
    BoundedUint(usize),
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
