#[derive(Debug)]
pub struct SyntaxTree {
	pub decls: DeclSet,
	pub module_name: String,
}

pub type Identifier = String;

#[derive(Debug)]
pub enum TypeRef {
	Name(Identifier),
	ConstPtr(Box<TypeRef>),
	MutPtr(Box<TypeRef>),
	Array(Box<TypeRef>, usize),
}

#[derive(Debug)]
pub struct DeclSet {
	pub functions: Vec<FuncDecl>,
}

#[derive(Debug)]
pub enum FuncDecl {
	Extern(ExternFunction),
	Intern(Function),
}

#[derive(Debug)]
pub struct ExternFunction {
	pub name: Identifier,
	pub returns: Option<TypeRef>,
	pub params: Vec<TypeRef>,
	//? abi: ¯\_(ツ)_/¯
}

#[derive(Debug)]
pub struct Function {
	pub name: Identifier,
	pub returns: Option<TypeRef>,
	pub params: Vec<(TypeRef, Identifier)>,
	pub body: Block,
}

#[derive(Debug)]
pub struct Block {
	pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
	Eval(Expression),
	Return(Option<Expression>),
	Let {
		expected_type: Option<TypeRef>,
		var: Identifier,
		val: Expression,
		is_mut: bool,
	},
}

#[derive(Debug)]
pub enum Expression {
	Int(i32),
	Float(f64),
	Var(Identifier),
	Array(Vec<Expression>),
	Call(Identifier, Vec<Box<Expression>>),
	Add(Box<Expression>, Box<Expression>),
	Sub(Box<Expression>, Box<Expression>),
	Mul(Box<Expression>, Box<Expression>),
	Div(Box<Expression>, Box<Expression>),
	Mod(Box<Expression>, Box<Expression>),
}