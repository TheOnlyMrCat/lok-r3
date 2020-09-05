use lalrpop_util::lalrpop_mod;

mod lexer;
use lexer::Lexer;

mod llvm;
use llvm::Compiler;

mod syntax;

lalrpop_mod!(pub parser);
use parser::LokParser;

fn main() {
	let filename = std::env::args().nth(1).unwrap();
	let parser = LokParser::new();
	let lexer = Lexer::new(&filename);
	let compiler = Compiler::new();
	let lok = parser.parse(&filename, lexer).unwrap();
	let module = lok.codegen(&compiler);

	let mut f = module.get_first_function();
	while let Some(func) = f {
		println!("{}", func.print_to_string().to_string());
		f = func.get_next_function();
	}

	compiler.assemble_module(&module);
}