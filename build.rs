fn main() {
	println!("cargo:rerun-if-changed=src/lexer.l");
	println!("cargo:rerun-if-changed=src/parser.lalrpop");

	cc::Build::new()
		.file("src/gen/lexer.c")
		.static_flag(true)
		.compile("lexer")
		;

	lalrpop::process_root().unwrap();
}