use std::collections::HashMap;

use crate::syntax::*;

use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::passes::PassManager;
use inkwell::module::{Module, Linkage};
use inkwell::targets::{Target, TargetMachine, RelocMode, CodeModel, FileType, InitializationConfig};
use inkwell::types::{BasicType, StructType, BasicTypeEnum};
use inkwell::values::{FunctionValue, PointerValue, StructValue, BasicValueEnum};
use inkwell::{AddressSpace, OptimizationLevel};

pub struct Compiler {
	pub llvm: Context,
	target: Target,
	target_machine: TargetMachine,
}

impl Compiler {
	#[inline]
	pub fn new() -> Compiler {
		Compiler::with_context(Context::create())
	}

	pub fn with_context(context: Context) -> Compiler {
		Target::initialize_all(&InitializationConfig {
			asm_printer: true,
			asm_parser: true,
			base: true,
			disassembler: false,
			info: true,
			machine_code: true,
		});
		let triple = TargetMachine::get_default_triple();
		let target = Target::from_triple(&triple).unwrap();
		println!("triple: {}", triple);
		println!("features: {}", &TargetMachine::get_host_cpu_features().to_string());
		let machine = target.create_target_machine(&triple, "generic", &TargetMachine::get_host_cpu_features().to_string(), OptimizationLevel::None, RelocMode::Default, CodeModel::Default).unwrap();
		Compiler {
			llvm: context,
			target,
			target_machine: machine,
		}
	}

	pub fn assemble_module<'ctx>(&'ctx self, module: &Module<'ctx>) {
		self.target_machine.write_to_file(module, FileType::Object, "output.o".as_ref()).unwrap();
	}

	fn pass_over<'ctx>(&'ctx self, module: &Module<'ctx>) {
		let pass_manager: PassManager<Module> = PassManager::create(());

		// Passes as from the Kaleidoscope tutorial (but the commented ones segfault for some reason)
		pass_manager.add_instruction_combining_pass();
		pass_manager.add_reassociate_pass();
		pass_manager.add_gvn_pass();
		pass_manager.add_cfg_simplification_pass();

		pass_manager.run_on(module);
	}

	fn get_type<'ctx>(&'ctx self, type_ref: &TypeRef) -> BasicTypeEnum<'ctx> {
		match type_ref {
			TypeRef::MutPtr(internal) |
			TypeRef::ConstPtr(internal) => self.get_type(&internal).ptr_type(AddressSpace::Generic).into(),
			TypeRef::Array(internal, size) => self.get_type(internal).array_type(*size as u32).into(),
			TypeRef::Name(id) => match &**id {
				"i32" => self.i32_type().into(),
				"f64" => self.f64_type().into(),
				_ => todo!(),
			}
		}
	}

	fn extrapolate_type<'ctx>(&'ctx self, module: &Module<'ctx>, expr: &Expression) -> BasicTypeEnum<'ctx> {
		match expr {
			Expression::Int(_) => self.i32_type().into(),
			Expression::Call(id, ..) => module.get_function(&id).unwrap().get_type().get_return_type().unwrap(),
			_ => todo!()
		}
	}

	fn get_zero_type<'ctx>(&'ctx self) -> StructType<'ctx> {
		self.struct_type(&[], false)
	}

	fn get_zero_value<'ctx>(&'ctx self) -> StructValue<'ctx> {
		self.const_struct(&[], false)
	}
}

impl std::ops::Deref for Compiler {
	type Target = Context;

	fn deref(&self) -> &Context {
		&self.llvm
	}
}

impl SyntaxTree {
	pub fn codegen<'ctx>(&self, compiler: &'ctx Compiler) -> Module<'ctx> {
		let module = compiler.llvm.create_module(&self.module_name);
		module.set_data_layout(&compiler.target_machine.get_target_data().get_data_layout());
		module.set_triple(&compiler.target_machine.get_triple());

		self.decls.codegen(compiler, &module);
		// compiler.pass_over(&module);
		module
	}
}

impl DeclSet {
	fn codegen<'ctx>(&self, compiler: &'ctx Compiler, module: &Module<'ctx>) {
		// Do structs first, but they don't exist yet

		// Functions; first know what they all are
		let mut functions: Vec<(&Function, FunctionValue<'ctx>)> = vec![];
		for func in &self.functions {
			match func {
				FuncDecl::Extern(decl) => {
					let args = decl.params.iter().map(|ty| compiler.get_type(ty)).collect::<Vec<BasicTypeEnum<'ctx>>>();
					module.add_function(
						&decl.name,
						match &decl.returns {
							Some(ty) => compiler.get_type(&ty).fn_type(&args, false),
							None => compiler.void_type().fn_type(&args, false),
						},
						Some(Linkage::External),
					);
				},
				FuncDecl::Intern(decl) => {
					let args = decl.params.iter().map(|(ty, _)| compiler.get_type(ty)).collect::<Vec<BasicTypeEnum<'ctx>>>();
					let function = module.add_function(
						&decl.name,
						match &decl.returns {
							Some(ty) => compiler.get_type(&ty).fn_type(&args, false),
							None => compiler.void_type().fn_type(&args, false),
						},
						Some(Linkage::External),
					);
					functions.push((decl, function));
				}
			}
		}

		// Then evaluate them
		for (func, value) in functions {
			func.codegen(compiler, module, value);
		}
	}
}

impl Function {
	fn codegen<'ctx>(&self, compiler: &'ctx Compiler, module: &Module<'ctx>, value: FunctionValue<'ctx>) {
		self.body.codegen(compiler, module, value, "entry");
	}
}

impl Block {
	fn codegen<'ctx>(&self, compiler: &'ctx Compiler, module: &Module<'ctx>, value: FunctionValue<'ctx>, name: &str) {
		let builder = compiler.create_builder();
		let block = compiler.append_basic_block(value, name);
		builder.position_at_end(block);

		let mut variables = HashMap::new();
		let mut has_returned = false;

		for statement in &self.statements {
			assert!(!has_returned, "Statement after return");
			match statement {
				Statement::Let { var: name, val, expected_type, .. } => {
					if let Some(i) = block.get_first_instruction() {
						builder.position_before(&i);
					} // Otherwise the builder's already at the start anyway
					let expr_value = val.codegen(compiler, module, value, &builder, &variables).unwrap();
					if let Some(ty) = expected_type {
						assert_eq!(compiler.get_type(ty), expr_value.get_type(), "Expression type doesn't match expected type!"); // basically an unwrap (this comment is here just so I can search for "unwrap" when implementing errors properly)
					}
					let ptr = builder.build_alloca(expr_value.get_type(), &name);
					builder.position_at_end(block);
					builder.build_store(ptr, expr_value);
					variables.insert(name.clone(), ptr);
				},
				Statement::Eval(expr) => {
					expr.codegen(compiler, module, value, &builder, &variables);
				},
				Statement::Return(expr) => {
					match expr {
						Some(x) => {
							let expr_value = x.codegen(compiler, module, value, &builder, &variables).unwrap();
							assert_eq!(expr_value.get_type(), value.get_type().get_return_type().unwrap());
							builder.build_return(Some(&expr_value));
						},
						None => {
							builder.build_return(None);
						},
					}
					has_returned = true;
				}
				_ => todo!()
			}
		}

		if !has_returned {
			builder.build_return(None);
		}
	}
}

impl Expression {
	fn codegen<'ctx>(&self, compiler: &'ctx Compiler, module: &Module<'ctx>, function: FunctionValue<'ctx>, builder: &Builder<'ctx>, variables: &HashMap<String, PointerValue<'ctx>>) -> Option<BasicValueEnum<'ctx>> {
		match self {
			Expression::Int(i) => Some(BasicValueEnum::IntValue(compiler.i32_type().const_int(*i as u64, true))), //TODO Other int types???
			Expression::Float(f) => Some(BasicValueEnum::FloatValue(compiler.f64_type().const_float(*f))),
			Expression::Call(name, args) => {
				let callee = module.get_function(&name).unwrap();
				let arguments = args.iter().map(|expr| expr.codegen(compiler, module, function, builder, variables)).collect::<Option<Vec<_>>>()?;
				builder.build_call(callee, &arguments, "calltmp").try_as_basic_value().left()
			},
			Expression::Var(id) => Some(builder.build_load(*variables.get(id).unwrap(), "loadtmp")),
			Expression::Add(a, b) => {
				let side_a = a.codegen(compiler, module, function, builder, variables).unwrap();
				let side_b = b.codegen(compiler, module, function, builder, variables).unwrap();
				match side_a.get_type() {
					BasicTypeEnum::IntType(t_a) => {
						match side_b.get_type() {
							BasicTypeEnum::IntType(t_b) => {
								assert_eq!(t_a, t_b, "Cannot add integers of different sizes");
								Some(builder.build_int_add(side_a.into_int_value(), side_b.into_int_value(), "addtmp").into())
							},
							_ => todo!(),
						}
					},
					BasicTypeEnum::FloatType(t_a) => {
						match side_b.get_type() {
							BasicTypeEnum::FloatType(t_b) => {
								assert_eq!(t_a, t_b, "Cannot add floats of different sizes");
								Some(builder.build_float_add(side_a.into_float_value(), side_b.into_float_value(), "addtmp").into())
							},
							_ => todo!(),
						}
					},
					_ => todo!(),
				}
			}
			_ => todo!(),
		}
	}
}