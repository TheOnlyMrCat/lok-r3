use std::ffi::CString;
use std::os::raw::{c_char, c_uchar, c_int};

#[repr(C)]
#[allow(dead_code)] // The code is NOT dead
enum CTokenType {
	Eof = 0,
	Let,
	Mut,
	Fn,
	Extern,
	Const,
	Return,

	OpenPar,
	ClosePar,
	OpenBra,
	CloseBra,
	Semicolon,
	Comma,
	Star,
	Equals,

	Identifier,
	Integer,
	Invalid = 0x7F,
}

#[repr(C)]
struct CToken {
	token_type: CTokenType,
	text_ptr: *const c_uchar,
	text_leng: c_int,
	skipped_bytes: c_int,
}

#[link(name = "lexer", kind = "static")]
extern {
	fn setInput(file_name: *const c_char) -> c_int;
	fn nextToken() -> CToken;
}

#[derive(Clone, Debug)]
pub enum Token {
	KWLet,
	KWMut,
	KWFn,
	KWExtern,
	KWConst,
	KWReturn,
	SOP,
	SCP,
	SOB,
	SCB,
	SSC,
	SCM,
	SST,
	SSQ,
	Identifier(String),
	Integer(i32),
}

#[derive(Clone, Debug)]
pub enum LexicalError {
	InvalidToken(String),
	InvalidEncoding,
}

/// THIS STRUCT IS NOT RE-ENTRANT!
/// The underlying scanner relies on global data to scan and return results. Creating a second Lexer
/// when the first has not run completely will give unexpected results. This will be fixed eventually,
/// but is not necessary currently.
pub struct Lexer {
	loc: usize,
}

impl Lexer {
	pub fn new(file_name: &str) -> Lexer {
		let c_file_name = CString::new(file_name).unwrap();
		unsafe { setInput(c_file_name.as_ptr()) };
		Lexer {
			loc: 0,
		}
	}
}

impl Iterator for Lexer {
	type Item = Result<(usize, Token, usize), LexicalError>;

	fn next(&mut self) -> Option<Self::Item> {
		let tk = unsafe { nextToken() };
		self.loc += tk.skipped_bytes as usize;
		let start_loc = self.loc;
		let text = match std::str::from_utf8(unsafe { std::slice::from_raw_parts(tk.text_ptr, tk.text_leng as usize) }) {
			Ok(s) => s,
			Err(_) => return Some(Err(LexicalError::InvalidEncoding)),
		};
		self.loc += text.len();
		match tk.token_type {
			CTokenType::Eof => None,
			CTokenType::Invalid => Some(Err(LexicalError::InvalidToken(text.to_owned()))),

			CTokenType::Identifier => Some(Ok((start_loc, Token::Identifier(text.to_owned()), self.loc))),
			CTokenType::Integer => Some(Ok((start_loc, Token::Integer({
				let mut t = text.to_owned();
				t.retain(|c| c != '_');
				i32::from_str_radix(&t, 10).unwrap()
			}), self.loc))),

			CTokenType::Let => Some(Ok((start_loc, Token::KWLet, self.loc))),
			CTokenType::Mut => Some(Ok((start_loc, Token::KWMut, self.loc))),
			CTokenType::Fn => Some(Ok((start_loc, Token::KWFn, self.loc))),
			CTokenType::Extern => Some(Ok((start_loc, Token::KWExtern, self.loc))),
			CTokenType::Const => Some(Ok((start_loc, Token::KWConst, self.loc))),
			CTokenType::Return => Some(Ok((start_loc, Token::KWReturn, self.loc))),

			CTokenType::OpenPar => Some(Ok((start_loc, Token::SOP, self.loc))),
			CTokenType::ClosePar => Some(Ok((start_loc, Token::SCP, self.loc))),
			CTokenType::OpenBra => Some(Ok((start_loc, Token::SOB, self.loc))),
			CTokenType::CloseBra => Some(Ok((start_loc, Token::SCB, self.loc))),
			CTokenType::Semicolon => Some(Ok((start_loc, Token::SSC, self.loc))),
			CTokenType::Comma => Some(Ok((start_loc, Token::SCM, self.loc))),
			CTokenType::Star => Some(Ok((start_loc, Token::SST, self.loc))),
			CTokenType::Equals => Some(Ok((start_loc, Token::SSQ, self.loc))),
		}
	}
}
