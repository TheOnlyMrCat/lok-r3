%{
#include <stdio.h>

enum TokenType {
	END = 0,
	LET,
	MUT,
	FN,
	EXTERN,
	CONST,
	RETURN,

	OPENPAR,
	CLOSEPAR,
	OPENBRACE,
	CLOSEBRACE,
	SEMICOLON,
	COMMA,
	STAR,
	EQUALS,

	IDENTIFIER,
	INTEGER,
	NEWLINES = 0x7E, // Doesn't get revealed to rust
	INVALID = 0x7F,
};

int skippedBytes = 0;
%}

%option noyywrap
%option nostdinit
%option nodefault

%x COMMENT
%%
	skippedBytes = 0;

"//".* {}
"/*" BEGIN(COMMENT);
<COMMENT>[^*\n]*        skippedBytes += yyleng;
<COMMENT>[^*\n]*\n      skippedBytes += yyleng;
<COMMENT>"*"+[^*/\n]*   skippedBytes += yyleng;
<COMMENT>"*"+[^*/\n]*\n skippedBytes += yyleng;
<COMMENT>"*"+"/"        skippedBytes += yyleng; BEGIN(INITIAL);

let return LET;
mut return MUT;
fn return FN;
extern return EXTERN;
const return CONST;
return return RETURN;

[[:alpha:]_][[:alnum:]_]* return IDENTIFIER;

[[:digit:]_]+ return INTEGER;

"(" return OPENPAR;
")" return CLOSEPAR;
"{" return OPENBRACE;
"}" return CLOSEBRACE;
";" return SEMICOLON;
"," return COMMA;
"*" return STAR;
"=" return EQUALS;

<<EOF>>  return END;
[ \t\n]+ skippedBytes += yyleng;
.        return INVALID;

%%

typedef struct {
	enum TokenType type;
	const char *textPtr;
	int textLeng;
	int skippedBytes;
} Token;

int setInput(const char *path) {
	FILE *file = fopen(path, "r");
	if (file) {
		yyin = file;
		return 0;
	} else {
		return errno;
	}
}

Token nextToken() {
	enum TokenType type = yylex();
	Token tk = {
		.type = type,
		.textPtr = yytext,
		.textLeng = yyleng,
		.skippedBytes = skippedBytes,
	};
	return tk;
}