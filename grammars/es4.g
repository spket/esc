%Options lalr=2
%Options programming_language=cpp
%Options table,error_maps,scopes,nobacktrack
%Options prefix=TK_
%Options package=asc
%Options action-block=("*act.cpp", "/.", "./")
%Options action-block=("*init.cpp", "/:", ":/")
%Options action-block=("*act.h", "/!", "!/")

--%options first,follow
--%options states
--%options names=MAX
--%Options SINGLE-PRODUCTIONS
--%options VERBOSE
--%Options trace=conflicts
--%Options trace=full

%Include
	action.g
%End

%Terminals
	break case catch class continue default delete do else extends false
	finally for function if in instanceof new null return super switch this throw
	true try typeof var void while with

	const each get implements import interface is let namespace
	package private protected public set use xml yield
	
	as include
	
	identifier

	StringLiteral
	NumberLiteral
	
	XMLName
	XMLPI
	XMLText
	XMLCDATA
	XMLComment
	XMLAttributeValue

	CLOSURE
	NO_LINE_BREAK
	VirtualSemicolon

	PLUS_PLUS
	MINUS_MINUS
	EQUAL_EQUAL
	LESS_EQUAL
	GREATER_EQUAL
	NOT_EQUAL
	LEFT_SHIFT
	RIGHT_SHIFT
	UNSIGNED_RIGHT_SHIFT
	PLUS_EQUAL
	MINUS_EQUAL
	MULTIPLY_EQUAL
	DIVIDE_EQUAL
	AND_EQUAL
	OR_EQUAL
	XOR_EQUAL
	REMAINDER_EQUAL
	LEFT_SHIFT_EQUAL
	RIGHT_SHIFT_EQUAL
	UNSIGNED_RIGHT_SHIFT_EQUAL
	OR_OR
	AND_AND
	PLUS
	MINUS
	NOT
	REMAINDER
	XOR
	AND
	MULTIPLY
	OR
	TWIDDLE
	DIVIDE
	GREATER
	LESS
	LPAREN
	RPAREN
	LBRACE
	RBRACE
	LBRACKET
	RBRACKET
	SEMICOLON
	QUESTION
	COLON
	COMMA
	EQUAL
	AT

	DOT
	DOT_DOT
	DOT_LESS
	ELLIPSIS
	
	--XOR_XOR
	--XOR_XOR_EQUAL
	COLON_COLON
	AND_AND_EQUAL
	OR_OR_EQUAL
	IDENTICAL
	NOT_IDENTICAL
	
	TAG_END
	TAG_EMPTY

%End

%Alias

	'++'   ::= PLUS_PLUS
	'--'   ::= MINUS_MINUS
	'=='   ::= EQUAL_EQUAL
	'<='   ::= LESS_EQUAL
	'>='   ::= GREATER_EQUAL
	'!='   ::= NOT_EQUAL
	'<<'   ::= LEFT_SHIFT
	'>>'   ::= RIGHT_SHIFT
	'>>>'  ::= UNSIGNED_RIGHT_SHIFT
	'+='   ::= PLUS_EQUAL
	'-='   ::= MINUS_EQUAL
	'*='   ::= MULTIPLY_EQUAL
	'/='   ::= DIVIDE_EQUAL
	'&='   ::= AND_EQUAL
	'|='   ::= OR_EQUAL
	'^='   ::= XOR_EQUAL
	'%='   ::= REMAINDER_EQUAL
	'<<='  ::= LEFT_SHIFT_EQUAL
	'>>='  ::= RIGHT_SHIFT_EQUAL
	'>>>=' ::= UNSIGNED_RIGHT_SHIFT_EQUAL
	'||'   ::= OR_OR
	'&&'   ::= AND_AND
	'+'    ::= PLUS
	'-'    ::= MINUS
	'!'    ::= NOT
	'%'    ::= REMAINDER
	'^'    ::= XOR
	'&'    ::= AND
	'*'    ::= MULTIPLY
	'|'    ::= OR
	'~'    ::= TWIDDLE
	'/'    ::= DIVIDE
	'>'    ::= GREATER
	'<'    ::= LESS
	'('    ::= LPAREN
	')'    ::= RPAREN
	'{'    ::= LBRACE
	'}'    ::= RBRACE
	'['    ::= LBRACKET
	']'    ::= RBRACKET
	';'    ::= SEMICOLON
	'?'    ::= QUESTION
	','    ::= COMMA
	'='    ::= EQUAL
	'@'	   ::= AT

	'.'    ::= DOT
	'..'   ::= DOT_DOT
	'.<'   ::= DOT_LESS
	'...'  ::= ELLIPSIS
	
	':'    ::= COLON
	'::'   ::= COLON_COLON
	
	--'^^'   ::= XOR_XOR
	--'^^='  ::= XOR_XOR_EQUAL
	'&&='  ::= AND_AND_EQUAL
	'||='  ::= OR_OR_EQUAL
	'==='  ::= IDENTICAL
	'!=='  ::= NOT_IDENTICAL
	
	'</'   ::= TAG_END
	'/>'   ::= TAG_EMPTY
%End

%EOF
	EOF_TOKEN
%End

%ERROR
	ERROR_TOKEN
%End

%EOL
	;
%End

%Start

	Goal

%End

%Rules
	/.$BeginActions./
%End

%Include
	esi.g
%End

%Trailers
	/.$EndActions./
%End

%Names

PLUS_PLUS ::=    '++'
MINUS_MINUS ::=    '--'
EQUAL_EQUAL ::=    '=='
LESS_EQUAL ::=    '<='
GREATER_EQUAL ::=    '>='
NOT_EQUAL ::=    '!='
LEFT_SHIFT ::=    '<<'
RIGHT_SHIFT ::=    '>>'
UNSIGNED_RIGHT_SHIFT ::=    '>>>'
PLUS_EQUAL ::=    '+='
MINUS_EQUAL ::=    '-='
MULTIPLY_EQUAL ::=    '*='
DIVIDE_EQUAL ::=    '/='
AND_EQUAL ::=    '&='
OR_EQUAL ::=    '|='
XOR_EQUAL ::=    '^='
REMAINDER_EQUAL ::=    '%='
LEFT_SHIFT_EQUAL ::=    '<<='
RIGHT_SHIFT_EQUAL ::=    '>>='
UNSIGNED_RIGHT_SHIFT_EQUAL ::=    '>>>='
OR_OR ::=    '||'
AND_AND ::=    '&&'
PLUS ::=    '+'
MINUS ::=    '-'
NOT ::=    '!'
REMAINDER ::=    '%'
XOR ::=    '^'
AND ::=    '&'
MULTIPLY ::=    '*'
OR ::=    '|'
TWIDDLE ::=    '~'
DIVIDE ::=    '/'
GREATER ::=    '>'
LESS ::=    '<'
LPAREN ::=    '('
RPAREN ::=    ')'
LBRACE ::=    '{'
RBRACE ::=    '}'
LBRACKET ::=    '['
RBRACKET ::=    ']'
SEMICOLON ::=    ';'
QUESTION ::=    '?'
COLON ::=    ':'
COMMA ::=    ','
EQUAL ::=    '='
AT ::=    '@'

DOT      ::=    '.'
DOT_DOT  ::= '..'
DOT_LESS ::= '.<'
ELLIPSIS ::=    '...'

COLON_COLON   ::= '::'
--XOR_XOR       ::= '^^'
--XOR_XOR_EQUAL ::= '^^='
AND_AND_EQUAL ::= '&&='
OR_OR_EQUAL   ::= '||='
IDENTICAL     ::= '==='
NOT_IDENTICAL ::= '!=='

TAG_END   ::= '</'
TAG_EMPTY ::= '/>'

%End

%Include
	names.g
%End
