%Rules

Goal ::= ')' Program
/.$NoAction./

Program ::= Directives
/:$Set$MakeProgram;:/
/!
	void MakeProgram();!/
/.$Location
void Parser::MakeProgram()
{
	Program* program = astPool->newProgram();
	LIST2ARRAY(Statement, 1)
	program->statements = list;
	sym(1) = program;
}
./

Directives ::= %Empty
/.$NullAction./
	| DirectivesPrefix Directive_abbrev
/:$Set$AppendList;:/

DirectivesPrefix ::= %Empty
/.$NullAction./
	| DirectivesPrefix Directive_full
/:$Set$AppendList;:/

Directive_full ::= EmptyStatement
/.$NoAction./
	| Statement_full
/.$NoAction./
	| SuperStatement_full
/.$NoAction./
	| AnnotatableDirective_full
/.$NoAction./
	| Attributes AnnotatableDirective_full
/:$Set$MakeAttributes;:/
/!
	void MakeAttributes();!/
/.$Location
void Parser::MakeAttributes()
{
	AttributeStatement* astmt = CAST<AttributeStatement*>(sym(2));
	LIST2ARRAY(Expression, 1);
	astmt->attributes = list;
}
./
	| Attributes Block
/:$Set$MakeAttributes;:/

EmptyStatement ::= ';'
/:$Set$MakeEmptyStatement;:/
/!
	void MakeEmptyStatement();!/
/.$Location
void Parser::MakeEmptyStatement()
{
	EmptyStatement* es = astPool->newEmptyStatement(token(1));
	es->semicolon = token(1);
	sym(1) = es;
}
./

Statement_full ::= Block
/.$NoAction./
	| BreakStatement Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| ContinueStatement Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| DefaultXMLNamespaceStatement Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| DoStatement Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| ExpressionStatement Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| ForStatement_full
/.$NoAction./
	| IfStatement_full
/.$NoAction./
	| LabeledStatement_full
/.$NoAction./
	| LetBlockStatement
/.$NoAction./
	| ReturnStatement Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| SwitchStatement
/.$NoAction./
	| ThrowStatement Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| TryStatement
/.$NoAction./
	| WhileStatement_full
/.$NoAction./
	| WithStatement_full
/.$NoAction./

Block ::= '{' Directives '}'
/:$Set$MakeBlock;:/
/!
	void MakeBlock();!/
/.$Location
void Parser::MakeBlock()
{
	Block* block = astPool->newBlock();
	block->leftBrace = token(1);
	LIST2ARRAY(Statement, 2)
	block->statements = list;
	block->rightBrace = token(3);
	sym(1) = block;
}
./

BreakStatement ::= 'break' Marker Marker
/:$Set$MakeBreakStatement;:/
/!
	void MakeBreakStatement();!/
/.$Location
void Parser::MakeBreakStatement()
{
	BreakStatement* bs = astPool->newBreakStatement();
	//
	sym(1) = bs;
}
./
	| 'break' NO_LINE_BREAK Identifier
/:$Set$MakeBreakStatement;:/

Marker ::= %Empty
/.$NullAction./

Identifier ::= 'identifier'
/:$Set$MakeIdentifier;:/
/!
	void MakeIdentifier();!/
/.$Location
void Parser::MakeIdentifier()
{
	sym(1) = astPool->newIdentifier(token(1));
}
./
	| ContextuallyReservedIdentifier
/:$Set$MakeIdentifier;:/

ContextuallyReservedIdentifier ::= 'each'
/.$NoAction./
	| 'extends'
/.$NoAction./
	| 'get'
/.$NoAction./
	| 'implements'
/.$NoAction./
	| 'namespace'
/.$NoAction./
	| 'set'
/.$NoAction./
	| 'xml'
/.$NoAction./

Semicolon_full ::= ';'
/.$NoAction./
	| VirtualSemicolon
/.$NoAction./

ContinueStatement ::= 'continue' Marker Marker
/:$Set$MakeContinueStatement;:/
/!
	void MakeContinueStatement();!/
/.$Location
void Parser::MakeContinueStatement()
{
	ContinueStatement* cs = astPool->newContinueStatement();
	//cs-> = token(3);
	sym(1) = cs;
}
./
	| 'continue' NO_LINE_BREAK Identifier
/:$Set$MakeContinueStatement;:/

DefaultXMLNamespaceStatement ::= 'default' 'xml' 'namespace' '=' CommaExpression_allowIn_FULL
/:$Set$MakeDefaultXMLNamespaceStatement;:/
/!
	void MakeDefaultXMLNamespaceStatement();!/
/.$Location
void Parser::MakeDefaultXMLNamespaceStatement()
{
	//TODO for AS3 only NonAssignmentExpression[allowIn] is allowed
}
./

CommaExpression_allowIn_FULL ::= AssignmentExpression_allowIn_FULL
/:$Set$StartList;:/
	| CommaExpression_allowIn_FULL ',' AssignmentExpression_allowIn_FULL
/:$Set$AddList2;:/

AssignmentExpression_allowIn_FULL ::= ConditionalExpression_allowIn_FULL
/.$NoAction./
	| LeftHandSideExpression_allowIn_FULL '=' AssignmentExpression_allowIn_FULL
/:$Set$MakeAssignment;:/
/!
	void MakeAssignment();!/
/.$Location
void Parser::MakeAssignment()
{
	Assignment* ae = astPool->newAssignment();
	ae->lhs = CAST<Expression*>(sym(1));
	//ae->operator_token = token(2);
	ae->expression = CAST<Expression*>(sym(3));
	sym(1) = ae;
}
./
	| LeftHandSideExpression_allowIn_FULL CompoundAssignmentOperator AssignmentExpression_allowIn_FULL
/:$Set$MakeAssignment;:/

ConditionalExpression_allowIn_FULL ::= YieldExpression_allowIn
/.$NoAction./
	| LogicalOrExpression_allowIn_FULL
/.$NoAction./
	| LogicalOrExpression_allowIn_FULL '?' AssignmentExpression_allowIn_FULL ':' AssignmentExpression_allowIn_FULL
/:$Set$MakeConditionalExpression;:/
/!
	void MakeConditionalExpression();!/
/.$Location
void Parser::MakeConditionalExpression()
{
	Conditional* ce = astPool->newConditional();
	ce->condition = CAST<Expression*>(sym(1));
	ce->valueTrue = CAST<Expression*>(sym(3));
	ce->valueFalse = CAST<Expression*>(sym(5));
	sym(1) = ce;
}
./

YieldExpression_allowIn ::= 'yield' Marker Marker
/:$Set$MakeYieldExpression;:/
/!
	void MakeYieldExpression();!/
/.$Location
void Parser::MakeYieldExpression()
{
	Yield* ye = astPool->newYield();
	ye->expression = CAST<Expression*>(sym(3));
	sym(1) = ye;
}
./
	| 'yield' NO_LINE_BREAK AssignmentExpression_allowIn_FULL
/:$Set$MakeYieldExpression;:/

LogicalOrExpression_allowIn_FULL ::= LogicalAndExpression_allowIn_FULL
/.$NoAction./
	| LogicalAndExpression_allowIn_FULL '||' LogicalOrExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

LogicalAndExpression_allowIn_FULL ::= BitwiseOrExpression_allowIn_FULL
/.$NoAction./
	| LogicalAndExpression_allowIn_FULL '&&' BitwiseOrExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseOrExpression_allowIn_FULL ::= BitwiseXorExpression_allowIn_FULL
/.$NoAction./
	| BitwiseOrExpression_allowIn_FULL '|' BitwiseXorExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseXorExpression_allowIn_FULL ::= BitwiseAndExpression_allowIn_FULL
/.$NoAction./
	| BitwiseXorExpression_allowIn_FULL '^' BitwiseAndExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseAndExpression_allowIn_FULL ::= EqualityExpression_allowIn_FULL
/.$NoAction./
	| BitwiseAndExpression_allowIn_FULL '&' EqualityExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

EqualityExpression_allowIn_FULL ::= RelationalExpression_allowIn_FULL
/.$NoAction./
	| EqualityExpression_allowIn_FULL '==' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_allowIn_FULL '!=' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_allowIn_FULL '===' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_allowIn_FULL '!==' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

RelationalExpression_allowIn_FULL ::= ShiftExpression_allowIn_FULL
/.$NoAction./
	| RelationalExpression_allowIn_FULL '<' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_FULL '>' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_FULL '<=' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_FULL '>=' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_FULL 'in' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_FULL 'instanceof' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_FULL 'as' TypeExpression
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_FULL 'is' TypeExpression
/:$Set$MakeBinaryExpression;:/

ShiftExpression_allowIn_FULL ::= AdditiveExpression_allowIn_FULL
/.$NoAction./
	| ShiftExpression_allowIn_FULL '<<' AdditiveExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| ShiftExpression_allowIn_FULL '>>' AdditiveExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| ShiftExpression_allowIn_FULL '>>>' AdditiveExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

AdditiveExpression_allowIn_FULL ::= MultiplicativeExpression_allowIn_FULL
/.$NoAction./
	| AdditiveExpression_allowIn_FULL '+' MultiplicativeExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| AdditiveExpression_allowIn_FULL '-' MultiplicativeExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

MultiplicativeExpression_allowIn_FULL ::= UnaryExpression_allowIn_FULL
/.$NoAction./
	| MultiplicativeExpression_allowIn_FULL '*' UnaryExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
/!
	void MakeBinaryExpression();!/
/.$Location
void Parser::MakeBinaryExpression()
{
	BinaryExpression* be = astPool->newBinaryExpression();
	be->left = CAST<Expression*>(sym(1));
	be->op = kind(2);
	be->right = CAST<Expression*>(sym(3));
	sym(1) = be;
}
./
	| MultiplicativeExpression_allowIn_FULL '/' UnaryExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| MultiplicativeExpression_allowIn_FULL '%' UnaryExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

UnaryExpression_allowIn_FULL ::= PostfixExpression_allowIn_FULL
/.$NoAction./
	| 'delete' PostfixExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
/!
	void MakeUnaryExpression();!/
/.$Location
void Parser::MakeUnaryExpression()
{
	UnaryExpression* ue = astPool->newUnaryExpression();
	ue->op = kind(1);
	ue->expression = CAST<Expression*>(sym(2));
}
./
	| 'void' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| 'typeof' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '++' PostfixExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '--' PostfixExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '+' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '-' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '~' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '!' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/

PostfixExpression_allowIn_FULL ::= LeftHandSideExpression_allowIn_FULL
/.$NoAction./
	| LeftHandSideExpression_allowIn_FULL NO_LINE_BREAK '++'
/:$Set$MakePostfixExpression;:/
/!
	void MakePostfixExpression();!/
/.$Location
void Parser::MakePostfixExpression()
{
	PostfixExpression* pe = astPool->newPostfixExpression();
	pe->lhs = CAST<Expression*>(sym(1));
	pe->operator_token = token(3);
	sym(1) = pe;
}
./
	| LeftHandSideExpression_allowIn_FULL NO_LINE_BREAK '--'
/:$Set$MakePostfixExpression;:/

LeftHandSideExpression_allowIn_FULL ::= NewExpression_allowIn_FULL
/.$NoAction./
	| CallExpression_allowIn_FULL
/.$NoAction./

NewExpression_allowIn_FULL ::= MemberExpression_allowIn_FULL
/.$NoAction./
	| 'new' NewExpression_allowIn_FULL Marker
/:$Set$MakeNewExpression;:/

MemberExpression_allowIn_FULL ::= PrimaryExpression_allowIn_FULL
/.$NoAction./
	| 'new' MemberExpression_allowIn_FULL Arguments
/:$Set$MakeNewExpression;:/
/!
	void MakeNewExpression();!/
/.$Location
void Parser::MakeNewExpression()
{
	New* ne = astPool->newNew();
	ne->new_token = token(1);
	ne->expression = CAST<Expression*>(sym(2));
	
	LIST2ARRAY(Expression, 3)
	ne->arguments = list;
	
	sym(1) = ne;
}
./
	| SuperExpression PropertyOperator
/:$Set$MakeMemberAccess;:/
/!
	void MakeMemberAccess();!/
/.$Location
void Parser::MakeMemberAccess()
{
	Ast* ast = sym(2);
	if (Ast::PROPERTY_ACCESS == ast->kind) {
		PropertyAccess* pa = CAST<PropertyAccess*>(ast);
		pa->base = CAST<Expression*>(sym(1));
		sym(1) = pa;
	} else if (Ast::ARRAY_ACCESS == ast->kind) {
		ArrayAccess* ba = CAST<ArrayAccess*>(ast);
		ba->base = CAST<Expression*>(sym(1));
		sym(1) = ba;
	} else if (Ast::TYPE_ACCESS == ast->kind) {
		TypeAccess* ta = CAST<TypeAccess*>(ast);
		ta->base = CAST<Expression*>(sym(1));
		sym(1) = ta;
	} else {
		assert(false);
	}
}
./
	| MemberExpression_allowIn_FULL PropertyOperator
/:$Set$MakeMemberAccess;:/

PrimaryExpression_allowIn_FULL ::= 'null'
/:$Set$MakeNullLiteral;:/
/!
	void MakeNullLiteral();!/
/.$Location
void Parser::MakeNullLiteral()
{
	sym(1) = astPool->newNullLiteral(token(1));
}
./
	| 'true'
/:$Set$MakeBooleanLiteral;:/
/!
	void MakeBooleanLiteral();!/
/.$Location
void Parser::MakeBooleanLiteral()
{
	sym(1) = astPool->newBooleanLiteral(token(1));
}
./
	| 'false'
/:$Set$MakeBooleanLiteral;:/
	| 'this'
/:$Set$MakeThisExpression;:/
/!
	void MakeThisExpression();!/
/.$Location
void Parser::MakeThisExpression()
{
	sym(1) = astPool->newThisExpression(token(1));
}
./
	| NumberLiteral
/:$Set$MakeNumberLiteral;:/
/!
	void MakeNumberLiteral();!/
/.$Location
void Parser::MakeNumberLiteral()
{
	sym(1) = astPool->newNumberLiteral(token(1));
}
./
	| StringLiteral
/:$Set$MakeStringLiteral;:/
/!
	void MakeStringLiteral();!/
/.$Location
void Parser::MakeStringLiteral()
{
	sym(1) = astPool->newStringLiteral(token(1));
}
./
	| '/'
/:$Set$MakeRegExpInitialiser;:/
/!
	void MakeRegExpInitialiser();!/
/.$Location
void Parser::MakeRegExpInitialiser()
{
	lexer->scanRegExp();
	
	sym(1) = astPool->newRegExpLiteral(token(1));
}
./
	| '/='
/:$Set$MakeRegExpInitialiser;:/
	| ObjectLiteral
/.$NoAction./
	| FunctionExpression_allowIn
/.$NoAction./
	| ParenExpression
/.$NoAction./
	| Attribute
/.$NoAction./
	| LetExpression_allowIn
/.$NoAction./
	| XMLInitialiser
/.$NoAction./
	| XMLListInitialiser
/.$NoAction./

ObjectLiteral ::= '{' FieldList '}'
/:$Set$MakeObjectLiteral;:/
/!
	void MakeObjectLiteral();!/
/.$Location
void Parser::MakeObjectLiteral()
{
	ObjectLiteral* ol = astPool->newObjectLiteral();
	ol->leftBrace = token(1);
	ol->rightBrace = token(3);
	
	LIST2ARRAY(Expression, 2)
	ol->fields = list;
	
	sym(1) = ol;
}
./

FieldList ::= %Empty
/.$NullAction./
	| NonemptyFieldList
/.$NoAction./
	| NonemptyFieldList ','
/.$NoAction./

NonemptyFieldList ::= LiteralField
/:$Set$StartList;:/
	| NonemptyFieldList ',' LiteralField
/:$Set$AddList3;:/

LiteralField ::= FieldName ':' AssignmentExpression_allowIn_FULL
/:$Set$MakeField;:/
/!
	void MakeField();!/
/.$Location
void Parser::MakeField()
{
	LiteralField* lf = astPool->newLiteralField();
	lf->name = CAST<Expression*>(sym(1));
	lf->value = CAST<Expression*>(sym(3));
	sym(1) = lf;
}
./
	| 'get' FieldName FunctionSignature FunctionExpressionBody_allowIn
/:$Set$MakeProperty;:/
/!
	void MakeProperty();!/
/.$Location
void Parser::MakeProperty()
{
	LiteralField* lf = astPool->newLiteralField();
	lf->type_token = token(1);
	lf->name = CAST<Expression*>(sym(2));
	Function* func = CAST<Function*>(sym(3));
	func->body = CAST<Block*>(sym(4));
	lf->value = func;
	sym(1) = lf;
}
./
	| 'set' FieldName FunctionSignature FunctionExpressionBody_allowIn
/:$Set$MakeProperty;:/

FieldName ::= StringLiteral
/:$Set$MakeStringLiteral;:/
	| NumberLiteral
/:$Set$MakeNumberLiteral;:/
	| ReservedIdentifier
/:$Set$MakeIdentifier;:/
	| QualifiedName
/.$NoAction./

ReservedIdentifier ::= 'as'
/.$NoAction./
	| 'break'
/.$NoAction./
	| 'case'
/.$NoAction./
	| 'catch'
/.$NoAction./
	| 'class'
/.$NoAction./
	| 'const'
/.$NoAction./
	| 'continue'
/.$NoAction./
	| 'default'
/.$NoAction./
	| 'delete'
/.$NoAction./
	| 'do'
/.$NoAction./
	| 'else'
/.$NoAction./
	| 'false'
/.$NoAction./
	| 'finally'
/.$NoAction./
	| 'for'
/.$NoAction./
	| 'function'
/.$NoAction./
	| 'if'
/.$NoAction./
	| 'import'
/.$NoAction./
	| 'in'
/.$NoAction./
	| 'include'
/.$NoAction./
	| 'instanceof'
/.$NoAction./
	| 'interface'
/.$NoAction./
	| 'is'
/.$NoAction./
	| 'let'
/.$NoAction./
	| 'new'
/.$NoAction./
	| 'null'
/.$NoAction./
	| 'package'
/.$NoAction./
	| 'private'
/.$NoAction./
	| 'protected'
/.$NoAction./
	| 'public'
/.$NoAction./
	| 'return'
/.$NoAction./
	| 'super'
/.$NoAction./
	| 'switch'
/.$NoAction./
	| 'this'
/.$NoAction./
	| 'throw'
/.$NoAction./
	| 'true'
/.$NoAction./
	| 'try'
/.$NoAction./
	| 'typeof'
/.$NoAction./
	| 'use'
/.$NoAction./
	| 'var'
/.$NoAction./
	| 'void'
/.$NoAction./
	| 'while'
/.$NoAction./
	| 'with'
/.$NoAction./
	| 'yield'
/.$NoAction./

QualifiedName ::= SimpleQualifiedName
/.$NoAction./
	| ExpressionQualifiedName
/.$NoAction./

SimpleQualifiedName ::= Identifier
/.$NoAction./
	| '*'
/:$Set$MakeWildcard;:/
	| '*='
/:$Set$MakeWildcard;:/
	| Qualifier '::' QualifiedNameIdentifier
/:$Set$MakeQualfiedIdentifier;:/
/!
	void MakeQualfiedIdentifier();!/
/.$Location
void Parser::MakeQualfiedIdentifier()
{
	QualifiedName* qn = astPool->newQualifiedName();
	qn->ns = CAST<Expression*>(sym(1));
	qn->name = CAST<Expression*>(sym(3));
	sym(1) = qn;
}
./

Qualifier ::= '*'
/:$Set$MakeWildcard;:/
	| Identifier
/.$NoAction./
	| ReservedNamespace
/.$NoAction./

ReservedNamespace ::= 'private'
/:$Set$MakeIdentifier;:/
	| 'protected'
/:$Set$MakeIdentifier;:/
	| 'public'
/:$Set$MakeIdentifier;:/

QualifiedNameIdentifier ::= PropertyIdentifier
/.$NoAction./
	| '*'
/:$Set$MakeWildcard;:/
/!
	void MakeWildcard();!/
/.$Location
void Parser::MakeWildcard()
{
	if (TK_MULTIPLY_EQUAL == lexer->getKind())
		lexer->split();
	sym(1) = astPool->newWildcard(token(1));
}
./
	| '*='
/:$Set$MakeWildcard;:/
	| Brackets
/.$NoAction./

PropertyIdentifier ::= Identifier
/.$NoAction./
	| ReservedIdentifier
/:$Set$MakeIdentifier;:/

Brackets ::= '[' CommaExpression_allowIn_FULL ']'
/:$Set$MakeArguments;:/
	| '[' ResetExpression ']'
/:$Set$MakeArguments;:/
	| '[' CommaExpression_allowIn_FULL ',' ResetExpression ']'
/:$Set$MergeArguments;:/

ResetExpression ::= '...' AssignmentExpression_allowIn_FULL
/:$Set$MakeResetExpression;:/
/!
	void MakeResetExpression();!/
/.$Location
void Parser::MakeResetExpression()
{
	ResetExpression* re = astPool->newResetExpression();
	re->reset_token = token(1);
	re->expression = CAST<Expression*>(sym(2));
	sym(1) = re;
}
./

ExpressionQualifiedName ::= ParenExpression '::' PropertyIdentifier
/:$Set$MakeQualfiedIdentifier;:/

ParenExpression ::= '(' ArrayComprehension ')'
/:$Set$MakeParenExpression;:/
	| '(' CommaExpression_allowIn_FULL ')'
/:$Set$MakeParenExpression;:/
/!
	void MakeParenExpression();!/
/.$Location
void Parser::MakeParenExpression()
{
	ParenExpression* pe = astPool->newParenExpression();
	pe->leftParen = token(1);
	pe->expression = toExpression(2);
	pe->rightParen = token(3);
}
./

ArrayComprehension ::= AssignmentExpression_allowIn_FULL ComprehensionExpression
/:$Set$MakeArrayComprehension;:/
/!
	void MakeArrayComprehension();!/
/.$Location
void Parser::MakeArrayComprehension()
{
	ArrayComprehension* ac = astPool->newArrayComprehension();
	ac->initializer = CAST<Expression*>(sym(1));
	ac->tail = CAST<Expression*>(sym(2));
	sym(1) = ac;
}
./

ComprehensionExpression ::= 'for' '(' TypedPattern 'in' CommaExpression_allowIn_FULL ')' ComprehensionClause
/:$Set$MakeComprehensionExpression;:/
/!
	void MakeComprehensionExpression();!/
/.$Location
void Parser::MakeComprehensionExpression()
{
	int index = 2;
	Comprehension* ce = astPool->newComprehension();
	ce->for_token = token(1);
	if (TK_each == kind(2)) {
		index++;
		ce->each_token = token(2);
	}
	ce->left_paren_token = token(index++);
	ce->initializer = CAST<Expression*>(sym(index++));
	index++;
	LIST2ARRAY(Expression,index)
	ce->conditions = list;
	ce->right_paren_token = token(++index);
	ce->cause = CAST<Expression*>(sym(++index));
	
	sym(1) = ce;
}
./
	| 'for' 'each' '(' TypedPattern 'in' CommaExpression_allowIn_FULL ')' ComprehensionClause
/:$Set$MakeComprehensionExpression;:/

TypedPattern ::= TypedIdentifier
/.$NoAction./
	| ArrayLiteral
/.$NoAction./
	| ObjectLiteral
/.$NoAction./

TypedIdentifier ::= PropertyIdentifier Marker Marker
/.$NoAction./
	| PropertyIdentifier ':' TypeExpression
/:$Set$MakeTypedIdentifier;:/
/!
	void MakeTypedIdentifier();!/
/.$Location
void Parser::MakeTypedIdentifier()
{
	TypedIdentifier* ti = astPool->newTypedIdentifier();
	ti->id = CAST<Expression*>(sym(1));
	ti->type = CAST<Expression*>(sym(3));
	sym(1) = ti;
}./

TypeExpression ::= BasicTypeExpression
/.$NoAction./

BasicTypeExpression ::= 'null'
/:$Set$MakeNullLiteral;:/
	| TypeReference
/.$NoAction./

TypeReference ::= PrimaryName
/.$NoAction./
	| PrimaryName TypeArguments
/.$NoAction./

PrimaryName ::= QualifiedName
/.$NoAction./
	| AttributeName
/.$NoAction./

AttributeName ::= '@' Brackets
/:$Set$MakeAttributeName;:/
/!
	void MakeAttributeName();!/
/.$Location
void Parser::MakeAttributeName()
{
	AttributeName* attr = astPool->newAttributeName(token(1));
	attr->expression = CAST<Expression*>(sym(2));
	sym(1) = attr;
}
./
	| '@' QualifiedName
/:$Set$MakeAttributeName;:/
	| '@' ReservedIdentifier
/.$Action
$DefaultHeader
{
	AttributeName* attr = astPool->newAttributeName(token(1));
	attr->expression = astPool->newIdentifier(token(2));
	sym(1) = attr;
}
./

TypeArguments ::= '.<' TypeExpressionList TypeEnd
/:$Set$setSym2ToSym1;:/

TypeExpressionList ::= TypeExpression
/:$Set$StartList;:/
	| TypeExpressionList ',' TypeExpression
/:$Set$AddList3;:/

TypeEnd ::= '>'
/.$NoAction./
	| '>>'
/:$Set$CheckTypeEnd;:/
/!
	void CheckTypeEnd();!/
/.$Location
void Parser::CheckTypeEnd()
{
	lexer->split();
}
./
	| '>>>'
/:$Set$CheckTypeEnd;:/
	| '>='
/:$Set$CheckTypeEnd;:/
	| '>>='
/:$Set$CheckTypeEnd;:/
	| '>>>='
/:$Set$CheckTypeEnd;:/

ArrayLiteral ::= '[' ElementList ']'
/:$Set$MakeArrayLiteral;:/
/!
	void MakeArrayLiteral();!/
/.$Location
void Parser::MakeArrayLiteral()
{
	Ast* exp = sym(2);
	ArrayLiteral* al = astPool->newArrayLiteral();
	if (exp) {
		if (Ast::LIST_NODE == exp->kind) { //ElementList
			AstListNode* tail = CAST<AstListNode*>(exp);
			if (tail->index || tail->element) {
				LIST2ARRAY(Expression, 2);
				al->elements = list;
			} else {
				FreeCircularList(tail);
			}
		} else { //ArrayComprehension
			Expressions* list = astPool->AllocateExpressions(1);
			list->next() = CAST<Expression*>(sym(2));
		}
	}
	sym(1) = al;
}
./
	| '[' ArrayComprehension ']'
/:$Set$MakeArrayLiteral;:/

ElementList ::= %Empty
/:$Set$StartList;:/
	| LiteralElement
/:$Set$StartList;:/
	| ElementList ',' Marker
/:$Set$AddList3;:/
	| ElementList ',' LiteralElement
/:$Set$AddList3;:/

LiteralElement ::= AssignmentExpression_allowIn_FULL
/.$NoAction./

ComprehensionClause ::= %Empty
/.$NullAction./
	| 'if' ParenExpression
/:$Set$MakeComprehensionClause;:/
/!
	void MakeComprehensionClause();!/
/.$Location
void Parser::MakeComprehensionClause()
{
	ComprehensionClause* cc = astPool->newComprehensionClause();
	cc->if_token = token(1);
	cc->expression = CAST<Expression*>(sym(2));
	sym(1) = cc;
}
./
	| ComprehensionExpression
/.$NoAction./

FunctionSignature ::= TypeParameters '(' Parameters ')' ResultType
/:$Set$MakeFunctionSignature;:/
/!
	void MakeFunctionSignature();!/
/.$Location
void Parser::MakeFunctionSignature()
{
	Function* func = astPool->newFunction();
	if (sym(1)) {
		LIST2ARRAY(Identifier,1);
		func->type_parameters = list;
	}
	//func->left_paren_token = token(2);
	if (sym(4)) {
		LIST2ARRAY(Parameter,3);
		func->parameters = list;
	}
	//func->right_paren_token = token(4);
	func->result = CAST<Expression*>(sym(5));
	sym(1) = func;
}
./

TypeParameters ::= %Empty
/.$NullAction./
	| '.<' TypeParameterList TypeEnd
/:$Set$setSym2ToSym1;:/

TypeParameterList ::= Identifier
/:$Set$StartList;:/
	| TypeParameterList ',' Identifier
/:$Set$AddList3;:/

Parameters ::= %Empty
/.$NullAction./
	| NonemptyParameters
/.$NoAction./

NonemptyParameters ::= NonRestParameters
/.$NoAction./
	| RestParameter
/:$Set$StartList;:/
	| NonRestParameters ',' RestParameter
/:$Set$AddList3;:/

NonRestParameters ::= ParameterList
/.$NoAction./
	| OptionalParameters
/.$NoAction./
	| ParameterList ',' OptionalParameters
/:$Set$MergeList3;:/

ParameterList ::= Parameter
/:$Set$StartList;:/
	| ParameterList ',' Parameter
/:$Set$AddList3;:/

Parameter ::= TypedPattern
/:$Set$MakeParameter;:/
/!
	void MakeParameter();!/
/.$Location
void Parser::MakeParameter()
{
	Parameter* param = astPool->newParameter();
	param->name = CAST<Expression*>(sym(1));
	sym(1) = param;
}./
	| 'const' TypedPattern
/:$Set$MakeConstParameter;:/
/!
	void MakeConstParameter();!/
/.$Location
void Parser::MakeConstParameter()
{
	Parameter* param = astPool->newParameter(true);
	param->name = CAST<Expression*>(sym(2));
	sym(1) = param;
}./

OptionalParameters ::= OptionalParameter
/:$Set$StartList;:/
	| OptionalParameters ',' OptionalParameter
/:$Set$AddList3;:/

OptionalParameter ::= Parameter '=' NonAssignmentExpression_allowIn
/:$Set$MakeOptionalParameter;:/
/!
	void MakeOptionalParameter();!/
/.$Location
void Parser::MakeOptionalParameter()
{
	Parameter* param = CAST<Parameter*>(sym(1));
	param->defaultValue = CAST<Expression*>(sym(3));
}
./

NonAssignmentExpression_allowIn ::= YieldExpression_allowIn
/.$NoAction./
	| LogicalOrExpression_allowIn_FULL
/.$NoAction./
	| LogicalOrExpression_allowIn_FULL '?' NonAssignmentExpression_allowIn ':' NonAssignmentExpression_allowIn
/:$Set$MakeConditionalExpression;:/

RestParameter ::= '...' Marker
/:$Set$MakeRestParameter;:/
/!
	void MakeRestParameter();!/
/.$Location
void Parser::MakeRestParameter()
	{
		Parameter* param = NULL;
		if (sym(2)) {
			param = CAST<Parameter*>(sym(2));
		} else {
			//anonymous parameter..

			param = astPool->newParameter();
		}
		//param->kind |= REST
		sym(1) = param;
	}
	./
	| '...' Parameter
/:$Set$MakeRestParameter;:/

ResultType ::= %Empty
/.$NullAction./
	| ':' 'void'
/:$Set$MakeVoidType;:/
/!
	void MakeVoidType();!/
/.$Location
void Parser::MakeVoidType()
{
	sym(1) = astPool->newIdentifier(token(2));
}./
	| ':' TypeExpression
/:$Set$setSym2ToSym1;:/

FunctionExpressionBody_allowIn ::= Block
/.$NoAction./
	| EnterClosure AssignmentExpression_allowIn_NoBrace CLOSURE
/:$Set$MakeAnonymousBlock;:/
/!
	void MakeAnonymousBlock();!/
/.$Location
void Parser::MakeAnonymousBlock()
{
	Block* block = astPool->newBlock();
	sym(1) = block;
}
./

EnterClosure ::= %Empty
/:$Set$EnterClosure;:/
/!
	void EnterClosure();!/
/.$Location
void Parser::EnterClosure()
{
	enterClosure();
}
./

AssignmentExpression_allowIn_NoBrace ::= ConditionalExpression_allowIn_NoBrace
/.$NoAction./
	| LeftHandSideExpression_allowIn_NoBrace '=' AssignmentExpression_allowIn_FULL
/:$Set$MakeAssignment;:/
	| LeftHandSideExpression_allowIn_NoBrace CompoundAssignmentOperator AssignmentExpression_allowIn_FULL
/:$Set$MakeAssignment;:/

ConditionalExpression_allowIn_NoBrace ::= YieldExpression_allowIn
/.$NoAction./
	| LogicalOrExpression_allowIn_NoBrace
/.$NoAction./
	| LogicalOrExpression_allowIn_NoBrace '?' AssignmentExpression_allowIn_FULL ':' AssignmentExpression_allowIn_FULL
/:$Set$MakeConditionalExpression;:/

LogicalOrExpression_allowIn_NoBrace ::= LogicalAndExpression_allowIn_NoBrace
/.$NoAction./
	| LogicalAndExpression_allowIn_NoBrace '||' LogicalOrExpression_allowIn_NoBrace
/:$Set$MakeBinaryExpression;:/

LogicalAndExpression_allowIn_NoBrace ::= BitwiseOrExpression_allowIn_NoBrace
/.$NoAction./
	| LogicalAndExpression_allowIn_NoBrace '&&' BitwiseOrExpression_allowIn_NoBrace
/:$Set$MakeBinaryExpression;:/

BitwiseOrExpression_allowIn_NoBrace ::= BitwiseXorExpression_allowIn_NoBrace
/.$NoAction./
	| BitwiseOrExpression_allowIn_NoBrace '|' BitwiseXorExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseXorExpression_allowIn_NoBrace ::= BitwiseAndExpression_allowIn_NoBrace
/.$NoAction./
	| BitwiseXorExpression_allowIn_NoBrace '^' BitwiseAndExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseAndExpression_allowIn_NoBrace ::= EqualityExpression_allowIn_NoBrace
/.$NoAction./
	| BitwiseAndExpression_allowIn_NoBrace '&' EqualityExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

EqualityExpression_allowIn_NoBrace ::= RelationalExpression_allowIn_NoBrace
/.$NoAction./
	| EqualityExpression_allowIn_NoBrace '==' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_allowIn_NoBrace '!=' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_allowIn_NoBrace '===' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_allowIn_NoBrace '!==' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

RelationalExpression_allowIn_NoBrace ::= ShiftExpression_allowIn_NoBrace
/.$NoAction./
	| RelationalExpression_allowIn_NoBrace '<' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_NoBrace '>' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_NoBrace '<=' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_NoBrace '>=' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_NoBrace 'in' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_NoBrace 'instanceof' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_NoBrace 'as' TypeExpression
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_NoBrace 'is' TypeExpression
/:$Set$MakeBinaryExpression;:/

ShiftExpression_allowIn_NoBrace ::= AdditiveExpression_allowIn_NoBrace
/.$NoAction./
	| ShiftExpression_allowIn_NoBrace '<<' AdditiveExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| ShiftExpression_allowIn_NoBrace '>>' AdditiveExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| ShiftExpression_allowIn_NoBrace '>>>' AdditiveExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

AdditiveExpression_allowIn_NoBrace ::= MultiplicativeExpression_allowIn_NoBrace
/.$NoAction./
	| AdditiveExpression_allowIn_NoBrace '+' MultiplicativeExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| AdditiveExpression_allowIn_NoBrace '-' MultiplicativeExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

MultiplicativeExpression_allowIn_NoBrace ::= UnaryExpression_allowIn_NoBrace
/.$NoAction./
	| MultiplicativeExpression_allowIn_NoBrace '*' UnaryExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| MultiplicativeExpression_allowIn_NoBrace '/' UnaryExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| MultiplicativeExpression_allowIn_NoBrace '%' UnaryExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

UnaryExpression_allowIn_NoBrace ::= PostfixExpression_allowIn_NoBrace
/.$NoAction./
	| 'delete' PostfixExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| 'void' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| 'typeof' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '++' PostfixExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '--' PostfixExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '+' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '-' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '~' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '!' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/

PostfixExpression_allowIn_NoBrace ::= LeftHandSideExpression_allowIn_NoBrace
/.$NoAction./
	| LeftHandSideExpression_allowIn_NoBrace NO_LINE_BREAK '++'
/:$Set$MakePostfixExpression;:/
	| LeftHandSideExpression_allowIn_NoBrace NO_LINE_BREAK '--'
/:$Set$MakePostfixExpression;:/

LeftHandSideExpression_allowIn_NoBrace ::= NewExpression_allowIn_NoBrace
/.$NoAction./
	| CallExpression_allowIn_NoBrace
/.$NoAction./

NewExpression_allowIn_NoBrace ::= MemberExpression_allowIn_NoBrace
/.$NoAction./
	| 'new' NewExpression_allowIn_FULL Marker
/:$Set$MakeNewExpression;:/

MemberExpression_allowIn_NoBrace ::= PrimaryExpression_allowIn_NoBrace
/.$NoAction./
	| 'new' MemberExpression_allowIn_FULL Arguments
/:$Set$MakeNewExpression;:/
	| SuperExpression PropertyOperator
/:$Set$MakeMemberAccess;:/
	| MemberExpression_allowIn_NoBrace PropertyOperator
/:$Set$MakeMemberAccess;:/

PrimaryExpression_allowIn_NoBrace ::= 'null'
/:$Set$MakeNullLiteral;:/
	| 'true'
/:$Set$MakeBooleanLiteral;:/
	| 'false'
/:$Set$MakeBooleanLiteral;:/
	| 'this'
/:$Set$MakeThisExpression;:/
	| NumberLiteral
/:$Set$MakeNumberLiteral;:/
	| StringLiteral
/:$Set$MakeStringLiteral;:/
	| '/'
/:$Set$MakeRegExpInitialiser;:/
	| '/='
/:$Set$MakeRegExpInitialiser;:/
	| FunctionExpression_allowIn
/.$NoAction./
	| ParenExpression
/.$NoAction./
	| Attribute
/.$NoAction./
	| LetExpression_allowIn
/.$NoAction./
	| XMLInitialiser
/.$NoAction./
	| XMLListInitialiser
/.$NoAction./

FunctionExpression_allowIn ::= 'function' PropertyIdentifier FunctionSignature FunctionExpressionBody_allowIn
/:$Set$MakeFunctionExpression;:/
/!
	void MakeFunctionExpression();!/
/.$Location
void Parser::MakeFunctionExpression()
{
	Function* func = CAST<Function*>(sym(3));
	func->function_token = token(1);
	//func->identifier = CAST<Identifier*>(sym(2));
	func->body = CAST<Block*>(sym(4));
	sym(1) = func;
}
./
	| 'function' Marker FunctionSignature FunctionExpressionBody_allowIn
/:$Set$MakeFunctionExpression;:/

Attribute ::= PrimaryName
/.$NoAction./
	| ReservedNamespace
/.$NoAction./
	| ArrayLiteral
/.$NoAction./

LetExpression_allowIn ::= 'let' '(' LetBindingList ')' EnterClosure AssignmentExpression_allowIn_FULL CLOSURE
/:$Set$MakeLetExpression;:/
/!
	void MakeLetExpression();!/
/.$Location
void Parser::MakeLetExpression()
{
	Let* let = astPool->newLet();
	
	LIST2ARRAY(Variable, 3);
	let->bindings = list;
	
	let->expression = CAST<Expression*>(sym(5));
	sym(1) = let;
}
./

LetBindingList ::= %Empty
/.$NullAction./
	| VariableBindingList_allowIn
/.$NoAction./

VariableBindingList_allowIn ::= VariableBinding_allowIn
/:$Set$StartList;:/
	| VariableBindingList_allowIn ',' VariableBinding_allowIn
/:$Set$AddList3;:/

VariableBinding_allowIn ::= TypedPattern Marker Marker
/:$Set$MakeVariableBinding;:/
/!
	void MakeVariableBinding();!/
/.$Location
void Parser::MakeVariableBinding()
{
	Variable* var = astPool->newVariable();
	var->name = CAST<Expression*>(sym(1));
	var->initializer = CAST<Expression*>(sym(3));
	sym(1) = var;
}./
	| TypedPattern '=' AssignmentExpression_allowIn_FULL
/:$Set$MakeVariableBinding;:/

XMLInitialiser ::= XMLMarkup
/.$NoAction./
	| XMLElement
/.$NoAction./

XMLMarkup ::= XMLComment
/.$NoAction./
	| XMLCDATA
/.$NoAction./
	| XMLPI
/.$NoAction./

XMLElement ::= XMLStart XMLTagContent '/>'
/.$NoAction./
	| XMLStart XMLTagContent '>' XMLElementContentOpt '</' XMLTagName '>'
/.$NoAction./

XMLStart ::= '<'
/.$NoAction./

XMLTagContent ::= XMLTagName XMLAttributesOpt
/.$NoAction./

XMLTagName ::= XMLName
/.$NoAction./
	| '{' CommaExpression_allowIn_FULL '}'
/.$NoAction./

XMLAttributesOpt ::= %Empty
/.$NullAction./
	| XMLAttributes
/.$NoAction./

XMLAttributes ::= XMLAttribute
/.$NoAction./
	| XMLAttributes XMLAttribute
/.$NoAction./

XMLAttribute ::= XMLTagName '=' XMLAttributeValue
/.$NoAction./
	| XMLTagName '=' '{' CommaExpression_allowIn_FULL '}'
/.$NoAction./
	| '{' CommaExpression_allowIn_FULL '}'
/.$NoAction./

XMLElementContentOpt ::= %Empty
/.$NullAction./
	| XMLElementContent
/.$NoAction./

XMLElementContent ::= XMLMarkup XMLElementContentOpt
/.$NoAction./
	| XMLText XMLElementContentOpt
/.$NoAction./
	| XMLElement XMLElementContentOpt
/.$NoAction./
	| '{' CommaExpression_allowIn_FULL '}' XMLElementContentOpt
/.$NoAction./

XMLListInitialiser ::= XMLStart '>' XMLElementContent '</' '>'
/.$NoAction./

Arguments ::= '(' Marker ')'
/:$Set$MakeArguments;:/
/!
	void MakeArguments();!/
/.$Location
void Parser::MakeArguments()
{
	/*
	Ast* exp = sym(2);
	Arguments* args = astPool->newArguments(token(1), token(3));
	if (exp) {
		if (Ast::LIST_NODE == exp->kind) {
			LIST2ARRAY(Expression, 2);
			args->arguments = list;
		} else {
			Expressions* list = astPool->AllocateExpressions(1);
			list->next() = CAST<Expression*>(exp);
			args->arguments = list;
		}
	}
	
	sym(1) = args;
	*/
	sym(1) = sym(2);
}
./
	| '(' ResetExpression ')'
/:$Set$MakeArguments;:/
	| '(' ArrayComprehension ')'
/:$Set$MakeArguments;:/
	| '(' CommaExpression_allowIn_FULL ')'
/:$Set$MakeArguments;:/
	| '(' CommaExpression_allowIn_FULL ',' ResetExpression ')'
/:$Set$MergeArguments;:/
/!
	void MergeArguments();!/
/.$Location
void Parser::MergeArguments()
{
	ADD_LIST(2, 4)
	assignToken(5, 3);
	MakeArguments();
}
./

SuperExpression ::= 'super' Marker
/:$Set$MakeSuperExpression;:/
/!
	void MakeSuperExpression();!/
/.$Location
void Parser::MakeSuperExpression()
{
	SuperExpression* se = astPool->newSuperExpression(token(1));
	if (sym(2))
		se->argument = CAST<Expression*>(sym(2));
	
	sym(1) = se;
}
./
	| 'super' ParenExpression
/:$Set$MakeSuperExpression;:/

PropertyOperator ::= '.' PrimaryName
/:$Set$MakePropertyAccess;:/
/!
	void MakePropertyAccess();!/
/.$Location
void Parser::MakePropertyAccess()
{
	PropertyAccess* pa = astPool->newPropertyAccess();
	pa->op = kind(1);
	pa->name = CAST<Expression*>(sym(2));
	sym(1) = pa;
}
./
	| '.' ReservedIdentifier
/:$Set$MakePropertyAccess;:/
	| '..' PrimaryName
/:$Set$MakePropertyAccess;:/
	| '..' ReservedIdentifier
/:$Set$MakePropertyAccess;:/
	| '.' ParenExpression
/:$Set$MakePropertyAccess;:/
	| Brackets
/:$Set$MakeArrayAccess;:/
/!
	void MakeArrayAccess();!/
/.$Location
void Parser::MakeArrayAccess()
{
	ArrayAccess* aa = astPool->newArrayAccess();
	LIST2ARRAY(Expression, 1);
	aa->dims = list;
	sym(1) = aa;
}
./
	| TypeArguments
/:$Set$MakeTypeAccess;:/
/!
	void MakeTypeAccess();!/
/.$Location
void Parser::MakeTypeAccess()
{
	TypeAccess* ta = astPool->newTypeAccess();
	LIST2ARRAY(Expression, 1);
	ta->arguments = list;
	sym(1) = ta;
}
./

CallExpression_allowIn_NoBrace ::= MemberExpression_allowIn_NoBrace Arguments
/:$Set$MakeMemberExpression;:/
/!
	void MakeMemberExpression();!/
/.$Location
void Parser::MakeMemberExpression()
{
	Call* call = astPool->newCall();
	call->expression = CAST<Expression*>(sym(1));
	if (sym(2)) {
		LIST2ARRAY(Expression, 2);
		call->arguments = list;
	}
	sym(1) = call;
}
./
	| CallExpression_allowIn_NoBrace Arguments
/:$Set$MakeMemberExpression;:/
	| CallExpression_allowIn_NoBrace PropertyOperator
/:$Set$MakeMemberAccess;:/

CompoundAssignmentOperator ::= '*='
/.$NoAction./
	| '/='
/.$NoAction./
	| '%='
/.$NoAction./
	| '+='
/.$NoAction./
	| '-='
/.$NoAction./
	| '<<='
/.$NoAction./
	| '>>='
/.$NoAction./
	| '>>>='
/.$NoAction./
	| '&='
/.$NoAction./
	| '^='
/.$NoAction./
	| '|='
/.$NoAction./
	| '&&='
/.$NoAction./
	| '||='
/.$NoAction./

CallExpression_allowIn_FULL ::= MemberExpression_allowIn_FULL Arguments
/:$Set$MakeMemberExpression;:/
	| CallExpression_allowIn_FULL Arguments
/:$Set$MakeMemberExpression;:/
	| CallExpression_allowIn_FULL PropertyOperator
/:$Set$MakeMemberAccess;:/

DoStatement ::= 'do' Substatement_abbrev 'while' ParenExpression
/:$Set$MakeDoStatement;:/
/!
	void MakeDoStatement();!/
/.$Location
void Parser::MakeDoStatement()
{
	DoStatement* ds = astPool->newDoStatement();
	ds->action = CAST<Statement*>(sym(2));
	ds->condition = CAST<Expression*>(sym(4));
	sym(1) = ds;
}
./

Substatement_abbrev ::= EmptyStatement
/.$NoAction./
	| Statement_abbrev
/.$NoAction./
	| VariableDefinition_allowIn Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/

Statement_abbrev ::= Block
/.$NoAction./
	| BreakStatement Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| ContinueStatement Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| DefaultXMLNamespaceStatement Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| DoStatement Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| ExpressionStatement Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| ForStatement_abbrev
/.$NoAction./
	| IfStatement_abbrev
/.$NoAction./
	| LabeledStatement_abbrev
/.$NoAction./
	| LetBlockStatement
/.$NoAction./
	| ReturnStatement Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| SwitchStatement
/.$NoAction./
	| ThrowStatement Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| TryStatement
/.$NoAction./
	| WhileStatement_abbrev
/.$NoAction./
	| WithStatement_abbrev
/.$NoAction./

Semicolon_abbrev ::= %Empty
/.$NullAction./
	| ';'
/.$NoAction./
	| VirtualSemicolon
/.$NoAction./

ExpressionStatement ::= CommaExpression_allowIn_ES
/:$Set$MakeExpressionStatement;:/
/!
	void MakeExpressionStatement();!/
/.$Location
void Parser::MakeExpressionStatement()
{
	ExpressionStatement* es = astPool->newExpressionStatement();
	es->expression = toExpression(1);
	sym(1) = es;
}
./

CommaExpression_allowIn_ES ::= AssignmentExpression_allowIn_ES
/:$Set$StartList;:/
	| CommaExpression_allowIn_ES ',' AssignmentExpression_allowIn_ES
/:$Set$AddList2;:/

AssignmentExpression_allowIn_ES ::= ConditionalExpression_allowIn_ES
/.$NoAction./
	| LeftHandSideExpression_allowIn_ES '=' AssignmentExpression_allowIn_FULL
/:$Set$MakeAssignment;:/
	| LeftHandSideExpression_allowIn_ES CompoundAssignmentOperator AssignmentExpression_allowIn_FULL
/:$Set$MakeAssignment;:/

ConditionalExpression_allowIn_ES ::= YieldExpression_allowIn
/.$NoAction./
	| LogicalOrExpression_allowIn_ES
/.$NoAction./
	| LogicalOrExpression_allowIn_ES '?' AssignmentExpression_allowIn_FULL ':' AssignmentExpression_allowIn_FULL
/:$Set$MakeConditionalExpression;:/

LogicalOrExpression_allowIn_ES ::= LogicalAndExpression_allowIn_ES
/.$NoAction./
	| LogicalAndExpression_allowIn_ES '||' LogicalOrExpression_allowIn_ES
/:$Set$MakeBinaryExpression;:/

LogicalAndExpression_allowIn_ES ::= BitwiseOrExpression_allowIn_ES
/.$NoAction./
	| LogicalAndExpression_allowIn_ES '&&' BitwiseOrExpression_allowIn_ES
/:$Set$MakeBinaryExpression;:/

BitwiseOrExpression_allowIn_ES ::= BitwiseXorExpression_allowIn_ES
/.$NoAction./
	| BitwiseOrExpression_allowIn_ES '|' BitwiseXorExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseXorExpression_allowIn_ES ::= BitwiseAndExpression_allowIn_ES
/.$NoAction./
	| BitwiseXorExpression_allowIn_ES '^' BitwiseAndExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseAndExpression_allowIn_ES ::= EqualityExpression_allowIn_ES
/.$NoAction./
	| BitwiseAndExpression_allowIn_ES '&' EqualityExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

EqualityExpression_allowIn_ES ::= RelationalExpression_allowIn_ES
/.$NoAction./
	| EqualityExpression_allowIn_ES '==' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_allowIn_ES '!=' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_allowIn_ES '===' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_allowIn_ES '!==' RelationalExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

RelationalExpression_allowIn_ES ::= ShiftExpression_allowIn_ES
/.$NoAction./
	| RelationalExpression_allowIn_ES '<' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_ES '>' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_ES '<=' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_ES '>=' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_ES 'in' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_ES 'instanceof' ShiftExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_ES 'as' TypeExpression
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_allowIn_ES 'is' TypeExpression
/:$Set$MakeBinaryExpression;:/

ShiftExpression_allowIn_ES ::= AdditiveExpression_allowIn_ES
/.$NoAction./
	| ShiftExpression_allowIn_ES '<<' AdditiveExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| ShiftExpression_allowIn_ES '>>' AdditiveExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| ShiftExpression_allowIn_ES '>>>' AdditiveExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

AdditiveExpression_allowIn_ES ::= MultiplicativeExpression_allowIn_ES
/.$NoAction./
	| AdditiveExpression_allowIn_ES '+' MultiplicativeExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| AdditiveExpression_allowIn_ES '-' MultiplicativeExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

MultiplicativeExpression_allowIn_ES ::= UnaryExpression_allowIn_ES
/.$NoAction./
	| MultiplicativeExpression_allowIn_ES '*' UnaryExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| MultiplicativeExpression_allowIn_ES '/' UnaryExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/
	| MultiplicativeExpression_allowIn_ES '%' UnaryExpression_allowIn_FULL
/:$Set$MakeBinaryExpression;:/

UnaryExpression_allowIn_ES ::= PostfixExpression_allowIn_ES
/.$NoAction./
	| 'delete' PostfixExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| 'void' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| 'typeof' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '++' PostfixExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '--' PostfixExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '+' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '-' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '~' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '!' UnaryExpression_allowIn_FULL
/:$Set$MakeUnaryExpression;:/

PostfixExpression_allowIn_ES ::= LeftHandSideExpression_allowIn_ES
/.$NoAction./
	| LeftHandSideExpression_allowIn_ES NO_LINE_BREAK '++'
/:$Set$MakePostfixExpression;:/
	| LeftHandSideExpression_allowIn_ES NO_LINE_BREAK '--'
/:$Set$MakePostfixExpression;:/

LeftHandSideExpression_allowIn_ES ::= NewExpression_allowIn_ES
/.$NoAction./
	| CallExpression_allowIn_ES
/.$NoAction./

NewExpression_allowIn_ES ::= MemberExpression_allowIn_ES
/.$NoAction./
	| 'new' NewExpression_allowIn_FULL Marker
/:$Set$MakeNewExpression;:/

MemberExpression_allowIn_ES ::= PrimaryExpression_allowIn_ES
/.$NoAction./
	| 'new' MemberExpression_allowIn_FULL Arguments
/:$Set$MakeNewExpression;:/
	| SuperExpression PropertyOperator
/:$Set$MakeMemberAccess;:/
	| MemberExpression_allowIn_ES PropertyOperator
/:$Set$MakeMemberAccess;:/

PrimaryExpression_allowIn_ES ::= 'null'
/:$Set$MakeNullLiteral;:/
	| 'true'
/:$Set$MakeBooleanLiteral;:/
	| 'false'
/:$Set$MakeBooleanLiteral;:/
	| 'this'
/:$Set$MakeThisExpression;:/
	| NumberLiteral
/:$Set$MakeNumberLiteral;:/
	| StringLiteral
/:$Set$MakeStringLiteral;:/
	| '/'
/:$Set$MakeRegExpInitialiser;:/
	| '/='
/:$Set$MakeRegExpInitialiser;:/
	| ParenExpression
/.$NoAction./
	| Attribute
/.$NoAction./
	| XMLInitialiser
/.$NoAction./
	| XMLListInitialiser
/.$NoAction./

CallExpression_allowIn_ES ::= MemberExpression_allowIn_ES Arguments
/:$Set$MakeMemberExpression;:/
	| CallExpression_allowIn_ES Arguments
/:$Set$MakeMemberExpression;:/
	| CallExpression_allowIn_ES PropertyOperator
/:$Set$MakeMemberAccess;:/

ForStatement_abbrev ::= 'for' '(' ForInitialiser ';' OptionalExpression ';' OptionalExpression ')' Substatement_abbrev
/:$Set$MakeForStatement;:/
/!
	void MakeForStatement();!/
/.$Location
void Parser::MakeForStatement()
{
	ForStatement* fs = astPool->newForStatement();
	fs->initialization = toExpression(3);
	if (sym(5)) {
		LIST2ARRAY(Expression, 5);
		fs->conditions = list;
	}
	if (sym(7)) {
		LIST2ARRAY(Expression, 7);
		fs->increments = list;
	}
	fs->action = CAST<Statement*>(sym(9));
	sym(1) = fs;
}./
	| 'for' '(' ForInBinding 'in' CommaExpression_allowIn_FULL ')' Substatement_abbrev
/:$Set$MakeForeachStatement;:/
/!
	void MakeForeachStatement();!/
/.$Location
void Parser::MakeForeachStatement()
{
	ForeachStatement* fs = astPool->newForeachStatement();
	int index = 2;
	if (TK_each == kind(index))
		index++;
	
	index++;
	fs->binding = CAST<Expression*>(sym(index));
	LIST2ARRAY(Expression, index + 2);
	fs->expressions = list;
	fs->action = CAST<Statement*>(sym(index + 4));
	sym(1) = fs;
}
./
	| 'for' 'each' '(' ForInBinding 'in' CommaExpression_allowIn_FULL ')' Substatement_abbrev
/:$Set$MakeForeachStatement;:/

ForInitialiser ::= %Empty
/.$NullAction./
	| CommaExpression_noIn_FULL
/.$NoAction./
	| VariableDefinition_noIn
/.$NoAction./

CommaExpression_noIn_FULL ::= AssignmentExpression_noIn_FULL
/:$Set$StartList;:/
	| CommaExpression_noIn_FULL ',' AssignmentExpression_noIn_FULL
/:$Set$AddList2;:/

AssignmentExpression_noIn_FULL ::= ConditionalExpression_noIn_FULL
/.$NoAction./
	| LeftHandSideExpression_noIn_FULL '=' AssignmentExpression_noIn_FULL
/:$Set$MakeAssignment;:/
	| LeftHandSideExpression_noIn_FULL CompoundAssignmentOperator AssignmentExpression_noIn_FULL
/:$Set$MakeAssignment;:/

ConditionalExpression_noIn_FULL ::= YieldExpression_noIn
/.$NoAction./
	| LogicalOrExpression_noIn_FULL
/.$NoAction./
	| LogicalOrExpression_noIn_FULL '?' AssignmentExpression_noIn_FULL ':' AssignmentExpression_noIn_FULL
/:$Set$MakeConditionalExpression;:/

YieldExpression_noIn ::= 'yield' Marker Marker
/:$Set$MakeYieldExpression;:/
	| 'yield' NO_LINE_BREAK AssignmentExpression_noIn_FULL
/:$Set$MakeYieldExpression;:/

LogicalOrExpression_noIn_FULL ::= LogicalAndExpression_noIn_FULL
/.$NoAction./
	| LogicalAndExpression_noIn_FULL '||' LogicalOrExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

LogicalAndExpression_noIn_FULL ::= BitwiseOrExpression_noIn_FULL
/.$NoAction./
	| LogicalAndExpression_noIn_FULL '&&' BitwiseOrExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseOrExpression_noIn_FULL ::= BitwiseXorExpression_noIn_FULL
/.$NoAction./
	| BitwiseOrExpression_noIn_FULL '|' BitwiseXorExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseXorExpression_noIn_FULL ::= BitwiseAndExpression_noIn_FULL
/.$NoAction./
	| BitwiseXorExpression_noIn_FULL '^' BitwiseAndExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseAndExpression_noIn_FULL ::= EqualityExpression_noIn_FULL
/.$NoAction./
	| BitwiseAndExpression_noIn_FULL '&' EqualityExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

EqualityExpression_noIn_FULL ::= RelationalExpression_noIn_FULL
/.$NoAction./
	| EqualityExpression_noIn_FULL '==' RelationalExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_noIn_FULL '!=' RelationalExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_noIn_FULL '===' RelationalExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_noIn_FULL '!==' RelationalExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

RelationalExpression_noIn_FULL ::= ShiftExpression_noIn_FULL
/.$NoAction./
	| RelationalExpression_noIn_FULL '<' ShiftExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_FULL '>' ShiftExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_FULL '<=' ShiftExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_FULL '>=' ShiftExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_FULL 'instanceof' ShiftExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_FULL 'as' TypeExpression
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_FULL 'is' TypeExpression
/:$Set$MakeBinaryExpression;:/

ShiftExpression_noIn_FULL ::= AdditiveExpression_noIn_FULL
/.$NoAction./
	| ShiftExpression_noIn_FULL '<<' AdditiveExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| ShiftExpression_noIn_FULL '>>' AdditiveExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| ShiftExpression_noIn_FULL '>>>' AdditiveExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

AdditiveExpression_noIn_FULL ::= MultiplicativeExpression_noIn_FULL
/.$NoAction./
	| AdditiveExpression_noIn_FULL '+' MultiplicativeExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| AdditiveExpression_noIn_FULL '-' MultiplicativeExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

MultiplicativeExpression_noIn_FULL ::= UnaryExpression_noIn_FULL
/.$NoAction./
	| MultiplicativeExpression_noIn_FULL '*' UnaryExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| MultiplicativeExpression_noIn_FULL '/' UnaryExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| MultiplicativeExpression_noIn_FULL '%' UnaryExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

UnaryExpression_noIn_FULL ::= PostfixExpression_noIn_FULL
/.$NoAction./
	| 'delete' PostfixExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| 'void' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| 'typeof' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '++' PostfixExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '--' PostfixExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '+' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '-' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '~' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '!' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/

PostfixExpression_noIn_FULL ::= LeftHandSideExpression_noIn_FULL
/.$NoAction./
	| LeftHandSideExpression_noIn_FULL NO_LINE_BREAK '++'
/:$Set$MakePostfixExpression;:/
	| LeftHandSideExpression_noIn_FULL NO_LINE_BREAK '--'
/:$Set$MakePostfixExpression;:/

LeftHandSideExpression_noIn_FULL ::= NewExpression_noIn_FULL
/.$NoAction./
	| CallExpression_noIn_FULL
/.$NoAction./

NewExpression_noIn_FULL ::= MemberExpression_noIn_FULL
/.$NoAction./
	| 'new' NewExpression_noIn_FULL Marker
/:$Set$MakeNewExpression;:/

MemberExpression_noIn_FULL ::= PrimaryExpression_noIn_FULL
/.$NoAction./
	| 'new' MemberExpression_noIn_FULL Arguments
/:$Set$MakeNewExpression;:/
	| SuperExpression PropertyOperator
/:$Set$MakeMemberAccess;:/
	| MemberExpression_noIn_FULL PropertyOperator
/:$Set$MakeMemberAccess;:/

PrimaryExpression_noIn_FULL ::= 'null'
/:$Set$MakeNullLiteral;:/
	| 'true'
/:$Set$MakeBooleanLiteral;:/
	| 'false'
/:$Set$MakeBooleanLiteral;:/
	| 'this'
/:$Set$MakeThisExpression;:/
	| NumberLiteral
/:$Set$MakeNumberLiteral;:/
	| StringLiteral
/:$Set$MakeStringLiteral;:/
	| '/'
/:$Set$MakeRegExpInitialiser;:/
	| '/='
/:$Set$MakeRegExpInitialiser;:/
	| ObjectLiteral
/.$NoAction./
	| FunctionExpression_noIn
/.$NoAction./
	| ParenExpression
/.$NoAction./
	| Attribute
/.$NoAction./
	| LetExpression_noIn
/.$NoAction./
	| XMLInitialiser
/.$NoAction./
	| XMLListInitialiser
/.$NoAction./

FunctionExpression_noIn ::= 'function' PropertyIdentifier FunctionSignature FunctionExpressionBody_noIn
/:$Set$MakeFunctionExpression;:/
	| 'function' Marker FunctionSignature FunctionExpressionBody_noIn
/:$Set$MakeFunctionExpression;:/

FunctionExpressionBody_noIn ::= Block
/.$NoAction./
	| EnterClosure AssignmentExpression_noIn_NoBrace CLOSURE
/:$Set$MakeAnonymousBlock;:/

AssignmentExpression_noIn_NoBrace ::= ConditionalExpression_noIn_NoBrace
/.$NoAction./
	| LeftHandSideExpression_noIn_NoBrace '=' AssignmentExpression_noIn_FULL
/:$Set$MakeAssignment;:/
	| LeftHandSideExpression_noIn_NoBrace CompoundAssignmentOperator AssignmentExpression_noIn_FULL
/:$Set$MakeAssignment;:/

ConditionalExpression_noIn_NoBrace ::= YieldExpression_noIn
/.$NoAction./
	| LogicalOrExpression_noIn_NoBrace
/.$NoAction./
	| LogicalOrExpression_noIn_NoBrace '?' AssignmentExpression_noIn_FULL ':' AssignmentExpression_noIn_FULL
/:$Set$MakeConditionalExpression;:/

LogicalOrExpression_noIn_NoBrace ::= LogicalAndExpression_noIn_NoBrace
/.$NoAction./
	| LogicalAndExpression_noIn_NoBrace '||' LogicalOrExpression_noIn_NoBrace
/:$Set$MakeBinaryExpression;:/

LogicalAndExpression_noIn_NoBrace ::= BitwiseOrExpression_noIn_NoBrace
/.$NoAction./
	| LogicalAndExpression_noIn_NoBrace '&&' BitwiseOrExpression_noIn_NoBrace
/:$Set$MakeBinaryExpression;:/

BitwiseOrExpression_noIn_NoBrace ::= BitwiseXorExpression_noIn_NoBrace
/.$NoAction./
	| BitwiseOrExpression_noIn_NoBrace '|' BitwiseXorExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseXorExpression_noIn_NoBrace ::= BitwiseAndExpression_noIn_NoBrace
/.$NoAction./
	| BitwiseXorExpression_noIn_NoBrace '^' BitwiseAndExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

BitwiseAndExpression_noIn_NoBrace ::= EqualityExpression_noIn_NoBrace
/.$NoAction./
	| BitwiseAndExpression_noIn_NoBrace '&' EqualityExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

EqualityExpression_noIn_NoBrace ::= RelationalExpression_noIn_NoBrace
/.$NoAction./
	| EqualityExpression_noIn_NoBrace '==' RelationalExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_noIn_NoBrace '!=' RelationalExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_noIn_NoBrace '===' RelationalExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| EqualityExpression_noIn_NoBrace '!==' RelationalExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

RelationalExpression_noIn_NoBrace ::= ShiftExpression_noIn_NoBrace
/.$NoAction./
	| RelationalExpression_noIn_NoBrace '<' ShiftExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_NoBrace '>' ShiftExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_NoBrace '<=' ShiftExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_NoBrace '>=' ShiftExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_NoBrace 'instanceof' ShiftExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_NoBrace 'as' TypeExpression
/:$Set$MakeBinaryExpression;:/
	| RelationalExpression_noIn_NoBrace 'is' TypeExpression
/:$Set$MakeBinaryExpression;:/

ShiftExpression_noIn_NoBrace ::= AdditiveExpression_noIn_NoBrace
/.$NoAction./
	| ShiftExpression_noIn_NoBrace '<<' AdditiveExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| ShiftExpression_noIn_NoBrace '>>' AdditiveExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| ShiftExpression_noIn_NoBrace '>>>' AdditiveExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

AdditiveExpression_noIn_NoBrace ::= MultiplicativeExpression_noIn_NoBrace
/.$NoAction./
	| AdditiveExpression_noIn_NoBrace '+' MultiplicativeExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| AdditiveExpression_noIn_NoBrace '-' MultiplicativeExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

MultiplicativeExpression_noIn_NoBrace ::= UnaryExpression_noIn_NoBrace
/.$NoAction./
	| MultiplicativeExpression_noIn_NoBrace '*' UnaryExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| MultiplicativeExpression_noIn_NoBrace '/' UnaryExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/
	| MultiplicativeExpression_noIn_NoBrace '%' UnaryExpression_noIn_FULL
/:$Set$MakeBinaryExpression;:/

UnaryExpression_noIn_NoBrace ::= PostfixExpression_noIn_NoBrace
/.$NoAction./
	| 'delete' PostfixExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| 'void' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| 'typeof' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '++' PostfixExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '--' PostfixExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '+' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '-' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '~' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/
	| '!' UnaryExpression_noIn_FULL
/:$Set$MakeUnaryExpression;:/

PostfixExpression_noIn_NoBrace ::= LeftHandSideExpression_noIn_NoBrace
/.$NoAction./
	| LeftHandSideExpression_noIn_NoBrace NO_LINE_BREAK '++'
/:$Set$MakePostfixExpression;:/
	| LeftHandSideExpression_noIn_NoBrace NO_LINE_BREAK '--'
/:$Set$MakePostfixExpression;:/

LeftHandSideExpression_noIn_NoBrace ::= NewExpression_noIn_NoBrace
/.$NoAction./
	| CallExpression_noIn_NoBrace
/.$NoAction./

NewExpression_noIn_NoBrace ::= MemberExpression_noIn_NoBrace
/.$NoAction./
	| 'new' NewExpression_noIn_FULL Marker
/:$Set$MakeNewExpression;:/

MemberExpression_noIn_NoBrace ::= PrimaryExpression_noIn_NoBrace
/.$NoAction./
	| 'new' MemberExpression_noIn_FULL Arguments
/:$Set$MakeNewExpression;:/
	| SuperExpression PropertyOperator
/:$Set$MakeMemberAccess;:/
	| MemberExpression_noIn_NoBrace PropertyOperator
/:$Set$MakeMemberAccess;:/

PrimaryExpression_noIn_NoBrace ::= 'null'
/:$Set$MakeNullLiteral;:/
	| 'true'
/:$Set$MakeBooleanLiteral;:/
	| 'false'
/:$Set$MakeBooleanLiteral;:/
	| 'this'
/:$Set$MakeThisExpression;:/
	| NumberLiteral
/:$Set$MakeNumberLiteral;:/
	| StringLiteral
/:$Set$MakeStringLiteral;:/
	| '/'
/:$Set$MakeRegExpInitialiser;:/
	| '/='
/:$Set$MakeRegExpInitialiser;:/
	| FunctionExpression_noIn
/.$NoAction./
	| ParenExpression
/.$NoAction./
	| Attribute
/.$NoAction./
	| LetExpression_noIn
/.$NoAction./
	| XMLInitialiser
/.$NoAction./
	| XMLListInitialiser
/.$NoAction./

LetExpression_noIn ::= 'let' '(' LetBindingList ')' EnterClosure AssignmentExpression_noIn_FULL CLOSURE
/:$Set$MakeLetExpression;:/

CallExpression_noIn_NoBrace ::= MemberExpression_noIn_NoBrace Arguments
/:$Set$MakeMemberExpression;:/
	| CallExpression_noIn_NoBrace Arguments
/:$Set$MakeMemberExpression;:/
	| CallExpression_noIn_NoBrace PropertyOperator
/:$Set$MakeMemberAccess;:/

CallExpression_noIn_FULL ::= MemberExpression_noIn_FULL Arguments
/:$Set$MakeMemberExpression;:/
	| CallExpression_noIn_FULL Arguments
/:$Set$MakeMemberExpression;:/
	| CallExpression_noIn_FULL PropertyOperator
/:$Set$MakeMemberAccess;:/

VariableDefinition_noIn ::= VariableDefinitionKind VariableBindingList_noIn
/:$Set$MakeVariableDefinition;:/
/!
	void MakeVariableDefinition();!/
/.$Location
void Parser::MakeVariableDefinition()
{
	VariableDefinition* var = astPool->newVariableDefinition();
	var->kind_token = token(1);
	LIST2ARRAY(Variable, 2);
	var->variables = list;
	sym(1) = var;
}./

VariableDefinitionKind ::= 'const'
/.$NoAction./
	| 'var'
/.$NoAction./
	| 'let'
/.$NoAction./

VariableBindingList_noIn ::= VariableBinding_noIn
/:$Set$StartList;:/
	| VariableBindingList_noIn ',' VariableBinding_noIn
/:$Set$AddList3;:/

VariableBinding_noIn ::= TypedPattern Marker Marker
/:$Set$MakeVariableBinding;:/
	| TypedPattern '=' AssignmentExpression_noIn_FULL
/:$Set$MakeVariableBinding;:/

OptionalExpression ::= %Empty
/.$NullAction./
	| CommaExpression_allowIn_FULL
/.$NoAction./

ForInBinding ::= LeftHandSideExpression_noIn_FULL
/.$NoAction./
	| VariableDefinitionKind VariableBinding_noIn
/:$Set$SetVariableKind;:/
/!
	void SetVariableKind();!/
/.$Location
void Parser::SetVariableKind()
{
	//TODO
	sym(1) = sym(2);
}./

IfStatement_abbrev ::= 'if' ParenExpression Substatement_abbrev
/:$Set$MakeIfStatement;:/
/!
	void MakeIfStatement();!/
/.$Location
void Parser::MakeIfStatement()
{
	IfStatement* is = astPool->newIfStatement();
	is->condition = CAST<Expression*>(sym(2));
	is->thenStatement = CAST<Statement*>(sym(3));
	sym(1) = is;
}
./
	| 'if' ParenExpression Substatement_noShortIf 'else' Substatement_abbrev
/:$Set$MakeIfElseStatement;:/
/!
	void MakeIfElseStatement();!/
/.$Location
void Parser::MakeIfElseStatement()
{
	IfStatement* is = astPool->newIfStatement();
	is->condition = CAST<Expression*>(sym(2));
	is->thenStatement = CAST<Statement*>(sym(3));
	is->elseStatement = CAST<Statement*>(sym(5));
	sym(1) = is;
}
./

Substatement_noShortIf ::= EmptyStatement
/.$NoAction./
	| Statement_noShortIf
/.$NoAction./
	| VariableDefinition_allowIn Semicolon_noShortIf
/:$Set$UpdateSemicolonPosition;:/

Statement_noShortIf ::= Block
/.$NoAction./
	| BreakStatement Semicolon_noShortIf
/:$Set$UpdateSemicolonPosition;:/
	| ContinueStatement Semicolon_noShortIf
/:$Set$UpdateSemicolonPosition;:/
	| DefaultXMLNamespaceStatement Semicolon_noShortIf
/:$Set$UpdateSemicolonPosition;:/
	| DoStatement Semicolon_noShortIf
/:$Set$UpdateSemicolonPosition;:/
	| ExpressionStatement Semicolon_noShortIf
/:$Set$UpdateSemicolonPosition;:/
	| ForStatement_noShortIf
/.$NoAction./
	| IfStatement_noShortIf
/.$NoAction./
	| LabeledStatement_noShortIf
/.$NoAction./
	| LetBlockStatement
/.$NoAction./
	| ReturnStatement Semicolon_noShortIf
/:$Set$UpdateSemicolonPosition;:/
	| SwitchStatement
/.$NoAction./
	| ThrowStatement Semicolon_noShortIf
/:$Set$UpdateSemicolonPosition;:/
	| TryStatement
/.$NoAction./
	| WhileStatement_noShortIf
/.$NoAction./
	| WithStatement_noShortIf
/.$NoAction./

Semicolon_noShortIf ::= %Empty
/.$NullAction./
	| ';'
/.$NoAction./
	| VirtualSemicolon
/.$NoAction./

ForStatement_noShortIf ::= 'for' '(' ForInitialiser ';' OptionalExpression ';' OptionalExpression ')' Substatement_noShortIf
/:$Set$MakeForStatement;:/
	| 'for' '(' ForInBinding 'in' CommaExpression_allowIn_FULL ')' Substatement_noShortIf
/:$Set$MakeForeachStatement;:/
	| 'for' 'each' '(' ForInBinding 'in' CommaExpression_allowIn_FULL ')' Substatement_noShortIf
/:$Set$MakeForeachStatement;:/

IfStatement_noShortIf ::= 'if' ParenExpression Substatement_noShortIf 'else' Substatement_noShortIf
/:$Set$MakeIfElseStatement;:/

LabeledStatement_noShortIf ::= Identifier ':' Substatement_noShortIf
/:$Set$MakeLabeledStatement;:/
/!
	void MakeLabeledStatement();!/
/.$Location
void Parser::MakeLabeledStatement()
{
	LabeledStatement* ls = astPool->newLabeledStatement();
	/*
	ls->label_token = token(1);
	ls->colon_token = token(2);
	*/
	ls->statement = CAST<Statement*>(sym(3));
	sym(1) = ls;
}
./

LetBlockStatement ::= 'let' '(' LetBindingList ')' Block
/:$Set$MakeLetStatement;:/
/!
	void MakeLetStatement();!/
/.$Location
void Parser::MakeLetStatement()
{
	LetStatement* ls = astPool->newLetStatement();
	LIST2ARRAY(Variable, 3);
	ls->bindings = list;
	ls->block = CAST<Block*>(sym(5));
	sym(1) = ls;
}
./

ReturnStatement ::= 'return' Marker Marker
/:$Set$MakeReturnStatement;:/
/!
	void MakeReturnStatement();!/
/.$Location
void Parser::MakeReturnStatement()
{
	ReturnStatement* rs = astPool->newReturnStatement();
	rs->expression = toExpression(3);
	sym(1) = rs;
}
./
	| 'return' NO_LINE_BREAK CommaExpression_allowIn_FULL
/:$Set$MakeReturnStatement;:/

SwitchStatement ::= 'switch' ParenExpression '{' CaseElements '}'
/:$Set$MakeSwitchStatement;:/
/!
	void MakeSwitchStatement();!/
/.$Location
void Parser::MakeSwitchStatement()
{
	SwitchStatement* ss = astPool->newSwitchStatement();
	ss->condition = CAST<Expression*>(sym(2));
	LIST2ARRAY(Statement, 4);
	ss->statements = list;
	sym(1) = ss;
}
./

CaseElements ::= %Empty
/.$NullAction./
	| CaseLabel
/:$Set$StartList;:/
	| CaseLabel CaseElementsPrefix CaseLabel
/:$Set$MakeCaseElements;:/
/!
	void MakeCaseElements();!/
/.$Location
void Parser::MakeCaseElements()
{
	StartList();
	
	MergeList(1, 2);
	
	AddList3();
}
./
	| CaseLabel CaseElementsPrefix Directive_abbrev
/:$Set$MakeCaseElements;:/

CaseLabel ::= 'case' CommaExpression_allowIn_FULL ':'
/:$Set$MakeCaseLabel;:/
/!
	void MakeCaseLabel();!/
/.$Location
void Parser::MakeCaseLabel()
{
	CaseLabel* cl = astPool->newCaseLabel();
	cl->expression = toExpression(2);
	sym(1) = cl;
}
./
	| 'default' Marker ':'
/:$Set$MakeCaseLabel;:/

CaseElementsPrefix ::= %Empty
/.$NullAction./
	| CaseElementsPrefix CaseLabel
/:$Set$AppendList;:/
	| CaseElementsPrefix Directive_full
/:$Set$AppendList;:/

Directive_abbrev ::= EmptyStatement
/.$NoAction./
	| Statement_abbrev
/.$NoAction./
	| SuperStatement_abbrev
/.$NoAction./
	| AnnotatableDirective_abbrev
/.$NoAction./
	| Attributes AnnotatableDirective_abbrev
/:$Set$MakeAttributes;:/
	| Attributes Block
/:$Set$MakeAttributes;:/

SuperStatement_abbrev ::= 'super' Arguments Semicolon_abbrev
/:$Set$MakeSuperStatement;:/
/!
	void MakeSuperStatement();!/
/.$Location
void Parser::MakeSuperStatement()
{
	SuperStatement* ss = astPool->newSuperStatement();
	LIST2ARRAY(Expression, 2);
	ss->arguments = list;
	sym(1) = ss;
	UpdateSemicolonPosition(3);
}
./

AnnotatableDirective_abbrev ::= VariableDefinition_allowIn Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| FunctionDeclaration Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| FunctionDefinition_abbrev
/.$NoAction./
	| ClassDefinition
/.$NoAction./
	| InterfaceDefinition
/.$NoAction./
	| IncludeDirective
/.$NoAction./
	| UsePragma Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| ImportDirective Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| NamespaceDefinition Semicolon_abbrev
/:$Set$UpdateSemicolonPosition;:/
	| PackageDefinition
/.$NoAction./

VariableDefinition_allowIn ::= VariableDefinitionKind VariableBindingList_allowIn
/:$Set$MakeVariableDefinition;:/

FunctionDeclaration ::= 'function' FunctionName FunctionSignature
/:$Set$MakeFunctionDeclaration;:/
/!
	void MakeFunctionDeclaration();!/
/.$Location
void Parser::MakeFunctionDeclaration()
{
	FunctionDeclaration* decl = astPool->newFunctionDeclaration();
	decl->func = CAST<Function*>(sym(3));
	//TODO func->name = 
	sym(1) = decl;
}
./

FunctionName ::= PropertyIdentifier
/.$NoAction./
	| OverloadedOperator
/.$NoAction./
	| 'get' PropertyIdentifier
/:$Set$setSym2ToSym1;:/
	| 'set' PropertyIdentifier
/:$Set$setSym2ToSym1;:/

OverloadedOperator ::= '+'
/.$NoAction./
	| '-'
/.$NoAction./
	| '~'
/.$NoAction./
	| '*'
/.$NoAction./
	| '/'
/.$NoAction./
	| '%'
/.$NoAction./
	| '<'
/.$NoAction./
	| '>'
/.$NoAction./
	| '<='
/.$NoAction./
	| '>='
/.$NoAction./
	| '=='
/.$NoAction./
	| '<<'
/.$NoAction./
	| '>>'
/.$NoAction./
	| '>>>'
/.$NoAction./
	| '&'
/.$NoAction./
	| '|'
/.$NoAction./
	| '==='
/.$NoAction./
	| '!='
/.$NoAction./
	| '!=='
/.$NoAction./
	| '[' ']'
/.$NoAction./

FunctionDefinition_abbrev ::= FunctionDeclaration FunctionBody_abbrev
/:$Set$MakeFunctionDefinition;:/
/!
	void MakeFunctionDefinition();!/
/.$Location
void Parser::MakeFunctionDefinition()
{
	FunctionDeclaration* decl = CAST<FunctionDeclaration*>(sym(1));
	decl->func->body = CAST<Block*>(sym(2));
}
./

FunctionBody_abbrev ::= Block
/.$NoAction./
	| AssignmentExpression_allowIn_NoBrace Semicolon_abbrev
/:$Set$MakeAnonymousBody;:/
/!
	void MakeAnonymousBody();!/
/.$Location
void Parser::MakeAnonymousBody()
{
	Block* block = astPool->newBlock();
	sym(1) = block;
}./

ClassDefinition ::= 'class' Identifier TypeParameters SuperOpt InterfacesOpt Block
/:$Set$MakeClassDefinition;:/
/!
	void MakeClassDefinition();!/
/.$Location
void Parser::MakeClassDefinition()
{
	//TODO
}
./

SuperOpt ::= %Empty
/.$NullAction./
	| 'extends' TypeReference
/:$Set$setSym2ToSym1;:/

InterfacesOpt ::= %Empty
/.$NullAction./
	| 'implements' TypeReferenceList
/:$Set$setSym2ToSym1;:/

TypeReferenceList ::= TypeReference
/:$Set$StartList;:/
	| TypeReferenceList ',' TypeReference
/:$Set$AddList3;:/

InterfaceDefinition ::= 'interface' Identifier TypeParameters InterfaceInheritance Block
/:$Set$MakeInterfaceDefinition;:/
/!
	void MakeInterfaceDefinition();!/
/.$Location
void Parser::MakeInterfaceDefinition()
{
	//TODO
}
./

InterfaceInheritance ::= %Empty
/.$NullAction./
	| 'extends' TypeReferenceList
/:$Set$setSym2ToSym1;:/

IncludeDirective ::= 'include' StringLiteral
/:$Set$MakeInclude;:/
/!
	void MakeInclude();!/
/.$Location
void Parser::MakeInclude()
{
	Include* inc = astPool->newInclude();
	inc->include_token = token(1);
	inc->url = CAST<StringLiteral*>(sym(2));
	sym(1) = inc;
}
./

UsePragma ::= 'use' 'namespace' NonAssignmentExpression_allowIn
/:$Set$MakeUsePragma;:/
/!
	void MakeUsePragma();!/
/.$Location
void Parser::MakeUsePragma()
{
	Use* use = astPool->newUse();
	use->use_token = token(1);
	use->ns = CAST<Expression*>(sym(3));
	sym(1) = use;
}
./
	| 'use' IncludeDirective
/:$Set$MakeUseInclude;:/
/!
	void MakeUseInclude();!/
/.$Location
void Parser::MakeUseInclude()
{
	Include* inc = CAST<Include*>(sym(2));
	inc->use_token = token(1);
	sym(1) = inc;
}
./

ImportDirective ::= 'import' ImportName Marker Marker
/:$Set$MakeImportDirective;:/
/!
	void MakeImportDirective();!/
/.$Location
void Parser::MakeImportDirective()
{
	Import* impt;
	if (sym(4)) {
		impt = CAST<Import*>(sym(4));
		impt->alias = CAST<Identifier*>(sym(2));
	} else {
		impt = CAST<Import*>(sym(2));
	}
	impt->importToken = token(1);
	sym(1) = impt;
}
./
	| 'import' Identifier '=' ImportName
/:$Set$MakeImportDirective;:/

ImportName ::= PackageName Marker Marker
/:$Set$MakeImportName;:/
/!
	void MakeImportName();!/
/.$Location
void Parser::MakeImportName()
{
	Import* impt = astPool->newImport();
	LIST2ARRAY(Expression,1)
	impt->names = list;
	if (token(3) > token(2))
		impt->starToken = token(3);
	sym(1) = impt;
}
./
	| PackageName '.' '*'
/:$Set$MakeImportName;:/

PackageName ::= Identifier
/:$Set$StartList;:/
	| PackageName '.' Identifier
/:$Set$AddList3;:/

NamespaceDefinition ::= 'namespace' Identifier Marker Marker
/:$Set$MakeNamespaceDefinition;:/
/!
	void MakeNamespaceDefinition();!/
/.$Location
void Parser::MakeNamespaceDefinition()
{
	//TODO
}
./
	| 'namespace' Identifier '=' AssignmentExpression_allowIn_FULL
/.$NoAction./

PackageDefinition ::= 'package' PackageNameOpt Block
/:$Set$MakePackageDefinition;:/
/!
	void MakePackageDefinition();!/
/.$Location
void Parser::MakePackageDefinition()
{
	Package* pkg = astPool->newPackage();
	pkg->packageToken = token(1);
	if (sym(2)) {
		LIST2ARRAY(Expression,2);
		pkg->names = list;
	}
	pkg->body = CAST<Block*>(sym(3));
	sym(1) = pkg;
}
./

PackageNameOpt ::= %Empty
/.$NullAction./
	| PackageName
/.$NoAction./

Attributes ::= Attribute NO_LINE_BREAK
/:$Set$StartList;:/
	| Attributes Attribute NO_LINE_BREAK
/:$Set$AddList2;:/

ThrowStatement ::= 'throw' NO_LINE_BREAK CommaExpression_allowIn_FULL
/:$Set$MakeThrowStatement;:/
/!
	void MakeThrowStatement();!/
/.$Location
void Parser::MakeThrowStatement()
{
	ThrowStatement* ts = astPool->newThrowStatement();
	ts->expression = toExpression(3);
	sym(1) = ts;
}
./

TryStatement ::= 'try' Block CatchClauses Marker
/:$Set$MakeTryStatement;:/
/!
	void MakeTryStatement();!/
/.$Location
void Parser::MakeTryStatement()
{
	TryStatement* ts = astPool->newTryStatement();
	ts->block = CAST<Block*>(sym(2));
	if (sym(3)) {
		LIST2ARRAY(CatchClause, 3);
		ts->catchs = list;
	}
	if (sym(4)) {
		ts->finallyClause = CAST<FinallyClause*>(sym(4));
	}
	sym(1) = ts;
}
./
	| 'try' Block CatchClauses Finally
/:$Set$MakeTryStatement;:/
	| 'try' Block Marker Finally
/:$Set$MakeTryStatement;:/

CatchClauses ::= CatchClause
/:$Set$StartList;:/
	| CatchClauses CatchClause
/:$Set$AddList2;:/

CatchClause ::= 'catch' '(' Parameter Marker Marker ')' Block
/:$Set$MakeCatchClause;:/
/!
	void MakeCatchClause();!/
/.$Location
void Parser::MakeCatchClause()
{
	CatchClause* cc = astPool->newCatchClause();
	cc->parameter = CAST<Parameter*>(sym(3));
	if (sym(5))
		cc->condition = toExpression(5);
	
	cc->block = CAST<Block*>(sym(5));
	sym(1) = cc;
}
./
	| 'catch' '(' Parameter 'if' CommaExpression_allowIn_FULL ')' Block
/.$NoAction./

Finally ::= 'finally' Block
/:$Set$MakeFinallyClause;:/
/!
	void MakeFinallyClause();!/
/.$Location
void Parser::MakeFinallyClause()
{
	FinallyClause* fc = astPool->newFinallyClause();
	fc->block = CAST<Block*>(sym(2));
	sym(1) = fc;
}
./

WhileStatement_noShortIf ::= 'while' ParenExpression Substatement_noShortIf
/:$Set$MakeWhileStatement;:/
/!
	void MakeWhileStatement();!/
/.$Location
void Parser::MakeWhileStatement()
{
	WhileStatement* ws = astPool->newWhileStatement();
	ws->condition = CAST<Expression*>(sym(2));
	ws->action = CAST<Statement*>(sym(3));
	sym(1) = ws;
}
./

WithStatement_noShortIf ::= 'with' ParenExpression Substatement_noShortIf
/:$Set$MakeWithStatement;:/
/!
	void MakeWithStatement();!/
/.$Location
void Parser::MakeWithStatement()
{
	WithStatement* ws = astPool->newWithStatement();
	ws->object = CAST<Expression*>(sym(2));
	ws->statement = CAST<Statement*>(sym(3));
	sym(1) = ws;
}
./

LabeledStatement_abbrev ::= Identifier ':' Substatement_abbrev
/:$Set$MakeLabeledStatement;:/

WhileStatement_abbrev ::= 'while' ParenExpression Substatement_abbrev
/:$Set$MakeWhileStatement;:/

WithStatement_abbrev ::= 'with' ParenExpression Substatement_abbrev
/:$Set$MakeWithStatement;:/

ForStatement_full ::= 'for' '(' ForInitialiser ';' OptionalExpression ';' OptionalExpression ')' Substatement_full
/:$Set$MakeForStatement;:/
	| 'for' '(' ForInBinding 'in' CommaExpression_allowIn_FULL ')' Substatement_full
/:$Set$MakeForeachStatement;:/
	| 'for' 'each' '(' ForInBinding 'in' CommaExpression_allowIn_FULL ')' Substatement_full
/:$Set$MakeForeachStatement;:/

Substatement_full ::= EmptyStatement
/.$NoAction./
	| Statement_full
/.$NoAction./
	| VariableDefinition_allowIn Semicolon_full
/:$Set$UpdateSemicolonPosition;:/

IfStatement_full ::= 'if' ParenExpression Substatement_full
/:$Set$MakeIfStatement;:/
	| 'if' ParenExpression Substatement_noShortIf 'else' Substatement_full
/:$Set$MakeIfElseStatement;:/

LabeledStatement_full ::= Identifier ':' Substatement_full
/:$Set$MakeLabeledStatement;:/

WhileStatement_full ::= 'while' ParenExpression Substatement_full
/:$Set$MakeWhileStatement;:/

WithStatement_full ::= 'with' ParenExpression Substatement_full
/:$Set$MakeWithStatement;:/

SuperStatement_full ::= 'super' Arguments Semicolon_full
/:$Set$MakeSuperStatement;:/

AnnotatableDirective_full ::= VariableDefinition_allowIn Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| FunctionDeclaration Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| FunctionDefinition_full
/.$NoAction./
	| ClassDefinition
/.$NoAction./
	| InterfaceDefinition
/.$NoAction./
	| IncludeDirective
/.$NoAction./
	| UsePragma Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| ImportDirective Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| NamespaceDefinition Semicolon_full
/:$Set$UpdateSemicolonPosition;:/
	| PackageDefinition
/.$NoAction./

FunctionDefinition_full ::= FunctionDeclaration FunctionBody_full
/:$Set$MakeFunctionDefinition;:/

FunctionBody_full ::= Block
/.$NoAction./
	| AssignmentExpression_allowIn_NoBrace Semicolon_full
/:$Set$MakeAnonymousBody;:/

%End
