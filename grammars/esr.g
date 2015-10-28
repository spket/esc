ReservedIdentifier ::= 'as'
	| 'break'
	| 'case'
	| 'catch'
	| 'class'
	| 'const'
	| 'continue'
	| 'default'
	| 'delete'
	| 'do'
	| 'else'
	| 'false'
	| 'finally'
	| 'for'
	| 'function'
	| 'if'
	| 'import'
	| 'in'
	| 'include'
	| 'instanceof'
	| 'interface'
	| 'is'
	| 'let'
	| 'new'
	| 'null'
	| 'package'
	| 'private'
	| 'protected'
	| 'public'
	| 'return'
	| 'super'
	| 'switch'
	| 'this'
	| 'throw'
	| 'true'
	| 'try'
	| 'typeof'
	| 'use'
	| 'var'
	| 'void'
	| 'while'
	| 'with'
	| 'yield'

ContextuallyReservedIdentifier ::= 'each'
	| 'extends'
	| 'get'
	| 'implements'
	| 'namespace'
	| 'set'
	| 'xml'

Identifier ::= 'identifier'
	/.$MakeIdentifier
	{
		sym(1) = astPool->newIdentifier(token(1));
	}
	./
	| ContextuallyReservedIdentifier
	/.$MakeIdentifier./

PropertyIdentifier ::= Identifier
	| ReservedIdentifier
	/.$MakeIdentifier./

ReservedNamespace ::= 'private'
	/.$MakeIdentifier./
	| 'protected'
	/.$MakeIdentifier./
	| 'public'
	/.$MakeIdentifier./

Marker ::= %Empty

QualifiedNameIdentifier ::= PropertyIdentifier
	| '*'
	/.$MakeWildcard
	{
		if (TK_MULTIPLY_EQUAL == lexer->getKind())
			lexer->split();
		sym(1) = astPool->newWildcard(token(1));
	}
	./
	| '*='
	/.$MakeWildcard./
	| Brackets

Qualifier ::= '*'
	/.$MakeWildcard./
	| Identifier
	| ReservedNamespace

SimpleQualifiedName ::= Identifier
	| '*'
	/.$MakeWildcard./
	| '*='
	/.$MakeWildcard./
	| Qualifier '::' QualifiedNameIdentifier
	/.$MakeQualfiedIdentifier
	{
		QualifiedName* qn = astPool->newQualifiedName();
		qn->ns = CAST<Expression*>(sym(1));
		qn->name = CAST<Expression*>(sym(3));
		sym(1) = qn;
	}
	./

ExpressionQualifiedName ::= ParenExpression '::' PropertyIdentifier
/.$MakeQualfiedIdentifier./

QualifiedName ::= SimpleQualifiedName
	| ExpressionQualifiedName

AttributeName ::= '@' Brackets
	/.$MakeAttributeName
	{
		AttributeName* attr = astPool->newAttributeName(token(1));
		attr->expression = CAST<Expression*>(sym(2));
		sym(1) = attr;
	}
	./
	| '@' QualifiedName
	/.$MakeAttributeName./
	| '@' ReservedIdentifier
	/.$Action
	$DefaultHeader
	{
		AttributeName* attr = astPool->newAttributeName(token(1));
		attr->expression = astPool->newIdentifier(token(2));
		sym(1) = attr;
	}
	./

PrimaryName ::= QualifiedName
	| AttributeName

ParenExpression ::=  '(' ArrayComprehension ')'
	/.$MakeParenExpression./
	| '(' CommaExpression[allowIn] ')'
	/.$MakeParenExpression
	{
		ParenExpression* pe = astPool->newParenExpression();
		pe->leftParen = token(1);
		pe->expression = toExpression(2);
		pe->rightParen = token(3);
	}
	./

FunctionExpression[b] ::= 'function' PropertyIdentifier FunctionSignature FunctionExpressionBody[b]
	/.$MakeFunctionExpression
	{
		Function* func = CAST<Function*>(sym(3));
		func->function_token = token(1);
		//func->identifier = CAST<Identifier*>(sym(2));
		func->body = CAST<Block*>(sym(4));
		sym(1) = func;
	}
	./
	| 'function' Marker FunctionSignature FunctionExpressionBody[b]
	/.$MakeFunctionExpression./

FunctionExpressionBody[b] ::= Block
	| EnterClosure AssignmentExpression[NoBrace,b] CLOSURE
	/.$MakeAnonymousBlock
	{
		Block* block = astPool->newBlock();
		sym(1) = block;
	}
	./

EnterClosure ::= %Empty
/.$EnterClosure
{
	enterClosure();
}
./

ObjectLiteral ::= '{' FieldList '}'
/.$MakeObjectLiteral
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
	| NonemptyFieldList
	| NonemptyFieldList ','

NonemptyFieldList ::= LiteralField
	/.$StartList./
	| NonemptyFieldList ',' LiteralField
	/.$AddList3./

LiteralField ::= FieldName ':' AssignmentExpression[allowIn]
	/.$MakeField
	{
		LiteralField* lf = astPool->newLiteralField();
		lf->name = CAST<Expression*>(sym(1));
		lf->value = CAST<Expression*>(sym(3));
		sym(1) = lf;
	}
	./
	| 'get' FieldName FunctionSignature FunctionExpressionBody[allowIn]
	/.$MakeProperty
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
	| 'set' FieldName FunctionSignature FunctionExpressionBody[allowIn]
	/.$MakeProperty./

FieldName ::= StringLiteral
	/.$MakeStringLiteral./
	| NumberLiteral
	/.$MakeNumberLiteral./
	| ReservedIdentifier
	/.$MakeIdentifier./
	| QualifiedName

ArrayLiteral ::= '[' ElementList ']'
	/.$MakeArrayLiteral
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
	/.$MakeArrayLiteral./

ElementList ::= %Empty
	/.$StartList./
	| LiteralElement
	/.$StartList./
	| ElementList ',' Marker
	/.$AddList3./
	| ElementList ',' LiteralElement
	/.$AddList3./

LiteralElement ::= AssignmentExpression[allowIn]

ArrayComprehension ::= AssignmentExpression[allowIn] ComprehensionExpression
/.$MakeArrayComprehension
{
	ArrayComprehension* ac = astPool->newArrayComprehension();
	ac->initializer = CAST<Expression*>(sym(1));
	ac->tail = CAST<Expression*>(sym(2));
	sym(1) = ac;
}
./

ComprehensionExpression ::= 'for' '(' TypedPattern 'in' CommaExpression[allowIn] ')' ComprehensionClause
	/.$MakeComprehensionExpression
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
	| 'for' 'each' '(' TypedPattern 'in' CommaExpression[allowIn] ')' ComprehensionClause
	/.$MakeComprehensionExpression./

ComprehensionClause ::= %Empty
	| 'if' ParenExpression
	/.$MakeComprehensionClause
	{
		ComprehensionClause* cc = astPool->newComprehensionClause();
		cc->if_token = token(1);
		cc->expression = CAST<Expression*>(sym(2));
		sym(1) = cc;
	}
	./
	| ComprehensionExpression

PrimaryExpression[FULL,b] ::= 'null'
	/.$MakeNullLiteral
	{
		sym(1) = astPool->newNullLiteral(token(1));
	}
	./
	| 'true'
	/.$MakeBooleanLiteral
	{
		sym(1) = astPool->newBooleanLiteral(token(1));
	}
	./
	| 'false'
	/.$MakeBooleanLiteral./
	| 'this'
	/.$MakeThisExpression
	{
		sym(1) = astPool->newThisExpression(token(1));
	}
	./
	| NumberLiteral
	/.$MakeNumberLiteral
	{
		sym(1) = astPool->newNumberLiteral(token(1));
	}
	./
	| StringLiteral
	/.$MakeStringLiteral
	{
		sym(1) = astPool->newStringLiteral(token(1));
	}
	./
	| '/'
	/.$MakeRegExpInitialiser
	{
		lexer->scanRegExp();
		
		sym(1) = astPool->newRegExpLiteral(token(1));
	}
	./
	| '/='
	/.$MakeRegExpInitialiser./
	| ObjectLiteral
	| FunctionExpression[b]
	--| Lambda
	| ParenExpression
	| Attribute
	| LetExpression[b]
	| XMLInitialiser
	| XMLListInitialiser

PrimaryExpression[NoBrace,b] ::= 'null'
	/.$MakeNullLiteral./
	| 'true'
	/.$MakeBooleanLiteral./
	| 'false'
	/.$MakeBooleanLiteral./
	| 'this'
	/.$MakeThisExpression./
	| NumberLiteral
	/.$MakeNumberLiteral./
	| StringLiteral
	/.$MakeStringLiteral./
	| '/'
		/.$MakeRegExpInitialiser./
	| '/='
		/.$MakeRegExpInitialiser./
	| FunctionExpression[b]
	--| Lambda
	| ParenExpression
	| Attribute
	| LetExpression[b]
	| XMLInitialiser
	| XMLListInitialiser
	
PrimaryExpression[ES,b] ::= 'null'
	/.$MakeNullLiteral./
	| 'true'
	/.$MakeBooleanLiteral./
	| 'false'
	/.$MakeBooleanLiteral./
	| 'this'
	/.$MakeThisExpression./
	| NumberLiteral
	/.$MakeNumberLiteral./
	| StringLiteral
	/.$MakeStringLiteral./
	| '/'
		/.$MakeRegExpInitialiser./
	| '/='
		/.$MakeRegExpInitialiser./
	--| Lambda
	| ParenExpression
	| Attribute
	| XMLInitialiser
	| XMLListInitialiser

--Lambda ::= '&' IdentifierOpt ParametersOpt Block
--
--IdentifierOpt ::= %Empty
--	| PropertyIdentifier
--
--ParametersOpt ::= %Empty
--	| '(' Parameters ')'

--	| '&' Identifier '(' Parameters ')' Block
--Lambda ::= '(' LambdaParameters ')' Block

SuperExpression ::= 'super' Marker
	/.$MakeSuperExpression
	{
		SuperExpression* se = astPool->newSuperExpression(token(1));
		if (sym(2))
			se->argument = CAST<Expression*>(sym(2));
		
		sym(1) = se;
	}
	./
	| 'super' ParenExpression
	/.$MakeSuperExpression./

Arguments ::= '(' Marker ')'
	/.$MakeArguments
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
	/.$MakeArguments./
	| '(' ArrayComprehension ')'
	/.$MakeArguments./
	| '(' CommaExpression[allowIn] ')'
	/.$MakeArguments./
	| '(' CommaExpression[allowIn] ',' ResetExpression ')'
	/.$MergeArguments
	{
		ADD_LIST(2, 4)
		assignToken(5, 3);
		MakeArguments();
	}
	./

ResetExpression ::= '...' AssignmentExpression[allowIn]
/.$MakeResetExpression
{
	ResetExpression* re = astPool->newResetExpression();
	re->reset_token = token(1);
	re->expression = CAST<Expression*>(sym(2));
	sym(1) = re;
}
./

PropertyOperator ::= '.' PrimaryName
	/.$MakePropertyAccess
	{
		PropertyAccess* pa = astPool->newPropertyAccess();
		pa->op = kind(1);
		pa->name = CAST<Expression*>(sym(2));
		sym(1) = pa;
	}
	./
	| '.' ReservedIdentifier
	/.$MakePropertyAccess./
	| '..' PrimaryName
	/.$MakePropertyAccess./
	| '..' ReservedIdentifier
	/.$MakePropertyAccess./
	| '.' ParenExpression
	/.$MakePropertyAccess./
	| Brackets
	/.$MakeArrayAccess
	{
		ArrayAccess* aa = astPool->newArrayAccess();
		LIST2ARRAY(Expression, 1);
		aa->dims = list;
		sym(1) = aa;
	}
	./
	| TypeArguments
	/.$MakeTypeAccess
	{
		TypeAccess* ta = astPool->newTypeAccess();
		LIST2ARRAY(Expression, 1);
		ta->arguments = list;
		sym(1) = ta;
	}
	./

Brackets ::= '[' CommaExpression[allowIn] ']'
	/.$MakeArguments./
	| '[' ResetExpression ']'
	/.$MakeArguments./
	| '[' CommaExpression[allowIn] ',' ResetExpression ']'
	/.$MergeArguments./

TypeEnd ::= '>'
	| '>>'
	/.$CheckTypeEnd
	{
		lexer->split();
	}
	./
	| '>>>'
	/.$CheckTypeEnd./
	| '>='
	/.$CheckTypeEnd./
	| '>>='
	/.$CheckTypeEnd./
	| '>>>='
	/.$CheckTypeEnd./

TypeArguments ::= '.<' TypeExpressionList TypeEnd
/.$setSym2ToSym1./

TypeExpressionList ::= TypeExpression
	/.$StartList./
	| TypeExpressionList ',' TypeExpression
	/.$AddList3./

MemberExpression[g,b] ::= PrimaryExpression[g,b]
	| 'new' MemberExpression[b] Arguments
	/.$MakeNewExpression
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
	/.$MakeMemberAccess
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
	| MemberExpression[g,b] PropertyOperator
	/.$MakeMemberAccess./

CallExpression[g,b] ::= MemberExpression[g,b] Arguments
	/.$MakeMemberExpression
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
	| CallExpression[g,b] Arguments
	/.$MakeMemberExpression./
	| CallExpression[g,b] PropertyOperator
	/.$MakeMemberAccess./

NewExpression[g,b] ::= MemberExpression[g,b]
	| 'new' NewExpression[b] Marker
	/.$MakeNewExpression./

LeftHandSideExpression[g,b] ::= NewExpression[g,b]
	| CallExpression[g,b]

PostfixExpression[g,b] ::= LeftHandSideExpression[g,b]
	| LeftHandSideExpression[g,b] NO_LINE_BREAK '++'
	/.$MakePostfixExpression
	{
		PostfixExpression* pe = astPool->newPostfixExpression();
		pe->lhs = CAST<Expression*>(sym(1));
		pe->operator_token = token(3);
		sym(1) = pe;
	}
	./
	| LeftHandSideExpression[g,b] NO_LINE_BREAK '--'
	/.$MakePostfixExpression./

UnaryExpression[g,b] ::= PostfixExpression[g,b]
	| 'delete' PostfixExpression[b]
	/.$MakeUnaryExpression
	{
		UnaryExpression* ue = astPool->newUnaryExpression();
		ue->op = kind(1);
		ue->expression = CAST<Expression*>(sym(2));
	}
	./
	| 'void' UnaryExpression[b]
	/.$MakeUnaryExpression./
	| 'typeof' UnaryExpression[b]
	/.$MakeUnaryExpression./
	| '++' PostfixExpression[b]
	/.$MakeUnaryExpression./
	| '--' PostfixExpression[b]
	/.$MakeUnaryExpression./
	| '+' UnaryExpression[b]
	/.$MakeUnaryExpression./
	| '-' UnaryExpression[b]
	/.$MakeUnaryExpression./
	| '~' UnaryExpression[b]
	/.$MakeUnaryExpression./
	| '!' UnaryExpression[b]
	/.$MakeUnaryExpression./

UnaryExpression[ES,b] ::= PostfixExpression[ES,b]
	| 'delete' PostfixExpression[b]
	/.$MakeUnaryExpression./
	| 'void' UnaryExpression[b]
	/.$MakeUnaryExpression./
	| 'typeof' UnaryExpression[b]
	/.$MakeUnaryExpression./
	| '++' PostfixExpression[b]
	/.$MakeUnaryExpression./
	| '--' PostfixExpression[b]
	/.$MakeUnaryExpression./
	| '+' UnaryExpression[b]
	/.$MakeUnaryExpression./
	| '-' UnaryExpression[b]
	/.$MakeUnaryExpression./
	| '~' UnaryExpression[b]
	/.$MakeUnaryExpression./
	| '!' UnaryExpression[b]
	/.$MakeUnaryExpression./

MultiplicativeExpression[g,b] ::= UnaryExpression[g,b]
	| MultiplicativeExpression[g,b] '*' UnaryExpression[b]
	/.$MakeBinaryExpression
	{
		BinaryExpression* be = astPool->newBinaryExpression();
		be->left = CAST<Expression*>(sym(1));
		be->op = kind(2);
		be->right = CAST<Expression*>(sym(3));
		sym(1) = be;
	}
	./
	| MultiplicativeExpression[g,b] '/' UnaryExpression[b]
	/.$MakeBinaryExpression./
	| MultiplicativeExpression[g,b] '%' UnaryExpression[b]
	/.$MakeBinaryExpression./

AdditiveExpression[g,b] ::= MultiplicativeExpression[g,b]
	| AdditiveExpression[g,b] '+' MultiplicativeExpression[b]
	/.$MakeBinaryExpression./
	| AdditiveExpression[g,b] '-' MultiplicativeExpression[b]
	/.$MakeBinaryExpression./

ShiftExpression[g,b] ::= AdditiveExpression[g,b]
	| ShiftExpression[g,b] '<<' AdditiveExpression[b]
	/.$MakeBinaryExpression./
	| ShiftExpression[g,b] '>>' AdditiveExpression[b]
	/.$MakeBinaryExpression./
	| ShiftExpression[g,b] '>>>' AdditiveExpression[b]
	/.$MakeBinaryExpression./

RelationalExpression[g,allowIn] ::= ShiftExpression[g,b]
	| RelationalExpression[g,allowIn] '<' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,allowIn] '>' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,allowIn] '<=' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,allowIn] '>=' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,allowIn] 'in' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,allowIn] 'instanceof' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,allowIn] 'as' TypeExpression
	/.$MakeBinaryExpression./
	| RelationalExpression[g,allowIn] 'is' TypeExpression
	/.$MakeBinaryExpression./

RelationalExpression[g,noIn] ::= ShiftExpression[g,b]
	| RelationalExpression[g,noIn] '<' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,noIn] '>' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,noIn] '<=' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,noIn] '>=' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,noIn] 'instanceof' ShiftExpression[b]
	/.$MakeBinaryExpression./
	| RelationalExpression[g,noIn] 'as' TypeExpression
	/.$MakeBinaryExpression./
	| RelationalExpression[g,noIn] 'is' TypeExpression
	/.$MakeBinaryExpression./

EqualityExpression[g,b] ::= RelationalExpression[g,b]
	| EqualityExpression[g,b] '==' RelationalExpression[b]
	/.$MakeBinaryExpression./
	| EqualityExpression[g,b] '!=' RelationalExpression[b]
	/.$MakeBinaryExpression./
	| EqualityExpression[g,b] '===' RelationalExpression[b]
	/.$MakeBinaryExpression./
	| EqualityExpression[g,b] '!==' RelationalExpression[b]
	/.$MakeBinaryExpression./

BitwiseAndExpression[g,b] ::= EqualityExpression[g,b]
	| BitwiseAndExpression[g,b] '&' EqualityExpression[b]
	/.$MakeBinaryExpression./

BitwiseXorExpression[g,b] ::= BitwiseAndExpression[g,b]
	| BitwiseXorExpression[g,b] '^' BitwiseAndExpression[b]
	/.$MakeBinaryExpression./

BitwiseOrExpression[g,b] ::= BitwiseXorExpression[g,b]
	| BitwiseOrExpression[g,b] '|' BitwiseXorExpression[b]
	/.$MakeBinaryExpression./

LogicalAndExpression[g,b] ::= BitwiseOrExpression[g,b]
	| LogicalAndExpression[g,b] '&&' BitwiseOrExpression[g,b]
	/.$MakeBinaryExpression./

LogicalOrExpression[g,b] ::= LogicalAndExpression[g,b]
	| LogicalAndExpression[g,b] '||' LogicalOrExpression[g,b]
	/.$MakeBinaryExpression./

ConditionalExpression[g,b] ::= YieldExpression[b]
	| LogicalOrExpression[g,b]
	| LogicalOrExpression[g,b] '?' AssignmentExpression[b] ':' AssignmentExpression[b]
	/.$MakeConditionalExpression
	{
		Conditional* ce = astPool->newConditional();
		ce->condition = CAST<Expression*>(sym(1));
		ce->valueTrue = CAST<Expression*>(sym(3));
		ce->valueFalse = CAST<Expression*>(sym(5));
		sym(1) = ce;
	}
	./

NonAssignmentExpression[b] ::= YieldExpression[b]
	| LogicalOrExpression[b]
	| LogicalOrExpression[b] '?' NonAssignmentExpression[b] ':' NonAssignmentExpression[b]
	/.$MakeConditionalExpression./

LetExpression[b] ::= 'let' '(' LetBindingList ')' EnterClosure AssignmentExpression[b] CLOSURE
/.$MakeLetExpression
{
	Let* let = astPool->newLet();
	
	LIST2ARRAY(Variable, 3);
	let->bindings = list;
	
	let->expression = CAST<Expression*>(sym(5));
	sym(1) = let;
}
./

LetBindingList ::= %Empty
	| VariableBindingList[allowIn]

YieldExpression[b] ::= 'yield' Marker Marker
	/.$MakeYieldExpression
	{
		Yield* ye = astPool->newYield();
		ye->expression = CAST<Expression*>(sym(3));
		sym(1) = ye;
	}
	./
	| 'yield' NO_LINE_BREAK AssignmentExpression[b]
	/.$MakeYieldExpression./
	
AssignmentExpression[g,b] ::= ConditionalExpression[g,b]
	| LeftHandSideExpression[g,b] '=' AssignmentExpression[b]
	/.$MakeAssignment
	{
		Assignment* ae = astPool->newAssignment();
		ae->lhs = CAST<Expression*>(sym(1));
		//ae->operator_token = token(2);
		ae->expression = CAST<Expression*>(sym(3));
		sym(1) = ae;
	}
	./
	| LeftHandSideExpression[g,b] CompoundAssignmentOperator AssignmentExpression[b]
	/.$MakeAssignment./

CompoundAssignmentOperator ::= '*='
	| '/='
	| '%='
	| '+='
	| '-='
	| '<<='
	| '>>='
	| '>>>='
	| '&='
	| '^='
	| '|='
	| '&&='
	| '||='

CommaExpression[g,b] ::= AssignmentExpression[g,b]
	/.$StartList./
	| CommaExpression[g,b] ',' AssignmentExpression[g,b]
	/.$AddList2./

TypedIdentifier ::= PropertyIdentifier Marker Marker
	| PropertyIdentifier ':' TypeExpression
	/.$MakeTypedIdentifier
	{
		TypedIdentifier* ti = astPool->newTypedIdentifier();
		ti->id = CAST<Expression*>(sym(1));
		ti->type = CAST<Expression*>(sym(3));
		sym(1) = ti;
	}./

TypeExpression ::= BasicTypeExpression
--FIXME	ActionScript support it
--	| BasicTypeExpression '?'
--	| BasicTypeExpression '!'

BasicTypeExpression ::= 'null'
	/.$MakeNullLiteral./
	| TypeReference

Statement[e] ::= Block
	| BreakStatement Semicolon[e]
	/.$UpdateSemicolonPosition./
	| ContinueStatement Semicolon[e]
	/.$UpdateSemicolonPosition./
	| DefaultXMLNamespaceStatement Semicolon[e]
	/.$UpdateSemicolonPosition./
	| DoStatement Semicolon[e]
	/.$UpdateSemicolonPosition./
	| ExpressionStatement Semicolon[e]
	/.$UpdateSemicolonPosition./
	| ForStatement[e]
	| IfStatement[e]
	| LabeledStatement[e]
	| LetBlockStatement
	| ReturnStatement Semicolon[e]
	/.$UpdateSemicolonPosition./
	| SwitchStatement
	| ThrowStatement Semicolon[e]
	/.$UpdateSemicolonPosition./
	| TryStatement
	| WhileStatement[e]
	| WithStatement[e]

SuperStatement[e] ::= 'super' Arguments Semicolon[e]
/.$MakeSuperStatement
{
	SuperStatement* ss = astPool->newSuperStatement();
	LIST2ARRAY(Expression, 2);
	ss->arguments = list;
	sym(1) = ss;
	UpdateSemicolonPosition(3);
}
./

Substatement[e] ::= EmptyStatement
	| Statement[e]
	| VariableDefinition[allowIn] Semicolon[e]
	/.$UpdateSemicolonPosition./

Semicolon_abbrev ::= %Empty
	| ';'
	| VirtualSemicolon

Semicolon_noShortIf ::= %Empty
	| ';'
	| VirtualSemicolon

Semicolon_full ::= ';'
	| VirtualSemicolon

EmptyStatement ::= ';'
/.$MakeEmptyStatement
{
	EmptyStatement* es = astPool->newEmptyStatement(token(1));
	es->semicolon = token(1);
	sym(1) = es;
}
./

ExpressionStatement ::= CommaExpression[ES,allowIn]
/.$MakeExpressionStatement
{
	ExpressionStatement* es = astPool->newExpressionStatement();
	es->expression = toExpression(1);
	sym(1) = es;
}
./

LabeledStatement[e] ::= Identifier ':' Substatement[e]
/.$MakeLabeledStatement
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
/.$MakeLetStatement
{
	LetStatement* ls = astPool->newLetStatement();
	LIST2ARRAY(Variable, 3);
	ls->bindings = list;
	ls->block = CAST<Block*>(sym(5));
	sym(1) = ls;
}
./

IfStatement[e] ::= 'if' ParenExpression Substatement[e]
	/.$MakeIfStatement
	{
		IfStatement* is = astPool->newIfStatement();
		is->condition = CAST<Expression*>(sym(2));
		is->thenStatement = CAST<Statement*>(sym(3));
		sym(1) = is;
	}
	./
	| 'if' ParenExpression Substatement_noShortIf 'else' Substatement[e]
	/.$MakeIfElseStatement
	{
		IfStatement* is = astPool->newIfStatement();
		is->condition = CAST<Expression*>(sym(2));
		is->thenStatement = CAST<Statement*>(sym(3));
		is->elseStatement = CAST<Statement*>(sym(5));
		sym(1) = is;
	}
	./

IfStatement_noShortIf ::= 'if' ParenExpression Substatement_noShortIf 'else' Substatement_noShortIf
/.$MakeIfElseStatement./

WithStatement[e] ::= 'with' ParenExpression Substatement[e]
/.$MakeWithStatement
{
	WithStatement* ws = astPool->newWithStatement();
	ws->object = CAST<Expression*>(sym(2));
	ws->statement = CAST<Statement*>(sym(3));
	sym(1) = ws;
}
./

SwitchStatement ::= 'switch' ParenExpression '{' CaseElements '}'
/.$MakeSwitchStatement
{
	SwitchStatement* ss = astPool->newSwitchStatement();
	ss->condition = CAST<Expression*>(sym(2));
	LIST2ARRAY(Statement, 4);
	ss->statements = list;
	sym(1) = ss;
}
./

CaseElements ::= %Empty
	| CaseLabel
	/.$StartList./
	| CaseLabel CaseElementsPrefix CaseLabel
	/.$MakeCaseElements
	{
		StartList();
		
		MergeList(1, 2);
		
		AddList3();
	}
	./
	| CaseLabel CaseElementsPrefix Directive_abbrev
	/.$MakeCaseElements./

CaseElementsPrefix ::= %Empty
	| CaseElementsPrefix CaseLabel
	/.$AppendList./
	| CaseElementsPrefix Directive_full
	/.$AppendList./

CaseLabel ::= 'case' CommaExpression[allowIn] ':'
	/.$MakeCaseLabel
	{
		CaseLabel* cl = astPool->newCaseLabel();
		cl->expression = toExpression(2);
		sym(1) = cl;
	}
	./
	| 'default' Marker ':'
	/.$MakeCaseLabel./

DoStatement ::= 'do' Substatement_abbrev 'while' ParenExpression
/.$MakeDoStatement
{
	DoStatement* ds = astPool->newDoStatement();
	ds->action = CAST<Statement*>(sym(2));
	ds->condition = CAST<Expression*>(sym(4));
	sym(1) = ds;
}
./

WhileStatement[e] ::= 'while' ParenExpression Substatement[e]
/.$MakeWhileStatement
{
	WhileStatement* ws = astPool->newWhileStatement();
	ws->condition = CAST<Expression*>(sym(2));
	ws->action = CAST<Statement*>(sym(3));
	sym(1) = ws;
}
./

ForStatement[e] ::= 'for' '(' ForInitialiser ';' OptionalExpression ';' OptionalExpression ')' Substatement[e]
	/.$MakeForStatement
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
	| 'for' '(' ForInBinding 'in' CommaExpression[allowIn] ')' Substatement[e]
	/.$MakeForeachStatement
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
	| 'for' 'each' '(' ForInBinding 'in' CommaExpression[allowIn] ')' Substatement[e]
	/.$MakeForeachStatement./

ForInitialiser ::= %Empty
	| CommaExpression[noIn]
	| VariableDefinition[noIn]

ForInBinding ::= LeftHandSideExpression[noIn]
	| VariableDefinitionKind VariableBinding[noIn]
	/.$SetVariableKind
	{
		//TODO
		sym(1) = sym(2);
	}./

OptionalExpression ::= %Empty
	| CommaExpression[allowIn]

ContinueStatement ::= 'continue' Marker Marker
	/.$MakeContinueStatement
	{
		ContinueStatement* cs = astPool->newContinueStatement();
		//cs-> = token(3);
		sym(1) = cs;
	}
	./
	| 'continue' NO_LINE_BREAK Identifier
	/.$MakeContinueStatement./
	
BreakStatement ::= 'break' Marker Marker
	/.$MakeBreakStatement
	{
		BreakStatement* bs = astPool->newBreakStatement();
		//
		sym(1) = bs;
	}
	./
	| 'break' NO_LINE_BREAK Identifier
	/.$MakeBreakStatement./

ReturnStatement ::= 'return' Marker Marker
	/.$MakeReturnStatement
	{
		ReturnStatement* rs = astPool->newReturnStatement();
		rs->expression = toExpression(3);
		sym(1) = rs;
	}
	./
	| 'return' NO_LINE_BREAK CommaExpression[allowIn]
	/.$MakeReturnStatement./

ThrowStatement ::= 'throw' NO_LINE_BREAK CommaExpression[allowIn]
/.$MakeThrowStatement
{
	ThrowStatement* ts = astPool->newThrowStatement();
	ts->expression = toExpression(3);
	sym(1) = ts;
}
./

TryStatement ::= 'try' Block CatchClauses Marker
	/.$MakeTryStatement
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
	/.$MakeTryStatement./
	| 'try' Block Marker Finally
	/.$MakeTryStatement./

CatchClauses ::= CatchClause
	/.$StartList./
	| CatchClauses CatchClause
	/.$AddList2./

CatchClause ::= 'catch' '(' Parameter Marker Marker ')' Block
	/.$MakeCatchClause
	{
		CatchClause* cc = astPool->newCatchClause();
		cc->parameter = CAST<Parameter*>(sym(3));
		if (sym(5))
			cc->condition = toExpression(5);
		
		cc->block = CAST<Block*>(sym(5));
		sym(1) = cc;
	}
	./
	| 'catch' '(' Parameter 'if' CommaExpression[allowIn] ')' Block

Finally ::= 'finally' Block
/.$MakeFinallyClause
{
	FinallyClause* fc = astPool->newFinallyClause();
	fc->block = CAST<Block*>(sym(2));
	sym(1) = fc;
}
./

DefaultXMLNamespaceStatement ::= 'default' 'xml' 'namespace' '=' CommaExpression[allowIn]
/.$MakeDefaultXMLNamespaceStatement
{
	//TODO for AS3 only NonAssignmentExpression[allowIn] is allowed
}
./

Directives ::= %Empty
	| DirectivesPrefix Directive[abbrev]
	/.$AppendList./

DirectivesPrefix ::= %Empty
	| DirectivesPrefix Directive[full]
	/.$AppendList./

Directive[e] ::= EmptyStatement
	| Statement[e]
	| SuperStatement[e]
	| AnnotatableDirective[e]
	| Attributes AnnotatableDirective[e]
	/.$MakeAttributes
	{
		AttributeStatement* astmt = CAST<AttributeStatement*>(sym(2));
		LIST2ARRAY(Expression, 1);
		astmt->attributes = list;
	}
	./
	| Attributes Block
	/.$MakeAttributes./

Attributes ::= Attribute NO_LINE_BREAK
	/.$StartList./
	| Attributes Attribute NO_LINE_BREAK
	/.$AddList2./

Attribute ::= PrimaryName
	| ReservedNamespace
	| ArrayLiteral

IncludeDirective ::= 'include' StringLiteral
/.$MakeInclude
{
	Include* inc = astPool->newInclude();
	inc->include_token = token(1);
	inc->url = CAST<StringLiteral*>(sym(2));
	sym(1) = inc;
}
./

AnnotatableDirective[e] ::= VariableDefinition[allowIn] Semicolon[e]
	/.$UpdateSemicolonPosition./
	| FunctionDeclaration Semicolon[e]
	/.$UpdateSemicolonPosition./
	| FunctionDefinition[e]
	| ClassDefinition
	| InterfaceDefinition
	| IncludeDirective
	| UsePragma Semicolon[e]
	/.$UpdateSemicolonPosition./
	| ImportDirective Semicolon[e]
	/.$UpdateSemicolonPosition./
	| NamespaceDefinition Semicolon[e]
	/.$UpdateSemicolonPosition./
	| PackageDefinition

VariableDefinition[b] ::= VariableDefinitionKind VariableBindingList[b]
/.$MakeVariableDefinition
{
	VariableDefinition* var = astPool->newVariableDefinition();
	var->kind_token = token(1);
	LIST2ARRAY(Variable, 2);
	var->variables = list;
	sym(1) = var;
}./

VariableDefinitionKind ::= 'const'
	| 'var'
	| 'let'

VariableBindingList[b] ::= VariableBinding[b]
	/.$StartList./
	| VariableBindingList[b] ',' VariableBinding[b]
	/.$AddList3./

VariableBinding[b] ::= TypedPattern Marker Marker
	/.$MakeVariableBinding
	{
		Variable* var = astPool->newVariable();
		var->name = CAST<Expression*>(sym(1));
		var->initializer = CAST<Expression*>(sym(3));
		sym(1) = var;
	}./
	| TypedPattern '=' AssignmentExpression[b]
	/.$MakeVariableBinding./

TypedPattern ::= TypedIdentifier
	| ArrayLiteral
	| ObjectLiteral

FunctionDeclaration ::= 'function' FunctionName FunctionSignature
/.$MakeFunctionDeclaration
{
	FunctionDeclaration* decl = astPool->newFunctionDeclaration();
	decl->func = CAST<Function*>(sym(3));
	//TODO func->name = 
	sym(1) = decl;
}
./

FunctionDefinition[e] ::= FunctionDeclaration FunctionBody[e]
/.$MakeFunctionDefinition
{
	FunctionDeclaration* decl = CAST<FunctionDeclaration*>(sym(1));
	decl->func->body = CAST<Block*>(sym(2));
}
./

FunctionBody[e] ::= Block
	| AssignmentExpression[NoBrace,allowIn] Semicolon[e]
	/.$MakeAnonymousBody
	{
		Block* block = astPool->newBlock();
		sym(1) = block;
	}./

FunctionName ::= PropertyIdentifier
	| OverloadedOperator
	| 'get' PropertyIdentifier
	/.$setSym2ToSym1./
	| 'set' PropertyIdentifier
	/.$setSym2ToSym1./

OverloadedOperator ::= '+'
	| '-'
	| '~'
	| '*'
	| '/'
	| '%'
	| '<'
	| '>'
	| '<='
	| '>='
	| '=='
	| '<<'
	| '>>'
	| '>>>'
	| '&'
	| '|'
	| '==='
	| '!='
	| '!=='
	| '[' ']'

FunctionSignature ::= TypeParameters '(' Parameters ')' ResultType
/.$MakeFunctionSignature
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
	| '.<' TypeParameterList TypeEnd
	/.$setSym2ToSym1./

TypeParameterList ::= Identifier
	/.$StartList./
	| TypeParameterList ',' Identifier
	/.$AddList3./

Parameters ::= %Empty
	| NonemptyParameters

NonemptyParameters ::= NonRestParameters
	| RestParameter
	/.$StartList./
	| NonRestParameters ',' RestParameter
	/.$AddList3./

NonRestParameters ::= ParameterList
	| OptionalParameters
	| ParameterList ',' OptionalParameters
	/.$MergeList3./

ParameterList ::= Parameter
	/.$StartList./
	| ParameterList ',' Parameter
	/.$AddList3./

OptionalParameters ::= OptionalParameter
	/.$StartList./
	| OptionalParameters ',' OptionalParameter
	/.$AddList3./

Parameter ::= TypedPattern
	/.$MakeParameter
	{
		Parameter* param = astPool->newParameter();
		param->name = CAST<Expression*>(sym(1));
		sym(1) = param;
	}./
	| 'const' TypedPattern
	/.$MakeConstParameter
	{
		Parameter* param = astPool->newParameter(true);
		param->name = CAST<Expression*>(sym(2));
		sym(1) = param;
	}./

ParameterAttribute ::= %Empty
	| 'const'

OptionalParameter ::= Parameter '=' NonAssignmentExpression_allowIn
/.$MakeOptionalParameter
{
	Parameter* param = CAST<Parameter*>(sym(1));
	param->defaultValue = CAST<Expression*>(sym(3));
}
./

RestParameter ::= '...' Marker
	/.$MakeRestParameter
	{
		Parameter* param = NULL;
		if (sym(2)) {
			param = CAST<Parameter*>(sym(2));
		} else {
			//anonymous parameter...
			param = astPool->newParameter();
		}
		//param->kind |= REST
		sym(1) = param;
	}
	./
	| '...' Parameter
	/.$MakeRestParameter./

ResultType ::= %Empty
	| ':' 'void'
	/.$MakeVoidType
	{
		sym(1) = astPool->newIdentifier(token(2));
	}./
	| ':' TypeExpression
	/.$setSym2ToSym1./

ClassDefinition ::= 'class' Identifier TypeParameters SuperOpt InterfacesOpt Block
/.$MakeClassDefinition
{
	//TODO
}
./

SuperOpt ::= %Empty
	| 'extends' TypeReference
	/.$setSym2ToSym1./

InterfacesOpt ::= %Empty
	| 'implements' TypeReferenceList
	/.$setSym2ToSym1./

TypeReferenceList ::= TypeReference
	/.$StartList./
	| TypeReferenceList ',' TypeReference
	/.$AddList3./

TypeReference ::= PrimaryName
	| PrimaryName TypeArguments --TODO

InterfaceDefinition ::= 'interface' Identifier TypeParameters InterfaceInheritance Block
/.$MakeInterfaceDefinition
{
	//TODO
}
./

InterfaceInheritance ::= %Empty
	| 'extends' TypeReferenceList
	/.$setSym2ToSym1./

NamespaceDefinition ::= 'namespace' Identifier Marker Marker
	/.$MakeNamespaceDefinition
	{
		//TODO
	}
	./
	| 'namespace' Identifier '=' AssignmentExpression[allowIn]

PackageDefinition ::= 'package' PackageNameOpt Block
/.$MakePackageDefinition
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
	| PackageName

PackageName ::= Identifier
	/.$StartList./
	| PackageName '.' Identifier
	/.$AddList3./

UsePragma ::= 'use' 'namespace' NonAssignmentExpression[allowIn]
	/.$MakeUsePragma
	{
		Use* use = astPool->newUse();
		use->use_token = token(1);
		use->ns = CAST<Expression*>(sym(3));
		sym(1) = use;
	}
	./
	| 'use' IncludeDirective
	/.$MakeUseInclude
	{
		Include* inc = CAST<Include*>(sym(2));
		inc->use_token = token(1);
		sym(1) = inc;
	}
	./

ImportDirective ::= 'import' ImportName Marker Marker
	/.$MakeImportDirective
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
	/.$MakeImportDirective./

ImportName ::= PackageName Marker Marker
	/.$MakeImportName
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
	/.$MakeImportName./

Block ::= '{' Directives '}'
/.$MakeBlock
{
	Block* block = astPool->newBlock();
	block->leftBrace = token(1);
	LIST2ARRAY(Statement, 2)
	block->statements = list;
	block->rightBrace = token(3);
	sym(1) = block;
}
./

Program ::= Directives
/.$MakeProgram
{
	Program* program = astPool->newProgram();
	LIST2ARRAY(Statement, 1)
	program->statements = list;
	sym(1) = program;
}
./

Goal ::= ')' Program

XMLInitialiser ::= XMLMarkup
	| XMLElement

XMLStart ::= '<'

XMLElement ::= XMLStart XMLTagContent '/>'
	| XMLStart XMLTagContent '>' XMLElementContentOpt '</' XMLTagName '>'

XMLListInitialiser ::= XMLStart '>' XMLElementContent '</' '>'

XMLTagContent ::= XMLTagName XMLAttributesOpt

XMLTagName ::= XMLName
	| '{' CommaExpression_allowIn '}'

XMLAttributesOpt ::= %Empty
	| XMLAttributes

XMLAttributes ::= XMLAttribute
	| XMLAttributes XMLAttribute

XMLAttribute ::= XMLTagName '=' XMLAttributeValue
	| XMLTagName '=' '{' CommaExpression_allowIn '}'
	| '{' CommaExpression_allowIn '}'

XMLTagContentOpt ::= %Empty
	| XMLTagContent

XMLElementContent ::= XMLMarkup XMLElementContentOpt
	| XMLText XMLElementContentOpt
	| XMLElement XMLElementContentOpt
	| '{' CommaExpression_allowIn '}' XMLElementContentOpt

XMLElementContentOpt ::= %Empty
	| XMLElementContent

XMLMarkup ::= XMLComment
	| XMLCDATA
	| XMLPI
