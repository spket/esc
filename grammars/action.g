%Define

$BeginActions
/:
	ruleAction[0] = &Parser::BadAction;:/

$EndActions
/:
:/

$Location
/.
//Rule $rule_number:  $rule_text./

$DefaultName /.Act$rule_number$./
$DefaultHeader /.$Location
void Parser::$DefaultName$()./

$DclAction
/!
	void $DefaultName();!/

$SetAction
/:
	ruleAction[$rule_number] = &Parser::$DefaultName;:/

$NullAction
/:
	ruleAction[$rule_number] = &Parser::NullAction;:/

$NoAction
/:
	ruleAction[$rule_number] = &Parser::NoAction;:/

$Action
/. $DclAction $SetAction ./

$Sym1
/.
$DefaultHeader
{ Sym(1) = astPool ->./

$Set
/:
	ruleAction[$rule_number] = &Parser:::/

-- $Sym2_1
-- /:$Set$setSym2ToSym1;:/
-- 
-- $AddList2
-- /:$Set$AddList2;:/
-- 
-- $AddList3
-- /:$Set$AddList3;:/
-- 
-- $MergeList
-- /:$Set$MergeList;:/
-- 
-- $MergeCaseElements
-- /:$Set$MergeCaseElements;:/
-- 
-- $StartList
-- /:$Set$StartList;:/
-- 
-- $UpdateSemicolonPosition
-- /:$Set$UpdateSemicolonPosition;:/
--    
-- $MakeArguments
-- /:$Set$MakeArguments;:/
-- 
-- $MakeAssignment
-- /:$Set$MakeAssignment;:/
-- 
-- $MakeAttributeName
-- /:$Set$MakeAttributeName;:/
-- 
-- $MakeBlock
-- /:$Set$MakeBlock;:/
-- 
-- $MakeBinaryExpression
-- /:$Set$MakeBinaryExpression;:/
-- 
-- $MakeBooleanLiteral
-- /:$Set$MakeBooleanLiteral;:/
-- 
-- $MakeBreakStatement
-- /:$Set$MakeBreakStatement;:/
-- 
-- $MakeCaseLabel
-- /:$Set$MakeCaseLabel;:/
-- 
-- $MakeCatchClause
-- /:$Set$MakeCatchClause;:/
-- 
-- $MakeClassDefinition
-- /:$Set$MakeClassDefinition;:/
-- 
-- $MakeConditionalExpression
-- /:$Set$MakeConditionalExpression;:/
-- 
-- $MakeContinueStatement
-- /:$Set$MakeContinueStatement;:/
-- 
-- $MakeDefaultXMLNamespaceStatement
-- /:$Set$MakeDefaultXMLNamespaceStatement;:/
-- 
-- $MakeDoStatement
-- /:$Set$MakeDoStatement;:/
-- 
-- $MakeEmptyStatement
-- /:$Set$MakeEmptyStatement;:/
-- 
-- $MakeExpressionStatement
-- /:$Set$MakeExpressionStatement;:/
-- 
-- $MakeFinallyClause
-- /:$Set$MakeFinallyClause;:/
-- 
-- $MakeForeachStatement
-- /:$Set$MakeForeachStatement;:/
-- 
-- $MakeForStatement
-- /:$Set$MakeForStatement;:/
-- 
-- $MakeFunctionDeclaration
-- /:$Set$MakeFunctionDeclaration;:/
-- 
-- $MakeFunctionDefinition
-- /:$Set$MakeFunctionDefinition;:/
-- 
-- $MakeFunctionExpression
-- /:$Set$MakeFunctionExpression;:/
-- 
-- $MakeFunctionLambda
-- /:$Set$MakeFunctionLambda;:/
-- 
-- $MakeFunctionSignature
-- /:$Set$MakeFunctionSignature;:/
-- 
-- $MakeIdentifier
-- /:$Set$MakeIdentifier;:/
-- 
-- $MakeIfElseStatement
-- /:$Set$MakeIfElseStatement;:/
-- 
-- $MakeIfStatement
-- /:$Set$MakeIfStatement;:/
-- 
-- $MakeImportDirective
-- /:$Set$MakeImportDirective;:/
-- 
-- $MakeIncludeDirective
-- /:$Set$MakeIncludeDirective;:/
-- 
-- $MakeInterfaceDefinition
-- /:$Set$MakeInterfaceDefinition;:/
-- 
-- $MakeLabeledStatement
-- /:$Set$MakeLabeledStatement;:/
-- 
-- $MakeLetExpression
-- /:$Set$MakeLetExpression;:/
-- 
-- $MakeLetStatement
-- /:$Set$MakeLetStatement;:/
-- 
-- $MakeLiteralField
-- /:$Set$MakeLiteralField;:/
-- 
-- $MakeMemberExpression
-- /:$Set$MakeMemberExpression;:/
-- 
-- $MakeNamespaceDefinition
-- /:$Set$MakeNamespaceDefinition;:/
-- 
-- $MakeNewExpression
-- /:$Set$MakeNewExpression;:/
-- 
-- $MakeNullLiteral
-- /:$Set$MakeNullLiteral;:/
-- 
-- $MakeNumberLiteral
-- /:$Set$MakeNumberLiteral;:/
-- 
-- $MakeOptionalParameter
-- /:$Set$MakeOptionalParameter;:/
-- 
-- $MakePackageDefinition
-- /:$Set$MakePackageDefinition;:/
-- 
-- $MakeParameter
-- /:$Set$MakeParameter;:/
-- 
-- $MakePostfixExpression
-- /:$Set$MakePostfixExpression;:/
-- 
-- $MakePrefixExpression
-- /:$Set$MakePrefixExpression;:/
-- 
-- $MakeProgram
-- /:$Set$MakeProgram;:/
-- 
-- $MakeQualifiedName
-- /:$Set$MakeQualifiedName;:/
-- 
-- $MakeRegExpInitialiser
-- /:$Set$MakeRegExpInitialiser;:/
-- 
-- $MakeRestParameter
-- /:$Set$MakeRestParameter;:/
-- 
-- $MakeReturnStatement
-- /:$Set$MakeReturnStatement;:/
-- 
-- $MakeStringLiteral
-- /:$Set$MakeStringLiteral;:/
-- 
-- $MakeSuperExpression
-- /:$Set$MakeSuperExpression;:/
-- 
-- $MakeSuperStatement
-- /:$Set$MakeSuperStatement;:/
-- 
-- $MakeSwitchStatement
-- /:$Set$MakeSwitchStatement;:/
-- 
-- $MakeThisExpression
-- /:$Set$MakeThisExpression;:/
-- 
-- $MakeThrowStatement
-- /:$Set$MakeThrowStatement;:/
-- 
-- $MakeTryStatement
-- /:$Set$MakeTryStatement;:/
-- 
-- $MakeUnaryExpression
-- /:$Set$MakeUnaryExpression;:/
-- 
-- $MakeUsePragma
-- /:$Set$MakeUsePragma;:/
-- 
-- $MakeWhileStatement
-- /:$Set$MakeWhileStatement;:/
-- 
-- $MakeWildcard
-- /:$Set$MakeWildcard;:/
-- 
-- $MakeWithStatement
-- /:$Set$MakeWithStatement;:/
-- 
-- $MakeYieldExpression
-- /:$Set$MakeYieldExpression;:/

%End
