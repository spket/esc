#include "parser.h"
#include "ast.h"

#define ADD_LIST(i1, i2) \
	AstListNode* tail = CAST<AstListNode*>(sym(i1)); \
	AstListNode* p = AllocateListNode(); \
	p->element = sym(i2); \
	p->index = tail->index + 1; \
	p->next = tail->next; \
	tail->next = p; \
	sym(i1) = p;

#define START_LIST(idx) \
	AstListNode* p = AllocateListNode(); \
	p->next = p; \
	p->element = sym(idx); \
	p->index = 0; \
	sym(1) = p;

#define LIST2ARRAY(Type, idx) \
	AstArray<Type*>* list = nullptr; \
	if (sym(idx)) { \
		AstListNode* tail = CAST<AstListNode*>(sym(idx)); \
		AstListNode* root = tail; \
		list = new (astPool) AstArray<Type*>(astPool, tail->index + 1); \
		do { \
			root = root->next; \
			list->next() = CAST<Type*>(root->element); \
		} while (root != tail); \
		FreeCircularList(tail); \
	}

enum { STACK_INCREMENT = 256 };

namespace es {

Parser::Parser() :
	astStack(nullptr),
	stack(nullptr),
	stackLength(0),
	tempStack(nullptr),
	tokenStack(nullptr),
	lexer(nullptr)
{
	actions = new int[actionLength = STACK_INCREMENT];
	closures = new uint[closureLength = 16];

#include "es4init.cpp"

}

Parser::~Parser()
{
	if (listPool)
		delete listPool;

	delete[] actions;
	delete[] stack;
	delete[] tempStack;
	delete[] astStack;
	delete[] tokenStack;
	delete[] closures;
}

void Parser::reallocateStacks()
{
	int oldLength = stackLength;
	stackLength += STACK_INCREMENT;
	int* oldStack = stack;
	stack = (int*) memcpy(new int[stackLength], stack, oldLength * sizeof(int));
	delete[] oldStack;
	
	oldStack = tempStack;
	tempStack = (int*) memcpy(new int[stackLength], tempStack, oldLength * sizeof(int));
	delete[] oldStack;
	
	Ast** oldAstStack = astStack;
	astStack = (Ast**) memcpy(new Ast*[stackLength], astStack, oldLength * sizeof(Ast*));
	delete[] oldAstStack;
	memset(astStack, 0, STACK_INCREMENT * sizeof(Ast*));
	
	uint* tokens = tokenStack;
	tokenStack = (uint*) memcpy(new uint[stackLength], tokenStack, oldLength * sizeof(uint));
	delete[] tokens;
	memset(tokenStack, 0, STACK_INCREMENT * sizeof(uint));
}

int Parser::parse()
{
	int rc;
	
	size_t length = lexer->getSourceLength();
	
	if (length) {
		freeList = nullptr;
		reallocateStacks();
		if (listPool) {
			listPool->clear();
		} else {
			listPool = new AstPool(1024);
		}
		
		closures[closurePtr = 0] = 0;
		stack[stateStackTop = 0] = es4prs::START_STATE;
	
		rc = parse(TK_RPAREN);
	
		delete listPool;
	}
	
	return rc;
}

int Parser::parse(Lexer* l, AstPool* pool)
{
	int rc;
	if (lexer != l) {
		if (lexer)
			delete lexer;

		lexer = l;
	}
	astPool = pool;
	rc = parse();
	astPool = nullptr;

	return rc;
}

int Parser::parse(int firstToken)
{
	int act;
	//int last = -1;
	int token = firstToken;
	
	act = parse(token, 0);
	token = lexer->next();
	act = parse(token, 0);
	if (es4prs::ERROR_ACTION != act) {
		while (es4prs::ACCEPT_ACTION != act) {
			switch (token) {
				case TK_LBRACE:
				case TK_LPAREN:
				case TK_LBRACKET:
					pushClosure();
					break;
				case TK_RBRACE:
				case TK_RPAREN:
				case TK_RBRACKET:
					popClosure();
					break;
			}
			
			token = lexer->next();
			act = parse(token, 0);
			if (es4prs::ERROR_ACTION == act) {
				if (hasClosure()) {
					int last = act;
					while (hasClosure()) {
						act = parse(TK_CLOSURE, token);
						if (es4prs::ERROR_ACTION == act) {
							break;
						} else {
							last = act;
							
							lexer->insert(TK_CLOSURE);
	
							exitClosure();
						}
					}
					if (es4prs::ERROR_ACTION != act) {
						act = parse(token, 0);
						if (es4prs::ERROR_ACTION == act && lexer->hasNewLine()) {
							act = parse(TK_VirtualSemicolon, token);
							if (es4prs::ERROR_ACTION != act)
								act = parse(token, 0);
						}
					} else if (es4prs::ERROR_ACTION != last) { // still has closure
						act = parse(token, 0);
					}
				} else if (!(token == TK_SEMICOLON || token == TK_RPAREN || token == TK_RBRACE || token == TK_RBRACKET || token == TK_EOF_TOKEN)) {
					if (lexer->hasNewLine()) {
						act = parse(TK_VirtualSemicolon, token);
					} else {
						act = parse(TK_NO_LINE_BREAK, token);
					}
					if (es4prs::ERROR_ACTION != act)
						act = parse(token, 0);
				}
				if (es4prs::ERROR_ACTION == act)
					break;
			}
		}
	}

	return ((act == es4prs::ACCEPT_ACTION) ? 0 : -1);
}

int Parser::parse(int token, int nextToken)
{
	int cnt = 0;
	int total = 0;
	int pos = stateStackTop;
	int tempStackTop = stateStackTop - 1;
	int act = tAction(stack[stateStackTop], token, nextToken);
	while (act <= es4prs::NUM_RULES) {
		actions[cnt++] = act;
		do {
			total++;
			tempStackTop -= (es4prs::rhs[act] - 1);
			int state = (tempStackTop > pos ? tempStack[tempStackTop] : stack[tempStackTop]);
			act = es4prs::nt_action(state, es4prs::lhs[act]);
		} while (act <= es4prs::NUM_RULES);
		
		pos = pos < tempStackTop ? pos : tempStackTop;
		if (tempStackTop + 1 >= stackLength)
			reallocateStacks();
		tempStack[tempStackTop + 1] = act;
		
		act = tAction(act, token, nextToken);
	}
	
	if (act != es4prs::ERROR_ACTION) {
		//if (TK_NO_LINE_BREAK == token) printf("NO_LINE_BREAK: %d\n", total);
		//Token curtok = {};
		if (cnt > 0) {
			int action = act;
			stateStackTop--;
			//tokenStack[stateStackTop--] = curtok;
			for (int i = 0; i < cnt; i++) {
				act = actions[i];
				do {
					stateStackTop -= (es4prs::rhs[act] - 1);
					//printf("%d\n", act);
					(this ->* ruleAction[act])();
					act = es4prs::nt_action(stack[stateStackTop], es4prs::lhs[act]);
				} while (act <= es4prs::NUM_RULES);
				stack[stateStackTop + 1] = act;
				//tokenStack[stateStackTop + 1] = curtok;
			}
			act = action;
			stateStackTop++;
			assert(stateStackTop == tempStackTop + 1);
		}
		
		consumeToken(token);
		if (act > es4prs::ERROR_ACTION) {
			act -= es4prs::ERROR_ACTION;
			do {
				stateStackTop -= (es4prs::rhs[act] - 1);
				//printf("%d\n", act);
				(this ->* ruleAction[act])();
				act = es4prs::nt_action(stack[stateStackTop], es4prs::lhs[act]);
			} while (act <= es4prs::NUM_RULES);
			
			assert(act != es4prs::ERROR_ACTION && "ERROR_ACTION");
		}
		if (++stateStackTop >= stackLength)
			reallocateStacks();
		stack[stateStackTop] = act;
		//tokenStack[stateStackTop] = curtok;
	}
	
	return act;
}

void Parser::BadAction() { assert(false); }

void Parser::NoAction() {}

void Parser::nullptrAction()
{
	sym(1) = nullptr;
	tokenStack[stateStackTop] = lexer->getIndex();
}

AstListNode* Parser::AllocateListNode()
{
	AstListNode* p;
	
	if (freeList) {
		p = freeList;
		freeList = freeList->next;
	} else {
		p = listPool->newListNode();
	}
	
	return p;
}

void Parser::FreeCircularList(AstListNode* tail) 
{
	if (tail) {
		AstListNode* root = tail->next;
		tail->next = freeList;
		freeList = root;
	}
}

Expression* Parser::toExpression(int idx)
{
	Ast* node = sym(idx);
	Expression* expr = nullptr;
	if (node) {
		if (Ast::LIST_NODE == node->kind) {
			AstListNode *tail = CAST<AstListNode*>(node);
			AstListNode* root = tail;
			if (tail->index) {
				CommaExpression* ce = astPool->newCommaExpression();
				Expressions* list = astPool->AllocateExpressions(tail->index + 1);
				expr = ce;
				do {
					root = root->next;
					list->next() = CAST<Expression*>(root->element);
				} while (root != tail);
			} else {
				expr = CAST<Expression*>(root->element);
			}
			FreeCircularList(tail);
		} else {
			expr = CAST<Expression*>(node);
		}
	}
	
	return expr;
}

void Parser::StartList()
{
	START_LIST(1);
}

void Parser::AddList2()
{
	ADD_LIST(1, 2);
}

void Parser::AddList3()
{
	ADD_LIST(1, 3);
}

void Parser::AppendList()
{
	if (sym(1)) {
		ADD_LIST(1, 2);
	} else {
		START_LIST(2);
	}
}

void Parser::MergeList3()
{
	MergeList(1, 3);
}

void Parser::MergeList(int l1, int l2)
{
	if (sym(l2)) {
		AstListNode* tail = CAST<AstListNode*>(sym(l1));
		AstListNode* list = CAST<AstListNode*>(sym(l2));
		AstListNode* root = tail->next;
		int index = tail->index;
		tail->next = list->next;
		list->next = root;
		tail = tail->next;
		while (tail != root) {
			index++;
			tail->index = index;
			tail = tail->next;
		}
		sym(l1) = list;
	}
}

void Parser::setSym2ToSym1()
{
	sym(1) = sym(2);
}

void Parser::UpdateSemicolonPosition()
{
	UpdateSemicolonPosition(2);
}

void Parser::UpdateSemicolonPosition(int token)
{
	//TODO
}

#include "es4act.cpp"

}
