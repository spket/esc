#ifndef ES_PARSER_H_
#define ES_PARSER_H_

#include "es4prs.h"
#include "lexer.h"

namespace es {

class Ast;
class AstListNode;
class Expression;
class AstPool;

class Parser
{
public:
	Parser();
	virtual ~Parser();

	int parse(Lexer* lexer, AstPool* pool);

	inline int getCurrentLine()
	{
		return lexer->getCurrentLine();
	}

	inline size_t getTokenOffset()
	{
		return lexer->getTokenOffset();
	}

protected:
	int* actions;
	int actionLength;

	int* stack;
	int* tempStack;
	Ast** astStack;
	uint* tokenStack;

	int stackLength;
	int stateStackTop;

	uint* closures;
	uint closurePtr;
	uint closureLength;

	int firstToken;
	Lexer* lexer;

	AstPool* astPool;
	AstPool* listPool;

	AstListNode* freeList;

	void parse();
	virtual int parse(int startToken);

	AstListNode* AllocateListNode();
	void FreeCircularList(AstListNode*);

	inline int lookahead(int act, int token)
	{
		act = es4prs::look_ahead(act - es4prs::LA_STATE_OFFSET, token);
		return (act <= es4prs::LA_STATE_OFFSET ? act : es4prs::ERROR_ACTION);
	}

	inline int tAction(int act, int token, int nextToken)
	{
		act = es4prs::t_action(act, token);

		return (act <= es4prs::LA_STATE_OFFSET ? act : lookahead(act, nextToken == 0 ? peekToken() : nextToken));
	}

	void reallocateStacks();

	inline bool hasClosure()
	{
		return (closures[closurePtr] > 0);
	}

	inline void enterClosure()
	{
		closures[closurePtr]++;
	}

	inline void exitClosure()
	{
		if (closures[closurePtr] > 0) {
			closures[closurePtr]--;
		} else {
			assert(false);
			//TODO report parse error
		}
	}

	inline void pushClosure()
	{
		closurePtr++;
		if (closurePtr == closureLength) {
			uint* oldClosures = closures;
			uint oldLength = closureLength;
			closureLength += 16;
			closures = (uint*) memcpy(new uint[closureLength], closures, oldLength * sizeof(uint));
			delete oldClosures;
		}
		closures[closurePtr] = 0;
	}

	inline void popClosure()
	{
		if (closurePtr > 0) {
			if (closures[closurePtr] > 0) {
					//TODO report parse error
					assert(false);
			}
			closurePtr--;
		} else {
			assert(false);
			//TODO report parse error
		}
	}

	inline int peekToken()
	{
		return lexer->peek();
	}

	inline void consumeToken(int token)
	{
		tokenStack[stateStackTop] = lexer->getIndex();
	}

	inline Ast*& sym(int i) { return astStack[stateStackTop + i - 1]; }
	inline uint token(int i) { return tokenStack[stateStackTop + i - 1]; }
	inline int kind(int i) { return lexer->getKind(tokenStack[stateStackTop + i - 1]); }
	inline void assignToken(int from, int to)
	{
		tokenStack[stateStackTop + to - 1] = tokenStack[stateStackTop + from - 1];
	}

	int parse(int token, int nextToken);

	void BadAction();
	void NoAction();
	void NullAction();

	void StartList();
	void AddList2();
	void AddList3();
	void AppendList();
	void MergeList3();
	void MergeList(int l1, int l2);
	void setSym2ToSym1();
	void UpdateSemicolonPosition();
	void UpdateSemicolonPosition(int token);

	Expression* toExpression(int idx);

	void (Parser::*ruleAction[es4prs::NUM_RULES + 1])();

	#include "es4act.h"
};

}

#endif // ES_PARSER_H_
