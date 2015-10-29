#ifndef ES_POOL_H_
#define ES_POOL_H_

#include <cstdlib>

namespace es {

class MemoryPool
{
public:
	MemoryPool() {}
	virtual ~MemoryPool() {}

	void* alloc(std::size_t size)
	{
		//TODO
		return std::malloc(n);
	}
	
	void clear() { /*TODO*/ }
};

}

#endif //ES_POOL_H_
