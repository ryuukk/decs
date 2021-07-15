module decs.memory;

import core.stdc.stdio;
import core.stdc.stdlib;
import core.stdc.string;
import core.lifetime;

import decs.dbg;

__gshared MallocAllocator MALLOCATOR = MallocAllocator();

// TODO: handle failures
struct Allocator
{
	void* function(Allocator*, size_t) vt_allocate;
	void* function(Allocator*, void*, size_t) vt_reallocate;
	void function(Allocator*, void*) vt_free;


	Allocator* ptr() return
	{
		return &this;
	}

	// INTERFACE

	void* allocate(size_t size)
	{
		assert(size > 0);
		return vt_allocate(&this, size);
	}
	
	void* reallocate(void* ptr, size_t size)
	{
		assert(size > 0);
		return vt_reallocate(&this, ptr, size);
	}

	// prefer safe_delete(T)(ref T* ptr)
	void free(void* ptr)
	{
		assert(ptr != null);
		vt_free(&this, ptr);
	}

	// END INTERFACE

	void safe_delete(T)(ref T* ptr)
	{
		if(ptr)
		{
			free(ptr);
			ptr = null;
		}
	}



	void[] alloc(size_t size)
	{
		assert(size > 0);

		void* ptr = malloc(size);
		if (!ptr)
			assert(0, "Out of memory!");
		return ptr[0 .. size];
	}

	T[] alloc_array(T)(size_t count)
	{
		assert(count > 0, "can't allocate empty array");
		return cast(T[]) alloc(T.sizeof * count);
	}

	T[] reallocate_array(T)(T* ptr, size_t count)
	{
		assert(count > 0, "can't reallocate empty array");

		auto size = T.sizeof * count;
		auto ret = reallocate(ptr, size);
		return cast(T[]) ret[0 .. size];
	}

	T* create(T, Args...)(Args args = Args.init)
	{
		static assert(is(T == struct), "it's not a struct");

		void* ptr = allocate(T.sizeof);
		if (!ptr)
			assert(0, "Out of memory!");

		auto ret = cast(T*) ptr;
		return emplace(ret, args);
	}
}

struct MallocAllocator
{
	Allocator base = Allocator( 
		&MallocAllocator.allocate,
		&MallocAllocator.reallocate,
		&MallocAllocator.free,
	);
	alias base this;

	static void* allocate(Allocator* self, size_t size)
	{
		assert(size > 0);
		return core.stdc.stdlib.malloc(size);
	}
	
	static void* reallocate(Allocator* self, void* ptr, size_t size)
	{
		assert(size > 0);
		return core.stdc.stdlib.realloc(ptr, size);
	}

	static void free(Allocator* self, void* ptr)
	{
		assert(ptr != null);
		core.stdc.stdlib.free(ptr);
	}
}

T[] dupe(T)(Allocator* a, T[] orig)
{
	assert(orig.length != 0);

	T[] ret = a.alloc_array!T(orig.length);

	memcpy(ret.ptr, orig.ptr, orig.length * T.sizeof);

	return ret;
}
