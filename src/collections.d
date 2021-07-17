module decs.collections;

import decs.dbg;
import decs.memory;

struct Array(T)
{
    Allocator* allocator;

    T[] _items;
    private size_t _count = 0;
    size_t capacity = 0;

    static Array createWith(Allocator* allocator, size_t capacity = 16)
    {
        Array ret;
        ret.allocator = allocator;
        ret._count = 0;
        ret.capacity = capacity;
        ret._items = allocator.alloc_array!T(capacity);
        return ret;
    }

    void create(Allocator* allocator, size_t capacity = 16)
    {
        this.allocator = allocator;
        this._count = 0;
        this.capacity = capacity;    
        _items = allocator.alloc_array!T(capacity);    
    }

    void destroy()
    {
        allocator.free(_items.ptr);
    }

    size_t length()
    {
        return _count;
    }

    bool empty()
    {
        return _count == 0;
    }

    ref T opIndex(size_t index)
    {
        if ((index < 0) || (index >= _count))
            panic("out of bound");
        return _items[index];
    }

    void opIndexAssign(T value, in size_t index)
    {
        if (index >= _count)
            panic("out of bound");
        _items[index] = value;
    }

    int opApply(int delegate(ref T) dg)
    {
        int result;
        //foreach (ref T item; _items)
        for (int i = 0; i < _count; i++)
            if ((result = dg(_items[i])) != 0)
                break;
        return result;
    }

    int opApply(int delegate(T*) dg)
    {
        int result;
        //foreach (ref T item; _items)
        for (int i = 0; i < _count; i++)
            if ((result = dg(&_items[i])) != 0)
                break;
        return result;
    }

    int opApply(int delegate(size_t, T*) dg)
    {
        int result;
        //foreach (ref T item; _items)
        for (size_t i = 0; i < _count; i++)
            if ((result = dg(i, &_items[i])) != 0)
                break;
        return result;
    }

    T get(size_t index)
    {
        if ((index < 0) || (index >= _count))
            panic("out of bound");
        return _items[index];
    }

    void set(size_t index, ref T value)
    {
        if (index >= _count)
            panic("out of bound");
        _items[index] = value;
    }

    void ensureTotalCapacity(size_t new_capacity)
    {
         size_t better_capacity = capacity;
         if (better_capacity >= new_capacity) return;

         while (true) {
             better_capacity += better_capacity / 2 + 8;
             if (better_capacity >= new_capacity) break;
         }

         size_t originalLength = _items.length;
         size_t diff = new_capacity - capacity;

         // TODO This can be optimized to avoid needlessly copying undefined memory.
         T[] new_memory = allocator.reallocate_array(_items.ptr, better_capacity);
         _items = new_memory;
         capacity = new_memory.length;
        
        if (diff > 0)
        {
            // todo: fill stuff with default values
            for (size_t i = originalLength; i < originalLength + diff; i++)
            {
                _items[i] = T.init;
            }
        }
    }

    void expandToCapacity()
    {
        _count = capacity;
    }

    void resize(size_t newSize)
    {
        ensureTotalCapacity(cast(int)newSize);
        _count = cast(int) newSize;
    }

    void clear()
    {
        for (int i = 0; i < _items.length; i++)
        {
            _items[i] = T.init;
        }

        _count = 0;
    }

    void add(T item)
    {
        auto length = cast(int) _items.length;
        if (_count + 1 > length)
        {
            auto expand = (length < 1000) ? (length + 1) * 4 : 1000;

            ensureTotalCapacity(length + expand);
        }

        _items[_count++] = item;
    }

    void add_all(ref Array!T items)
    {
        // todo: optimize
        for (int i = 0; i < items.length(); i++)
            add(items[i]);
    }

    bool remove(T item)
    {
        for (int i = 0; i < _count; i++)
        {
            if (_items[i] == item)
            {
                return remove_at(i);
            }
        }
        return false;
    }


    // TODO: add tests for both of them
    T removeSwap(size_t index)
    {
        if (length - 1 == index) return pop();
        
         auto old_item = _items[index];
         _items[index] = pop();
         return old_item;
    }

    T pop()
    {
        auto val = _items[length - 1];
        _count -= 1;
        return val;
    }

    int index_of(T item)
    {
        for (int i = 0; i < _count; i++)
            if (_items[i] == item)
                return i;
        return -1;
    }

    bool remove_at(size_t index)
    {
        import core.stdc.string : memmove;

        T val = _items[index];
        _count--;

        static if (__traits(isPOD, T))
        {
            memmove(_items.ptr + index, // dest
                    _items.ptr + index + 1, // src
                    (_count - index) * T.sizeof); // num bytes
        }
        else
        {
            for (auto j = index; j < _count; j++)
            {
                _items[j] = _items[j + 1];
            }
        }
        return true;
    }

    bool remove_back()
    {
        return remove_at(_count - 1);
    }

    T[] get_slice()
    {
        return _items[0 .. _count];
    }
}

struct HashMap(Key, Value,
        Hasher = HashFunc!(Key), Comparer = HashComp!(Key),
        ubyte MIN_HASH_TABLE_POWER = 3, ubyte RELATIONSHIP = 8)
{

    static struct Pair
    {
        Key key;
        Value value;
    }

    static struct Element
    {
        uint hash = 0;
        Element* next = null;
        Pair pair;
    }

    Element** hash_table = null;
    ubyte hash_table_power = 0;
    uint elements = 0;
    Allocator* allocator = null;

    static HashMap create(Allocator* alloc)
    {
        HashMap ret;
        ret.allocator = alloc;
        return ret;
    }

private:
    void make_hash_table()
    {
        if (!allocator)
            allocator = &MALLOCATOR.base;

        hash_table = cast(Element**) allocator.allocate((Element*).sizeof * (1 << MIN_HASH_TABLE_POWER));
        hash_table_power = MIN_HASH_TABLE_POWER;
        elements = 0;
        for (int i = 0; i < (1 << MIN_HASH_TABLE_POWER); i++)
        {
            hash_table[i] = null;
        }
    }

    void erase_hash_table()
    {
        //ERR_FAIL_COND_MSG(elements, "Cannot erase hash table if there are still elements inside.");
        //memdelete_arr(hash_table);

        allocator.free(hash_table);
        hash_table = null;
        hash_table_power = 0;
        elements = 0;
    }

    void check_hash_table()
    {
        int new_hash_table_power = -1;

        if (cast(int) elements > ((1 << hash_table_power) * RELATIONSHIP))
        {
            /* rehash up */
            new_hash_table_power = hash_table_power + 1;

            while (cast(int) elements > ((1 << new_hash_table_power) * RELATIONSHIP))
            {
                new_hash_table_power++;
            }

        }
        else if ((hash_table_power > cast(int) MIN_HASH_TABLE_POWER) && (
                cast(int) elements < ((1 << (hash_table_power - 1)) * RELATIONSHIP)))
        {
            /* rehash down */
            new_hash_table_power = hash_table_power - 1;

            while (cast(int) elements < ((1 << (new_hash_table_power - 1)) * RELATIONSHIP))
            {
                new_hash_table_power--;
            }

            if (new_hash_table_power < cast(int) MIN_HASH_TABLE_POWER)
            {
                new_hash_table_power = MIN_HASH_TABLE_POWER;
            }
        }

        if (new_hash_table_power == -1)
        {
            return;
        }

        //Element **new_hash_table = memnew_arr(Element*, (cast(ulong)1 << new_hash_table_power));
        Element** new_hash_table = cast(Element**) allocator.allocate((Element*).sizeof * (cast(ulong) 1 << new_hash_table_power));
        //ERR_FAIL_COND_MSG(!new_hash_table, "Out of memory.");

        for (int i = 0; i < (1 << new_hash_table_power); i++)
        {
            new_hash_table[i] = null;
        }

        if (hash_table)
        {
            for (int i = 0; i < (1 << hash_table_power); i++)
            {
                while (hash_table[i])
                {
                    Element* se = hash_table[i];
                    hash_table[i] = se.next;
                    int new_pos = se.hash & ((1 << new_hash_table_power) - 1);
                    se.next = new_hash_table[new_pos];
                    new_hash_table[new_pos] = se;
                }
            }
            //memdelete_arr(hash_table);
            allocator.free(hash_table);
        }
        hash_table = new_hash_table;
        hash_table_power = cast(ubyte) new_hash_table_power;
    }

    const Element* get_element(const ref Key p_key)
    {

        if (!hash_table)
            return null;

        uint hash = Hasher.hash(p_key);
        uint index = hash & ((1 << hash_table_power) - 1);

        Element* e = cast(Element*) hash_table[index];

        while (e)
        {
            /* checking hash first avoids comparing key, which may take longer */
            if (e.hash == hash && Comparer.compare(e.pair.key, p_key))
            {
                /* the pair exists in this hashtable, so just update data */
                return e;
            }
            e = e.next;
        }

        return null;
    }

    Element* create_element(const ref Key p_key)
    {
        /* if element doesn't exist, create it */
        Element* e = cast(Element*) allocator.allocate(Element.sizeof);
        //ERR_FAIL_COND_V_MSG(!e, nullptr, "Out of memory.");
        uint hash = Hasher.hash(p_key);
        uint index = hash & ((1 << hash_table_power) - 1);
        e.next = hash_table[index];
        e.hash = hash;
        e.pair.key = cast(Key)p_key; // TODO: when i use pointer as key, i need this
        e.pair.value = Value.init;

        hash_table[index] = e;
        elements++;
        return e;
    }

public:
    Element* set(const ref Key key, const ref Value value)
    {
        Element* e = null;
        if (!hash_table)
        {
            make_hash_table(); // if no table, make one
        }
        else
        {
            e = cast(Element*)(get_element(key));
        }

        /* if we made it up to here, the pair doesn't exist, create and assign */

        if (!e)
        {
            e = create_element(key);
            if (!e)
            {
                return null;
            }
            check_hash_table(); // perform mantenience routine
        }

        e.pair.value = cast(Value) value;
        return e;
    }

    ref Value get(const ref Key p_key)
    {
        Value* res = getptr(p_key);
        //CRASH_COND_MSG(!res, "Map key not found.");
        return *res;
    }

    Value* getptr(const ref Key p_key)
    {
        if (!hash_table)
        {
            return null;
        }

        Element* e = cast(Element*)(get_element(p_key));

        if (e)
        {
            return &e.pair.value;
        }

        return null;
    }

    bool erase(const ref Key p_key)
    {
        if (!hash_table)
        {
            return false;
        }

        uint hash = Hasher.hash(p_key);
        uint index = hash & ((1 << hash_table_power) - 1);

        Element* e = hash_table[index];
        Element* p = null;
        while (e)
        {
            /* checking hash first avoids comparing key, which may take longer */
            if (e.hash == hash && Comparer.compare(e.pair.key, p_key))
            {
                if (p)
                {
                    p.next = e.next;
                }
                else
                {
                    //begin of list
                    hash_table[index] = e.next;
                }

                allocator.free(e);
                elements--;

                if (elements == 0)
                {
                    erase_hash_table();
                }
                else
                {
                    check_hash_table();
                }
                return true;
            }

            p = e;
            e = e.next;
        }

        return false;
    }

    bool has(const ref Key p_key)
    {
        return getptr(p_key) != null;
    }

    uint size() const {
		return elements;
	}

	bool is_empty() const {
		return elements == 0;
	}

    void clear()
    {
		/* clean up */
		if (hash_table) {
			for (int i = 0; i < (1 << hash_table_power); i++) {
				while (hash_table[i]) {
					Element *e = hash_table[i];
					hash_table[i] = e.next;
                    allocator.free(e);
				}
			}
            allocator.free(hash_table); // TODO: check if that's the right way to delete T**
		}

		hash_table = null;
		hash_table_power = 0;
		elements = 0;
    }

    int opApply(int delegate(Pair*) dg)
    {
        if(!hash_table) return 0;

        int result;
        for (int i = 0; i < (1 << hash_table_power); i++) {
            Element* e = hash_table[i];
            while(e) {
                if ((result = dg(&e.pair)) != 0)
                    break;
                e = e.next;
            }
        }
        return result;
    }

    int opApply(int delegate(Key*, Value*) dg)
    {
        if(!hash_table) return 0;

        int result;
        for (int i = 0; i < (1 << hash_table_power); i++) {
            Element* e = hash_table[i];
            while(e) {
                if ((result = dg(&e.pair.key, &e.pair.value)) != 0)
                    break;
                e = e.next;
            }
        }
        return result;
    }

    void opIndexAssign(const ref Value value, const Key key) {
        set(key, value);
    }

    // TODO: how to handle error
    ref Value opIndex(const ref Key key) {
        if(!has(key)) panic("key not found");
        return get(key);
    }

}

struct HashFunc(T)
{
    static uint hash(const ref T v)
    {
        static if(is(T == U*, U) && __traits(isScalar, T)) 
        {
            return hash_one_uint64(cast(ulong) v);
        }
        else static if( is(T == int) || is(T == uint)) 
        {
            return cast(uint) v;
        }
        else static if( is(T == long) || is(T == ulong) ) 
        {
            return hash_one_uint64(cast(ulong) v);
        }
        else static if( is(T == float) || is(T == double) ) 
        {
            return hash_djb2_one_float(v);
        }
        else static if ( is (T == string) )
        {
            return cast(int) hashOf(v);
        }
        else static assert(0, "not supported");
    }
/*
    static uint hash(const(char)* p_cstr)
    {
        return hash_djb2(p_cstr);
    }

    static uint hash(const ulong p_int)
    {
        return hash_one_uint64(p_int);
    }

    static uint hash(const long p_int)
    {
        return hash(cast(ulong)(p_int));
    }

    static uint hash(const float p_float)
    {
        return hash_djb2_one_float(p_float);
    }

    static uint hash(const double p_double)
    {
        return hash_djb2_one_float(p_double);
    }

    static uint hash(const uint p_int)
    {
        return p_int;
    }

    static uint hash(const int p_int)
    {
        return cast(uint) p_int;
    }

    static uint hash(const ushort p_int)
    {
        return p_int;
    }

    static uint hash(const short p_int)
    {
        return cast(uint) p_int;
    }

    static uint hash(const ubyte p_int)
    {
        return p_int;
    }

    static uint hash(const byte p_int)
    {
        return cast(uint) p_int;
    }

    static uint hash(const char p_uchar)
    {
        return cast(uint) p_uchar;
    }

    static uint hash(const wchar p_wchar)
    {
        return cast(uint) p_wchar;
    }

    static uint hash(const dchar p_uchar)
    {
        return cast(uint) p_uchar;
    }
*/
}

struct HashComp(T)
{
    static bool compare()(const ref T p_lhs, const ref T p_rhs)
    {
        static if(is(T == U*, U) && __traits(isScalar, T)) 
        {
            return p_lhs == p_rhs;
        }
        else static if( is(T == int) || is(T == uint)) 
        {
            return p_lhs == p_rhs;
        }
        else static if( is(T == ulong) || is(T == ulong)) 
        {
            return p_lhs == p_rhs;
        }
        else static if( is(T == float) || is(T == double) ) 
        {
            return (p_lhs == p_rhs) || (is_nan(p_lhs) && is_nan(p_rhs));
        } 
        else static if ( is (T == string) )
        {
            return (p_lhs == p_rhs);
        }
        
        else static assert(0, "not supported");
    }
}


private static uint hash_djb2(const(char)* p_cstr)
{
    const(ubyte)* chr = cast(const(ubyte)*) p_cstr;
    uint hash = 5381;
    uint c;

    while ((c = *chr++) == 1)
    { // TODO: check == 1
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }

    return hash;
}

private static ulong hash_djb2_one_float_64(double p_in, ulong p_prev = 5381)
{
    union U
    {
        double d;
        ulong i;
    }

    U u;

    // Normalize +/- 0.0 and NaN values so they hash the same.
    if (p_in == 0.0f)
    {
        u.d = 0.0;
    }
    else if (is_nan(p_in))
    {
        u.d = float.nan;
    }
    else
    {
        u.d = p_in;
    }

    return ((p_prev << 5) + p_prev) + u.i;
}

private static ulong hash_djb2_one_64(ulong p_in, ulong p_prev = 5381)
{
    return ((p_prev << 5) + p_prev) + p_in;
}

private static uint hash_one_uint64(const ulong p_int)
{
    ulong v = p_int;
    v = (~v) + (v << 18); // v = (v << 18) - v - 1;
    v = v ^ (v >> 31);
    v = v * 21; // v = (v + (v << 2)) + (v << 4);
    v = v ^ (v >> 11);
    v = v + (v << 6);
    v = v ^ (v >> 22);
    return cast(int) v;
}

private static uint hash_djb2_one_float(double p_in, uint p_prev = 5381)
{
    union U
    {
        double d;
        ulong i;
    }

    U u;

    // Normalize +/- 0.0 and NaN values so they hash the same.
    if (p_in == 0.0f)
    {
        u.d = 0.0;
    }
    else if (is_nan(p_in))
    {
        u.d = float.nan;
    }
    else
    {
        u.d = p_in;
    }

    return ((p_prev << 5) + p_prev) + hash_one_uint64(u.i);
}


T nextPOT(T)(T x) {
	--x;
	x |= x >> 1;
	x |= x >> 2;
	x |= x >> 4;
	static if (T.sizeof >= 16) x |= x >>  8;
	static if (T.sizeof >= 32) x |= x >> 16;
	static if (T.sizeof >= 64) x |= x >> 32;
	++x;

	return x;
}

struct SparseSet(SparseT)
{
    enum page_size = 4096;

    Array!(Optional!(SparseT[])) sparse;
    Array!SparseT dense;
    SparseT entity_mask;
    Allocator* allocator;

    static SparseSet create(Allocator* allocator)
    {
        SparseSet ret;
        ret.sparse = Array!(Optional!(SparseT[])).createWith(allocator);
        ret.dense = Array!(SparseT).createWith(allocator);
        assert(ret.sparse.capacity == 16);
        assert(ret.dense.capacity == 16);

        ret.entity_mask = 0xFFFFFFFF;
        ret.allocator = allocator;
        return ret;
    }

    size_t page(SparseT entt)
    {
        //return size_type{(to_integral(entt) & traits_type::entity_mask) / page_size};
        return (entt & entity_mask) / page_size;
    }

    size_t offset(SparseT entt)
    {
        //return size_type{to_integral(entt) & (page_size - 1)};
        return entt & (page_size - 1);
    }

    SparseT index(SparseT sparse)
    {
        assert((contains(sparse)));

        return this.sparse.get(page(sparse)).value[offset(sparse)];
    }

    bool contains(SparseT sparse)
    {
        // TODO: not sure about the null on array thing
        // i just replaced with length  rn
        auto curr = page(sparse);
        return curr < this.sparse.length &&
            this.sparse.get(curr)
            .defined == true &&
            this.sparse.get(curr).value[offset(sparse)] != SparseT.max;
    }

    void add(SparseT sparse)
    {

        assert(!contains(sparse));

        // assure(page(entt))[offset(entt)] = packed.size()

        size_t pos = page(sparse);
        size_t off = offset(sparse);

        assure(pos)[off] = cast(SparseT) dense.length;

        dense.add(sparse);
    }

    void remove(SparseT sparse)
    {
        assert(contains(sparse));

        auto curr = page(sparse);
        auto pos = offset(sparse);
        auto last_dense = dense.get(dense.length - 1);

        dense.set(this.sparse.get(curr).value[pos], last_dense);
        this.sparse.get(page(last_dense))
            .value[offset(last_dense)] = this.sparse.get(curr).value[pos];
        this.sparse.get(curr).value[pos] = SparseT.max;

        dense.pop();
    }

    SparseT[] assure(size_t pos)
    {
        if (pos >= sparse.length)
        {
            auto start_pos = sparse.length;
            sparse.resize(pos + 1);
            sparse.expandToCapacity();
            sparse._items[start_pos .. $] = Optional!(SparseT[])();
        }

        if (sparse.get(pos).defined == false)
        {
            auto new_page = sparse.allocator.alloc_array!SparseT(page_size);
            new_page[0 .. $] = SparseT.max;
            auto o = Optional!(SparseT[])(new_page, true);
            sparse.set(pos, o);
        }

        return sparse.get(pos).value;
    }

    size_t len()
    {
        return dense.length;
    }
}

package struct Optional(T)
{
    enum undefined = Optional.init;

    private T value;
    private bool defined = false;

    //void opAssign ( T rhs )
    //{
    //    defined = true;
    //    value = rhs;
    //}
}