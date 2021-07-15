module decs.ecs;

import core.stdc.stdio;
import core.stdc.stdlib;
import core.stdc.string;
import core.lifetime;

import decs.collections;
import decs.dbg;
import decs.memory;

// commit: f9cf1322ddaf1e9b80349fcc2623babb79f83538
// https://github.com/prime31/zig-ecs

struct SparseSet(SparseT)
{
    enum page_size = 32_768;
    enum entity_per_page = page_size / SparseT.sizeof;

    Array!(SparseT[]) sparse;
    Array!SparseT dense;
    SparseT entity_mask;
    Allocator* allocator;

    static SparseSet create(Allocator* allocator)
    {
        SparseSet ret;
        ret.sparse = Array!(SparseT[]).createWith(allocator);
        ret.dense = Array!(SparseT).createWith(allocator);
        assert(ret.sparse.capacity == 16);
        assert(ret.dense.capacity == 16);

        ret.entity_mask = SparseT.max;
        ret.allocator = allocator;
        return ret;
    }

    void destroy()
    {
        sparse.expandToCapacity();
        foreach(size_t i, array; sparse)
        {
            if(array)
                sparse.allocator.free(array.ptr);
        }
    }

    size_t page(SparseT sparse)
    {
        return (sparse & entity_mask) / entity_per_page;
    }

    size_t offset(SparseT sparse)
    {
        return sparse & (entity_per_page - 1);
    }

    SparseT index(SparseT sparse)
    {
        assert((contains(sparse)));

        return this.sparse.get(page(sparse))[offset(sparse)];
    }

    bool contains(SparseT sparse)
    {
        // TODO: not sure about the null on array thing
        // i just replaced with length  rn
        auto curr = page(sparse);
        return curr < this.sparse.length  &&
        this.sparse.get(curr) != null &&
        this.sparse.get(curr)[offset(sparse)] != SparseT.max;
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

       dense.set(this.sparse.get(curr)[pos], last_dense);
       this.sparse.get(page(last_dense))[offset(last_dense)] = this.sparse.get(curr)[pos];
       this.sparse.get(curr)[pos] = SparseT.max;

       dense.pop();
    }

    SparseT[] assure(size_t pos) {
        if (pos >= sparse.length) {
            //printf("resize! %llu\n", pos);
            auto start_pos = sparse.length;
            sparse.resize(pos + 1);
            sparse.expandToCapacity();
            sparse._items[start_pos .. $] = null;
        }

        if ( sparse._items[pos].ptr) { // TODO: not good to get stuff from u_ array
            return sparse._items[pos];
        }

        //printf("alloc bad\n");
        auto new_page = sparse.allocator.alloc_array!SparseT(entity_per_page); 
        new_page[0 .. $] = SparseT.max;

        sparse.set(pos, new_page);

        return sparse.get(pos);
    }

    size_t len()
    {
        return dense.length;
    }
}

struct EntityTraitsDefinition(EntityType, IndexType, VersionType) 
{
    EntityType entity_type;
    IndexType index_type;
    VersionType version_type;
    /// Mask to use to get the entity index number out of an identifier
    EntityType entity_mask = IndexType.max;
    /// Mask to use to get the version out of an identifier
    EntityType version_mask = VersionType.max;
}


struct ComponentStorage(Component, Entity)
{
    // TODO: in zig-ecs they have to create a dummy component with a field u1
    // because of some weird thing about empty struct
    // this problem doesn't exist with D, i'll have to create tests with empty struct!!!!

    SparseSet!(Entity)* set;
    Array!(Component) instances;
    Allocator* allocator;
    /// doesnt really belong here...used to denote group ownership
    size_t superr = 0;
    void delegate(ComponentStorage*) safeDeinit;
    void delegate(ComponentStorage*, Entity, Entity, bool) safeSwap;
    void delegate(ComponentStorage*, Entity) safeRemoveIfContains;

    // TODO: implement signals
    //Signal!(Entity) construction;
    //Signal!(Entity) update;
    //Signal!(Entity) destruction;

    static ComponentStorage* createPtr(Allocator* allocator)
    {
        ComponentStorage* ret = allocator.create!ComponentStorage();
        ret.set = allocator.create!(SparseSet!Entity);
        *ret.set = SparseSet!(Entity).create(allocator);
        assert(ret.set.dense.capacity == 16);
        assert(ret.set.sparse.capacity == 16);
        // TODO: check empty struct
        ret.instances = Array!(Component).createWith(allocator);
        ret.allocator = allocator;
        ret.superr = 0;
        ret.safeDeinit = (ComponentStorage* c) {
            printf("YOOOOOO safeDeinit\n");
            c.instances.destroy();
        };
        ret.safeSwap = (ComponentStorage* c, Entity a, Entity b , bool instances_only) {
            printf("YOOOOOO safeSwap\n");
            if(!instances_only)
            {
                
            }
        };
        ret.safeRemoveIfContains = (ComponentStorage* c, Entity e) {
            printf("YOOOOOO safeRemvoeIfContains\n");
            if(c.contains(e))
                c.remove(e);
        };

        return ret;
    }

    void add(Entity entity, Component component)
    {
        // TODO: check empty struct when i sort this stuff out
        instances.add(component);
        set.add(entity);
        // TODO: signal construction
    }

    Component* get(Entity entity)
    {
        assert(contains(entity));
        return &instances._items[set.index(entity)];
    }

    bool contains(Entity entity)
    {
        return set.contains(entity);
    }

    void remove(Entity entity)
    {
        // TODO: signal destruction
        instances.removeSwap(set.index(entity));
        set.remove(entity);
    }

    void removeIfContains(Entity entity)
    {
        // TODO: need figure out why this
        static if( is(Component == bool) )
        {

        }

        if(contains(entity))
        {
            remove(entity);
        }
    }

    size_t len()
    {
        return set.len();
    }
}

struct Optional(T)
{
    enum undefined = Optional.init;

    private T value;
    private bool defined = false;

    void opAssign ( T rhs )
    {
        defined = true;
        value = rhs;
    }
}

struct Handles(HandleType, IndexType, VersionType)
{
    HandleType[] handles;
    IndexType append_cursor = 0;
    Optional!IndexType last_destroyed;

    Allocator* allocator;

    enum invalid_id = IndexType.max;

    static Handles createWith(Allocator* allocator, size_t capacity = 32)
    {
        Handles ret;
        ret.handles = allocator.alloc_array!HandleType(capacity);
        ret.allocator = allocator;
        return ret;
    }

    bool alive(HandleType handle)
    {
        auto id = extractId(handle);
        return id < append_cursor && handles[id] == handle;
    }

    HandleType create()
    {
        //printf("# create entity\n");
        if(!last_destroyed.defined)
        {
            //printf("\tlast destroyed null\n");
            // ensure capacity and grow if needed
            if(handles.length - 1 == append_cursor)
            {
                //printf("\treallocate handles\n");
                handles = allocator.reallocate_array(handles.ptr, handles.length * 2);
            }

            auto id = append_cursor;
            auto handle = forge(append_cursor, 0);
            handles[id] = handle;

            append_cursor += 1;

            
            //printf("\tnew handle: %llu\n", handle);
            return handle;
        }

        auto versionn = extractVersion(handles[last_destroyed.value]);
        auto destroyed_id =  extractId(handles[last_destroyed.value]);

        auto handle = forge(last_destroyed.value, versionn);
        handles[last_destroyed.value] = handle;

        // TODO: redo this optional bullshit
        last_destroyed = (destroyed_id == invalid_id) ? Optional!(IndexType)() : Optional!(IndexType)(destroyed_id, true); // destroyed_id;

        //printf("\thandle: %llu, destroyed_id: %lu,  last_destroyed: %lu :: invalid: %lu\n", handle, destroyed_id, last_destroyed.value, invalid_id);
        return handle;
    }

    void remove(HandleType handle)
    {
        //printf("# remove handle: %llu\n", handle);
        //printf("\tinvalid is: %lu\n", invalid_id);
        
        auto id = extractId(handle);
        if (id > append_cursor || handles[id] != handle)
            panic("RemovedInvalidHandle");

        //printf("\tid: %lu append_cursor: %lu\n", id, append_cursor);

        auto next_id = (last_destroyed.defined) ? last_destroyed.value : invalid_id;
        if (next_id == id)
            panic("ExhaustedEntityRemoval");

        //printf("\tnext_id: %lu\n", next_id);

        auto versionn = extractVersion(handle);
        handles[id] = forge(next_id, versionn + 1);

        last_destroyed = id;

        //printf("\tremove: %llu %lu %lu %lu %llu\n", handle, id, next_id, versionn, handles[id]);
    }

    IndexType extractId(HandleType handle)
    {
        // TODO: cast should be fine, but double check if errors occurs
        return cast(IndexType) handle;
    }

    VersionType extractVersion(HandleType handle)
    {   
        // TODO: cast should be fine, but double check if errors occurs
        return cast(VersionType) (handle >> (IndexType.sizeof * 8) );
    }

    HandleType forge(IndexType id, VersionType versionn)
    {
        // HandleType, IndexType, VersionType
        //      ulong,      uint,        uint
        return id | cast(HandleType) (versionn) << (IndexType.sizeof * 8);
    }
}

struct TypeStore
{
    HashMap!(uint, void*) map;
    Allocator* allocator;

    static TypeStore create(Allocator* allocator)
    {
        TypeStore ret;
        ret.map = HashMap!(uint, void*).create(allocator);
        ret.allocator = allocator;
        return ret;
    }
}


alias EntityTraitsDefinition_large = EntityTraitsDefinition!(ulong, uint, uint);
alias EntityHandle_large = Handles!(ulong, uint, uint);
alias Entity = ulong; // 1st
alias Storage(T) = ComponentStorage!(T, Entity);

struct GroupData
{
     ulong hash;
     ubyte size;
     /// optional. there will be an entity_set for non-owning groups and current for owning
     SparseSet!(Entity) entity_set;
     uint[] owned;
     uint[] include;
     uint[] exclude;
     Registry* registry;
     size_t current;

     static GroupData create(Allocator* allocator, Registry* registry, ulong hash, uint[] owned, uint[] include, uint[] exclude)
     {
         GroupData ret;
         ret.hash = hash;
         ret.size = cast(ubyte) (owned.length + include.length + exclude.length);
         ret.registry = registry;
         if(owned.length == 0)
            ret.entity_set = SparseSet!(Entity).create(allocator);
        ret.owned = dupe(allocator, owned);
        ret.include = dupe(allocator, include);
        ret.exclude = dupe(allocator, exclude);
        ret.registry = registry;
        ret.current = 0;
        return ret;
     }
}

struct Registry
{
    EntityHandle_large handles;
    HashMap!(ulong, size_t) components;
    HashMap!(uint, size_t) contexts;
    Array!(GroupData*) groups;
    TypeStore singletons;
    Allocator* allocator;


    static Registry create(Allocator* allocator)
    {
        Registry ret;
        ret.handles = EntityHandle_large.createWith(allocator);
        ret.components = HashMap!(ulong, size_t).create(allocator);
        ret.contexts = HashMap!(uint, size_t).create(allocator);
        ret.groups = Array!(GroupData*).createWith(allocator);
        ret.singletons = TypeStore.create(allocator);
        ret.allocator = allocator;    
        return ret;
    }

    bool valid(Entity entity)
    {
        return handles.alive(entity);
    }

    Entity create_entity()
    {
        return handles.create();
    }

    void destroy_entity(Entity entity)
    {
        assert(valid(entity));
        removeAll(entity);
        handles.remove(entity);
    }

    void removeAll(Entity entity)
    {
        assert(valid(entity));

        foreach(kv; components)
        {
            auto store = cast(Storage!(bool)*) kv.value;
            store.removeIfContains(entity);
        }
    }

    void add(T)(Entity entity, T value)
    {
        assert(valid(entity));
        auto s = assure!T();
        s.add(entity, value);
    }

    void remove(T)(Entity entity)
    {
        assert(valid(entity));
        auto s = assure!T();
        s.remove(entity);
    }

    T* get(T)(Entity entity)
    {
        assert(valid(entity));
        auto s = assure!T();
        assert(s);

        return s.get(entity);
    }

    bool has(T)(Entity entity)
    {
        assert(valid(entity));
        auto s = assure!T();
        assert(s);
        return s.contains(entity);

    }

    Storage!(T)* assure(T)()
    {
        auto type_id = type_id!T;

        if(components.has(type_id))
        {
            auto ptr = components.get(type_id);
            return cast(Storage!(T)*) ptr;
        }

        auto comp_set = Storage!(T).createPtr(allocator);
        auto comp_set_ptr = cast(size_t)(comp_set);
        components.set(type_id, comp_set_ptr);
        return comp_set;
    }

    auto view(Includes)()
    {
        return view!(Includes, Excludes!());
    }

    auto view(Includes, Excludes)()
    {
        static if (Includes.args.length == 1 && Excludes.args.length == 0)
        {
            auto storage = assure!(Includes.args[0]);
            return BasicView!(Includes.args[0]).create( storage );
        }
        else
        {

            ulong[Includes.args.length] includes_arr;
            static foreach (i, T; Includes.args)
            {
                assure!(T)();
                includes_arr[i] = type_id!(T);
            }
            ulong[Excludes.args.length] excludes_arr;
            static foreach (i, T; Excludes.args)
            {
                assure!(T)();
                excludes_arr[i] = type_id!(T);
            }
            return MultiView!(Includes.args.length, Excludes.args.length).create(&this, includes_arr, excludes_arr);
        }
    }
}
struct Includes(Args...) { alias args = Args; }
struct Excludes(Args...) { alias args = Args; }

struct BasicView(T)
{
    Storage!(T)* storage;

    static BasicView create(Storage!(T)* s)
    {
        return BasicView(s);
    }

    T* get(T)(Entity entity)
    {
        return storage.get(entity);
    }

    int opApply(scope int delegate(Entity) dg)
    {
        // TODO: should be reverse iteration

        int result = 0;
    
        foreach (Entity item; storage.set.dense)
        {
            result = dg(item);
            if (result)
                break;
        }
        return result;
    }
}

struct MultiView(size_t n_includes, size_t n_excludes)
{
    Registry* registry;
    ulong[n_includes] type_ids;
    ulong[n_excludes] exclude_type_ids;

    static MultiView create(Registry* reg, ulong[n_includes] includes, ulong[n_excludes] excludes)
    {
        return MultiView(reg, includes, excludes);
    }

    T* get(T)(Entity entity)
    {
        return registry.assure!(T).get(entity);
    }
    
    bool valid(T)()
    {
        auto t = type_id!T;
        bool v = false;
        foreach (tid; type_ids)
        {
            if (t == tid)
            {
                v = true;
            }
        }

        foreach (tid; exclude_type_ids)
        {
            if(t == tid)
            {
                v = false;
                break;
            }
        }
        return v;
    }

    void sort()
    {
        size_t[n_includes] sub_items;
        for(int i = 0; i < type_ids.length; i++)
        {
            auto ptr = registry.components.get(type_ids[i]);
            auto storage = cast(Storage!(ubyte)*) ptr;
            sub_items[i] = storage.len();
        }

        sortSub(sub_items[0 .. $], type_ids[ 0 .. $], (size_t a, size_t b) {
            return a < b;
        });
    }

    int opApply(scope int delegate(Entity) dg)
    {
        sort();

        auto ptr = registry.components.get(type_ids[0]);
        auto entities = (cast(Storage!(ubyte)*) ptr).set.dense;

        size_t size = entities.length;
        int result = 0;

        for (size_t i = size; i-- > 0;)
        {
            auto entity = entities.get(i);
            auto valid = true;

            foreach(tid; type_ids)
            {
                auto sptr = registry.components.get(tid);
                auto has = (cast(Storage!(ubyte)*) sptr).contains(entity);
                if(!has) 
                {
                    valid = false;
                    goto keep;
                }
            }

            foreach(tid; exclude_type_ids)
            {
                auto sptr = registry.components.get(tid);
                auto has = (cast(Storage!(ubyte)*) sptr).contains(entity);
                if(has) 
                {
                    valid = false;
                    goto keep;
                }
            }

            keep:

            if(valid)
                result = dg(entity);
        }

        //foreach (item; array)
        //{
        //    result = dg(item);
        //    if (result)
        //        break;
        //}
    
        return result;
    }
}

template type_id(alias type)
{
    enum ulong type_id = hashStringFnv(__traits(identifier, type));

    ulong hashStringFnv(string str)
    {
        enum ulong FNV_64_PRIME = 0x100000001b3UL;
        ulong value;
        foreach (c; str)
        {
            value *= FNV_64_PRIME;
            value ^= c;
        }
        return value;
    }
}

void sortSub(T1, T2)(T1[] items, T2[] sub_items, scope bool delegate(T1, T2) lessThan)
{
    size_t i = 1;
    while(i < items.length)
    {
        auto x = items[i];
        auto y = sub_items[i];
        size_t j = i;
        while(j > 0 && lessThan(x, items[j - 1]))
        {
            items[j] = items[j - 1];
            sub_items[j] = sub_items[j - 1];

            j -= 1;
        }
        items[j] = x;
        sub_items[j] = y;

        i += 1;
    }
}


@("ecs")
unittest
{
    import std.stdio: writeln;

    struct Pos
    {
        int x, y;
    }

    struct Empty{}
    struct Pos01{float x, y;}
    struct Pos02{float x, y;}
    struct Pos03{float x, y;}
    struct Pos04{float x, y;}
    struct Pos05{float x, y;}

    auto registry = Registry.create(MALLOCATOR.ptr);
    for(int i = 0; i < 10_000; i++)
    {
        Entity e = registry.create_entity();
        //writeln("ENTITY: ",e);
        assert(registry.valid(e));

        registry.add(e,Pos01(5,6));
        registry.add(e,Pos02(5,6));
        registry.add(e,Pos03(5,6));
        registry.add(e,Pos04(5,6));
        registry.add(e,Pos05(5,6));

        if(i == 20) registry.add(e, Empty());

        assert(registry.has!(Pos01)(e));
        assert(registry.has!(Pos02)(e));
        assert(registry.has!(Pos03)(e));
        assert(registry.has!(Pos04)(e));
        assert(registry.has!(Pos05)(e));
    }

    auto viewSolo = registry.view!(Includes!(Pos01));
    auto viewMulti = registry.view!(Includes!(Pos01, Pos05));
    auto viewExclude = registry.view!(Includes!(Pos01), Excludes!(Empty));

    foreach(e; viewSolo)
    {
        assert(registry.valid(e));
        assert(viewSolo.get!(Pos01)(e));
    }

    foreach(e; viewMulti)
    {
        assert(registry.valid(e));
        assert(viewMulti.get!(Pos01)(e));
        assert(viewMulti.get!(Pos05)(e));
    }
    foreach(e; viewExclude)
    {
        assert(registry.valid(e));
        assert(viewExclude.get!(Pos01)(e));
        assert(!viewExclude.valid!(Empty));
    }
}
