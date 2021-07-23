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

alias Entity = ulong; // 1st
alias Index = uint;
alias Version = uint;

enum entity_mask = 0xFFFFFFFF;
enum version_mask = 0xFFFFFFFF;
enum entity_shift = 32u;

alias Storage(T) = ComponentStorage!(T, Entity);
alias Registry = RegistryImpl!(Entity, Index, Version);

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
            c.instances.destroy();
        };
        ret.safeSwap = (ComponentStorage* c, Entity a, Entity b , bool instances_only) {
            if(!instances_only)
            {
                
            }
        };
        ret.safeRemoveIfContains = (ComponentStorage* c, Entity e) {
            if (c.contains(e))
            {
                c.remove(e);
            }
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
            safeRemoveIfContains(&this, entity);
        }
        else
        {
            if (contains(entity))
            {
                remove(entity);
            }
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

    //void opAssign ( T rhs )
    //{
    //    defined = true;
    //    value = rhs;
    //}
}

Index extract_id(Entity handle)
{
    return handle & entity_mask;
}

Version extract_version(Entity handle)
{
    auto mask = cast(Entity)(version_mask) << entity_shift;
    return cast(Version)((handle & mask) >> entity_shift);
}

Entity forge(Index id, Version versionn)
{
    return (id & entity_mask) | (cast(Entity)(versionn) << entity_shift);
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
        ret.size = cast(ubyte)(owned.length + include.length + exclude.length);
        ret.registry = registry;
        if (owned.length == 0)
            ret.entity_set = SparseSet!(Entity).create(allocator);
        ret.owned = dupe(allocator, owned);
        ret.include = dupe(allocator, include);
        ret.exclude = dupe(allocator, exclude);
        ret.registry = registry;
        ret.current = 0;
        return ret;
    }
}

struct RegistryImpl(EntityType, IndexType, VersionType)
{
    HashMap!(ulong, size_t) components;
    HashMap!(uint, size_t) contexts;
    Array!(GroupData*) groups;
    TypeStore singletons;
    Allocator* allocator;

    EntityType[] entities;
    IndexType append_cursor = 0;
    Optional!IndexType last_destroyed;
    
    enum invalid_id = entity_mask;

    static Registry create(Allocator* allocator)
    {
        Registry ret;
        ret.components = HashMap!(ulong, size_t).create(allocator);
        ret.contexts = HashMap!(uint, size_t).create(allocator);
        ret.groups = Array!(GroupData*).createWith(allocator);
        ret.singletons = TypeStore.create(allocator);
        ret.entities = allocator.alloc_array!(EntityType)(32);
        ret.last_destroyed.value = entity_mask;
        ret.last_destroyed.defined = false;
        ret.allocator = allocator;
        return ret;
    }

    bool valid(Entity entity)
    {
        auto id = extract_id(entity);
        return id < append_cursor && entities[id] == entity;
    }

    Entity generate_id()
    {
        //ENTT_ASSERT(static_cast<typename traits_type::entity_type>(entities.size()) < traits_type::entity_mask);
        // auto e = entity_type{static_cast<typename traits_type::entity_type>(entities.size())};
        //return entities.emplace_back(e);

        return 0;
    }

    Entity recycle_id()
    {
        return 0;
    }

    void release_entity(Entity entity, Version v)
    {
    }

    Entity create_entity()
    {
        //printf("# create entity\n");
        if (!last_destroyed.defined)
        {
            //printf("\tlast destroyed null\n");
            // ensure capacity and grow if needed
            if (entities.length - 1 == append_cursor)
            {
                //printf("\treallocate handles\n");
                entities = allocator.reallocate_array(entities.ptr, entities.length * 2);
            }

            auto id = append_cursor;
            auto handle = forge(append_cursor, 0);
            entities[id] = handle;

            append_cursor += 1;

            //printf("\tnew handle: %llu\n", handle);
            return handle;
        }

        auto versionn = extract_version(entities[last_destroyed.value]);
        auto destroyed_id =  extract_id(entities[last_destroyed.value]);

        auto handle = forge(last_destroyed.value, versionn);
        entities[last_destroyed.value] = handle;

        // TODO: redo this optional bullshit
        last_destroyed = (destroyed_id == invalid_id) ? Optional!(IndexType)(entity_mask, false) : Optional!(IndexType)(destroyed_id, true); // destroyed_id;
        return handle;
    }

    void destroy_entity(Entity entity)
    {
        assert(valid(entity));
        removeAll(entity);

        auto id = extract_id(entity);
        if (id > append_cursor || entities[id] != entity)
            panic("RemovedInvalidHandle");

        //printf("\tid: %lu append_cursor: %lu\n", id, append_cursor);

        auto next_id = (last_destroyed.defined) ? last_destroyed.value : invalid_id;
        if (next_id == id)
            panic("ExhaustedEntityRemoval");

        //printf("\tnext_id: %lu\n", next_id);

        auto versionn = extract_version(entity);
        entities[id] = forge(next_id, versionn + 1);

        last_destroyed = Optional!(IndexType)(id, true);

        //printf("\tremove: %llu %lu %lu %lu %llu\n", handle, id, next_id, versionn, handles[id]);

    }

    void removeAll(Entity entity)
    {
        assert(valid(entity));

        foreach (kv; components)
        {
            auto store = cast(Storage!(bool)*) kv.value;
            store.removeIfContains(entity);
        }
    }

    void add(T)(Entity entity, T value = T.init)
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

        if (components.has(type_id))
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
            return BasicView!(Includes.args[0]).create(storage);
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
/*
    void view_it(Includes)(scope void delegate(Entity, Includes.args*) cb)
    {
        view_it!(Includes, Excludes!())(cb);
    }

    void view_it(Includes, Excludes)(scope void delegate(Entity, Includes.args) cb)
    {
        auto v = view!(Includes, Excludes)();
        
        foreach(e; v)
        {
            // i need to get types from the Includes aliasseq, so i get the component and pass them in the delegate
            // i'm not sure how i can achieve it             
            Includes.args* args; 
            foreach(i, a; Includes.args)
            {
                args[i] = v.get!(a)(e);
            }
            cb(e, args);
        }
    }
*/
    // TODO: add support for groups
    //auto group(T, Includes, Excludes)()
    //{
    //}
}
struct Includes(Args...) { alias args = Args; }
struct Excludes(Args...) { alias args = Args; }
struct Results(Args...) { alias args = Args; }

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

struct BasicGroup
{
    Registry* registry;
    GroupData* group_data;
}

struct OwningGroup
{
    Registry* registy;
    GroupData* group_data;
    size_t* superr;
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
template type_name(alias type)
{
    enum auto type_name = __traits(identifier, type);
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
    struct Pos06{float x, y;}
    struct Pos07{float x, y;}
    struct Pos08{float x, y;}
    struct Pos09{float x, y;}
    struct Pos10{float x, y;}
    struct Pos11{float x, y;}
    struct Pos12{float x, y;}
    struct Pos13{float x, y;}
    struct Pos14{float x, y;}
    struct Pos15{float x, y;}
    struct Pos16{float x, y;}
    struct Pos17{float x, y;}
    struct Pos18{float x, y;}
    struct Pos19{float x, y;}

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

        assert(registry.has!(Pos01)(e));
        assert(registry.has!(Pos02)(e));
        assert(registry.has!(Pos03)(e));
        assert(registry.has!(Pos04)(e));
        assert(registry.has!(Pos05)(e));
    }

    //registry.view_it!(Includes!(Pos01))( (Entity e, Pos01* p) {
    //} );
}

@("ecs - views")
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

@("ecs - large entity count")
unittest
{
    struct A{}
    struct B{int[10] large;}
    auto registry = Registry.create(MALLOCATOR.ptr);

    auto a = Array!(Entity).createWith(MALLOCATOR.ptr);
    for(size_t i = 0; i < 0xFFFFFFFF + 1000; i++)
    {
        auto e = registry.create_entity();
        registry.add(e, A());
        registry.add(e, B());
        a.add(e); 
    }
    foreach(Entity e; a)
    {
        assert(registry.has!(A)(e));
        assert(registry.has!(B)(e));
        registry.destroy_entity(e);
    }
    a.clear();    
    for(size_t i = 0; i < 0xFFFFFFFF + 1000; i++)
    {
        auto e = registry.create_entity();
        registry.add(e, A());
        registry.add(e, B());
        a.add(e); 
    }
    foreach(Entity e; a)
    {
        registry.destroy_entity(e);
    }
    a.clear();
}
