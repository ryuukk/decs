# DECS

A pure D port of zig-ecs with the magic of metaprogramming

## Example

```D
struct Dead {}
struct Position { float x; float y; }
struct Velocity { float x; float y; }

auto registry = Registry.create(MALLOCATOR.ptr);

auto e1 = registry.create_entity();
registry.add(e1, Position(0, 0));
registry.add(e1, Velocity(1, 0));

auto e2 = registry.create_entity();
registry.add(e2, Position(0, 0));
registry.add(e2, Velocity(1, 0));
registry.add(e2, Dead());

auto view = registry.view!(Includes!(Position, Velocity), Excludes!(Dead));
foreach (e; view)
{
    auto pos = view.get!(Position)(e);
    auto vel = view.get!(Velocity)(e);

    pos.x += vel.x;
    pos.y += vel.y;

    printf("Entity: %llu:  NewPos: %f:%f Vel: %f:%f\n", e, pos.x, pos.y, vel.x, vel.y);
}
```


## Roadmap

- [x] Views
- [ ] Groups
- [ ] Signals
