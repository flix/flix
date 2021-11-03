// NB: These tests must be in the root namespace.
type alias T = Int
namespace N {
    enum T

    pub def f(): T
}

enum U
namespace M {
    type alias U = Int

    pub def f(): U
}
