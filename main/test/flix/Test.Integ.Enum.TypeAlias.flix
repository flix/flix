// NB: These tests must be in the root namespace.
pub type alias T = Int
namespace N {
    pub enum T

    pub def f(): T = ???
}

pub enum U
namespace M {
    pub type alias U = Int

    pub def f(): U = ???
}
