// NB: These tests must be in the root namespace.
pub type alias T = Int32
namespace N {
    pub enum T

    pub def f(): T = ???
}

pub enum U
namespace M {
    pub type alias U = Int32

    pub def f(): U = ???
}

namespace Test/Integ/Enum/TypeAlias {
    pub type alias T[ef: Region] = Unit -> Bool & ef
    enum E[ef: Region] {
        case E(T[ef])
    }
}
