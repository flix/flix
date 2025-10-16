// NB: These tests must be in the root module.
pub type alias T = Int32
mod N {
    pub enum T

    pub def f(): T = ???
}

pub enum U
mod M {
    pub type alias U = Int32

    pub def f(): U = ???
}

mod Test.Integ.Enum.TypeAlias {
    pub type alias T[ef: Region] = Unit -> Bool \ ef
    pub enum E[ef: Region] {
        case E(T[ef])
    }

    pub type alias Phantom[_: Type] = Bool
    pub enum E2[a: Type] {
        case E2(Phantom[a])
    }
}
