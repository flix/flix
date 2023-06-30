mod Test.Assoc.Eff.Contravariance {

    pub enum ParSet[_a] // A "parallel set"   data type whose operations must be pure.
    pub enum SeqSet[_a] // A "sequential set" data type whose operations may have any effect.
    pub enum SecSet[_a] // A "secret set"     data type whose operations must not have the `Network` effect.

    pub eff Network

    ///
    /// A type class for container types which can have their elements counted.
    ///
    class Countable[m: Type -> Type] {
        /// An associated effect which specifies an *upperbound* on the permitted effects.
        type UpperBound[m]: Eff

        /// We use intersection to restrict the effects of `f`.
        pub def count(f: a -> Bool \ (ef & Countable.UpperBound[m]), m: m[a]): Int32
    }

    instance Countable[ParSet] {
        /// A parallel set permits no effects: the upper bound is pure (i.e. empty set of effects).
        type UpperBound[ParSet] = Pure
        pub def count(f: a -> Bool \ {}, m: ParSet[a]): Int32 = ???
    }

    instance Countable[SeqSet] {
        /// A sequential set permits any effects: the upper bound is the universe.
        type UpperBound[SeqSet] = ~Pure
        pub def count(f: a -> Bool \ ef, m: SeqSet[a]): Int32 = ???
    }

    instance Countable[SecSet] {
        /// A secret set permits any effects, except the `Network` effect.
        type UpperBound[SecSet] = ~Network
        pub def count(f: a -> Bool \ ef & (~Network), m: SecSet[a]): Int32 = ???
    }

}
