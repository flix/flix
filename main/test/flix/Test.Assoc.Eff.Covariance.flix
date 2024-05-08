mod Test.Assoc.Eff.Covariance {

    pub enum List[_t]                       // An immutable data type.
    pub enum MutList[_r: Region, _t: Type]  // A mutable data type with a region.

    ///
    /// A trait for container types that can have their elements mapped.
    ///
    trait Functor[m: Type -> Type] {
        // An associated effect which represents the effect of accessing the data structure.
        type Eff: Eff

        pub def map(f: a -> b \ ef, m: m[a]): m[b] \ (ef + Functor.Eff[m])
    }

    instance Functor[List] {
        type Eff = {} // Accessing an immutable list is pure.

        pub def map(f: a -> b \ ef, x: List[a]): List[b] \ ef = checked_ecast(???)
    }

    instance Functor[MutList[r]] {
        type Eff = r // Accessing a mutable list has an effect in its region.

        pub def map(f: a -> b \ ef, x: MutList[r, a]): MutList[r, b] \ (ef + r) = checked_ecast(???)
    }

}
