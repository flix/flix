enum IntensionalSet[a] {
    case IntensionalSet(a -> Bool)
}

namespace IntensionalSet {

    ///
    /// Returns the empty set.
    ///
    def empty[a]: IntensionalSet[a] = IntensionalSet(x -> false)

    ///
    /// Returns the singleton set of `x`.
    ///
    def singleton[a](x: a): IntensionalSet[a] = IntensionalSet(y -> y == x)

    ///
    /// Returns an intensional set of the given predicate `f`.
    ///
    def of[a](f: a -> Bool): IntensionalSet[a] = IntensionalSet(f)

    ///
    /// Adds `x` to the set `s`.
    ///
    def insert[a](x: a, s: IntensionalSet[a]): IntensionalSet[a] =
        let IntensionalSet(f) = s;
            IntensionalSet(y -> y == x || f(y))

    ///
    /// Removes `x` from the set `s`.
    ///
    def delete[a](x: a, s: IntensionalSet[a]): IntensionalSet[a] =
        let IntensionalSet(f) = s;
            IntensionalSet(y -> y != x && f(y))

    ///
    /// Returns `true` if `a` is a member of the set `s`.
    ///
    def memberOf[a](a: a, s: IntensionalSet[a]): Bool =
        let IntensionalSet(f) = s;
            f(a)


}


namespace IntensionalSet {

    @test
    def testEmpty01: Bool = assertNot!(42 `memberOf` empty())

    @test
    def testSingleton01: Bool = assert!(42 `memberOf` singleton(42))

    @test
    def testSingleton02: Bool = assertNot!(21 `memberOf` singleton(42))

    @test
    def testInsert01: Bool = assert!(42 `memberOf` (insert(42, empty())))

    @test
    def testInsert02: Bool = assertNot!(21 `memberOf` (insert(42, empty())))

    @test
    def testDelete01: Bool = assert!(42 `memberOf` delete(21, singleton(42)))

    @test
    def testDelete02: Bool = assertNot!(42 `memberOf` delete(42, singleton(42)))

    @test
    def testDelete03: Bool = assert!(42 `memberOf` insert(42, delete(42, singleton(42))))

}

//
//## Set Predicates
//- **null(xs: Set[A]): Bool**
//    - Returns true if the set is empty.
//- **memberOf(x: A, xs: Set[A]): Bool**
//    - Returns true if x is a member of the set xs.
//- **isSubsetOf(xs: Set[A], ys: Set[A]): Bool**
//    - Returns true if every element in xs appears in ys.
//- **isProperSubsetOf(xs: Set[A], ys: Set[A]): Bool**
//    - Returns true if every element in xs appears in ys and xs != ys.
//
//## Elementary Set Operations
//- **union(xs: Set[A], ys: Set[A]): Set[A]**
//    - Returns a new set of the union of xs and ys.
//- **intersection(xs: Set[A], ys: Set[A]): Set[A]**
//    - Returns a new set of the intersection of xs and ys.
//- **difference(xs: Set[A], ys: Set[A]): Set[A]**
//    - Returns a new set of the difference of xs and ys.
//- **subsets(xs: Set[A]): Set[Set[A]]**
//    - Returns the subsets of the set xs.
//
//## Set Transformation
//- **filter(f: A => Bool, xs: Set[A]): Set[A]**
//    - Returns the subset of xs where the elements satisfy the predicate f.
//- **map(f: A => B, xs: Set[A]): Set[B]**
//    - Returns the result of applying the function f to every element in the set.
//    - Note: The returned set may be smaller than the given set.
//- **flatMap(f: A => Set[B], xs: Set[A]): Set[B]**
//    - Returns the result of applying the function f to every element in the set and taking the union of the result.
//
