enum IntensionalSet[a] {
    case IntensionalSet(a -> Bool)
}

namespace IntensionalSet {

    ///
    /// Returns an intensional set of the given predicate `f`.
    ///
    def of[a](f: a -> Bool): IntensionalSet[a] = IntensionalSet(f)

    ///
    /// Returns the empty set.
    ///
    def empty[a]: IntensionalSet[a] = IntensionalSet(x -> false)

    ///
    /// Returns the full set.
    ///
    def full[a]: IntensionalSet[a] = IntensionalSet(x -> true)

    ///
    /// Returns the singleton set of `x`.
    ///
    def singleton[a](x: a): IntensionalSet[a] = IntensionalSet(y -> y == x)

    ///
    /// Returns `true` if this set is empty.
    ///
    /// NB: This function may be uncomputable.
    ///
    def null[a](s: IntensionalSet[a]): Bool =
        let IntensionalSet(f) = s;
            ∀(x: a). !f(x)

    ///
    /// Returns `true` if every element in `s1` appears in `s2`.
    ///
    /// NB: This function may be uncomputable.
    ///
    def isSubsetOf[a](s1: IntensionalSet[a], s2: IntensionalSet[a]): Bool =
        let IntensionalSet(f1) = s1;
        let IntensionalSet(f2) = s2;
            ∀(x: a). f1(x) → f2(x)

    //- **isProperSubsetOf(xs: Set[A], ys: Set[A]): Bool**
    //    - Returns true if every element in xs appears in ys and xs != ys.

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

    ///
    /// Returns the union of `s1` and `s2`.
    ///
    def union[a](s1: IntensionalSet[a], s2: IntensionalSet[a]): IntensionalSet[a] =
        let IntensionalSet(f1) = s1;
        let IntensionalSet(f2) = s2;
            IntensionalSet(y -> f1(y) || f2(y))

    ///
    /// Returns the intersection of `s1` and `s2`.
    ///
    def intersection[a](s1: IntensionalSet[a], s2: IntensionalSet[a]): IntensionalSet[a] =
        let IntensionalSet(f1) = s1;
        let IntensionalSet(f2) = s2;
            IntensionalSet(y -> f1(y) && f2(y))

    ///
    /// Returns the difference of `s1` and `s2`.
    ///
    def difference[a](s1: IntensionalSet[a], s2: IntensionalSet[a]): IntensionalSet[a] =
        let IntensionalSet(f1) = s1;
        let IntensionalSet(f2) = s2;
            IntensionalSet(y -> f1(y) && !f2(y))



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

}


namespace IntensionalSet {

    @test
    def testOf01: Bool = assert!(42 `memberOf` of(x -> x == 42))

    @test
    def testOf02: Bool = assert!(42 `memberOf` of(x -> x % 2 == 0))

    @test
    def testOf03: Bool = assertNot!(21 `memberOf` of(x -> x % 2 == 0))

    @test
    def testEmpty01: Bool = assertNot!(42 `memberOf` empty())

    @test
    def testFull01: Bool = assert!(42 `memberOf` full())

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

    @test
    def testUnion01: Bool = assert!(21 `memberOf` union(singleton(21), singleton(42)))

    @test
    def testUnion02: Bool = assert!(42 `memberOf` union(singleton(21), singleton(42)))

    @test
    def testIntersection01: Bool = assert!(21 `memberOf` intersection(singleton(21), singleton(21)))

    @test
    def testIntersection02: Bool = assertNot!(21 `memberOf` intersection(singleton(21), singleton(42)))

    @test
    def testDifference01: Bool = assert!(21 `memberOf` difference(singleton(21), singleton(42)))

    @test
    def testDifference02: Bool = assertNot!(21 `memberOf` difference(singleton(21), singleton(21)))

}
