enum IntensionalSet[a] {
    case IntensionalSet(a -> Bool)
}

namespace IntensionalSet {

    ///
    /// # Set Construction
    ///

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
    /// # Set Predicates.
    ///

    ///
    /// Returns `true` if this set is empty.
    ///
    /// NB: This function may be uncomputable.
    ///
    def null[a](s: IntensionalSet[a]): Bool =
        let IntensionalSet(f) = s;
            ∀(x: a). !f(x)

    ///
    /// Returns `true` if `a` is a member of the set `s`.
    ///
    def memberOf[a](a: a, s: IntensionalSet[a]): Bool =
        let IntensionalSet(f) = s;
            f(a)

    ///
    /// Returns `true` if every element in `s1` appears in `s2`.
    ///
    /// NB: This function may not be computable.
    ///
    def isSubsetOf[a](s1: IntensionalSet[a], s2: IntensionalSet[a]): Bool =
        let IntensionalSet(f1) = s1;
        let IntensionalSet(f2) = s2;
            ∀(x: a). f1(x) → f2(x)

    // TODO: isProperSubsetOf

    ///
    /// # Elementary Set Operations.
    ///

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

    ///
    /// # Set Transformations.
    ///

    ///
    /// Returns the subset of elements that satisfy the predicate `p`.
    ///
    // TODO: Use other name for filter?
    def filter[a](p: a -> Bool, s: IntensionalSet[a]): IntensionalSet[a] =
        let IntensionalSet(f) = s;
            IntensionalSet(y -> p(y) && f(y))

    ///
    /// Returns a set obtained by applying the function `t` to every element of `s`.
    ///
    /// NB: This function may not be computable.
    ///
    def map[a, b](t: a -> b, s: IntensionalSet[a]): IntensionalSet[b] =
        let IntensionalSet(f) = s;
            IntensionalSet(y -> ∃(x: a). f(x) && t(x) == y)

    // TODO: flatMap

    ///
    /// Returns the result of applying the function `t` to each pair in the cross product of `s1` and `s2`.
    ///
    def crossProductWithMap[a, b](t: (a, a) -> b, s1: IntensionalSet[a], s2: IntensionalSet[a]): IntensionalSet[b] =
        let IntensionalSet(f1) = s1;
        let IntensionalSet(f2) = s2;
            IntensionalSet(z -> ∀(x: a, y: a). f1(x) && f2(y) && t(x, y) == z)

}


namespace IntensionalSet {

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

    @test
    def testFilter01: Bool = assert!(21 `memberOf` filter(x -> x % 2 != 0, singleton(21)))

    @test
    def testFilter02: Bool = assertNot!(21 `memberOf` filter(x -> x % 2 == 0, singleton(21)))

}
