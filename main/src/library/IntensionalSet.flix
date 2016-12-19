enum IntensionalSet[a] {
    case IntensionalSet(a -> Bool)
}

namespace IntensionalSet {

    ///
    /// Returns the empty set.
    ///
    def empty[a]: IntensionalSet[a] = IntensionalSet(x -> false)

    ///
    /// Returns the singleton set of `a`.
    ///
    def singleton[a](a: a): IntensionalSet[a] = IntensionalSet(x -> x == a)

    ///
    /// Returns `true` if
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





}

