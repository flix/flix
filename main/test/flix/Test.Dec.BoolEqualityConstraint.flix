mod Test.Dec.BoolEqualityConstraint {

    use Assert.assertTrue

    ///
    /// A phantom type indexed by a type-level Boolean (kind `Bool`).
    ///
    enum B[_: Bool] {
        case B
    }

    def mkTrue(): B[true] = B.B

    def mkFalse(): B[false] = B.B

    def isTrue(_: B[true]): Bool = true

    ///
    /// Assume side (Bool): under the assumption `b ~ true`, a value of type `B[b]` may be used
    /// where a `B[true]` is expected.
    ///
    def useAsTrue(x: B[b]): Bool where b ~ true = isTrue(x)

    ///
    /// Assume side (Bool), conjunctive: `a and b ~ true` forces both `a` and `b` to be `true`.
    ///
    def bothTrue(x: B[a], y: B[b]): Bool where a and b ~ true = isTrue(x) and isTrue(y)

    ///
    /// Assume side (Bool), negation: under `a ~ not b`, a `B[a]` is a `B[not b]`.
    ///
    def asNotB(x: B[a], _y: B[b]): B[not b] where a ~ not b = x

    ///
    /// Assume side (Eff): the body `f(x)` has effect `ef`, but the declared effect is `ef2`.
    /// The `where ef ~ ef2` lets the body typecheck under the assumption that they are equal.
    ///
    def applyAssume(f: a -> b \ ef, x: a): b \ ef2 where ef ~ ef2 = f(x)

    ///
    /// Demand side (Eff): the caller must satisfy `ef ~ {}`, i.e. the passed function must be pure.
    ///
    def onlyPure(f: a -> b \ ef, x: a): b \ ef where ef ~ {} = f(x)

    @Test
    def testBoolAssume01(): Unit \ Assert = assertTrue(useAsTrue(mkTrue()))

    @Test
    def testBoolConjunction01(): Unit \ Assert = assertTrue(bothTrue(mkTrue(), mkTrue()))

    @Test
    def testBoolNegation01(): Unit \ Assert = assertTrue(isTrue(asNotB(mkTrue(), mkFalse())))

    @Test
    def testEffAssume01(): Unit \ Assert = assertTrue(applyAssume(x -> x, true))

    @Test
    def testEffDemandPure01(): Unit \ Assert = assertTrue(onlyPure(x -> x, true))

}
