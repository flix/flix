mod Test.Eff.Advanced {

    // NB: Due to monomorphization some of these test cases are erased and never executed.
    // That is acceptable; we just want to ensure that they type check.

    @test
    def testAdvanced01(): Unit = requiresPure(pure)

    @test
    def testAdvanced02(): Unit = requiresUniv(univ)

    @test
    def testAdvanced03(): Unit = requiresPureAndUniv1(pure, univ)

    @test
    def testAdvanced04(): Unit = requiresPureAndUniv1(univ, pure)

    @test
    def testAdvanced05(): Unit = requiresPureAndUniv2(pure, univ)

    @test
    def testAdvanced06(): Unit = requiresPureAndUniv2(univ, pure)

    @test
    def testAdvanced07(): Unit = requiresAtmostOnePure1(univ, univ)

    @test
    def testAdvanced08(): Unit = requiresAtmostOnePure1(pure, univ)

    @test
    def testAdvanced09(): Unit = requiresAtmostOnePure1(univ, pure)

    @test
    def testAdvanced10(): Unit = requiresAtmostOnePure2(univ, univ)

    @test
    def testAdvanced11(): Unit = requiresAtmostOnePure2(pure, univ)

    @test
    def testAdvanced12(): Unit = requiresAtmostOnePure2(univ, pure)

    @test
    def testAdvanced13(): Unit = requiresTwoAndOne(pure, univ, pure)

    @test
    def testAdvanced14(): Unit = requiresTwoAndOne(univ, pure, univ)

    /// A pure function from Unit -> Unit.
    def pure(): Unit = ()

    /// An univ function from Unit -> Unit.
    def univ(): Unit \ Univ = checked_ecast(())

    /// A function that requires a pure function
    def requiresPure(_f: a -> b): Unit = ()

    /// A function that requires an univ function.
    def requiresUniv(_f: a -> b \ Univ): Unit = ()

    /// A function that requires a pure and an univ function.
    def requiresPureAndUniv1(_f: a -> b \ ef, _g: a -> b \ ~ef): Unit = ()

    /// A function that requires a pure and an univ function.
    def requiresPureAndUniv2(_f: a -> b \ ~ef, _g: a -> b \ ef): Unit = ()

    /// A function that requires at most one pure function.
    def requiresAtmostOnePure1(_f: a -> b \ ef1, _g: a -> b \ ~ef1 + ef2): Unit = ()

    /// A function that requires at most one pure function.
    def requiresAtmostOnePure2(_f: a -> b \ ~ef1 + ef2, _g: a -> b \ ef1): Unit = ()

    // First and last must be the same. Middle must be the inverse.
    def requiresTwoAndOne(_f: a -> b \ ef, _g: a -> b \ ~ef, _h: a -> b \ ef): Unit = ()

    // A unSAT instance.
    //pub def yikes(_f: a -> b \ {{(x \/ y \/ z) /\ (x \/ y \/ not z) /\ (x \/ not y \/ z) /\ (x \/ not y \/ not z) /\ (not x \/ y \/ z) /\ (not x \/ y \/ not z) /\ (not x \/ not y \/ z) /\ (not x \/ not y \/ not z)}}): Unit = ()

    //@test
    //def testYikes(): Unit = yikes(pure)

}
