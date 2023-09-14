mod Test.Eff.Advanced {

    // NB: Due to monomorphization some of these test cases are erased and never executed.
    // That is acceptable; we just want to ensure that they type check.

    @Test
    def testAdvanced01(): Unit = requiresPure(pure)

    @Test
    def testAdvanced02(): Unit = requiresImpure(impure)

    @Test
    def testAdvanced03(): Unit = requiresPureAndImpure1(pure, impure)

    @Test
    def testAdvanced04(): Unit = requiresPureAndImpure1(impure, pure)

    @Test
    def testAdvanced05(): Unit = requiresPureAndImpure2(pure, impure)

    @Test
    def testAdvanced06(): Unit = requiresPureAndImpure2(impure, pure)

    @Test
    def testAdvanced07(): Unit = requiresAtmostOnePure1(impure, impure)

    @Test
    def testAdvanced08(): Unit = requiresAtmostOnePure1(pure, impure)

    @Test
    def testAdvanced09(): Unit = requiresAtmostOnePure1(impure, pure)

    @Test
    def testAdvanced10(): Unit = requiresAtmostOnePure2(impure, impure)

    @Test
    def testAdvanced11(): Unit = requiresAtmostOnePure2(pure, impure)

    @Test
    def testAdvanced12(): Unit = requiresAtmostOnePure2(impure, pure)

    @Test
    def testAdvanced13(): Unit = requiresTwoAndOne(pure, impure, pure)

    @Test
    def testAdvanced14(): Unit = requiresTwoAndOne(impure, pure, impure)

    /// A pure function from Unit -> Unit.
    def pure(): Unit = ()

    /// An impure function from Unit -> Unit.
    def impure(): Unit \ Impure = discard Array#{1, 2, 3} @ Static; ()

    /// A function that requires a pure function
    def requiresPure(_f: a -> b): Unit = ()

    /// A function that requires an impure function.
    def requiresImpure(_f: a -> b \ Impure): Unit = ()

    /// A function that requires a pure and an impure function.
    def requiresPureAndImpure1(_f: a -> b \ ef, _g: a -> b \ ~ef): Unit = ()

    /// A function that requires a pure and an impure function.
    def requiresPureAndImpure2(_f: a -> b \ ~ef, _g: a -> b \ ef): Unit = ()

    /// A function that requires at most one pure function.
    def requiresAtmostOnePure1(_f: a -> b \ ef1, _g: a -> b \ ~ef1 + ef2): Unit = ()

    /// A function that requires at most one pure function.
    def requiresAtmostOnePure2(_f: a -> b \ ~ef1 + ef2, _g: a -> b \ ef1): Unit = ()

    // First and last must be the same. Middle must be the inverse.
    def requiresTwoAndOne(_f: a -> b \ ef, _g: a -> b \ ~ef, _h: a -> b \ ef): Unit = ()

    // A unSAT instance.
    //pub def yikes(_f: a -> b \ {{(x \/ y \/ z) /\ (x \/ y \/ not z) /\ (x \/ not y \/ z) /\ (x \/ not y \/ not z) /\ (not x \/ y \/ z) /\ (not x \/ y \/ not z) /\ (not x \/ not y \/ z) /\ (not x \/ not y \/ not z)}}): Unit = ()

    //@Test
    //def testYikes(): Unit = yikes(pure)

}
