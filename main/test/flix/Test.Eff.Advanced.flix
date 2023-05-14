  // TODO EFF-MIGRATION temporarily disabled
//mod Test.Eff.Advanced {
//
//    // NB: Due to monomorphization some of these test cases are erased and never executed.
//    // That is acceptable; we just want to ensure that they type check.
//
//    @test
//    def testAdvanced01(): Unit = requiresPure(pure)
//
//    @test
//    def testAdvanced02(): Unit = requiresImpure(impure)
//
//    @test
//    def testAdvanced03(): Unit = requiresPureAndImpure1(pure, impure)
//
//    @test
//    def testAdvanced04(): Unit = requiresPureAndImpure1(impure, pure)
//
//    @test
//    def testAdvanced05(): Unit = requiresPureAndImpure2(pure, impure)
//
//    @test
//    def testAdvanced06(): Unit = requiresPureAndImpure2(impure, pure)
//
//    @test
//    def testAdvanced07(): Unit = requiresAtmostOnePure1(impure, impure)
//
//    @test
//    def testAdvanced08(): Unit = requiresAtmostOnePure1(pure, impure)
//
//    @test
//    def testAdvanced09(): Unit = requiresAtmostOnePure1(impure, pure)
//
//    @test
//    def testAdvanced10(): Unit = requiresAtmostOnePure2(impure, impure)
//
//    @test
//    def testAdvanced11(): Unit = requiresAtmostOnePure2(pure, impure)
//
//    @test
//    def testAdvanced12(): Unit = requiresAtmostOnePure2(impure, pure)
//
//    @test
//    def testAdvanced13(): Unit = requiresTwoAndOne(pure, impure, pure)
//
//    @test
//    def testAdvanced14(): Unit = requiresTwoAndOne(impure, pure, impure)
//
//    /// A pure function from Unit -> Unit.
//    def pure(): Unit = ()
//
//    /// An impure function from Unit -> Unit.
//    def impure(): Unit \ IO = discard Array#{1, 2, 3} @ Static; ()
//
//    /// A function that requires a pure function
//    def requiresPure(_f: a -> b): Unit = ()
//
//    /// A function that requires an impure function.
//    def requiresImpure(_f: a -> b \ IO): Unit = ()
//
//    /// A function that requires a pure and an impure function.
//    def requiresPureAndImpure1(_f: a -> b \ ef, _g: a -> b \ {(not e)}): Unit = ()
//
//    /// A function that requires a pure and an impure function.
//    def requiresPureAndImpure2(_f: a -> b \ (not e), _g: a -> b \ ef): Unit = ()
//
//    /// A function that requires at most one pure function.
//    def requiresAtmostOnePure1(_f: a -> b \ ef1, _g: a -> b \ {(not ef1 and ef2)}): Unit = ()
//
//    /// A function that requires at most one pure function.
//    def requiresAtmostOnePure2(_f: a -> b \ {(not ef1 and ef2)}, _g: a -> b \ ef1): Unit = ()
//
//    // First and last must be the same. Middle must be the inverse.
//    def requiresTwoAndOne(_f: a -> b \ ef, _g: a -> b \ {(not e)}, _h: a -> b \ ef): Unit = ()
//
//    // A unSAT instance.
//    //pub def yikes(_f: a -> b \ {{(x \/ y \/ z) /\ (x \/ y \/ not z) /\ (x \/ not y \/ z) /\ (x \/ not y \/ not z) /\ (not x \/ y \/ z) /\ (not x \/ y \/ not z) /\ (not x \/ not y \/ z) /\ (not x \/ not y \/ not z)}}): Unit = ()
//
//    //@test
//    //def testYikes(): Unit = yikes(pure)
//
//}
