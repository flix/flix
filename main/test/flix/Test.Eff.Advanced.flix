namespace Test/Eff/Advanced {

    // NB: Due to monomorphization some of these test cases are erased and never executed.
    // That is acceptable; we just want to ensure that they type check.

    @test
    def testAdvanced01(): Unit = requiresPure(pure)

    @test
    def testAdvanced02(): Unit = requiresImpure(impure)

    @test
    def testAdvanced03(): Unit = requiresPureAndImpure1(pure, impure)

    @test
    def testAdvanced04(): Unit = requiresPureAndImpure1(impure, pure)

//
//    pub def foo(_f: a -> b & e1, _g: b -> b & {{not e1}}): Unit = ()
//
//    pub def bar(): Unit =
//        let f = x -> x;
//        let g = x -> x;
//        foo(f, g)


    /// A pure function from Unit -> Unit.
    def pure(): Unit = ()

    /// An impure function from Unit -> Unit.
    def impure(): Unit & Impure = [1, 2, 3]; ()


    /// A function that requires a pure function
    def requiresPure(_f: a -> b): Unit = ()

    /// A function that requires an impure function.
    def requiresImpure(_f: a -> b & Impure): Unit = ()

    /// A function that requires a pure and an impure function.
    def requiresPureAndImpure1(_f: a -> b & e, _g: a -> b & {{not e}}): Unit = ()

}
