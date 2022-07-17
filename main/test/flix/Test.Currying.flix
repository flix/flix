namespace Test/Currying {

    @test
    def testCurry05(): Bool = h(1, 2, 3, 4) == h(1)(2)(3)(4)

    ///
    /// A function of arity 4.
    ///
    def h(x: Int32, y: Int32, z: Int32, w: Int32): Int32 = x + y + z + w

}
