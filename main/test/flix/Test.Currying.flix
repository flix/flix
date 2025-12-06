mod Test.Currying {

    use Assert.assertEq

    @Test
    def curry01(): Unit \ Assert =
        assertEq(expected = f(1, 2), f(1)(2))

    @Test
    def curry02(): Unit \ Assert =
        assertEq(expected = g(1, 2, 3), g(1)(2)(3))

    @Test
    def curry03(): Unit \ Assert =
        assertEq(expected = g(1, 2, 3), g(1, 2)(3))

    @Test
    def curry04(): Unit \ Assert =
        assertEq(expected = g(1, 2, 3), g(1)(2, 3))

    @Test
    def curry05(): Unit \ Assert =
        assertEq(expected = h(1, 2, 3, 4), h(1)(2)(3)(4))

    @Test
    def curry06(): Unit \ Assert =
        assertEq(expected = h(1, 2, 3, 4), h(1, 2)(3)(4))

    @Test
    def curry07(): Unit \ Assert =
        assertEq(expected = h(1, 2, 3, 4), h(1)(2, 3)(4))

    @Test
    def curry08(): Unit \ Assert =
        assertEq(expected = h(1, 2, 3, 4), h(1)(2)(3, 4))

    @Test
    def curry09(): Unit \ Assert =
        assertEq(expected = h(1, 2, 3, 4), h(1, 2)(3, 4))

    @Test
    def curry10(): Unit \ Assert =
        assertEq(expected = h(1, 2, 3, 4), h(1, 2, 3)(4))

    @Test
    def curry11(): Unit \ Assert =
        assertEq(expected = h(1, 2, 3, 4), h(1)(2, 3, 4))

    @Test
    def curry12(): Unit \ Assert =
        assertEq(expected = i(1, 2), i(1)(2))

    @Test
    def curry13(): Unit \ Assert =
        assertEq(expected = j(1, 2, 3), j(1)(2)(3))

    @Test
    def curry14(): Unit \ Assert =
        assertEq(expected = j(1, 2, 3), j(1, 2)(3))

    @Test
    def curry15(): Unit \ Assert =
        assertEq(expected = j(1, 2, 3), j(1)(2, 3))

    @Test
    def curry16(): Unit \ Assert =
        assertEq(expected = k(x -> x, 42), k(x -> x)(42))

    @Test
    def partial01(): Unit \ Assert = region rc {
        let count = Ref.fresh(rc, 0);
        let inc = f({Ref.put(Ref.get(count) + 1, count); Ref.get(count)});
        assertEq(expected = 5, inc(1) + inc(2))
    }

    ///
    /// A function of arity 2.
    ///
    def f(x: Int32, y: Int32): Int32 = x + y

    ///
    /// A function of arity 3.
    ///
    def g(x: Int32, y: Int32, z: Int32): Int32 = x + y + z

    ///
    /// A function of arity 4.
    ///
    def h(x: Int32, y: Int32, z: Int32, w: Int32): Int32 = x + y + z + w

    ///
    /// A curried function of arity 2.
    ///
    def i(x: Int32): Int32 -> Int32 = y -> x + y

    ///
    /// A curried function of arity 3.
    ///
    def j(x: Int32): Int32 -> Int32 -> Int32 = y -> z -> x + y + z

    ///
    /// A curried higher-order function.
    ///
    def k(f: Int32 -> Int32): Int32 -> Int32 = x -> f(x)

}
