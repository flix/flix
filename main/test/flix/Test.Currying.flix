mod Test.Currying {

    @Test
    def testCurry01(): Bool = f(1, 2) == f(1)(2)

    @Test
    def testCurry02(): Bool = g(1, 2, 3) == g(1)(2)(3)

    @Test
    def testCurry03(): Bool = g(1, 2, 3) == g(1, 2)(3)

    @Test
    def testCurry04(): Bool = g(1, 2, 3) == g(1)(2, 3)

    @Test
    def testCurry05(): Bool = h(1, 2, 3, 4) == h(1)(2)(3)(4)

    @Test
    def testCurry06(): Bool = h(1, 2, 3, 4) == h(1, 2)(3)(4)

    @Test
    def testCurry07(): Bool = h(1, 2, 3, 4) == h(1)(2, 3)(4)

    @Test
    def testCurry08(): Bool = h(1, 2, 3, 4) == h(1)(2)(3, 4)

    @Test
    def testCurry09(): Bool = h(1, 2, 3, 4) == h(1, 2)(3, 4)

    @Test
    def testCurry10(): Bool = h(1, 2, 3, 4) == h(1, 2, 3)(4)

    @Test
    def testCurry11(): Bool = h(1, 2, 3, 4) == h(1)(2, 3, 4)

    @Test
    def testCurry12(): Bool = i(1, 2) == i(1)(2)

    @Test
    def testCurry13(): Bool = j(1, 2, 3) == j(1)(2)(3)

    @Test
    def testCurry14(): Bool = j(1, 2, 3) == j(1, 2)(3)

    @Test
    def testCurry15(): Bool = j(1, 2, 3) == j(1)(2, 3)

    @Test
    def testCurry16(): Bool = k(x -> x, 42) == k(x -> x)(42)

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
