mod Test.Eff.Poly.MixedParameters {

    use Assert.assertEq

    eff Exchange[a, b] {
        def exchange(offset: Int32, x: a, y: b): a
    }

    def performExchange(offset: Int32, x: a, y: b): a \ Exchange[a, b] =
        Exchange.exchange(offset, x, y)

    @Test
    def testPrimitiveAndReferenceParameters(): Unit \ Assert =
        let result = run performExchange(10, 32, "accepted") with handler Exchange {
            def exchange(offset, x, y, k) = {
                let resumed = k(offset + x);
                if (y == "accepted") resumed else -1
            }
        };
        assertEq(expected = 42, result)

    @Test
    def testReferenceAndPrimitiveParameters(): Unit \ Assert =
        let result = run performExchange(40, "answer", 2) with handler Exchange {
            def exchange(offset, x, y, k) = {
                let resumed = k("${x}-${offset + y}");
                if (x == "answer") resumed else "unexpected"
            }
        };
        assertEq(expected = "answer-42", result)

}
