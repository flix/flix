mod Test.Eff.Poly.MixedParameters {

    use Assert.assertEq

    eff Exchange[a, b] {
        def exchange(offset: Int32, x: a, y: b): a
    }

    eff Checked[a, b] {
        def check(left: a, right: a, label: b): b with Eq[a], ToString[b]
    }

    def performExchange(offset: Int32, x: a, y: b): a \ Exchange[a, b] =
        Exchange.exchange(offset, x, y)

    def performCheck(left: a, right: a, label: b): b \ Checked[a, b] with Eq[a], ToString[b] =
        Checked.check(left, right, label)

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

    @Test
    def testEqAndToStringBoundsInt32String(): Unit \ Assert =
        let result = run performCheck(42, 42, "equal") with handler Checked {
            def check(left, right, label, k) = {
                let resumed = k(label);
                if (left == right) "${label}:${resumed}" else "different"
            }
        };
        assertEq(expected = "equal:equal", result)

    @Test
    def testEqAndToStringBoundsStringInt32(): Unit \ Assert =
        let result = run performCheck("same", "same", 42) with handler Checked {
            def check(left, right, label, k) = {
                let rendered = "${label}";
                let resumed = k(label);
                if (left == right and rendered == "42") resumed else -1
            }
        };
        assertEq(expected = 42, result)

}
