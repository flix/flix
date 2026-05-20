mod Test.Exp.Block {

    use Assert.assertEq

    @Test
    def testBlock01(): Unit \ Assert = assertEq(expected = 42, {42})

    @Test
    def testBlock02(): Unit \ Assert = assertEq(expected = 42, {{42}})

    @Test
    def testBlock03(): Unit \ Assert = assertEq(expected = 42, {{{42}}})

    @Test
    def testBlock04(): Unit \ Assert = {
        let x = {
            let y = 42;
                y
        };
        assertEq(expected = 43, x + 1)
    }

    @Test
    def testBlock05(): Unit \ Assert = {
        let x = {
            let y = 42;
            let z = 21;
                y + z
        };
        assertEq(expected = 64, x + 1)
    }

    @Test
    def testBlock06(): Unit \ Assert = {
        let x = {
            let y = 42;
            let z = 21;
                y + z
        };
        let w = {
            let y = 84;
            let z = 11;
            y + z
        };
        assertEq(expected = 158, x + w)
    }

    @Test
    def testBlock07(): Unit \ Assert = {
        let x = {42};
        let y = {21};
        assertEq(expected = 63, x + y)
    }

    @Test
    def testBlock08(): Unit \ Assert = {
        let x = {42};
        let y = {
            let z = 21;
            let w = {11};
                z + w
        };
        assertEq(expected = 74, x + y)
    }

    @Test
    def testBlock09(): Unit \ Assert = {
        let x = {42};
        let y = {
            let z = 21;
            let w = {
                let q = 11;
                let u = 5;
                    q + u
            };
                z + w
        };
        assertEq(expected = 79, x + y)
    }

}
