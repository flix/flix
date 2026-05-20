mod Test.Exp.IfThenElse {

    use Assert.{assertEq, assertTrue}

    @Test
    def testIfThenElse01(): Unit \ Assert = assertTrue(if (true) true else false)

    @Test
    def testIfThenElse02(): Unit \ Assert = assertTrue(if (false) false else true)

    @Test
    def testIfThenElseThreeWay01(): Unit \ Assert =
        let x = 0;
        let r =
            if (x == 0)
                0
            else if (x == 1)
                1
            else
                2;
        assertEq(expected = 0, r)

    @Test
    def testIfThenElseThreeWay02(): Unit \ Assert =
        let x = 1;
        let r =
            if (x == 0)
                0
            else if (x == 1)
                1
            else
                2;
        assertEq(expected = 1, r)

    @Test
    def testIfThenElseThreeWay03(): Unit \ Assert =
        let x = 2;
        let r =
            if (x == 0)
                0
            else if (x == 1)
                1
            else
                2;
        assertEq(expected = 2, r)

    @Test
    def testIfThenElseFourWay01(): Unit \ Assert =
        let x = 0;
        let r =
            if (x == 0)
                0
            else if (x == 1)
                1
            else if (x == 2)
                2
            else
                3;
        assertEq(expected = 0, r)

    @Test
    def testIfThenElseFourWay02(): Unit \ Assert =
        let x = 1;
        let r =
            if (x == 0)
                0
            else if (x == 1)
                1
            else if (x == 2)
                2
            else
                3;
        assertEq(expected = 1, r)

    @Test
    def testIfThenElseFourWay03(): Unit \ Assert =
        let x = 2;
        let r =
            if (x == 0)
                0
            else if (x == 1)
                1
            else if (x == 2)
                2
            else
                3;
        assertEq(expected = 2, r)

    @Test
    def testIfThenElseFourWay04(): Unit \ Assert =
        let x = 3;
        let r =
            if (x == 0)
                0
            else if (x == 1)
                1
            else if (x == 2)
                2
            else
                3;
        assertEq(expected = 3, r)

    @Test
    def testIfThenElseNested01(): Unit \ Assert =
        assertTrue(
            if (true)
                if (true) true else false
            else
                false
        )

    @Test
    def testIfThenElseNested02(): Unit \ Assert =
        assertTrue(
            if (false)
                false
            else
                (if (true) true else false)
        )

    @Test
    def testIfThenElseBooleanFolding01(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 1);
        let r =
            if ({ Ref.put(2, y); true } or true)
                Ref.get(y)
            else
                3;
        assertEq(expected = 2, r)
    }

    @Test
    def testIfThenElseBooleanFolding02(): Unit \ Assert = region rc {
        let y = Ref.fresh(rc, 1);
        let r =
            if ({ Ref.put(2, y); true } and false)
                3
            else
                Ref.get(y);
        assertEq(expected = 2, r)
    }

}
