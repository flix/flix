mod Test.Exp.IfThenElse {

    @Test
    def testIfThenElse01(): Bool = if (true) true else false

    @Test
    def testIfThenElse02(): Bool = if (false) false else true

    @Test
    def testIfThenElseThreeWay01(): Bool =
        let x = 0;
        let r =
            if (x == 0)
                0
            else if (x == 1)
                1
            else
                2;
        r == 0

    @Test
    def testIfThenElseThreeWay02(): Bool =
        let x = 1;
        let r =
            if (x == 0)
                0
            else if (x == 1)
                1
            else
                2;
        r == 1

    @Test
    def testIfThenElseThreeWay03(): Bool =
        let x = 2;
        let r =
            if (x == 0)
                0
            else if (x == 1)
                1
            else
                2;
        r == 2

    @Test
    def testIfThenElseFourWay01(): Bool =
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
        r == 0

    @Test
    def testIfThenElseFourWay02(): Bool =
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
        r == 1

    @Test
    def testIfThenElseFourWay03(): Bool =
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
        r == 2

    @Test
    def testIfThenElseFourWay04(): Bool =
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
        r == 3

    @Test
    def testIfThenElseNested01(): Bool =
        if (true)
            if (true) true else false
        else
            false

    @Test
    def testIfThenElseNested02(): Bool =
        if (false)
            false
        else
            (if (true) true else false)

    @Test
    def testIfThenElseBooleanFolding01(): Bool = region rc {
        let y = ref 1 @ rc;
        let r =
            if ({y := 2; true} or true)
                deref y
            else
                3;
        r == 2
    }

    @Test
    def testIfThenElseBooleanFolding02(): Bool = region rc {
        let y = ref 1 @ rc;
        let r =
            if ({y := 2; true} and false)
                3
            else
                deref y;
        r == 2
    }

}
