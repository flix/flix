mod Test.Exp.IfThenElse {

    @test
    def testIfThenElse01(): Bool = if (true) true else false

    @test
    def testIfThenElse02(): Bool = if (false) false else true

    @test
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

    @test
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

    @test
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

    @test
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

    @test
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

    @test
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

    @test
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

    @test
    def testIfThenElseNested01(): Bool =
        if (true)
            if (true) true else false
        else
            false

    @test
    def testIfThenElseNested02(): Bool =
        if (false)
            false
        else
            (if (true) true else false)

    @test
    def testIfThenElseBooleanFolding01(): Bool = region rc {
        let y = Ref.fresh(rc, 1);
        let r =
            if ({ Ref.put(2, y); true } or true)
                deref y
            else
                3;
        r == 2
    }

    @test
    def testIfThenElseBooleanFolding02(): Bool = region rc {
        let y = Ref.fresh(rc, 1);
        let r =
            if ({ Ref.put(2, y); true } and false)
                3
            else
                deref y;
        r == 2
    }

}
