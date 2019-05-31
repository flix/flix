namespace Test/Predicate/Atom {
    rel A(x: Int, y: Int, z: Int)

    @test
    def testAtomWithParam01(): Bool =
        A(1, 2, 3). |= A(1, 2, 3).

    @test
    def testAtomWithParam02(): Bool =
        A<()>(1, 2, 3). |= A(1, 2, 3).

    @test
    def testAtomWithParam03(): Bool =
        A(1, 2, 3). |= A<()>(1, 2, 3).

    @test
    def testAtomWithParam04(): Bool =
        A<()>(1, 2, 3). |= A<()>(1, 2, 3).

    @test
    def testAtomWithParam05(): Bool =
        A<000>(1, 2, 3). |= A<000>(1, 2, 3).

    @test
    def testAtomWithParam06(): Bool =
        !(A<000>(1, 2, 3). |= A<111>(1, 2, 3).)

    @test
    def testAtomWithParam07(): Bool =
        !(A<111>(1, 2, 3). |= A<000>(1, 2, 3).)

    @test
    def testAtomWithParam08(): Bool =
        A<"abc">(1, 2, 3). |= A<"abc">(1, 2, 3).

    @test
    def testAtomWithParam09(): Bool =
        !(A<"abc">(1, 2, 3). |= A<"def">(1, 2, 3).)

    @test
    def testAtomWithParam10(): Bool =
        (A<0>(1, 2, 3). A<1>(1, 2, 3).) |= A<0>(1, 2, 3).

    @test
    def testAtomWithParam11(): Bool =
        (A<0>(1, 2, 3). A<1>(1, 2, 3).) |= A<1>(1, 2, 3).

    @test
    def testAtomWithParam12(): Bool =
        let x = 123;
        let y = 456;
        !((A<x>(1, 2, 3).) |= (A<y>(1, 2, 3).))

    @test
    def testAtomWithParam13(): Bool =
        let x = 123;
        let y = 456;
        (A<x>(1, 2, 3). A<y>(1, 2, 3).) |= (A<x>(1, 2, 3). A<y>(1, 2, 3).)

}
