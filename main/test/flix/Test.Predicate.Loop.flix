mod Test.Predicate.Loop {

    @Test
    def testLoopF2X2_01(): Bool =
        let ff = (_x, _y) -> Vector#{};
        let pr = #{
            A(1, 1).
            R(a, b) :- A(x, y), let (a, b) = ff(x, y).
        };
        let actual = query pr select (x, y) from R(x, y);
        Assert.eq(List#{}: List[(Int32, Int32)], actual)

    @Test
    def testLoopF2X2_02(): Bool =
        let ff = (x, y) -> Vector#{(x, y)};
        let pr = #{
            A(1, 1).
            R(a, b) :- A(x, y), let (a, b) = ff(x, y).
        };
        let actual = query pr select (x, y) from R(x, y);
        Assert.eq(List#{(1, 1)}, actual)

    @Test
    def testLoopF2X2_03(): Bool =
    let ff = (x, y) -> Vector#{(x, y), (x + 1, y + 1)};
        let pr = #{
            A(1, 1).
            R(a, b) :- A(x, y), let (a, b) = ff(x, y).
        };
        let actual = query pr select (x, y) from R(x, y);
        Assert.eq(List#{(1, 1), (2, 2)}, actual)

}
