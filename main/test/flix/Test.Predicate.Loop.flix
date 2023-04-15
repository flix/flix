mod Test.Predicate.Loop {

    @Test
    def testLoopF1X2_01(): Bool =
    let ff = _x -> Vector#{};
        let pr = #{
            A(1).
            R(a, b) :- A(x), let (a, b) = ff(x).
        };
        let actual = query pr select (x, y) from R(x, y);
        Assert.eq(List#{}: List[(Int32, Int32)], actual)

    @Test
    def testLoopF1X2_02(): Bool =
    let ff = x -> Vector#{(x, x)};
        let pr = #{
            A(1).
            R(a, b) :- A(x), let (a, b) = ff(x).
        };
        let actual = query pr select (x, y) from R(x, y);
        Assert.eq(List#{(1, 1)}, actual)

    @Test
    def testLoopF1X2_03(): Bool =
    let ff = x -> Vector#{(x, x), (x + 1, x + 1)};
        let pr = #{
            A(1).
            R(a, b) :- A(x), let (a, b) = ff(x).
        };
        let actual = query pr select (x, y) from R(x, y);
        Assert.eq(List#{(1, 1), (2, 2)}, actual)

    @Test
    def testLoopF2X1_01(): Bool =
    let ff = (_x, _y) -> Vector#{};
        let pr = #{
            A(1, 2).
            R(a) :- A(x, y), let a = ff(x, y).
        };
        let actual = query pr select x from R(x);
        Assert.eq(List#{}: List[Int32], actual)

    @Test
    def testLoopF2X1_02(): Bool =
    let ff = (x, _y) -> Vector#{x};
        let pr = #{
            A(1, 2).
            R(a) :- A(x, y), let a = ff(x, y).
        };
        let actual = query pr select x from R(x);
        Assert.eq(List#{1}, actual)

    @Test
    def testLoopF2X1_03(): Bool =
    let ff = (x, y) -> Vector#{x, y};
        let pr = #{
            A(1, 2).
            R(a) :- A(x, y), let a = ff(x, y).
        };
        let actual = query pr select x from R(x);
        Assert.eq(List#{1, 2}, actual)

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

    @Test
    def testLoopF5X5_01(): Bool =
    let ff = (_x, _y, _z, _u, _v) -> Vector#{};
        let pr = #{
            A(1, 1, 1, 1, 1).
            R(a, b, c, d, e) :- A(x, y, z, u, v), let (a, b, c, d, e) = ff(x, y, z, u, v).
        };
        let actual = query pr select (x, y, z, u, v) from R(x, y, z, u, v);
        Assert.eq(List#{}: List[(Int32, Int32, Int32, Int32, Int32)], actual)

    @Test
    def testLoopF5X5_02(): Bool =
    let ff = (x, y, z, u, v) -> Vector#{(x, y, z, u, v)};
        let pr = #{
            A(1, 1, 1, 1, 1).
            R(a, b, c, d, e) :- A(x, y, z, u, v), let (a, b, c, d, e) = ff(x, y, z, u, v).
        };
        let actual = query pr select (x, y, z, u, v) from R(x, y, z, u, v);
        Assert.eq(List#{(1, 1, 1, 1, 1)}, actual)

    @Test
    def testLoopF5X5_03(): Bool =
    let ff = (x, y, z, u, v) -> Vector#{(x, y, z, u, v), (x + 1, y + 1, z + 1, u + 1, v + 1)};
        let pr = #{
            A(1, 1, 1, 1, 1).
            R(a, b, c, d, e) :- A(x, y, z, u, v), let (a, b, c, d, e) = ff(x, y, z, u, v).
        };
        let actual = query pr select (x, y, z, u, v) from R(x, y, z, u, v);
        Assert.eq(List#{(1, 1, 1, 1, 1), (2, 2, 2, 2, 2)}, actual)


    @Test
    def testLoopDelta01(): Bool =
    let ff = (x, y) -> Vector#{(x, y)};
        let pr = #{
            A(1, 1). A(2, 2).

            B(x, y) :- A(x, y).
            C(x, y) :- A(x, y).
            R(a, b) :- B(x, y), C(x, y), let (a, b) = ff(x, y).
        };
        let actual = query pr select (x, y) from R(x, y);
        Assert.eq(List#{(1, 1), (2, 2)}, actual)

    @Test
    def testLoopDelta02(): Bool =
    let ff = (x, y) -> Vector#{(x, y)};
        let pr = #{
            A(1, 1). A(2, 2).

            B(x, y) :- A(x, y).
            C(x, y) :- A(x, y).
            R(a, b) :- B(x, y), let (a, b) = ff(x, y), C(x, y).
        };
        let actual = query pr select (x, y) from R(x, y);
        Assert.eq(List#{(1, 1), (2, 2)}, actual)

    @Test
    def testLoopDelta03(): Bool =
    let ff = (x, y) -> Vector#{(x, y)};
        let pr = #{
            A(1, 1). A(2, 2).

            B(x, y) :- A(x, y).
            C(x, y) :- A(x, y).
            R(a, b) :- let (a, b) = ff(x, y), C(x, y), B(x, y).
        };
        let actual = query pr select (x, y) from R(x, y);
        Assert.eq(List#{(1, 1), (2, 2)}, actual)

}
