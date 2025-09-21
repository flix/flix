mod TestPrelude {

    use Assert.{assertEq, assertTrue};

    /////////////////////////////////////////////////////////////////////////////
    // identity                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def identity01(): Unit \ Assert =
        assertEq(expected = 5, identity(5))

    @Test
    def identity02(): Unit \ Assert =
        assertEq(expected = true, identity(true))

    @Test
    def identity03(): Unit \ Assert =
        assertEq(expected = 6ii, identity(6ii))


    /////////////////////////////////////////////////////////////////////////////
    // flip                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def flip01(): Unit \ Assert =
        let f = flip((x, y) -> x - y);
        assertEq(expected = -1, f(5, 4))

    @Test
    def flip02(): Unit \ Assert =
        let f = flip((x, y) -> x - y);
        assertEq(expected = 1, f(4, 5))

    @Test
    def flip03(): Unit \ Assert =
        let f = flip((x, y) -> x / y);
        assertEq(expected = 0, f(10, 3))

    @Test
    def flip04(): Unit \ Assert =
        let f = flip((x, y) -> x / y);
        assertEq(expected = 3, f(3, 10))


    /////////////////////////////////////////////////////////////////////////////
    // fst                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def fst01(): Unit \ Assert =
        assertEq(expected = 5, fst((5, 0.0f32)))

    @Test
    def fst02(): Unit \ Assert =
        assertEq(expected = 0.0f32, fst((0.0f32, 5)))


    /////////////////////////////////////////////////////////////////////////////
    // snd                                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def snd01(): Unit \ Assert =
        assertEq(expected = 0.0f32, snd((5, 0.0f32)))

    @Test
    def snd02(): Unit \ Assert =
        assertEq(expected = 5, snd((0.0f32, 5)))


    /////////////////////////////////////////////////////////////////////////////
    // swap                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def swap01(): Unit \ Assert =
        assertEq(expected = (0.0f32, 5), swap((5, 0.0f32)))

    @Test
    def swap02(): Unit \ Assert =
        assertEq(expected = (5, 0.0f32), swap((0.0f32, 5)))


    /////////////////////////////////////////////////////////////////////////////
    // Function Composition                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def forwardComposition01(): Unit \ Assert =
        let f = x -> x + 1;
        let g = x -> x * 2;
        let h = f >> g;
        assertEq(expected = 2, h(0)) // (0 + 1) * 2

    @Test
    def forwardPipe01(): Unit \ Assert =
        let f = x -> x + 1;
        let g = x -> x * 2;
        let r = 0 |> f |> g;
        assertEq(expected = 2, r) // (0 + 1) * 2

    @Test
    def forwardPipe02(): Unit \ Assert =
        let f = match (x, y) -> x + y;
        let r = (1, 2) |> f;
        assertEq(expected = 3, r)


    /////////////////////////////////////////////////////////////////////////////
    // ||> (doublePipe)                                                        //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def doublePipe01(): Unit \ Assert =
        assertEq(expected = 3, ((1, 2) ||> Add.add))

    @Test
    def doublePipe02(): Unit \ Assert =
        assertEq(expected = "ab", (("a", "b") ||> Add.add))

    @Test
    def doublePipe03(): Unit \ Assert =
        assertEq(expected = "abcdef", (("abc", "def") ||> String.concat))

    @Test
    def doublePipe04(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: 5 :: Nil, ((1 :: 2 :: 3 :: Nil, 4 :: 5 :: Nil) ||> List.append))

    @Test
    def doublePipe05(): Unit \ Assert =
        assertEq(expected = 0 :: 1 :: 2 :: 3 :: 4 :: Nil, ((0, 5) ||> List.range))


    /////////////////////////////////////////////////////////////////////////////
    // !> (imperativePipe)                                                     //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def imperativePipe01(): Unit \ Assert = region rc {
        let m: MutList[Int32, _] = MutList.empty(rc);
        let f = (x) -> MutList.push(x, m);
        assertTrue((4 !> f) == 4);
        assertTrue(not MutList.isEmpty(m))
    }

    @Test
    def imperativePipe02(): Unit \ Assert = region rc {
        let m: MutList[Int32, _] = MutList.empty(rc);
        let f = (x) -> MutList.push(x, m);
        let a = 4 !> f;
        let b = 5 !> f;
        let c = 7 !> f;
        assertEq(expected = 4, actual = a);
        assertEq(expected = 5, actual = b);
        assertEq(expected = 7, actual = c);
        assertTrue(MutList.length(m) == 3)
    }

    @Test
    def imperativePipe03(): Unit \ Assert = region rc {
        let a = Ref.fresh(rc, 8);
        let f = x -> Ref.put(x, a);
        assertTrue((2 !> f) == 2);
        assertTrue(Ref.get(a) == 2)
    }

    @Test
    def imperativePipe04(): Unit \ Assert = region rc {
        let a = Ref.fresh(rc, 8);
        let f = x -> Ref.put(x, a);
        assertTrue((2 !> f) == 2);
        assertTrue((4 !> f) == 4);
        assertTrue(Ref.get(a) == 4)
    }

}
