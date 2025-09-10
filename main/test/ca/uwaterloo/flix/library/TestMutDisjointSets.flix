mod TestMutDisjointSets {

    use Assert.{assertEq, assertTrue, assertFalse};

    @Test
    def makeSet01(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        foreach (x <- List.range(0, 10)) {
            MutDisjointSets.makeSet(x, s)
        };
        let actual =
            List.range(0, 10)
            |> List.forAll(x -> MutDisjointSets.memberOf(x, s));
        assertTrue(actual)
    }

    @Test
    def makeSet02(): Unit \ Assert = region rc {
        let s: MutDisjointSets[Int32, _] = MutDisjointSets.empty(rc);
        foreach (x <- List.range(0, 10)) {
            MutDisjointSets.makeSet(x, s)
        };
        let actual = MutDisjointSets.size(s);
        assertEq(expected = 10, actual)
    }

    @Test
    def makeSets01(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        let actual =
            List.range(0, 10)
            |> List.forAll(x -> MutDisjointSets.memberOf(x, s));
        assertTrue(actual)
    }

    @Test
    def makeSets02(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        let actual = MutDisjointSets.size(s);
        assertEq(expected = 10, actual)
    }

    @Test
    def makeSets03(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets("a" :: "b" :: "c" :: Nil, s);
        let actual = MutDisjointSets.size(s);
        assertEq(expected = 3, actual)
    }

    @Test
    def find01(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(-10, 20), s);
        let actual = List.range(-10, 20) |> List.map(x -> MutDisjointSets.find(x, s));
        assertEq(expected = List.range(-10, 20) |> List.map(Some), actual)
    }

    @Test
    def find02(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets("x" :: "y" :: "z" :: Nil, s);
        let actual = ("x" :: "y" :: "z" :: Nil) |> List.map(x -> MutDisjointSets.find(x, s));
        assertEq(expected = ("x" :: "y" :: "z" :: Nil) |> List.map(Some), actual)
    }

    @Test
    def find03(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(0, 1, s);
        MutDisjointSets.union(2, 3, s);
        MutDisjointSets.union(4, 5, s);
        MutDisjointSets.union(6, 7, s);
        MutDisjointSets.union(8, 9, s);
        let actual = (0 :: 2 :: 4 :: 6 :: 8 :: Nil) |> List.map(x -> MutDisjointSets.find(x, s));
        assertEq(expected = (1 :: 3 :: 5 :: 7 :: 9 :: Nil) |> List.map(Some), actual)
    }

    @Test
    def find04(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(0, 1, s);
        MutDisjointSets.union(2, 3, s);
        MutDisjointSets.union(4, 5, s);
        MutDisjointSets.union(6, 7, s);
        MutDisjointSets.union(8, 9, s);
        let actual = (1 :: 3 :: 5 :: 7 :: 9 :: Nil) |> List.map(x -> MutDisjointSets.find(x, s));
        assertEq(expected = (1 :: 3 :: 5 :: 7 :: 9 :: Nil) |> List.map(Some), actual)
    }

    @Test
    def find05(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets("a" :: "b" :: "c" :: "d" :: Nil, s);
        let actual = MutDisjointSets.find("A", s);
        assertEq(expected = None, actual)
    }

    @Test
    def union01(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(0, 1, s);
        let actual = MutDisjointSets.find(0, s);
        assertEq(expected = Some(1), actual)
    }

    @Test
    def union02(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(0, 1, s);
        MutDisjointSets.union(0, 1, s);
        MutDisjointSets.union(0, 1, s);
        let actual = MutDisjointSets.find(0, s);
        assertEq(expected = Some(1), actual)
    }

    @Test
    def union03(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(1, 1, s);
        let actual = MutDisjointSets.find(1, s);
        assertEq(expected = Some(1), actual)
    }

    @Test
    def union04(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(0, 1, s);
        MutDisjointSets.union(1, 1, s);
        let actual = MutDisjointSets.find(1, s);
        assertEq(expected = Some(1), actual)
    }

    @Test
    def union05(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(0, 1, s);
        MutDisjointSets.union(1, 1, s);
        MutDisjointSets.union(1, 0, s);
        let actual = MutDisjointSets.find(1, s);
        assertEq(expected = Some(1), actual)
    }

    @Test
    def union06(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(0, 1, s);
        MutDisjointSets.union(1, 1, s);
        MutDisjointSets.union(1, 0, s);
        let actual = MutDisjointSets.find(0, s);
        assertEq(expected = Some(1), actual)
    }

    @Test
    def union07(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(1, 0, s);
        MutDisjointSets.union(0, 1, s);
        MutDisjointSets.union(1, 1, s);
        let actual = MutDisjointSets.find(0, s);
        assertEq(expected = Some(0), actual)
    }

    @Test
    def union08(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(1, 0, s);
        MutDisjointSets.union(0, 1, s);
        MutDisjointSets.union(1, 1, s);
        let actual = MutDisjointSets.find(1, s);
        assertEq(expected = Some(0), actual)
    }

    @Test
    def union09(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(0, 1, s);
        MutDisjointSets.union(1, 3, s);
        let actual = MutDisjointSets.find(3, s);
        assertEq(expected = Some(1), actual)
    }

    @Test
    def union10(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets("a" :: "b" :: "c" :: "d" :: Nil, s);
        MutDisjointSets.union("c", "d", s);
        let actual = MutDisjointSets.find("a", s);
        assertEq(expected = Some("a"), actual)
    }

    @Test
    def union11(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets("a" :: "b" :: "c" :: "d" :: Nil, s);
        MutDisjointSets.union("c", "d", s);
        MutDisjointSets.union("b", "c", s);
        MutDisjointSets.union("a", "b", s);
        let actual = MutDisjointSets.find("a", s);
        assertEq(expected = Some("d"), actual)
    }

    @Test
    def union12(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(7, 4, s);
        MutDisjointSets.union(7, 9, s);
        let actual = MutDisjointSets.find(9, s);
        assertEq(expected = Some(4), actual)
    }

    @Test
    def union13(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(7, 4, s);
        MutDisjointSets.union(7, 9, s);
        MutDisjointSets.union(8, 1, s);
        let actual = MutDisjointSets.find(9, s);
        assertEq(expected = Some(4), actual)
    }

    @Test
    def union14(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(7, 4, s);
        MutDisjointSets.union(7, 9, s);
        MutDisjointSets.union(8, 1, s);
        MutDisjointSets.union(8, 9, s);
        let actual = MutDisjointSets.find(8, s);
        assertEq(expected = Some(4), actual)
    }

    @Test
    def union15(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(7, 4, s);
        MutDisjointSets.union(7, 9, s);
        MutDisjointSets.union(8, 1, s);
        MutDisjointSets.union(2, 3, s);
        MutDisjointSets.union(2, 8, s);
        MutDisjointSets.union(8, 9, s);
        let actual = MutDisjointSets.find(4, s);
        assertEq(expected = Some(1), actual)
    }

    @Test
    def union16(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.union(20, 21, s);
        assertTrue(MutDisjointSets.memberOf(20, s))
    }

    @Test
    def union17(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.union(20, 21, s);
        assertTrue(MutDisjointSets.memberOf(21, s))
    }

    @Test
    def union18(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSet(0, s);
        MutDisjointSets.union(1, 0, s);
        assertTrue(MutDisjointSets.equivalent(1, 0, s))
    }

    @Test
    def union19(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSet(0, s);
        MutDisjointSets.union(0, 1, s);
        assertTrue(MutDisjointSets.equivalent(1, 0, s))
    }


    @Test
    def equivalent01(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(-10, 20), s);
        let actual = MutDisjointSets.equivalent(-10, -10, s);
        assertTrue(actual)
    }

    @Test
    def equivalent02(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(-10, 20), s);
        let actual = MutDisjointSets.equivalent(-10, 10, s);
        assertFalse(actual)
    }

    @Test
    def equivalent03(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(-10, 20), s);
        let actual = MutDisjointSets.equivalent(-10, 200, s);
        assertFalse(actual)
    }

    @Test
    def equivalent04(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(7, 4, s);
        MutDisjointSets.union(7, 9, s);
        let actual = MutDisjointSets.equivalent(7, 9, s);
        assertTrue(actual)
    }

    @Test
    def equivalent05(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(7, 4, s);
        MutDisjointSets.union(7, 9, s);
        let actual = MutDisjointSets.equivalent(7, 8, s);
        assertFalse(actual)
    }

    @Test
    def equivalent06(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(7, 4, s);
        MutDisjointSets.union(7, 9, s);
        MutDisjointSets.union(8, 1, s);
        let actual = MutDisjointSets.equivalent(7, 8, s);
        assertFalse(actual)
    }

    @Test
    def equivalent07(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(7, 4, s);
        MutDisjointSets.union(7, 9, s);
        MutDisjointSets.union(8, 1, s);
        MutDisjointSets.union(8, 7, s);
        let actual = MutDisjointSets.equivalent(7, 8, s);
        assertTrue(actual)
    }

    @Test
    def equivalent08(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        let actual = MutDisjointSets.equivalent(20, 30, s);
        assertFalse(actual)
    }

    @Test
    def equivalent09(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(-100, 100), s);
        let actual =
            List.range(0, 10)
            |> List.forAll(x ->
                List.range(0, 10)
                |> List.filter(y -> x != y)
                |> List.forAll(y -> not MutDisjointSets.equivalent(x, y, s))
            );
        assertTrue(actual)
    }

    @Test
    def size01(): Unit \ Assert = region rc {
        let s: MutDisjointSets[String, rc] = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(Nil, s);
        assertEq(expected = 0, MutDisjointSets.size(s))
    }

    @Test
    def size02(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        assertEq(expected = 10, MutDisjointSets.size(s))
    }

    @Test
    def size03(): Unit \ Assert = region rc {
        let s = MutDisjointSets.empty(rc);
        MutDisjointSets.makeSets(List.range(0, 10), s);
        MutDisjointSets.union(0, 1, s);
        assertEq(expected = 10, MutDisjointSets.size(s))
    }

}
