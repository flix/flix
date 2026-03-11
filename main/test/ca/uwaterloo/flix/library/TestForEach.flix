mod TestForEach {

    use Assert.{assertTrue, assertEq};
    use Int32.modulo;
    use ForEach.forEach;
    use ForEach.withFilter;
    use ForEach.withIndex;
    use ForEach.withMap;
    use ForEach.withZip;

    /////////////////////////////////////////////////////////////////////////////
    // withIndex                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def withIndex01(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach((i, x) <- withIndex(Nil)) {
            results |> MutList.push((i, x))
        };
        assertTrue(results |> MutList.isEmpty)
    }

    @Test
    pub def withIndex02(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach((i, x) <- withIndex(2 :: 4 :: 6 :: 8 :: Nil)) {
            results |> MutList.push((i, x))
        };
        assertEq(expected = (0, 2) :: (1, 4) :: (2, 6) :: (3, 8) :: Nil, results |> MutList.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // withFilter                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def withFilter01(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withFilter(x -> x `modulo` 2 == 0, Nil)) {
            results |> MutList.push(x)
        };
        assertTrue(results |> MutList.isEmpty)
    }

    @Test
    pub def withFilter02(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withFilter(x -> x `modulo` 2 == 0, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil)) {
            results |> MutList.push(x)
        };
        assertEq(expected = 0 :: 2 :: 4 :: 6 :: 8 :: Nil, results |> MutList.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // withMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def withMap01(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withMap(x -> x * 2, Nil)) {
            results |> MutList.push(x)
        };
        assertTrue(results |> MutList.isEmpty)
    }

    @Test
    pub def withMap02(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withMap(x -> x * 2, 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) {
            results |> MutList.push(x)
        };
        assertEq(expected = 0 :: 2 :: 4 :: 6 :: 8 :: 10 :: Nil, results |> MutList.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // withZip                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def withZip01(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(Nil, Nil)) {
            results |> MutList.push(x)
        };
        assertTrue(results |> MutList.isEmpty)
    }

    @Test
    pub def withZip02(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(1 :: 2 :: 3 :: 4 :: 5 :: Nil, Nil)) {
            results |> MutList.push(x)
        };
        assertTrue(results |> MutList.isEmpty)
    }

    @Test
    pub def withZip03(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(Nil, 1 :: 2 :: 3 :: 4 :: 5 :: Nil)) {
            results |> MutList.push(x)
        };
        assertTrue(results |> MutList.isEmpty)
    }

    @Test
    pub def withZip04(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(1 :: 2 :: 3 :: 4 :: 5 :: Nil, "one" :: "two" :: "three" :: Nil)) {
            results |> MutList.push(x)
        };
        assertEq(expected = (1, "one") :: (2, "two") :: (3, "three") :: Nil, results |> MutList.toList)
    }

    @Test
    pub def withZip05(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(1 :: 2 :: 3 :: Nil, "one" :: "two" :: "three" :: "four" :: "five" :: Nil)) {
            results |> MutList.push(x)
        };
        assertEq(expected = (1, "one") :: (2, "two") :: (3, "three") :: Nil, results |> MutList.toList)
    }

    @Test
    pub def withZip06(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        foreach(x <- withZip(1 :: 2 :: 3 :: 4 :: 5 :: Nil, "one" :: "two" :: "three" :: "four" :: "five" :: Nil)) {
            results |> MutList.push(x)
        };
        assertEq(expected = (1, "one") :: (2, "two") :: (3, "three") :: (4, "four") :: (5, "five") :: Nil, results |> MutList.toList)
    }

    /////////////////////////////////////////////////////////////////////////////
    // composite                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    pub def composite01(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil)
            |> withFilter(x -> x `modulo` 2 == 0)
            |> withIndex;
        foreach((i, x) <- list) {
            results |> MutList.push((i, x))
        };
        assertEq(expected = (0, 0) :: (1, 2) :: (2, 4) :: (3, 6) :: (4, 8) :: Nil, results |> MutList.toList)
    }

    @Test
    pub def composite02(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil)
            |> withIndex
            |> withFilter(pair -> {
                let (_, x) = pair;
                x `modulo` 2 == 0
            });
        foreach((i, x) <- list) {
            results |> MutList.push((i, x))
        };
        assertEq(expected = (0, 0) :: (2, 2) :: (4, 4) :: (6, 6) :: (8, 8) :: Nil, results |> MutList.toList)
    }

    @Test
    pub def composite03(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)
            |> withMap(x -> x * 2)
            |> withIndex;
        foreach((i, x) <- list) {
            results |> MutList.push((i, x))
        };
        assertEq(expected = (0, 0) :: (1, 2) :: (2, 4) :: (3, 6) :: (4, 8) :: (5, 10) :: Nil, results |> MutList.toList)
    }

    @Test
    pub def composite04(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)
            |> withIndex
            |> withMap(pair -> {
                let (i, x) = pair;
                i * x
            });
        foreach(x <- list) {
            results |> MutList.push(x)
        };
        assertEq(expected = 0 :: 1 :: 4 :: 9 :: 16 :: 25 :: Nil, results |> MutList.toList)
    }

    @Test
    pub def composite05(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil)
            |> withMap(x -> x * 2)
            |> withFilter(x -> x `modulo` 3 == 0);
        foreach(x <- list) {
            results |> MutList.push(x)
        };
        assertEq(expected = 0 :: 6 :: Nil, results |> MutList.toList)
    }

    @Test
    pub def composite06(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        let list = (0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil)
            |> withFilter(x -> x `modulo` 2 == 0)
            |> withMap(x -> x * 3);
        foreach(x <- list) {
            results |> MutList.push(x)
        };
        assertEq(expected = 0 :: 6 :: 12 :: 18 :: 24 :: Nil, results |> MutList.toList)
    }

    @Test
    pub def composite07(): Unit \ Assert = region rc {
        let results = MutList.empty(rc);
        let list = withIndex(withZip('0' :: '1' :: '2' :: '3' :: '4' :: '5' :: Nil, "zero" :: "one" :: "two" :: "three" :: "four" :: "five" :: Nil));
        foreach((i, (c, s)) <- list) {
            results |> MutList.push((i, c, s))
        };
        assertEq(expected = (0, '0', "zero") :: (1, '1', "one") :: (2, '2', "two") :: (3, '3', "three") :: (4, '4', "four") :: (5, '5', "five") :: Nil, results |> MutList.toList)
    }

}
