mod Test.Dec.Assoc.Type.Eff {

    use Assert.{assertEq, assertTrue}

    ////////////////////////////////////////////////////////
    // Type Definitions                                   //
    ////////////////////////////////////////////////////////

    eff OutInt32 {
        def toStream(x: Int32): Unit
    }

    eff OutString {
        def toStream(x: String): Unit
    }

    trait Runner[a] {
        pub type E: Eff
        pub def exec(x: a): Unit \ Runner.E[a]
    }

    instance Runner[Int32] {
        pub type E = OutInt32
        pub def exec(x: Int32): Unit \ OutInt32 =
            OutInt32.toStream(x)
    }

    instance Runner[String] {
        pub type E = OutString
        pub def exec(x: String): Unit \ OutString =
            OutString.toStream(x)
    }

    instance Runner[Vector[a]] with Runner[a] {
        pub type E = Runner.E[a]
        pub def exec(x: Vector[a]): Unit \ Runner.E[a] =
            let f: a -> Unit \ Runner.E[a] = Runner.exec;
            foreach(a <- x) f(a)
    }

    ////////////////////////////////////////////////////////
    // Polymorphic functions using Runner                 //
    ////////////////////////////////////////////////////////

    pub def runOnce(x: a): Unit \ Runner.E[a] with Runner[a] =
        Runner.exec(x)

    pub def runTwice(x: a): Unit \ Runner.E[a] with Runner[a] = {
        Runner.exec(x);
        Runner.exec(x)
    }


    ////////////////////////////////////////////////////////
    // Monomorphic functions using Runner                 //
    ////////////////////////////////////////////////////////

    // Int32

    pub def runOnceInt32_01(x: Int32): Unit \ OutInt32 =
        Runner.exec(x)

    pub def runTwiceInt32_01(x: Int32): Unit \ OutInt32 = {
        Runner.exec(x);
        Runner.exec(x)
    }

    pub def runOnceInt32_02(x: Int32): Unit \ OutInt32 =
        OutInt32.toStream(x)

    pub def runTwiceInt32_02(x: Int32): Unit \ OutInt32 = {
        OutInt32.toStream(x);
        OutInt32.toStream(x)
    }

    // String

    pub def runOnceString01(x: String): Unit \ OutString =
        Runner.exec(x)

    pub def runTwiceString01(x: String): Unit \ OutString = {
        Runner.exec(x);
        Runner.exec(x)
    }

    pub def runOnceString02(x: String): Unit \ OutString =
        OutString.toStream(x)

    pub def runTwiceString02(x: String): Unit \ OutString = {
        OutString.toStream(x);
        OutString.toStream(x)
    }

    // Vector[Int32]

    pub def runOnceVectorInt32_01(x: Vector[Int32]): Unit \ OutInt32 =
        Runner.exec(x)

    pub def runTwiceVectorInt32_01(x: Vector[Int32]): Unit \ OutInt32 = {
        Runner.exec(x);
        Runner.exec(x)
    }

    pub def runOnceVectorInt32_02(x: Vector[Int32]): Unit \ OutInt32 =
        foreach (a <- x) OutInt32.toStream(a)

    pub def runTwiceVectorInt32_02(x: Vector[Int32]): Unit \ OutInt32 = {
        foreach (a <- x) OutInt32.toStream(a);
        foreach (a <- x) OutInt32.toStream(a)
    }

    // Vector[String]

    pub def runOnceVectorString01(x: Vector[String]): Unit \ OutString =
        Runner.exec(x)

    pub def runTwiceVectorString01(x: Vector[String]): Unit \ OutString = {
        Runner.exec(x);
        Runner.exec(x)
    }

    pub def runOnceVectorString02(x: Vector[String]): Unit \ OutString =
        foreach (a <- x) OutString.toStream(a)

    pub def runTwiceVectorString02(x: Vector[String]): Unit \ OutString = {
        foreach (a <- x) OutString.toStream(a);
        foreach (a <- x) OutString.toStream(a)
    }

    // Vector[Vector[Int32]]

    pub def runOnceVectorVectorInt32_01(x: Vector[Vector[Int32]]): Unit \ OutInt32 =
        Runner.exec(x)

    pub def runTwiceVectorVectorInt32_01(x: Vector[Vector[Int32]]): Unit \ OutInt32 = {
        Runner.exec(x);
        Runner.exec(x)
    }

    pub def runOnceVectorVectorInt32_02(x: Vector[Vector[Int32]]): Unit \ OutInt32 =
        foreach (a <- x) foreach (b <- a) OutInt32.toStream(b)

    pub def runTwiceVectorVectorInt32_02(x: Vector[Vector[Int32]]): Unit \ OutInt32 = {
        foreach (a <- x) foreach (b <- a) OutInt32.toStream(b);
        foreach (a <- x) foreach (b <- a) OutInt32.toStream(b)
    }

    // Vector[Vector[String]]

    pub def runOnceVectorVectorString01(x: Vector[Vector[String]]): Unit \ OutString =
        Runner.exec(x)

    pub def runTwiceVectorVectorString01(x: Vector[Vector[String]]): Unit \ OutString = {
        Runner.exec(x);
        Runner.exec(x)
    }

    pub def runOnceVectorVectorString02(x: Vector[Vector[String]]): Unit \ OutString =
        foreach (a <- x) foreach (b <- a) OutString.toStream(b)

    pub def runTwiceVectorVectorString02(x: Vector[Vector[String]]): Unit \ OutString = {
        foreach (a <- x) foreach (b <- a) OutString.toStream(b);
        foreach (a <- x) foreach (b <- a) OutString.toStream(b)
    }


    ////////////////////////////////////////////////////////
    // Tests                                              //
    ////////////////////////////////////////////////////////

    // Int32

    pub def handleWithListInt32(f: a -> Unit \ OutInt32, x: a): List[Int32] =
        run {
            f(x); Nil
        } with handler OutInt32 {
            def toStream(a, k) =
                (a :: k()) ::: Nil
        }

    @Test
    pub def testInt32_01(): Unit \ Assert =
        assertEq(expected = 42 :: Nil, handleWithListInt32(runOnceInt32_01, 42))

    @Test
    pub def testInt32_02(): Unit \ Assert =
        assertEq(expected = 42 :: Nil, handleWithListInt32(runOnceInt32_02, 42))

    @Test
    pub def testInt32_03(): Unit \ Assert =
        assertEq(expected = 42 :: 42 :: Nil, handleWithListInt32(runTwiceInt32_01, 42))

    @Test
    pub def testInt32_04(): Unit \ Assert =
        assertEq(expected = 42 :: 42 :: Nil, handleWithListInt32(runTwiceInt32_02, 42))


    // String

    pub def handleWithListString(f: a -> Unit \ OutString, x: a): List[String] =
        run {
            f(x); Nil
        } with handler OutString {
            def toStream(a, k) =
                (a :: k()) ::: Nil
        }

    @Test
    pub def testString01(): Unit \ Assert =
        assertEq(expected = "Hello" :: Nil, handleWithListString(runOnceString01, "Hello"))

    @Test
    pub def testString02(): Unit \ Assert =
        assertEq(expected = "Hello" :: Nil, handleWithListString(runOnceString02, "Hello"))

    @Test
    pub def testString03(): Unit \ Assert =
        assertEq(expected = "Hello" :: "Hello" :: Nil, handleWithListString(runTwiceString01, "Hello"))

    @Test
    pub def testString04(): Unit \ Assert =
        assertEq(expected = "Hello" :: "Hello" :: Nil, handleWithListString(runTwiceString02, "Hello"))


    // Vector[Int32]

    @Test
    pub def testVectorInt32_01(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, handleWithListInt32(runOnceVectorInt32_01, Vector#{1, 2, 3}))

    @Test
    pub def testVectorInt32_02(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, handleWithListInt32(runOnceVectorInt32_02, Vector#{1, 2, 3}))

    @Test
    pub def testVectorInt32_03(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 1 :: 2 :: 3 :: Nil, handleWithListInt32(runTwiceVectorInt32_01, Vector#{1, 2, 3}))

    @Test
    pub def testVectorInt32_04(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 1 :: 2 :: 3 :: Nil, handleWithListInt32(runTwiceVectorInt32_02, Vector#{1, 2, 3}))

    @Test
    pub def testVectorInt32_05(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListInt32(runOnceVectorInt32_01, Vector#{}))

    @Test
    pub def testVectorInt32_06(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListInt32(runOnceVectorInt32_02, Vector#{}))

    @Test
    pub def testVectorInt32_07(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListInt32(runTwiceVectorInt32_01, Vector#{}))

    @Test
    pub def testVectorInt32_08(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListInt32(runTwiceVectorInt32_02, Vector#{}))

    @Test
    pub def testVectorInt32_09(): Unit \ Assert =
        let v = Vector.range(0, 100);
        let left = handleWithListInt32(runOnceVectorInt32_01, v);
        let right = handleWithListInt32(runOnceVectorInt32_02, v);
        assertTrue(left == right)

    @Test
    pub def testVectorInt32_10(): Unit \ Assert =
        let v = Vector.range(0, 100);
        let left = handleWithListInt32(runTwiceVectorInt32_01, v);
        let right = handleWithListInt32(runTwiceVectorInt32_02, v);
        assertTrue(left == right)


    // Vector[String]

    @Test
    pub def testVectorString01(): Unit \ Assert =
        assertEq(expected = "a" :: "b" :: "c" :: Nil, handleWithListString(runOnceVectorString01, Vector#{"a", "b", "c"}))

    @Test
    pub def testVectorString02(): Unit \ Assert =
        assertEq(expected = "a" :: "b" :: "c" :: Nil, handleWithListString(runOnceVectorString02, Vector#{"a", "b", "c"}))

    @Test
    pub def testVectorString03(): Unit \ Assert =
        assertEq(expected = "a" :: "b" :: "c" :: "a" :: "b" :: "c" :: Nil, handleWithListString(runTwiceVectorString01, Vector#{"a", "b", "c"}))

    @Test
    pub def testVectorString04(): Unit \ Assert =
        assertEq(expected = "a" :: "b" :: "c" :: "a" :: "b" :: "c" :: Nil, handleWithListString(runTwiceVectorString02, Vector#{"a", "b", "c"}))

    @Test
    pub def testVectorString05(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListString(runOnceVectorString01, Vector#{}))

    @Test
    pub def testVectorString06(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListString(runOnceVectorString02, Vector#{}))

    @Test
    pub def testVectorString07(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListString(runTwiceVectorString01, Vector#{}))

    @Test
    pub def testVectorString08(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListString(runTwiceVectorString02, Vector#{}))

    @Test
    pub def testVectorString09(): Unit \ Assert =
        let v = Vector.range(0, 100) |> Vector.map(Int32.toString);
        let left = handleWithListString(runOnceVectorString01, v);
        let right = handleWithListString(runOnceVectorString02, v);
        assertTrue(left == right)

    @Test
    pub def testVectorString10(): Unit \ Assert =
        let v = Vector.range(0, 100) |> Vector.map(Int32.toString);
        let left = handleWithListString(runTwiceVectorString01, v);
        let right = handleWithListString(runTwiceVectorString02, v);
        assertTrue(left == right)

    // Vector[Vector[Int32]]

    @Test
    pub def testVectorVectorInt32_01(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, handleWithListInt32(runOnceVectorVectorInt32_01, Vector#{Vector#{1, 2}, Vector#{3, 4}}))

    @Test
    pub def testVectorVectorInt32_02(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, handleWithListInt32(runOnceVectorVectorInt32_02, Vector#{Vector#{1, 2}, Vector#{3, 4}}))

    @Test
    pub def testVectorVectorInt32_03(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: 1 :: 2 :: 3 :: 4 :: Nil, handleWithListInt32(runTwiceVectorVectorInt32_01, Vector#{Vector#{1, 2}, Vector#{3, 4}}))

    @Test
    pub def testVectorVectorInt32_04(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: 1 :: 2 :: 3 :: 4 :: Nil, handleWithListInt32(runTwiceVectorVectorInt32_02, Vector#{Vector#{1, 2}, Vector#{3, 4}}))

    @Test
    pub def testVectorVectorInt32_05(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListInt32(runOnceVectorVectorInt32_01, Vector#{}))

    @Test
    pub def testVectorVectorInt32_06(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListInt32(runOnceVectorVectorInt32_02, Vector#{}))

    @Test
    pub def testVectorVectorInt32_07(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListInt32(runTwiceVectorVectorInt32_01, Vector#{}))

    @Test
    pub def testVectorVectorInt32_08(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListInt32(runTwiceVectorVectorInt32_02, Vector#{}))

    @Test
    pub def testVectorVectorInt32_09(): Unit \ Assert =
        let v = Vector.repeat(10, Vector.range(0, 10));
        let left = handleWithListInt32(runOnceVectorVectorInt32_01, v);
        let right = handleWithListInt32(runOnceVectorVectorInt32_02, v);
        assertTrue(left == right)

    @Test
    pub def testVectorVectorInt32_10(): Unit \ Assert =
        let v = Vector.repeat(10, Vector.range(0, 10));
        let left = handleWithListInt32(runTwiceVectorVectorInt32_01, v);
        let right = handleWithListInt32(runTwiceVectorVectorInt32_02, v);
        assertTrue(left == right)


    // Vector[Vector[String]]

    @Test
    pub def testVectorVectorString01(): Unit \ Assert =
        assertEq(expected = "a" :: "b" :: "c" :: "d" :: Nil, handleWithListString(runOnceVectorVectorString01, Vector#{Vector#{"a", "b"}, Vector#{"c", "d"}}))

    @Test
    pub def testVectorVectorString02(): Unit \ Assert =
        assertEq(expected = "a" :: "b" :: "c" :: "d" :: Nil, handleWithListString(runOnceVectorVectorString02, Vector#{Vector#{"a", "b"}, Vector#{"c", "d"}}))

    @Test
    pub def testVectorVectorString03(): Unit \ Assert =
        assertEq(expected = "a" :: "b" :: "c" :: "d" :: "a" :: "b" :: "c" :: "d" :: Nil, handleWithListString(runTwiceVectorVectorString01, Vector#{Vector#{"a", "b"}, Vector#{"c", "d"}}))

    @Test
    pub def testVectorVectorString04(): Unit \ Assert =
        assertEq(expected = "a" :: "b" :: "c" :: "d" :: "a" :: "b" :: "c" :: "d" :: Nil, handleWithListString(runTwiceVectorVectorString02, Vector#{Vector#{"a", "b"}, Vector#{"c", "d"}}))

    @Test
    pub def testVectorVectorString05(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListString(runOnceVectorVectorString01, Vector#{}))

    @Test
    pub def testVectorVectorString06(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListString(runOnceVectorVectorString02, Vector#{}))

    @Test
    pub def testVectorVectorString07(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListString(runTwiceVectorVectorString01, Vector#{}))

    @Test
    pub def testVectorVectorString08(): Unit \ Assert =
        assertEq(expected = Nil, handleWithListString(runTwiceVectorVectorString02, Vector#{}))

    @Test
    pub def testVectorVectorString09(): Unit \ Assert =
        let v = Vector.repeat(10, Vector.range(0, 10) |> Vector.map(Int32.toString));
        let left = handleWithListString(runOnceVectorVectorString01, v);
        let right = handleWithListString(runOnceVectorVectorString02, v);
        assertTrue(left == right)

    @Test
    pub def testVectorVectorString10(): Unit \ Assert =
        let v = Vector.repeat(10, Vector.range(0, 10) |> Vector.map(Int32.toString));
        let left = handleWithListString(runTwiceVectorVectorString01, v);
        let right = handleWithListString(runTwiceVectorVectorString02, v);
        assertTrue(left == right)

}
