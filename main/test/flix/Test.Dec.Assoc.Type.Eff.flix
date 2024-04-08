mod Test.Dec.Assoc.Type.Eff {

    ////////////////////////////////////////////////////////
    // Type Definitions                                   //
    ////////////////////////////////////////////////////////

    eff OutInt32 {
        pub def toStream(x: Int32): Unit
    }

    eff OutString {
        pub def toStream(x: String): Unit
    }

    trait Runner[a] {
        pub type E: Eff
        pub def run(x: a): Unit \ Runner.E[a]
    }

    instance Runner[Int32] {
        pub type E = OutInt32
        pub def run(x: Int32): Unit \ OutInt32 =
            do OutInt32.toStream(x)
    }

    instance Runner[String] {
        pub type E = OutString
        pub def run(x: String): Unit \ OutString =
            do OutString.toStream(x)
    }

    instance Runner[Vector[a]] with Runner[a] {
        pub type E = Runner.E[a]
        pub def run(x: Vector[a]): Unit \ Runner.E[a] =
            foreach (a <- x) Runner.run(a)
    }

    ////////////////////////////////////////////////////////
    // Polymorphic functions using Runner                 //
    ////////////////////////////////////////////////////////

    pub def runOnce(x: a): Unit \ Runner.E[a] with Runner[a] =
        Runner.run(x)

    pub def runTwice(x: a): Unit \ Runner.E[a] with Runner[a] = {
        Runner.run(x);
        Runner.run(x)
    }


    ////////////////////////////////////////////////////////
    // Monomorphic functions using Runner                 //
    ////////////////////////////////////////////////////////

    // Int32

    pub def runOnceInt3201(x: Int32): Unit \ OutInt32 =
        Runner.run(x)

    pub def runTwiceInt3201(x: Int32): Unit \ OutInt32 = {
        Runner.run(x);
        Runner.run(x)
    }

    pub def runOnceInt3202(x: Int32): Unit \ OutInt32 =
        do OutInt32.toStream(x)

    pub def runTwiceInt3202(x: Int32): Unit \ OutInt32 = {
        do OutInt32.toStream(x);
        do OutInt32.toStream(x)
    }

    // String

    pub def runOnceString01(x: String): Unit \ OutString =
        Runner.run(x)

    pub def runTwiceString01(x: String): Unit \ OutString = {
        Runner.run(x);
        Runner.run(x)
    }

    pub def runOnceString02(x: String): Unit \ OutString =
        do OutString.toStream(x)

    pub def runTwiceString02(x: String): Unit \ OutString = {
        do OutString.toStream(x);
        do OutString.toStream(x)
    }

    // Vector[Int32]

    pub def runOnceVectorInt3201(x: Vector[Int32]): Unit \ OutInt32 =
        Runner.run(x)

    pub def runTwiceVectorInt3201(x: Vector[Int32]): Unit \ OutInt32 = {
        Runner.run(x);
        Runner.run(x)
    }

    pub def runOnceVectorInt3202(x: Vector[Int32]): Unit \ OutInt32 =
        foreach (a <- x) do OutInt32.toStream(a)

    pub def runTwiceVectorInt3202(x: Vector[Int32]): Unit \ OutInt32 = {
        foreach (a <- x) do OutInt32.toStream(a);
        foreach (a <- x) do OutInt32.toStream(a)
    }

    // Vector[String]

    pub def runOnceVectorString01(x: Vector[String]): Unit \ OutString =
        Runner.run(x)

    pub def runTwiceVectorString01(x: Vector[String]): Unit \ OutString = {
        Runner.run(x);
        Runner.run(x)
    }

    pub def runOnceVectorString02(x: Vector[String]): Unit \ OutString =
        foreach (a <- x) do OutString.toStream(a)

    pub def runTwiceVectorString02(x: Vector[String]): Unit \ OutString = {
        foreach (a <- x) do OutString.toStream(a);
        foreach (a <- x) do OutString.toStream(a)
    }

    // Vector[Vector[Int32]]

    pub def runOnceVectorVectorInt3201(x: Vector[Vector[Int32]]): Unit \ OutInt32 =
        Runner.run(x)

    pub def runTwiceVectorVectorInt3201(x: Vector[Vector[Int32]]): Unit \ OutInt32 = {
        Runner.run(x);
        Runner.run(x)
    }

    pub def runOnceVectorVectorInt3202(x: Vector[Vector[Int32]]): Unit \ OutInt32 =
        foreach (a <- x) foreach (b <- a) do OutInt32.toStream(b)

    pub def runTwiceVectorVectorInt3202(x: Vector[Vector[Int32]]): Unit \ OutInt32 = {
        foreach (a <- x) foreach (b <- a) do OutInt32.toStream(b);
        foreach (a <- x) foreach (b <- a) do OutInt32.toStream(b)
    }

    // Vector[Vector[String]]

    pub def runOnceVectorVectorString01(x: Vector[Vector[String]]): Unit \ OutString =
        Runner.run(x)

    pub def runTwiceVectorVectorString01(x: Vector[Vector[String]]): Unit \ OutString = {
        Runner.run(x);
        Runner.run(x)
    }

    pub def runOnceVectorVectorString02(x: Vector[Vector[String]]): Unit \ OutString =
        foreach (a <- x) foreach (b <- a) do OutString.toStream(b)

    pub def runTwiceVectorVectorString02(x: Vector[Vector[String]]): Unit \ OutString = {
        foreach (a <- x) foreach (b <- a) do OutString.toStream(b);
        foreach (a <- x) foreach (b <- a) do OutString.toStream(b)
    }


    ////////////////////////////////////////////////////////
    // Tests                                              //
    ////////////////////////////////////////////////////////

    // Int32

    pub def handleWithListInt32(f: a -> Unit \ OutInt32, x: a): List[Int32] =
        try {
            f(x); Nil
        } with OutInt32 {
            def toStream(a, k) =
                (a :: k()) ::: Nil
        }

    @Test
    pub def testInt3201(): Bool =
        handleWithListInt32(runOnceInt3201, 42) == 42 :: Nil

    @Test
    pub def testInt3202(): Bool =
        handleWithListInt32(runOnceInt3202, 42) == 42 :: Nil

    @Test
    pub def testInt3203(): Bool =
        handleWithListInt32(runTwiceInt3201, 42) == 42 :: 42 :: Nil

    @Test
    pub def testInt3204(): Bool =
        handleWithListInt32(runTwiceInt3202, 42) == 42 :: 42 :: Nil


    // String

    pub def handleWithListString(f: a -> Unit \ OutString, x: a): List[String] =
        try {
            f(x); Nil
        } with OutString {
            def toStream(a, k) =
                (a :: k()) ::: Nil
        }

    @Test
    pub def testString01(): Bool =
        handleWithListString(runOnceString01, "Hello") == "Hello" :: Nil

    @Test
    pub def testString02(): Bool =
        handleWithListString(runOnceString02, "Hello") == "Hello" :: Nil

    @Test
    pub def testString03(): Bool =
        handleWithListString(runTwiceString01, "Hello") == "Hello" :: "Hello" :: Nil

    @Test
    pub def testString04(): Bool =
        handleWithListString(runTwiceString02, "Hello") == "Hello" :: "Hello" :: Nil


    // Vector[Int32]

    @Test
    pub def testVectorInt3201(): Bool =
        handleWithListInt32(runOnceVectorInt3201, Vector#{1, 2, 3}) == 1 :: 2 :: 3 :: Nil

}
