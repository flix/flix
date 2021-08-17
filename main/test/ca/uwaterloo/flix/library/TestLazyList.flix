namespace TestLazyListOld {

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def empty01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def isEmpty01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.isEmpty

    @test
    pub def isEmpty02(): Bool =
        not (LazyList.range(0, 1000) |> LazyList.isEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def range01(): Bool =
        LazyList.range(0, 0) |> LazyList.isEmpty

    @test
    pub def range02(): Bool =
        LazyList.range(100, 100) |> LazyList.isEmpty

    @test
    pub def range03(): Bool =
        LazyList.range(1, 0) |> LazyList.isEmpty

    @test
    pub def range04(): Bool =
        not (LazyList.range(0, 100) |> LazyList.isEmpty)

    @test
    pub def range05(): Bool =
        LazyList.range(0, 100) |> LazyList.toList == List.range(0, 100)

    @test
    pub def range06(): Bool =
        not (LazyList.range(-100, 100) |> LazyList.isEmpty)

    @test
    pub def range07(): Bool =
        LazyList.range(-100, 100) |> LazyList.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def head01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.head == None

    @test
    pub def head02(): Bool =
        LazyList.range(0, 1000) |> LazyList.head == Some(0)

    @test
    pub def head03(): Bool =
        let l = LazyList.range(0, 1000);
        LazyList.head(l) == Some(0)
            and LazyList.head(l) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def take01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.take(1000) |> LazyList.isEmpty

    @test
    pub def take02(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.take(0) |> LazyList.isEmpty

    @test
    pub def take03(): Bool =
        (LazyList.range(0, 1000)): LazyList[Int32] |> LazyList.take(0) |> LazyList.isEmpty

    @test
    pub def take04(): Bool =
        (LazyList.range(0, 1000)): LazyList[Int32]
            |> LazyList.take(500)
            |> LazyList.toList == List.range(0, 500)

    @test
    pub def take05(): Bool =
        (LazyList.range(0, 1000)): LazyList[Int32]
            |> LazyList.take(1)
            |> LazyList.toList == List.range(0, 1)


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def length01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.length == 0

    @test
    pub def length02(): Bool =
        LazyList.range(0, 100000) |> LazyList.length == 100000


    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool =
        LazyList.reverse(LazyList.empty(): LazyList[Unit]) |> LazyList.toList == Nil

    @test
    def reverse02(): Bool =
        (1 :: Nil) |> LazyList.toLazy |> LazyList.reverse |> LazyList.toList == 1 :: Nil

    @test
    def reverse03(): Bool =
        (1 :: 2 :: Nil) |> LazyList.toLazy |> LazyList.reverse |> LazyList.toList == 2 :: 1 :: Nil

    @test
    def reverse04(): Bool =
        (1 :: 1 :: Nil) |> LazyList.toLazy |> LazyList.reverse |> LazyList.toList == 1 :: 1 :: Nil

    @test
    def reverse05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.reverse |> LazyList.toList == 3 :: 2 :: 1 :: Nil

    @test
    def reverse06(): Bool =
        (1 :: 2 :: 3 :: 4 :: Nil) |> LazyList.toLazy |> LazyList.reverse |> LazyList.toList == 4 :: 3 :: 2 :: 1 :: Nil

    @test
    def reverse07(): Bool =
        LazyList.range(-100, 1000) |> LazyList.reverse |> LazyList.toList == List.range(-100, 1000) |> List.reverse


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testMapPure01(): Bool & Impure =
        Nil |> LazyList.toLazy |> LazyList.map(x -> x + 1) |> LazyList.toList == Nil

    @test
    def testMapPure02(): Bool & Impure =
        (1 :: Nil) |> LazyList.toLazy |> LazyList.map(x -> x + 1) |> LazyList.toList == 2 :: Nil

    @test
    def testMapPure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> LazyList.toLazy |> LazyList.map(x -> x + 1) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def testMapPure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.map(x -> x + 1) |> LazyList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testMapImpure01(): Bool & Impure =
        Nil |> LazyList.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == Nil

    @test
    def testMapImpure02(): Bool & Impure =
        (1 :: Nil) |> LazyList.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == 2 :: Nil

    @test
    def testMapImpure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> LazyList.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def testMapImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testMapMap01(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.map(x -> x + 1) |> LazyList.map(x -> x * 2) |> LazyList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def testMapMap02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.map(x -> x * 2) |> LazyList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def testMapMap03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.map(x -> x + 1) |> LazyList.map(x -> x * 2 as & Impure) |> LazyList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def testMapMap04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.map(x -> x * 2 as & Impure) |> LazyList.toList == 4 :: 6 :: 8 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |>
        LazyList.map(x -> {l := "a" :: deref l; x}) |>
        LazyList.map(x -> {l := "b" :: deref l; x}) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |>
        LazyList.map(x -> {l := "a" :: deref l; x} as & Pure) |>
        LazyList.map(x -> {l := "b" :: deref l; x} as & Pure) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def toList01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.toList == Nil

    @test
    pub def toList02(): Bool =
        LazyList.range(-1000, 1000000) |> LazyList.toList == List.range(-1000, 1000000)

    @test
    pub def toList03(): Bool =
        LazyList.range(-1000, 1000000) |> LazyList.take(1000) |> LazyList.toList == List.range(-1000, 0)

    @test
    pub def toList04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.toList == (1 :: 2 :: 3 :: Nil)
}