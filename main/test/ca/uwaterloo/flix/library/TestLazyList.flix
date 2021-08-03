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