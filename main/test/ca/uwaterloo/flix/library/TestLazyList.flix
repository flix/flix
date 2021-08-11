namespace TestLazyList {

    /////////////////////////////////////////////////////////////////////////////
    // Eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    pub def eq01(): Bool =
        ENil: LazyList[Unit] == ENil: LazyList[Unit]

    @test
    pub def eq02(): Bool =
        ECons(1, ENil) == ECons(1, ENil)

    @test
    pub def eq03(): Bool =
        LCons(1, lazy ENil) == LCons(1, lazy ENil)

    @test
    pub def eq04(): Bool =
        LList(lazy ECons(1, ENil)) == LList(lazy ECons(1, ENil))

    @test
    pub def eq05(): Bool =
        ECons(1, ENil) == LCons(1, lazy ENil)

    @test
    pub def eq06(): Bool =
        LCons(1, lazy ENil) == ECons(1, ENil)

    @test
    pub def eq07(): Bool =
        ECons(1, ENil) == LList(lazy ECons(1, ENil))

    @test
    pub def eq08(): Bool =
        LList(lazy ECons(1, ENil)) == ECons(1, ENil)

    @test
    pub def eq09(): Bool =
        ECons(1, ENil) == LList(lazy LCons(1, lazy ENil))

    @test
    pub def eq10(): Bool =
        LList(lazy LCons(1, lazy ENil)) == ECons(1, ENil)

    @test
    pub def eq11(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    pub def eq12(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @test
    pub def eq13(): Bool =
        LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    pub def eq14(): Bool =
        ECons(1, LCons(2, lazy ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @test
    pub def eq15(): Bool =
        LCons(1, lazy ECons(2, LCons(3, lazy ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    pub def eq16(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @test
    pub def eq17(): Bool =
        LList(lazy ECons(1, ECons(2, ECons(3, ENil)))) == LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @test
    pub def eq18(): Bool =
        LList(lazy ECons(1, ECons(2, ECons(3, ENil)))) == LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @test
    pub def eq19(): Bool =
        ECons(0, ENil) != ECons(1, ENil)

    @test
    pub def eq20(): Bool =
        ECons(0, ENil) != LCons(1, lazy ENil)

    @test
    pub def eq21(): Bool =
        LCons(0, lazy ENil) != ECons(1, ENil)

    @test
    pub def eq22(): Bool =
        LCons(0, lazy ENil) != LCons(1, lazy ENil)

    @test
    pub def eq23(): Bool =
        LList(lazy ECons(0, ENil)) != LCons(1, lazy ENil)

    @test
    pub def eq24(): Bool =
        LCons(0, lazy ENil) != LList(lazy ECons(1, ENil))

    @test
    pub def eq25(): Bool =
        ECons(0, ENil) != ECons(0, ECons(0, ENil))

    @test
    pub def eq26(): Bool =
        ECons(0, ENil) != ECons(0, ECons(1, ENil))

    @test
    pub def eq27(): Bool =
        ECons(0, ENil) != ENil

    @test
    pub def eq28(): Bool =
        ENil != ECons(0, ENil)

    @test
    pub def eq29(): Bool =
        LCons(0, lazy ENil) != ENil

    @test
    pub def eq30(): Bool =
        ENil != LCons(0, lazy ENil)


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
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testFilterPure01(): Bool & Impure =
        Nil |> LazyList.toLazy |> LazyList.filter(x -> x > 100) |> LazyList.toList == Nil

    @test
    def testFilterPure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 0) |> LazyList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def testFilterPure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 1) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def testFilterPure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 2) |> LazyList.toList == 3 :: Nil

    @test
    def testFilterPure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 3) |> LazyList.toList == Nil

    @test
    def testFilterPure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 100) |> LazyList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testFilterImpure01(): Bool & Impure =
        Nil |> LazyList.toLazy |> LazyList.filter(x -> x > 100 as & Impure) |> LazyList.toList == Nil

    @test
    def testFilterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 0 as & Impure) |> LazyList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def testFilterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 1 as & Impure) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def testFilterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 2 as & Impure) |> LazyList.toList == 3 :: Nil

    @test
    def testFilterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 3 as & Impure) |> LazyList.toList == Nil

    @test
    def testFilterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 100 as & Impure) |> LazyList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testFilterFilter01(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 1) |> LazyList.filter(x -> x < 3) |> LazyList.toList == 2 :: Nil

    @test
    def testFilterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 1 as & Impure) |> LazyList.filter(x -> x < 3) |> LazyList.toList == 2 :: Nil

    @test
    def testFilterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 1) |> LazyList.filter(x -> x < 3 as & Impure) |> LazyList.toList == 2 :: Nil

    @test
    def testFilterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |> LazyList.filter(x -> x > 1 as & Impure) |> LazyList.filter(x -> x < 3 as & Impure) |> LazyList.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |>
        LazyList.filter(_x -> {l := "a" :: deref l; true}) |>
        LazyList.filter(_x -> {l := "b" :: deref l; true}) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |>
        LazyList.filter(_x -> {l := "a" :: deref l; true} as & Pure) |>
        LazyList.filter(_x -> {l := "b" :: deref l; true} as & Pure) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testMapFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |>
        LazyList.map(    x -> {l := "a" :: deref l; x}) |>
        LazyList.filter(_x -> {l := "b" :: deref l; true}) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testMapFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |>
        LazyList.map(    x -> {l := "a" :: deref l; x}    as & Pure) |>
        LazyList.filter(_x -> {l := "b" :: deref l; true} as & Pure) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testFilterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |>
        LazyList.filter(_x -> {l := "a" :: deref l; true}) |>
        LazyList.map(    x -> {l := "b" :: deref l; x}) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testFilterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> LazyList.toLazy |>
        LazyList.filter(_x -> {l := "a" :: deref l; true} as & Pure) |>
        LazyList.map(    x -> {l := "b" :: deref l; x}    as & Pure) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
}