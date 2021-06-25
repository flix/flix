namespace TestIterator {

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testIsEmpty01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.isEmpty

    @test
    def testIsEmpty02(): Bool & Impure =
        not ((1 :: Nil) |> Iterator.toIter |> Iterator.isEmpty)

    @test
    def testIsEmpty03(): Bool & Impure =
        not ((1 :: 2 :: Nil) |> Iterator.toIter |> Iterator.isEmpty)

    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testCount01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.count(constant(true)) == 0

    @test
    def testCount02(): Bool & Impure =
        (1 :: Nil) |> Iterator.toIter |> Iterator.count(constant(true)) == 1

    @test
    def testCount03(): Bool & Impure =
        (1 :: 2 :: Nil) |> Iterator.toIter |> Iterator.count(constant(true)) == 2

    @test
    def testCount04(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.count(constant(false)) == 0

    @test
    def testCount05(): Bool & Impure =
        (1 :: Nil) |> Iterator.toIter |> Iterator.count(constant(false)) == 0

    @test
    def testCount06(): Bool & Impure =
        (1 :: 2 :: Nil) |> Iterator.toIter |> Iterator.count(constant(false)) == 0

    @test
    def testCount07(): Bool & Impure =
        (1 :: 2 :: 3 ::  Nil) |> Iterator.toIter |> Iterator.count(x -> x < 2) == 1


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testDrop01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.drop(0) |> Iterator.isEmpty

    @test
    def testDrop02(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.drop(0) |> Iterator.toList) == 1 :: 2 :: 3 :: Nil

    @test
    def testDrop03(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.drop(1) |> Iterator.toList) == 2 :: 3 :: Nil

    @test
    def testDrop04(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.drop(2) |> Iterator.toList) == 3 :: Nil

    @test
    def testDrop05(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.drop(3) |> Iterator.toList) == Nil

    @test
    def testDrop06(): Bool & Impure =
        ((1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.drop(1000) |> Iterator.toList) == Nil

    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testTake01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.take(0) |> Iterator.isEmpty

    @test
    def testTake02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.take(0) |> Iterator.toList == Nil

    @test
    def testTake03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.take(1) |> Iterator.toList == 1 :: Nil

    @test
    def testTake04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.take(2) |> Iterator.toList == 1 :: 2 :: Nil

    @test
    def testTake05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.take(3) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def testTake06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.take(1000) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testMapPure01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.map(x -> x + 1) |> Iterator.toList == Nil

    @test
    def testMapPure02(): Bool & Impure =
        (1 :: Nil) |> Iterator.toIter |> Iterator.map(x -> x + 1) |> Iterator.toList == 2 :: Nil

    @test
    def testMapPure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> Iterator.toIter |> Iterator.map(x -> x + 1) |> Iterator.toList == 2 :: 3 :: Nil

    @test
    def testMapPure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.map(x -> x + 1) |> Iterator.toList == 2 :: 3 :: 4 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                              //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testMapImpure01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.map(x -> x + 1 as & Impure) |> Iterator.toList == Nil

    @test
    def testMapImpure02(): Bool & Impure =
        (1 :: Nil) |> Iterator.toIter |> Iterator.map(x -> x + 1 as & Impure) |> Iterator.toList == 2 :: Nil

    @test
    def testMapImpure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> Iterator.toIter |> Iterator.map(x -> x + 1 as & Impure) |> Iterator.toList == 2 :: 3 :: Nil

    @test
    def testMapImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.map(x -> x + 1 as & Impure) |> Iterator.toList == 2 :: 3 :: 4 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testMapMap01(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.map(x -> x + 1) |> Iterator.map(x -> x * 2) |> Iterator.toList == 4 :: 6 :: 8 :: Nil

    @test
    def testMapMap02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.map(x -> x + 1 as & Impure) |> Iterator.map(x -> x * 2) |> Iterator.toList == 4 :: 6 :: 8 :: Nil

    @test
    def testMapMap03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.map(x -> x + 1) |> Iterator.map(x -> x * 2 as & Impure) |> Iterator.toList == 4 :: 6 :: 8 :: Nil

    @test
    def testMapMap04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.map(x -> x + 1 as & Impure) |> Iterator.map(x -> x * 2 as & Impure) |> Iterator.toList == 4 :: 6 :: 8 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map(x -> {l := "a" :: deref l; x}) |>
        Iterator.map(x -> {l := "b" :: deref l; x}) |>
        Iterator.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map(x -> {l := "a" :: deref l; x} as & Pure) |>
        Iterator.map(x -> {l := "b" :: deref l; x} as & Pure) |>
        Iterator.toList;
        List.reverse(deref l) == ( "a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFilterPure01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.filter(x -> x > 100) |> Iterator.toList == Nil

    @test
    def testFilterPure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 0) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def testFilterPure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 1) |> Iterator.toList == 2 :: 3 :: Nil

    @test
    def testFilterPure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 2) |> Iterator.toList == 3 :: Nil

    @test
    def testFilterPure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 3) |> Iterator.toList == Nil

    @test
    def testFilterPure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 100) |> Iterator.toList == Nil

    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFilterImpure01(): Bool & Impure =
        Nil |> Iterator.toIter |> Iterator.filter(x -> x > 100 as & Impure) |> Iterator.toList == Nil

    @test
    def testFilterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 0 as & Impure) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def testFilterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 1 as & Impure) |> Iterator.toList == 2 :: 3 :: Nil

    @test
    def testFilterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 2 as & Impure) |> Iterator.toList == 3 :: Nil

    @test
    def testFilterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 3 as & Impure) |> Iterator.toList == Nil

    @test
    def testFilterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 100 as & Impure) |> Iterator.toList == Nil

    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFilterFilter01(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 1) |> Iterator.filter(x -> x < 3) |> Iterator.toList == 2 :: Nil

    @test
    def testFilterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 1 as & Impure) |> Iterator.filter(x -> x < 3) |> Iterator.toList == 2 :: Nil

    @test
    def testFilterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 1) |> Iterator.filter(x -> x < 3 as & Impure) |> Iterator.toList == 2 :: Nil

    @test
    def testFilterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.filter(x -> x > 1 as & Impure) |> Iterator.filter(x -> x < 3 as & Impure) |> Iterator.toList == 2 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.filter(_x -> {l := "a" :: deref l; true}) |>
        Iterator.filter(_x -> {l := "b" :: deref l; true}) |>
        Iterator.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.filter(_x -> {l := "a" :: deref l; true} as & Pure) |>
        Iterator.filter(_x -> {l := "b" :: deref l; true} as & Pure) |>
        Iterator.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testMapFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map(    x -> {l := "a" :: deref l; x}) |>
        Iterator.filter(_x -> {l := "b" :: deref l; true}) |>
        Iterator.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testMapFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map(    x -> {l := "a" :: deref l; x}    as & Pure) |>
        Iterator.filter(_x -> {l := "b" :: deref l; true} as & Pure) |>
        Iterator.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFilterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.filter(_x -> {l := "a" :: deref l; true}) |>
        Iterator.map(    x -> {l := "b" :: deref l; x}) |>
        Iterator.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def testFilterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.filter(_x -> {l := "a" :: deref l; true} as & Pure) |>
        Iterator.map(    x -> {l := "b" :: deref l; x}    as & Pure) |>
        Iterator.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

}
