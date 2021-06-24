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
        let r = ref 0;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map(x -> {r := deref r + x; x}) |>
        Iterator.map(x -> {r := deref r * 2; x});
        deref r == (1 + 2 + 3) * 2 * 2 * 2

    @test
    def testMapFusion02(): Bool & Impure =
        let r = ref 0;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map(x -> {r := deref r + x; x}) |>
        Iterator.map(x -> {r := deref r * 2; x}) |>
        Iterator.map(identity);
        deref r == (1 + 2 + 3) * 2 * 2 * 2

    @test
    def testMapFusion03(): Bool & Impure =
        let r = ref 0;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map(x -> {r := deref r + x; x}) |>
        Iterator.map(x -> {r := deref r * 2; x}) |>
        Iterator.toList;
        deref r == (1 + 2 + 3) * 2 * 2 * 2

    @test
    def testMapFusion04(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map(x -> {l := "a" :: deref l; x}) |>
        Iterator.map(x -> {l := "b" :: deref l; x}) |>
        Iterator.toList;
        deref l == ("b" :: "b" :: "b" :: "a" :: "a" :: "a" :: Nil)

    @test
    def testMapFusion05(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map(x -> {l := "a" :: deref l; x} as & Pure) |>
        Iterator.map(x -> {l := "b" :: deref l; x} as & Pure) |>
        Iterator.toList;
        deref l == ("b" :: "a" :: "b" :: "a" :: "b" :: "a" :: Nil)

}
