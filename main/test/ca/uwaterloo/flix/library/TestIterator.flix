namespace TestIterator {

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testHead01(): Bool & Impure =
        (Nil: List[Int32] |> Iterator.toIter |> Iterator.head) == None

    @test
    def testHead02(): Bool & Impure =
        ((1 :: 2 :: Nil): List[Int32] |> Iterator.toIter |> Iterator.head) == Some(1)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool & Impure =
        Iterator.range(0, 0) |> Iterator.isEmpty

    @test
    def range02(): Bool & Impure =
        Iterator.range(10, 0) |> Iterator.isEmpty

    @test
    def range03(): Bool & Impure =
        let iter = Iterator.range(1, 4);
        let one = Iterator.head(iter);
        let two = Iterator.head(iter);
        let three = Iterator.head(iter);
        Some(1) == one and
        Some(2) == two and
        Some(3) == three and
        None == Iterator.head(iter)

    @test
    def range04(): Bool & Impure =
        let iter = Iterator.range(-1, 2);
        let minusOne = Iterator.head(iter);
        let zero = Iterator.head(iter);
        let one = Iterator.head(iter);
        Some(-1) == minusOne and
        Some(0) == zero and
        Some(1) == one and
        None == Iterator.head(iter)

    @test
    def range05(): Bool & Impure =
        (Iterator.range(-100, 200) |> Iterator.toList) == List.range(-100, 200)


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool & Impure =
        Iterator.repeat(10, 0) |> Iterator.isEmpty

    @test
    def repeat02(): Bool & Impure =
        Iterator.repeat(2, -100) |> Iterator.isEmpty

    @test
    def repeat03(): Bool & Impure =
        not (Iterator.repeat(2, 100) |> Iterator.isEmpty)

    @test
    def repeat04(): Bool & Impure =
        (Iterator.repeat(1, 3) |> Iterator.toList) == (1 :: 1 :: 1 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool & Impure =
        not (Nil: List[Int32] |> Iterator.toIter |> Iterator.memberOf(1))

    @test
    def memberOf02(): Bool & Impure =
        let iter = Iterator.range(-10, 2);
        iter |> Iterator.memberOf(1) and
        iter |> Iterator.isEmpty

    @test
    def memberOf03(): Bool & Impure =
        let el = "A";
        let iter = Iterator.repeat(el, 5);
        iter |> Iterator.memberOf(el) and
        iter |> Iterator.memberOf(el) and
        iter |> Iterator.memberOf(el) and
        iter |> Iterator.memberOf(el) and
        iter |> Iterator.memberOf(el) and
        iter |> Iterator.isEmpty


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

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFindLeft01(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.find(x -> x > 0) == Some(1)

    @test
    def testFindLeft02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.find(x -> x > 1) == Some(2)

    @test
    def testFindLeft03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.find(x -> x > 2) == Some(3)

    @test
    def testFindLeft04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |> Iterator.find(x -> x > 3) == None

    /////////////////////////////////////////////////////////////////////////////
    // findLeft fusion                                                         //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def testFindLeftFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map( x -> {l := "a" :: deref l; x}) |>
        Iterator.find(x -> {l := "b" :: deref l; x > 1});
        List.reverse(deref l) == "a" :: "a" :: "a" :: "b" :: "b" :: Nil

    @test
    def testFindLeftFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> Iterator.toIter |>
        Iterator.map( x -> {l := "a" :: deref l; x} as & Pure) |>
        Iterator.find(x -> {l := "b" :: deref l; x > 1});
        List.reverse(deref l) == "a" :: "b" :: "a" :: "b" :: Nil

}
