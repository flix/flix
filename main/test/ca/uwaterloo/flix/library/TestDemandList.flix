namespace TestDemandList {
/* turn into test cases

    pub def ex1(): Int32 =
        let l1 = range(1, 10);              // l1 must be eager
        let l2 = filter(x -> x > 5, l1);    // l2 must be eager
        sum(l2)

    pub def ex2(): Int32 & Impure =
        let l1 = range(1, 10);                          // l1 must be eager
        let l2 = filter(x -> x > 5 as & Impure, l1);    // l2 must be eager
        sum(l2)

    pub def ex3(): Option[Int32] =
        let l1 = range(1, 10);                          // l1 must be lazy (i.e. a free variable and not true).
        let l2 = filter(x -> x > 5, l1);                // l2 must be lazy (i.e. a free variable and not true).
        head(l2)

    pub def ex4(): Option[Int32] & Impure =
        let l1 = range(1, 10);                            // l1 must be eager
        let l2 = filter(x -> x > 5 as & Impure, l1);      // l2 must be eager
        head(l2)
*/

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        DemandList.empty() |> DemandList.isEmpty

    @test
    def isEmpty02(): Bool =
        not (List.toDemandList(1 :: Nil) |> DemandList.isEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // toDemandList & toList                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toDemandList01(): Bool =
        List.toDemandList(Nil) |> DemandList.toList == Nil

    @test
    def toDemandList02(): Bool =
        List.toDemandList(1 :: Nil) |> DemandList.toList == 1 :: Nil

    @test
    def toDemandList03(): Bool =
        List.toDemandList(1 :: 2 :: 3 :: Nil) |> DemandList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toDemandList04(): Bool =
        List.range(-1000, 1000) |>
            List.toDemandList |>
            DemandList.toList == List.range(-1000, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool =
        DemandList.range(0, 0) |> DemandList.isEmpty

    @test
    def range02(): Bool =
        DemandList.range(100, 100) |> DemandList.isEmpty

    @test
    def range03(): Bool =
        DemandList.range(1, 0) |> DemandList.isEmpty

    @test
    def range04(): Bool =
        not (DemandList.range(0, 100) |> DemandList.isEmpty)

    @test
    def range05(): Bool =
        DemandList.range(0, 100) |> DemandList.toList == List.range(0, 100)

    @test
    def range06(): Bool =
        not (DemandList.range(-100, 100) |> DemandList.isEmpty)

    @test
    def range07(): Bool =
        DemandList.range(-100, 100) |> DemandList.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool =
        Nil |> List.toDemandList |> DemandList.map(x -> x + 1) |> DemandList.toList == Nil

    @test
    def mapPure02(): Bool =
        (1 :: Nil) |> List.toDemandList |> DemandList.map(x -> x + 1) |> DemandList.toList == 2 :: Nil

    @test
    def mapPure03(): Bool =
        (1 :: 2 :: Nil) |> List.toDemandList |> DemandList.map(x -> x + 1) |> DemandList.toList == 2 :: 3 :: Nil

    @test
    def mapPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.map(x -> x + 1) |> DemandList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Nil |> List.toDemandList |> DemandList.map(x -> x + 1 as & Impure) |> DemandList.toList == Nil

    @test
    def mapImpure02(): Bool & Impure =
        (1 :: Nil) |> List.toDemandList |> DemandList.map(x -> x + 1 as & Impure) |> DemandList.toList == 2 :: Nil

    @test
    def mapImpure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> List.toDemandList |> DemandList.map(x -> x + 1 as & Impure) |> DemandList.toList == 2 :: 3 :: Nil

    @test
    def mapImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.map(x -> x + 1 as & Impure) |> DemandList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapMap01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.map(x -> x + 1) |> DemandList.map(x -> x * 2) |> DemandList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.map(x -> x + 1 as & Impure) |> DemandList.map(x -> x * 2) |> DemandList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.map(x -> x + 1) |> DemandList.map(x -> x * 2 as & Impure) |> DemandList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.map(x -> x + 1 as & Impure) |> DemandList.map(x -> x * 2 as & Impure) |> DemandList.toList == 4 :: 6 :: 8 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |>
        DemandList.map(x -> { l := "a" :: deref l; x }) |>
        DemandList.map(x -> { l := "b" :: deref l; x });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |>
        DemandList.map(x -> { l := "a" :: deref l; x } as & Pure) |>
        DemandList.map(x -> { l := "b" :: deref l; x } as & Pure) |>
        DemandList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool =
        Nil |> List.toDemandList |> DemandList.filter(x -> x > 100) |> DemandList.toList == Nil

    @test
    def filterPure02(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 0) |> DemandList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterPure03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 1) |> DemandList.toList == 2 :: 3 :: Nil

    @test
    def filterPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 2) |> DemandList.toList == 3 :: Nil

    @test
    def filterPure05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 3) |> DemandList.toList == Nil

    @test
    def filterPure06(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 100) |> DemandList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool & Impure =
        Nil |> List.toDemandList |> DemandList.filter(x -> x > 100 as & Impure) |> DemandList.toList == Nil

    @test
    def filterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 0 as & Impure) |> DemandList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 1 as & Impure) |> DemandList.toList == 2 :: 3 :: Nil

    @test
    def filterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 2 as & Impure) |> DemandList.toList == 3 :: Nil

    @test
    def filterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 3 as & Impure) |> DemandList.toList == Nil

    @test
    def filterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 100 as & Impure) |> DemandList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFilter01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 1) |> DemandList.filter(x -> x < 3) |> DemandList.toList == 2 :: Nil

    @test
    def filterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 1 as & Impure) |> DemandList.filter(x -> x < 3) |> DemandList.toList == 2 :: Nil

    @test
    def filterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 1) |> DemandList.filter(x -> x < 3 as & Impure) |> DemandList.toList == 2 :: Nil

    @test
    def filterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.filter(x -> x > 1 as & Impure) |> DemandList.filter(x -> x < 3 as & Impure) |> DemandList.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |>
        DemandList.filter(_ -> { l := "a" :: deref l; true }) |>
        DemandList.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |>
        DemandList.filter(_x -> { l := "a" :: deref l; true } as & Pure) |>
        DemandList.filter(_x -> { l := "b" :: deref l; true } as & Pure) |>
        DemandList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |>
        DemandList.map(   x -> { l := "a" :: deref l; x }) |>
        DemandList.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |>
        DemandList.map(   x -> { l := "a" :: deref l; x }    as & Pure) |>
        DemandList.filter(_ -> { l := "b" :: deref l; true } as & Pure) |>
        DemandList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |>
        DemandList.filter(_ -> { l := "a" :: deref l; true }) |>
        DemandList.map(   x -> { l := "b" :: deref l; x    });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |>
        DemandList.filter(_ -> { l := "a" :: deref l; true } as & Pure) |>
        DemandList.map(   x -> { l := "b" :: deref l; x    } as & Pure) |>
        DemandList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

}