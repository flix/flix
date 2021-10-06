namespace TestDemandList {

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

}