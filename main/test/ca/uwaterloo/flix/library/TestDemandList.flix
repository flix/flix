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


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
    	DemandList.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100, DemandList.empty()) == 100

    @test
    def foldLeft02(): Bool =
    	(1 :: Nil) |> List.toDemandList |> DemandList.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 198

    @test
    def foldLeft03(): Bool =
    	(1 :: 2 :: Nil) |> List.toDemandList |> DemandList.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 196

    @test
    def foldLeft04(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 386

    @test
    def foldLeft05(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.foldLeft((acc, x) -> x :: acc, Nil) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
    	DemandList.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100, DemandList.empty()) == 100

    @test
    def foldRight02(): Bool =
    	(1 :: Nil) |> List.toDemandList |> DemandList.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 198

    @test
    def foldRight03(): Bool =
    	(1 :: 2 :: Nil) |> List.toDemandList |> DemandList.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 194

    @test
    def foldRight04(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 382

    @test
    def foldRight05(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toDemandList |> DemandList.foldRight((x, acc) -> x :: acc, Nil) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        (DemandList.empty()) |> DemandList.take(1000) |> DemandList.isEmpty

    @test
    def take02(): Bool =
        (DemandList.empty()) |> DemandList.take(0) |> DemandList.isEmpty

    @test
    def take03(): Bool =
        (DemandList.range(0, 1000)) |> DemandList.take(0) |> DemandList.isEmpty

    @test
    def take04(): Bool =
        (DemandList.range(0, 1000))
            |> DemandList.take(500)
            |> DemandList.toList == List.range(0, 500)

    @test
    def take05(): Bool =
        (DemandList.range(0, 1000))
            |> DemandList.take(1)
            |> DemandList.toList == List.range(0, 1)


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool =
        Nil |> List.toDemandList |> DemandList.drop(-1) |> DemandList.toList == Nil

    @test
    def drop02(): Bool =
        Nil |> List.toDemandList |> DemandList.drop(0) |> DemandList.toList == Nil

    @test
    def drop03(): Bool =
        Nil |> List.toDemandList |> DemandList.drop(1) |> DemandList.toList == Nil

    @test
    def drop04(): Bool =
        (1 :: Nil) |> List.toDemandList |> DemandList.drop(-1) |>  DemandList.toList == 1 :: Nil

    @test
    def drop05(): Bool =
        (1 :: Nil) |> List.toDemandList |> DemandList.drop(0) |>  DemandList.toList == 1 :: Nil

    @test
    def drop06(): Bool =
        (1 :: Nil) |> List.toDemandList |> DemandList.drop(1) |>  DemandList.toList == Nil

    @test
    def drop07(): Bool =
        (1 :: Nil) |> List.toDemandList |> DemandList.drop(2) |>  DemandList.toList == Nil

    @test
    def drop08(): Bool =
        (1 :: 2 :: Nil) |> List.toDemandList |> DemandList.drop(0) |>  DemandList.toList == 1 :: 2 :: Nil

    @test
    def drop09(): Bool =
        (1 :: 2 :: Nil) |> List.toDemandList |> DemandList.drop(1) |>  DemandList.toList == 2 :: Nil

    @test
    def drop10(): Bool =
        (1 :: 2 :: Nil) |> List.toDemandList |> DemandList.drop(2) |>  DemandList.toList == Nil

    @test
    def drop11(): Bool =
        (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> List.toDemandList |> DemandList.drop(2) |>  DemandList.toList == 3 :: 4 :: 5 :: 6 :: Nil

    @test
    def drop12(): Bool =
        (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> List.toDemandList |> DemandList.drop(4) |>  DemandList.toList == 5 :: 6 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
        DemandList.append(List.toDemandList(Nil: List[Unit]), List.toDemandList(Nil)) |> DemandList.toList == Nil

    @test
    def append02(): Bool =
        DemandList.append(List.toDemandList(Nil), List.toDemandList(1 :: Nil)) |> DemandList.toList == 1 :: Nil

    @test
    def append03(): Bool =
        DemandList.append(List.toDemandList(Nil), List.toDemandList(1 :: 2 :: Nil)) |> DemandList.toList == 1 :: 2 :: Nil

    @test
    def append04(): Bool =
        DemandList.append(List.toDemandList(1 :: Nil), List.toDemandList(Nil)) |> DemandList.toList == 1 :: Nil

    @test
    def append05(): Bool =
        DemandList.append(List.toDemandList(1 :: 2 :: Nil), List.toDemandList(Nil)) |> DemandList.toList == 1 :: 2 :: Nil

    @test
    def append06(): Bool =
        DemandList.append(List.toDemandList(1 :: Nil), List.toDemandList(2 :: Nil)) |> DemandList.toList == 1 :: 2 :: Nil

    @test
    def append07(): Bool =
        DemandList.append(List.toDemandList(1 :: 2 :: Nil), List.toDemandList(3 :: Nil)) |> DemandList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def append08(): Bool =
        DemandList.append(List.toDemandList(1 :: Nil), List.toDemandList(2 :: 3 :: Nil)) |> DemandList.toList == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool =
        DemandList.repeat(1) |> DemandList.take(-1) |> DemandList.toList == Nil

    @test
    def repeat02(): Bool =
        DemandList.repeat(1) |> DemandList.take(0) |> DemandList.toList == Nil

    @test
    def repeat03(): Bool =
        DemandList.repeat(1) |> DemandList.take(1) |> DemandList.toList == 1 :: Nil

    @test
    def repeat04(): Bool =
        DemandList.repeat(1) |> DemandList.take(2) |> DemandList.toList == 1 :: 1 :: Nil

    @test
    def repeat05(): Bool =
        DemandList.repeat(1) |> DemandList.take(3) |> DemandList.toList == 1 :: 1 :: 1 :: Nil

    @test
    def repeat07(): Bool =
        DemandList.repeat(3) |> DemandList.take(3) |> DemandList.toList == 3 :: 3 :: 3 :: Nil

    @test
    def repeat08(): Bool =
        DemandList.repeat("a") |> DemandList.take(3) |> DemandList.toList == "a" :: "a" :: "a" :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapPure01(): Bool =
        DemandList.flatMap(i -> DemandList.range(i, i + 100), DemandList.empty()) |> DemandList.toList == Nil

    @test
    def flatMapPure02(): Bool =
        List.toDemandList(0 :: Nil) |> DemandList.flatMap(i -> DemandList.range(i, i + 100)) |> DemandList.toList == List.range(0, 100)

    @test
    def flatMapPure03(): Bool =
        List.toDemandList(1 :: Nil) |> DemandList.flatMap(i -> DemandList.range(i, i + 2)) |> DemandList.toList == 1 :: 2 :: Nil

    @test
    def flatMapPure04(): Bool =
        List.toDemandList(2 :: Nil) |> DemandList.flatMap(i -> DemandList.range(i, i + 2)) |> DemandList.toList == 2 :: 3 :: Nil

    @test
    def flatMapPure05(): Bool =
        List.toDemandList(1 :: 2 :: Nil) |> DemandList.flatMap(i -> DemandList.range(i, i + 2)) |> DemandList.toList == 1 :: 2 :: 2 :: 3 :: Nil

    @test
    def flatMapPure06(): Bool =
        List.toDemandList(2 :: 3 :: Nil) |> DemandList.flatMap(i -> DemandList.range(i, i + 2)) |> DemandList.toList == 2 :: 3 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapImpure01(): Bool & Impure =
        DemandList.flatMap(i -> DemandList.range(i, i + 100) as & Impure, DemandList.empty()) |> DemandList.toList == Nil

    @test
    def flatMapImpure02(): Bool & Impure =
        List.toDemandList(0 :: Nil) |> DemandList.flatMap(i -> DemandList.range(i, i + 100) as & Impure) |> DemandList.toList == List.range(0, 100)

    @test
    def flatMapImpure03(): Bool & Impure =
        List.toDemandList(1 :: Nil) |> DemandList.flatMap(i -> DemandList.range(i, i + 2) as & Impure) |> DemandList.toList == 1 :: 2 :: Nil

    @test
    def flatMapImpure04(): Bool & Impure =
        List.toDemandList(2 :: Nil) |> DemandList.flatMap(i -> DemandList.range(i, i + 2) as & Impure) |> DemandList.toList == 2 :: 3 :: Nil

    @test
    def flatMapImpure05(): Bool & Impure =
        List.toDemandList(1 :: 2 :: Nil) |> DemandList.flatMap(i -> DemandList.range(i, i + 2) as & Impure) |> DemandList.toList == 1 :: 2 :: 2 :: 3 :: Nil

    @test
    def flatMapImpure06(): Bool & Impure =
        List.toDemandList(2 :: 3 :: Nil) |> DemandList.flatMap(i -> DemandList.range(i, i + 2) as & Impure) |> DemandList.toList == 2 :: 3 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapFlatMap01(): Bool =
        List.toDemandList(1 :: 2 :: 3 :: Nil)                               |>
        DemandList.flatMap(i -> DemandList.repeat(i) |> DemandList.take(i)) |>
        DemandList.flatMap(i -> DemandList.repeat(i) |> DemandList.take(i)) |>
        DemandList.toList == 1 :: 2 :: 2 :: 2 :: 2 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: Nil

    @test
    def flatMapFlatMap02(): Bool & Impure =
        List.toDemandList(1 :: 2 :: 3 :: Nil)                                           |>
        DemandList.flatMap(i -> DemandList.repeat(i) |> DemandList.take(i) as & Impure) |>
        DemandList.flatMap(i -> DemandList.repeat(i) |> DemandList.take(i))             |>
        DemandList.toList == 1 :: 2 :: 2 :: 2 :: 2 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: Nil

    @test
    def flatMapFlatMap03(): Bool & Impure =
        List.toDemandList(1 :: 2 :: 3 :: Nil)                                           |>
        DemandList.flatMap(i -> DemandList.repeat(i) |> DemandList.take(i))             |>
        DemandList.flatMap(i -> DemandList.repeat(i) |> DemandList.take(i) as & Impure) |>
        DemandList.toList == 1 :: 2 :: 2 :: 2 :: 2 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: Nil

    @test
    def flatMapFlatMap04(): Bool & Impure =
        List.toDemandList(1 :: 2 :: 3 :: Nil)                                           |>
        DemandList.flatMap(i -> DemandList.repeat(i) |> DemandList.take(i) as & Impure) |>
        DemandList.flatMap(i -> DemandList.repeat(i) |> DemandList.take(i) as & Impure) |>
        DemandList.toList == 1 :: 2 :: 2 :: 2 :: 2 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap fusion                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapFusion01(): Bool & Impure =
        use DemandList.{DemandList};
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |>
        DemandList.flatMap(i -> { l := "a" :: deref l; DemandList.ECons(i, DemandList.ENil) }) |>
        DemandList.flatMap(i -> { l := "b" :: deref l; DemandList.ECons(i, DemandList.ENil) });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def flatMapFusion02(): Bool & Impure =
        use DemandList.{DemandList};
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toDemandList |>
        DemandList.flatMap(i -> { l := "a" :: deref l; DemandList.ECons(i, DemandList.ENil) } as & Pure) |>
        DemandList.flatMap(i -> { l := "b" :: deref l; DemandList.ECons(i, DemandList.ENil) } as & Pure) |>
        DemandList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        not (DemandList.exists(i -> i > 3, DemandList.empty()))

    @test
    def exists02(): Bool =
        not ((1 :: Nil) |> List.toDemandList |> DemandList.exists(i -> i > 3))

    @test
    def exists03(): Bool =
        (5 :: Nil) |> List.toDemandList |> DemandList.exists(i -> i > 3)

    @test
    def exists04(): Bool =
        not ((1 :: 2 :: Nil) |> List.toDemandList |> DemandList.exists(i -> i > 3))

    @test
    def exists05(): Bool =
        (1 :: 6 :: Nil) |> List.toDemandList |> DemandList.exists(i -> i > 3)

    @test
    def exists06(): Bool =
        (6 :: 1 :: Nil) |> List.toDemandList |> DemandList.exists(i -> i > 3)

    @test
    def exists07(): Bool =
        (16 :: 6 :: Nil) |> List.toDemandList |> DemandList.exists(i -> i > 3)

    @test
    def exists08(): Bool =
        not ((1 :: -9 :: 3 :: Nil) |> List.toDemandList |> DemandList.exists(i -> i > 3))

    @test
    def exists09(): Bool =
        (1 :: 9 :: 3 :: Nil) |> List.toDemandList |> DemandList.exists(i -> i > 3)


    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool =
        DemandList.forall(i -> i > 3, DemandList.empty())

    @test
    def forall02(): Bool =
        not ((1 :: Nil) |> List.toDemandList |> DemandList.forall(i -> i > 3))

    @test
    def forall03(): Bool =
        (5 :: Nil) |> List.toDemandList |> DemandList.forall(i -> i > 3)

    @test
    def forall04(): Bool =
        not ((1 :: 2 :: Nil) |> List.toDemandList |> DemandList.forall(i -> i > 3))

    @test
    def forall05(): Bool =
        not ((1 :: 6 :: Nil) |> List.toDemandList |> DemandList.forall(i -> i > 3))

    @test
    def forall06(): Bool =
        not ((6 :: 1 :: Nil) |> List.toDemandList |> DemandList.forall(i -> i > 3))

    @test
    def forall07(): Bool =
        (16 :: 6 :: Nil) |> List.toDemandList |> DemandList.forall(i -> i > 3)

    @test
    def forall08(): Bool =
        not ((1 :: -9 :: 3 :: Nil) |> List.toDemandList |> DemandList.forall(i -> i > 3))

    @test
    def forall09(): Bool =
        not ((1 :: 9 :: 3 :: Nil) |> List.toDemandList |> DemandList.forall(i -> i > 3))

    @test
    def forall10(): Bool =
        (11 :: 9 :: 31 :: Nil) |> List.toDemandList |> DemandList.forall(i -> i > 3)

}