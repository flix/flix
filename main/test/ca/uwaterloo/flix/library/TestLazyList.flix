namespace TestLazyList {

    /////////////////////////////////////////////////////////////////////////////
    // Eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def eq01(): Bool =
        ENil: LazyList[Unit] == ENil: LazyList[Unit]

    @test
    def eq02(): Bool =
        ECons(1, ENil) == ECons(1, ENil)

    @test
    def eq03(): Bool =
        LCons(1, lazy ENil) == LCons(1, lazy ENil)

    @test
    def eq04(): Bool =
        LList(lazy ECons(1, ENil)) == LList(lazy ECons(1, ENil))

    @test
    def eq05(): Bool =
        ECons(1, ENil) == LCons(1, lazy ENil)

    @test
    def eq06(): Bool =
        LCons(1, lazy ENil) == ECons(1, ENil)

    @test
    def eq07(): Bool =
        ECons(1, ENil) == LList(lazy ECons(1, ENil))

    @test
    def eq08(): Bool =
        LList(lazy ECons(1, ENil)) == ECons(1, ENil)

    @test
    def eq09(): Bool =
        ECons(1, ENil) == LList(lazy LCons(1, lazy ENil))

    @test
    def eq10(): Bool =
        LList(lazy LCons(1, lazy ENil)) == ECons(1, ENil)

    @test
    def eq11(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    def eq12(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @test
    def eq13(): Bool =
        LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    def eq14(): Bool =
        ECons(1, LCons(2, lazy ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @test
    def eq15(): Bool =
        LCons(1, lazy ECons(2, LCons(3, lazy ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    def eq16(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @test
    def eq17(): Bool =
        LList(lazy ECons(1, ECons(2, ECons(3, ENil)))) == LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @test
    def eq18(): Bool =
        LList(lazy ECons(1, ECons(2, ECons(3, ENil)))) == LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @test
    def eq19(): Bool =
        ECons(0, ENil) != ECons(1, ENil)

    @test
    def eq20(): Bool =
        ECons(0, ENil) != LCons(1, lazy ENil)

    @test
    def eq21(): Bool =
        LCons(0, lazy ENil) != ECons(1, ENil)

    @test
    def eq22(): Bool =
        LCons(0, lazy ENil) != LCons(1, lazy ENil)

    @test
    def eq23(): Bool =
        LList(lazy ECons(0, ENil)) != LCons(1, lazy ENil)

    @test
    def eq24(): Bool =
        LCons(0, lazy ENil) != LList(lazy ECons(1, ENil))

    @test
    def eq25(): Bool =
        ECons(0, ENil) != ECons(0, ECons(0, ENil))

    @test
    def eq26(): Bool =
        ECons(0, ENil) != ECons(0, ECons(1, ENil))

    @test
    def eq27(): Bool =
        ECons(0, ENil) != ENil

    @test
    def eq28(): Bool =
        ENil != ECons(0, ENil)

    @test
    def eq29(): Bool =
        LCons(0, lazy ENil) != ENil

    @test
    def eq30(): Bool =
        ENil != LCons(0, lazy ENil)


    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.isEmpty

    @test
    def isEmpty02(): Bool =
        not (LazyList.range(0, 1000) |> LazyList.isEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool =
        LazyList.range(0, 0) |> LazyList.isEmpty

    @test
    def range02(): Bool =
        LazyList.range(100, 100) |> LazyList.isEmpty

    @test
    def range03(): Bool =
        LazyList.range(1, 0) |> LazyList.isEmpty

    @test
    def range04(): Bool =
        not (LazyList.range(0, 100) |> LazyList.isEmpty)

    @test
    def range05(): Bool =
        LazyList.range(0, 100) |> LazyList.toList == List.range(0, 100)

    @test
    def range06(): Bool =
        not (LazyList.range(-100, 100) |> LazyList.isEmpty)

    @test
    def range07(): Bool =
        LazyList.range(-100, 100) |> LazyList.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.head == None

    @test
    def head02(): Bool =
        LazyList.range(0, 1000) |> LazyList.head == Some(0)

    @test
    def head03(): Bool =
        let l = LazyList.range(0, 1000);
        LazyList.head(l) == Some(0)
            and LazyList.head(l) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.take(1000) |> LazyList.isEmpty

    @test
    def take02(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.take(0) |> LazyList.isEmpty

    @test
    def take03(): Bool =
        (LazyList.range(0, 1000)): LazyList[Int32] |> LazyList.take(0) |> LazyList.isEmpty

    @test
    def take04(): Bool =
        (LazyList.range(0, 1000)): LazyList[Int32]
            |> LazyList.take(500)
            |> LazyList.toList == List.range(0, 500)

    @test
    def take05(): Bool =
        (LazyList.range(0, 1000)): LazyList[Int32]
            |> LazyList.take(1)
            |> LazyList.toList == List.range(0, 1)


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool =
        (LazyList.empty()): LazyList[Int32] |> LazyList.length == 0

    @test
    def length02(): Bool =
        LazyList.range(0, 100000) |> LazyList.length == 100000


    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool =
        LazyList.reverse(LazyList.empty(): LazyList[Unit]) |> LazyList.toList == Nil

    @test
    def reverse02(): Bool =
        (1 :: Nil) |> List.toLazy |> LazyList.reverse |> LazyList.toList == 1 :: Nil

    @test
    def reverse03(): Bool =
        (1 :: 2 :: Nil) |> List.toLazy |> LazyList.reverse |> LazyList.toList == 2 :: 1 :: Nil

    @test
    def reverse04(): Bool =
        (1 :: 1 :: Nil) |> List.toLazy |> LazyList.reverse |> LazyList.toList == 1 :: 1 :: Nil

    @test
    def reverse05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.reverse |> LazyList.toList == 3 :: 2 :: 1 :: Nil

    @test
    def reverse06(): Bool =
        (1 :: 2 :: 3 :: 4 :: Nil) |> List.toLazy |> LazyList.reverse |> LazyList.toList == 4 :: 3 :: 2 :: 1 :: Nil

    @test
    def reverse07(): Bool =
        LazyList.range(-100, 1000) |> LazyList.reverse |> LazyList.toList == List.range(-100, 1000) |> List.reverse


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool & Impure =
        Nil |> List.toLazy |> LazyList.map(x -> x + 1) |> LazyList.toList == Nil

    @test
    def mapPure02(): Bool & Impure =
        (1 :: Nil) |> List.toLazy |> LazyList.map(x -> x + 1) |> LazyList.toList == 2 :: Nil

    @test
    def mapPure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> List.toLazy |> LazyList.map(x -> x + 1) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def mapPure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.map(x -> x + 1) |> LazyList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Nil |> List.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == Nil

    @test
    def mapImpure02(): Bool & Impure =
        (1 :: Nil) |> List.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == 2 :: Nil

    @test
    def mapImpure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> List.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def mapImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapMap01(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.map(x -> x + 1) |> LazyList.map(x -> x * 2) |> LazyList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.map(x -> x * 2) |> LazyList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.map(x -> x + 1) |> LazyList.map(x -> x * 2 as & Impure) |> LazyList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.map(x -> x * 2 as & Impure) |> LazyList.toList == 4 :: 6 :: 8 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList.map(x -> {l := "a" :: deref l; x}) |>
        LazyList.map(x -> {l := "b" :: deref l; x}) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList.map(x -> {l := "a" :: deref l; x} as & Pure) |>
        LazyList.map(x -> {l := "b" :: deref l; x} as & Pure) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool & Impure =
        Nil |> List.toLazy |> LazyList.filter(x -> x > 100) |> LazyList.toList == Nil

    @test
    def filterPure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 0) |> LazyList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterPure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 1) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def filterPure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 2) |> LazyList.toList == 3 :: Nil

    @test
    def filterPure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 3) |> LazyList.toList == Nil

    @test
    def filterPure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 100) |> LazyList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool & Impure =
        Nil |> List.toLazy |> LazyList.filter(x -> x > 100 as & Impure) |> LazyList.toList == Nil

    @test
    def filterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 0 as & Impure) |> LazyList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 1 as & Impure) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def filterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 2 as & Impure) |> LazyList.toList == 3 :: Nil

    @test
    def filterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 3 as & Impure) |> LazyList.toList == Nil

    @test
    def filterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 100 as & Impure) |> LazyList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFilter01(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 1) |> LazyList.filter(x -> x < 3) |> LazyList.toList == 2 :: Nil

    @test
    def filterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 1 as & Impure) |> LazyList.filter(x -> x < 3) |> LazyList.toList == 2 :: Nil

    @test
    def filterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 1) |> LazyList.filter(x -> x < 3 as & Impure) |> LazyList.toList == 2 :: Nil

    @test
    def filterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.filter(x -> x > 1 as & Impure) |> LazyList.filter(x -> x < 3 as & Impure) |> LazyList.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList.filter(_x -> {l := "a" :: deref l; true}) |>
        LazyList.filter(_x -> {l := "b" :: deref l; true});
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList.filter(_x -> {l := "a" :: deref l; true} as & Pure) |>
        LazyList.filter(_x -> {l := "b" :: deref l; true} as & Pure) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList.map(    x -> {l := "a" :: deref l; x}) |>
        LazyList.filter(_x -> {l := "b" :: deref l; true});
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList.map(    x -> {l := "a" :: deref l; x}    as & Pure) |>
        LazyList.filter(_x -> {l := "b" :: deref l; true} as & Pure) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList.filter(_x -> {l := "a" :: deref l; true}) |>
        LazyList.map(    x -> {l := "b" :: deref l; x});
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |>
        LazyList.filter(_x -> {l := "a" :: deref l; true} as & Pure) |>
        LazyList.map(    x -> {l := "b" :: deref l; x}    as & Pure) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    
    /////////////////////////////////////////////////////////////////////////////    
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, ENil) == 100

    @test
    def foldLeft02(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy ENil)) == 100

    @test
    def foldLeft03(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ENil)) == 198

    @test
    def foldLeft04(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy ECons(1, LList(lazy ENil)))) == 198

    @test
    def foldLeft05(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, ECons(1, LList(lazy ENil))) == 198

    @test
    def foldLeft06(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy LList(lazy ENil))) == 198

    @test
    def foldLeft07(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == 196

    @test
    def foldLeft08(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == 196

    @test
    def foldLeft09(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == 196

    @test
    def foldLeft10(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, ECons(1, LCons(2, lazy ENil))) == 196

    @test
    def foldLeft11(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 386

    @test
    def foldLeft12(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 386

    @test
    def foldLeft13(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == 386

    @test
    def foldLeft14(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 386

    @test
    def foldLeft15(): Bool =
    	LazyList.foldLeft((acc, x) -> x :: acc, Nil, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 3 :: 2 :: 1 :: Nil

    @test
    def foldLeft16(): Bool =
    	LazyList.foldLeft((acc, x) -> x :: acc, Nil, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 3 :: 2 :: 1 :: Nil

    @test
    def foldLeft17(): Bool =
    	LazyList.foldLeft((acc, x) -> x :: acc, Nil, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == 3 :: 2 :: 1 :: Nil

    @test
    def foldLeft18(): Bool =
    	LazyList.foldLeft((acc, x) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, ENil) == 100

    @test
    def foldRight02(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy ENil)) == 100

    @test
    def foldRight03(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ENil)) == 198

    @test
    def foldRight04(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy ECons(1, LList(lazy ENil)))) == 198

    @test
    def foldRight05(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, ECons(1, LList(lazy ENil))) == 198

    @test
    def foldRight06(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy LList(lazy ENil))) == 198

    @test
    def foldRight07(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == 194

    @test
    def foldRight08(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == 194

    @test
    def foldRight09(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LCons(2, lazy ENil)))) == 194

    @test
    def foldRight10(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, ECons(1, LCons(2, lazy ENil))) == 194

    @test
    def foldRight11(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 382

    @test
    def foldRight12(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 382

    @test
    def foldRight13(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == 382

    @test
    def foldRight14(): Bool =
    	LazyList.foldRight((e, i) -> (i - e)*(e % 2 + 1), 100, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 382

    @test
    def foldRight15(): Bool =
    	LazyList.foldRight((x, acc) -> x :: acc, Nil, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 1 :: 2 :: 3 :: Nil

    @test
    def foldRight16(): Bool =
    	LazyList.foldRight((x, acc) -> x :: acc, Nil, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 1 :: 2 :: 3 :: Nil

    @test
    def foldRight17(): Bool =
    	LazyList.foldRight((x, acc) -> x :: acc, Nil, LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == 1 :: 2 :: 3 :: Nil

    @test
    def foldRight18(): Bool =
    	LazyList.foldRight((x, acc) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure =
    	LazyList.toArray(ENil: LazyList[Unit]) == []

    @test
    def toArray02(): Bool & Impure =
    	LazyList.toArray(LList(lazy ENil): LazyList[Unit]) == []

    @test
    def toArray03(): Bool & Impure =
    	LazyList.range(-100, 1000) |> LazyList.toArray == Array.range(-100, 1000)

    @test
    def toArray04(): Bool & Impure =
    	LazyList.toArray(LCons(1, lazy ENil)) == [1]

    @test
    def toArray05(): Bool & Impure =
    	LazyList.toArray(LList(lazy ECons(1, LList(lazy ENil)))) == [1]

    @test
    def toArray06(): Bool & Impure =
    	LazyList.toArray(ECons(1, LList(lazy ENil))) == [1]

    @test
    def toArray07(): Bool & Impure =
    	LazyList.toArray(LCons(1, lazy LList(lazy ENil))) == [1]

    @test
    def toArray08(): Bool & Impure =
    	LazyList.toArray(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == [1, 2, 3]

    @test
    def toArray09(): Bool & Impure =
    	LazyList.toArray(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == [1, 2, 3]

    @test
    def toArray10(): Bool & Impure =
    	LazyList.toArray(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == [1, 2, 3]

    @test
    def toArray11(): Bool & Impure =
    	LazyList.toArray(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == [1, 2, 3]


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool =
    	LazyList.toList(ENil): List[Unit] == Nil

    @test
    def toList02(): Bool =
    	LazyList.toList(LList(lazy ENil)): List[Unit] == Nil

    @test
    def toList03(): Bool =
        LazyList.range(-1000, 1000) |> LazyList.toList == List.range(-1000, 1000)

    @test
    def toList04(): Bool =
        LazyList.range(-1000, 1000000) |> LazyList.take(1000) |> LazyList.toList == List.range(-1000, 0)

    @test
    pub def toList05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazy |> LazyList.toList == (1 :: 2 :: 3 :: Nil)

    @test
    def toList06(): Bool =
    	LazyList.toList(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 1 :: 2 :: 3 :: Nil

    @test
    def toList07(): Bool =
    	LazyList.toList(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 1 :: 2 :: 3 :: Nil

    @test
    def toList08(): Bool =
    	LazyList.toList(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == 1 :: 2 :: 3 :: Nil

    @test
    def toList09(): Bool =
    	LazyList.toList(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil

    @test
    def toList10(): Bool =
    	LazyList.toList(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = LazyList.toMap(ENil: LazyList[(Unit, Unit)]) == Map#{}

    @test
    def toMap02(): Bool = LazyList.toMap(LList(lazy ENil): LazyList[(Unit, Unit)]) == Map#{}

    @test
    def toMap03(): Bool = LazyList.toMap(ECons((1, true), ENil)) == Map#{1 -> true}

    @test
    def toMap04(): Bool = LazyList.toMap(LCons((1, true), lazy ENil)) == Map#{1 -> true}

    @test
    def toMap05(): Bool = LazyList.toMap(LList(lazy ECons((1, true), ENil))) == Map#{1 -> true}

    @test
    def toMap06(): Bool = LazyList.toMap(LList(lazy LCons((1, true), lazy ENil))) == Map#{1 -> true}

    // ECons / LCons combinations
    @test
    def toMap07(): Bool = LazyList.toMap(ECons((1, true), ECons((2, false), ENil))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap08(): Bool = LazyList.toMap(LCons((1, true), lazy ECons((2, false), ENil))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap09(): Bool = LazyList.toMap(ECons((1, true), LCons((2, false), lazy ENil))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap10(): Bool = LazyList.toMap(LCons((1, true), lazy LCons((2, false), lazy ENil))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap11(): Bool = LazyList.toMap(LList(lazy LCons((1, true), lazy LList(lazy ECons((2, false), ENil))))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap12(): Bool = LazyList.toMap(LList(lazy ECons((1, true), LList(lazy LCons((2, false), lazy ENil))))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap13(): Bool = LazyList.toMap(LList(lazy LCons((1, true), lazy LList(lazy LCons((2, false), lazy ENil))))) == Map#{1 -> true, 2 -> false}

    @test
    def toMap14(): Bool = LazyList.toMap(ECons((1, true), ECons((1, false), ENil))) == Map#{1 -> false}


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool =
    	LazyList.toSet(ENil): Set[Unit] == Set.empty()

    @test
    def toSet02(): Bool =
    	LazyList.toSet(LList(lazy ENil)): Set[Unit] == Set.empty()

    @test
    def toSet03(): Bool =
        LazyList.range(-1000, 1000) |> LazyList.toSet == Set.range(-1000, 1000)

    @test
    def toSet04(): Bool =
        LazyList.range(-1000, 1000000) |> LazyList.take(1000) |> LazyList.toSet == Set.range(-1000, 0)

    @test
    def toSet05(): Bool =
    	LazyList.toSet(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == Set#{1, 2, 3}

    @test
    def toSet06(): Bool =
    	LazyList.toSet(LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == Set#{1, 2, 3}

    @test
    def toSet07(): Bool =
    	LazyList.toSet(LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil)))))) == Set#{1, 2, 3}

    @test
    def toSet08(): Bool =
    	LazyList.toSet(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == Set#{1, 2, 3}

    @test
    def toSet09(): Bool =
    	LazyList.toSet(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == Set#{1, 2, 3}

    @test
    def toSet10(): Bool =
    	LazyList.toSet(ECons(1, LCons(2, lazy ECons(3, ENil)))) == Set#{1, 2, 3}
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
    	LazyList.append(ENil: LazyList[Unit], ENil: LazyList[Unit]) == ENil

    @test
    def append02(): Bool =
    	LazyList.append(LList(lazy ENil): LazyList[Unit], ENil: LazyList[Unit]) == ENil

    @test
    def append03(): Bool =
    	LazyList.append(ENil, LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def append04(): Bool =
    	LazyList.append(LList(lazy ENil), LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def append05(): Bool =
    	LazyList.append(LCons(1, lazy ENil), LList(lazy ENil)) == ECons(1, ENil)

    @test
    def append06(): Bool =
    	LazyList.append(LList(lazy ECons(1, LList(lazy ENil))), LList(lazy ENil)) == ECons(1, ENil)

    @test
    def append07(): Bool =
    	LazyList.append(ECons(1, LList(lazy ENil)), LList(lazy ENil)) == ECons(1, ENil)

    @test
    def append08(): Bool =
    	LazyList.append(LCons(1, lazy ENil), ENil) == ECons(1, ENil)

    @test
    def append09(): Bool =
    	LazyList.append(ENil, LCons(1, lazy ENil)) == ECons(1, ENil)

    @test
    def append10(): Bool =
    	LazyList.append(ENil, LList(lazy ECons(1, LList(lazy ENil)))) == ECons(1, ENil)

    @test
    def append11(): Bool =
    	LazyList.append(LList(lazy ENil), ECons(1, LList(lazy ENil))) == ECons(1, ENil)

    @test
    def append12(): Bool =
    	LazyList.append(LList(lazy ENil), LCons(1, lazy LList(lazy ENil))) == ECons(1, ENil)

    @test
    def append13(): Bool =
    	LazyList.append(ECons(1, ECons(2, ECons(3, ENil))), ECons(4, LList(lazy ENil))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))

    @test
    def append14(): Bool =
    	LazyList.append(ECons(1, ECons(2, ECons(3, ENil))), LCons(4, lazy LList(lazy ENil))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))

    @test
    def append15(): Bool =
    	LazyList.append(ECons(4, ENil), ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(4, ECons(1, ECons(2, ECons(3, ENil))))

    @test
    def append16(): Bool =
    	LazyList.append(ECons(4, ENil), LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == ECons(4, ECons(1, ECons(2, ECons(3, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        LazyList.count(x -> x > 3, ENil) == 0

    @test
    def count02(): Bool =
        LazyList.count(x -> x > 3, LList(lazy ENil)) == 0

    @test
    def count03(): Bool =
    	LazyList.count(x -> x > 3, LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy ENil))))))) == 0

    @test
    def count04(): Bool =
    	LazyList.count(x -> x > 3, LCons(1, lazy LCons(1, lazy LCons(1, lazy ENil)))) == 0

    @test
    def count05(): Bool =
    	LazyList.count(x -> x > 3, ECons(4, ENil)) == 1

    @test
    def count06(): Bool =
    	LazyList.count(x -> x > 3, LList(lazy LCons(4, lazy ENil))) == 1

    @test
    def count07(): Bool =
        LazyList.count(_ -> true, LazyList.range(1000, 2000)) == 1000

    @test
    def count08(): Bool =
        LazyList.count(_ -> false, LazyList.range(1000, 2000)) == 0

    @test
    def count09(): Bool =
        LazyList.count(x -> x > 3, ECons(1, ECons(2, ENil))) == 0

    @test
    def count10(): Bool =
        LazyList.count(x -> x > 3, ECons(1, ECons(8, ENil))) == 1

    @test
    def count11(): Bool =
        LazyList.count(x -> x > 3, ECons(8, ECons(1, ENil))) == 1


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool =
        LazyList.drop(-1, ENil: LazyList[Unit]) == ENil

    @test
    def drop02(): Bool =
        LazyList.drop(0, ENil: LazyList[Unit]) == ENil

    @test
    def drop03(): Bool =
        LazyList.drop(-1, LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def drop04(): Bool =
        LazyList.drop(0, LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def drop05(): Bool =
        LazyList.drop(1, ENil: LazyList[Unit]) == ENil

    @test
    def drop06(): Bool =
        LazyList.drop(1, LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def drop07(): Bool =
        LazyList.drop(-1, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def drop08(): Bool =
        LazyList.drop(0, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def drop09(): Bool =
        LazyList.drop(1, ECons(1, ENil)) == ENil

    @test
    def drop10(): Bool =
    	LazyList.drop(1, LCons(1, lazy ENil)) == ENil

    @test
    def drop11(): Bool =
    	LazyList.drop(1, LList(lazy ECons(1, LList(lazy ENil)))) == ENil

    @test
    def drop12(): Bool =
        LazyList.drop(2, ECons(1, ENil)) == ENil

    @test
    def drop13(): Bool =
    	LazyList.drop(2, LCons(1, lazy ENil)) == ENil

    @test
    def drop14(): Bool =
    	LazyList.drop(2, LList(lazy ECons(1, LList(lazy ENil)))) == ENil

    @test
    def drop15(): Bool =
    	LazyList.range(0, 1000) |> LazyList.drop(500) == LazyList.range(500, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool =
        LazyList.flatten(ENil: LazyList[LazyList[Unit]]) == ENil

    @test
    def flatten02(): Bool =
        LazyList.flatten(LList(lazy ENil): LazyList[LazyList[Unit]]) == ENil

    @test
    def flatten03(): Bool =
        LazyList.flatten(ECons(ENil, ENil)): LazyList[Unit] == ENil

    @test
    def flatten04(): Bool =
        LazyList.flatten(ECons(LList(lazy ENil), ENil)): LazyList[Unit] == ENil

    @test
    def flatten05(): Bool =
        LazyList.flatten(LList(lazy ECons(ENil, ENil))): LazyList[Unit] == ENil

    @test
    def flatten06(): Bool =
        LazyList.flatten(ECons(ENil, LList(lazy ENil))): LazyList[Unit] == ENil

    @test
    def flatten07(): Bool =
        LazyList.flatten(ECons(ENil, ECons(ENil, ENil))): LazyList[Unit] == ENil

    @test
    def flatten08(): Bool =
    	LazyList.flatten(ECons(LCons(1, lazy ENil), ENil)) == ECons(1, ENil)

    @test
    def flatten09(): Bool =
    	LazyList.flatten(ECons(LList(lazy ECons(1, LList(lazy ENil))), ENil)) == ECons(1, ENil)

    @test
    def flatten10(): Bool =
    	LazyList.flatten(ECons(ECons(1, LList(lazy ENil)), ENil)) == ECons(1, ENil)

    @test
    def flatten11(): Bool =
    	LazyList.flatten(ECons(LCons(1, lazy LList(lazy ENil)), ENil)) == ECons(1, ENil)

    @test
    def flatten12(): Bool =
        LazyList.flatten(ECons((ECons(1, ECons(2, ENil))), ENil)) == ECons(1, ECons(2, ENil))

    @test
    def flatten13(): Bool =
    	LazyList.flatten(ECons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ENil)) == ECons(1, ECons(2, ENil))

    @test
    def flatten14(): Bool =
    	LazyList.flatten(ECons(ENil, ECons(ECons(1, ECons(2, ENil)), ENil))) == ECons(1, ECons(2, ENil))

    @test
    def flatten15(): Bool =
    	LazyList.flatten(ECons(ENil, ECons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ENil))) == ECons(1, ECons(2, ENil))

    @test
    def flatten16(): Bool =
    	LazyList.flatten(ECons(ENil, ECons(LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))), ENil))) == ECons(1, ECons(2, ENil))

    @test
    def flatten17(): Bool =
    	LazyList.flatten(ECons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ECons((ECons(1, ECons(2, ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @test
    def flatten18(): Bool =
    	LazyList.flatten(ECons(LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))), ECons((ECons(1, ECons(2, ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @test
    def flatten19(): Bool =
    	LazyList.flatten(ECons((ECons(1, ECons(2, ENil))), ECons(LList(lazy LCons(1, lazy LCons(2, lazy ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @test
    def flatten20(): Bool =
    	LazyList.flatten(ECons(ECons(1, LCons(2, lazy ENil)), ECons((ECons(1, ECons(2, ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @test
    def flatten21(): Bool =
    	LazyList.flatten(ECons((ECons(1, ECons(2, ENil))), ECons(ENil, ECons(ENil, ECons(LList(lazy LCons(3, lazy LCons(4, lazy ENil))), ENil))))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))

    @test
    def flatten22(): Bool =
    	LazyList.flatten(ECons(ECons(1, LCons(2, lazy ENil)), ECons(ENil, ECons(ENil, ECons((ECons(3, ECons(4, ENil))), ENil))))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool = LazyList.exists(i -> i > 3, ENil) == false

    @test
    def exists02(): Bool = (1 :: Nil) |> List.toLazy |> LazyList.exists(i -> i > 3) == false

    @test
    def exists03(): Bool = (5 :: Nil) |> List.toLazy |> LazyList.exists(i -> i > 3) == true

    @test
    def exists04(): Bool =  (1 :: 2 :: Nil) |> List.toLazy |> LazyList.exists(i -> i > 3) == false

    @test
    def exists05(): Bool =  (1 :: 6 :: Nil) |> List.toLazy |> LazyList.exists(i -> i > 3) == true

    @test
    def exists06(): Bool =  (6 :: 1 :: Nil) |> List.toLazy |> LazyList.exists(i -> i > 3) == true

    @test
    def exists07(): Bool = (16 :: 6 :: Nil) |> List.toLazy |> LazyList.exists(i -> i > 3) == true

    @test
    def exists08(): Bool = (1 :: -9 :: 3 :: Nil) |> List.toLazy |> LazyList.exists(i -> i > 3) == false

    @test
    def exists09(): Bool = (1 :: 9 :: 3 :: Nil) |> List.toLazy |> LazyList.exists(i -> i > 3) == true


    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool = LazyList.forall(i -> i > 3, ENil) == true

    @test
    def forall02(): Bool = (1 :: Nil) |> List.toLazy |> LazyList.forall(i -> i > 3) == false

    @test
    def forall03(): Bool = (5 :: Nil) |> List.toLazy |> LazyList.forall(i -> i > 3) == true

    @test
    def forall04(): Bool =  (1 :: 2 :: Nil) |> List.toLazy |> LazyList.forall(i -> i > 3) == false

    @test
    def forall05(): Bool =  (1 :: 6 :: Nil) |> List.toLazy |> LazyList.forall(i -> i > 3) == false

    @test
    def forall06(): Bool =  (6 :: 1 :: Nil) |> List.toLazy |> LazyList.forall(i -> i > 3) == false

    @test
    def forall07(): Bool = (16 :: 6 :: Nil) |> List.toLazy |> LazyList.forall(i -> i > 3) == true

    @test
    def forall08(): Bool = (1 :: -9 :: 3 :: Nil) |> List.toLazy |> LazyList.forall(i -> i > 3) == false

    @test
    def forall09(): Bool = (1 :: 9 :: 3 :: Nil) |> List.toLazy |> LazyList.forall(i -> i > 3) == false

    @test
    def forall10(): Bool = (11 :: 9 :: 31 :: Nil) |> List.toLazy |> LazyList.forall(i -> i > 3) == true
}