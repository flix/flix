mod TestDelayList {
    use DelayList.{ENil, ECons, LCons, LList}
    use Assert.{assertEq, assertNeq, assertTrue, assertFalse}

    /////////////////////////////////////////////////////////////////////////////
    // Eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def eq01(): Unit \ Assert =
        assertEq(expected = (ENil: DelayList[Unit]), (ENil: DelayList[Unit]))

    @Test
    def eq02(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), ECons(1, ENil))

    @Test
    def eq03(): Unit \ Assert =
        assertEq(expected = LCons(1, lazy ENil), LCons(1, lazy ENil))

    @Test
    def eq04(): Unit \ Assert =
        assertEq(expected = LList(lazy ECons(1, ENil)), LList(lazy ECons(1, ENil)))

    @Test
    def eq05(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), LCons(1, lazy ENil))

    @Test
    def eq06(): Unit \ Assert =
        assertEq(expected = LCons(1, lazy ENil), ECons(1, ENil))

    @Test
    def eq07(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), LList(lazy ECons(1, ENil)))

    @Test
    def eq08(): Unit \ Assert =
        assertEq(expected = LList(lazy ECons(1, ENil)), ECons(1, ENil))

    @Test
    def eq09(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), LList(lazy LCons(1, lazy ENil)))

    @Test
    def eq10(): Unit \ Assert =
        assertEq(expected = LList(lazy LCons(1, lazy ENil)), ECons(1, ENil))

    @Test
    def eq11(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(2, ECons(3, ENil))), ECons(1, ECons(2, ECons(3, ENil))))

    @Test
    def eq12(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(2, ECons(3, ENil))), LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @Test
    def eq13(): Unit \ Assert =
        assertEq(expected = LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))), ECons(1, ECons(2, ECons(3, ENil))))

    @Test
    def eq14(): Unit \ Assert =
        assertEq(expected = ECons(1, LCons(2, lazy ECons(3, ENil))), LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @Test
    def eq15(): Unit \ Assert =
        assertEq(expected = LCons(1, lazy ECons(2, LCons(3, lazy ENil))), ECons(1, ECons(2, ECons(3, ENil))))

    @Test
    def eq16(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(2, ECons(3, ENil))), LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @Test
    def eq17(): Unit \ Assert =
        assertEq(expected = LList(lazy ECons(1, ECons(2, ECons(3, ENil)))), LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))))

    @Test
    def eq18(): Unit \ Assert =
        assertEq(expected = LList(lazy ECons(1, ECons(2, ECons(3, ENil)))), LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))))

    @Test
    def eq19(): Unit \ Assert =
        assertNeq(unexpected = ECons(0, ENil), ECons(1, ENil))

    @Test
    def eq20(): Unit \ Assert =
        assertNeq(unexpected = ECons(0, ENil), LCons(1, lazy ENil))

    @Test
    def eq21(): Unit \ Assert =
        assertNeq(unexpected = LCons(0, lazy ENil), ECons(1, ENil))

    @Test
    def eq22(): Unit \ Assert =
        assertNeq(unexpected = LCons(0, lazy ENil), LCons(1, lazy ENil))

    @Test
    def eq23(): Unit \ Assert =
        assertNeq(unexpected = LList(lazy ECons(0, ENil)), LCons(1, lazy ENil))

    @Test
    def eq24(): Unit \ Assert =
        assertNeq(unexpected = LCons(0, lazy ENil), LList(lazy ECons(1, ENil)))

    @Test
    def eq25(): Unit \ Assert =
        assertNeq(unexpected = ECons(0, ENil), ECons(0, ECons(0, ENil)))

    @Test
    def eq26(): Unit \ Assert =
        assertNeq(unexpected = ECons(0, ENil), ECons(0, ECons(1, ENil)))

    @Test
    def eq27(): Unit \ Assert =
        assertNeq(unexpected = ECons(0, ENil), ENil)

    @Test
    def eq28(): Unit \ Assert =
        assertNeq(unexpected = ENil, ECons(0, ENil))

    @Test
    def eq29(): Unit \ Assert =
        assertNeq(unexpected = LCons(0, lazy ENil), ENil)

    @Test
    def eq30(): Unit \ Assert =
        assertNeq(unexpected = ENil, LCons(0, lazy ENil))


    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Unit \ Assert =
        assertEq(expected = Nil, (DelayList.empty(): DelayList[Int32]) |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert =
        assertEq(expected = true, (DelayList.empty(): DelayList[Int32]) |> DelayList.isEmpty)

    @Test
    def isEmpty02(): Unit \ Assert =
        assertEq(expected = false, DelayList.range(0, 1000) |> DelayList.isEmpty)

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert =
        assertEq(expected = false, (DelayList.empty(): DelayList[Int32]) |> DelayList.nonEmpty)

    @Test
    def nonEmpty02(): Unit \ Assert =
        assertEq(expected = true, DelayList.range(0, 1000) |> DelayList.nonEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Unit \ Assert =
        assertEq(expected = true, DelayList.range(0, 0) |> DelayList.isEmpty)

    @Test
    def range02(): Unit \ Assert =
        assertEq(expected = true, DelayList.range(100, 100) |> DelayList.isEmpty)

    @Test
    def range03(): Unit \ Assert =
        assertEq(expected = true, DelayList.range(1, 0) |> DelayList.isEmpty)

    @Test
    def range04(): Unit \ Assert =
        assertEq(expected = false, DelayList.range(0, 100) |> DelayList.isEmpty)

    @Test
    def range05(): Unit \ Assert =
        assertEq(expected = List.range(0, 100), DelayList.range(0, 100) |> DelayList.toList)

    @Test
    def range06(): Unit \ Assert =
        assertEq(expected = false, DelayList.range(-100, 100) |> DelayList.isEmpty)

    @Test
    def range07(): Unit \ Assert =
        assertEq(expected = List.range(-100, 100), DelayList.range(-100, 100) |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Unit \ Assert =
        assertEq(expected = None, (DelayList.empty(): DelayList[Int32]) |> DelayList.head)

    @Test
    def head02(): Unit \ Assert =
        assertEq(expected = Some(0), DelayList.range(0, 1000) |> DelayList.head)

    @Test
    def head03(): Unit \ Assert =
        let l = DelayList.range(0, 1000);
        assertEq(expected = Some(0), DelayList.head(l));
        assertEq(expected = Some(0), DelayList.head(l))


    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tail01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.tail((ENil: DelayList[Unit])))

    @Test
    def tail02(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.tail((LList(lazy ENil): DelayList[Unit])))

    @Test
    def tail03(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.range(0, 1) |> DelayList.tail)

    @Test
    def tail04(): Unit \ Assert =
    	assertEq(expected = ENil, LCons(1, lazy ENil) |> DelayList.tail)

    @Test
    def tail05(): Unit \ Assert =
    	assertEq(expected = ENil, LList(lazy ECons(1, LList(lazy ENil))) |> DelayList.tail)

    @Test
    def tail06(): Unit \ Assert =
    	assertEq(expected = ENil, ECons(1, LList(lazy ENil)) |> DelayList.tail)

    @Test
    def tail07(): Unit \ Assert =
    	assertEq(expected = ENil, LCons(1, lazy LList(lazy ENil)) |> DelayList.tail)

    @Test
    def tail08(): Unit \ Assert =
    	assertEq(expected = DelayList.range(2, 4), LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))) |> DelayList.tail)

    @Test
    def tail09(): Unit \ Assert =
    	assertEq(expected = DelayList.range(2, 4), LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))) |> DelayList.tail)

    @Test
    def tail10(): Unit \ Assert =
    	assertEq(expected = DelayList.range(2, 4), LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))) |> DelayList.tail)

    @Test
    def tail11(): Unit \ Assert =
    	assertEq(expected = DelayList.range(2, 4), LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> DelayList.tail)

    @Test
    def tail12(): Unit \ Assert =
    	assertEq(expected = DelayList.range(1, 1000), DelayList.range(0, 1000) |> DelayList.tail)


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def take01(): Unit \ Assert =
        assertEq(expected = true, (DelayList.empty(): DelayList[Int32]) |> DelayList.take(1000) |> DelayList.isEmpty)

    @Test
    def take02(): Unit \ Assert =
        assertEq(expected = true, (DelayList.empty(): DelayList[Int32]) |> DelayList.take(0) |> DelayList.isEmpty)

    @Test
    def take03(): Unit \ Assert =
        assertEq(expected = true, (DelayList.range(0, 1000): DelayList[Int32]) |> DelayList.take(0) |> DelayList.isEmpty)

    @Test
    def take04(): Unit \ Assert =
        assertEq(expected = List.range(0, 500), (DelayList.range(0, 1000): DelayList[Int32])
            |> DelayList.take(500)
            |> DelayList.toList)

    @Test
    def take05(): Unit \ Assert =
        assertEq(expected = List.range(0, 1), (DelayList.range(0, 1000): DelayList[Int32])
            |> DelayList.take(1)
            |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Unit \ Assert =
        assertEq(expected = 0, (DelayList.empty(): DelayList[Int32]) |> DelayList.length)

    @Test
    def length02(): Unit \ Assert =
        assertEq(expected = 100000, DelayList.range(0, 100000) |> DelayList.length)


    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert =
        assertEq(expected = 0, (DelayList.empty(): DelayList[Int32]) |> DelayList.size)

    @Test
    def size02(): Unit \ Assert =
        assertEq(expected = 100000, DelayList.range(0, 100000) |> DelayList.size)


    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Unit \ Assert =
        assertEq(expected = Nil, DelayList.reverse((DelayList.empty(): DelayList[Unit])) |> DelayList.toList)

    @Test
    def reverse02(): Unit \ Assert =
        assertEq(expected = 1 :: Nil, (1 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList)

    @Test
    def reverse03(): Unit \ Assert =
        assertEq(expected = 2 :: 1 :: Nil, (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList)

    @Test
    def reverse04(): Unit \ Assert =
        assertEq(expected = 1 :: 1 :: Nil, (1 :: 1 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList)

    @Test
    def reverse05(): Unit \ Assert =
        assertEq(expected = 3 :: 2 :: 1 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList)

    @Test
    def reverse06(): Unit \ Assert =
        assertEq(expected = 4 :: 3 :: 2 :: 1 :: Nil, (1 :: 2 :: 3 :: 4 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList)

    @Test
    def reverse07(): Unit \ Assert =
        assertEq(expected = List.range(-100, 1000) |> List.reverse, DelayList.range(-100, 1000) |> DelayList.reverse |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapPure01(): Unit \ Assert =
        assertEq(expected = Nil, Nil |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList)

    @Test
    def mapPure02(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, (1 :: Nil) |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList)

    @Test
    def mapPure03(): Unit \ Assert =
        assertEq(expected = 2 :: 3 :: Nil, (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList)

    @Test
    def mapPure04(): Unit \ Assert =
        assertEq(expected = 2 :: 3 :: 4 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapImpure01(): Unit \ Assert + IO =
        assertEq(expected = Nil, Nil |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList)

    @Test
    def mapImpure02(): Unit \ Assert + IO =
        assertEq(expected = 2 :: Nil, (1 :: Nil) |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList)

    @Test
    def mapImpure03(): Unit \ Assert + IO =
        assertEq(expected = 2 :: 3 :: Nil, (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList)

    @Test
    def mapImpure04(): Unit \ Assert + IO =
        assertEq(expected = 2 :: 3 :: 4 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapMap01(): Unit \ Assert =
        assertEq(expected = 4 :: 6 :: 8 :: Nil, (1 :: 2 :: 3 :: Nil)         |>
            List.toDelayList          |>
            DelayList.map(x -> x + 1) |>
            DelayList.map(x -> x * 2) |>
            DelayList.toList)

    @Test
    def mapMap02(): Unit \ Assert + IO =
        assertEq(expected = 4 :: 6 :: 8 :: Nil, (1 :: 2 :: 3 :: Nil)                     |>
            List.toDelayList                      |>
            DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |>
            DelayList.map(x -> x * 2)             |>
            DelayList.toList)

    @Test
    def mapMap03(): Unit \ Assert + IO =
        assertEq(expected = 4 :: 6 :: 8 :: Nil, (1 :: 2 :: 3 :: Nil)                     |>
            List.toDelayList                      |>
            DelayList.map(x -> x + 1)             |>
            DelayList.map(x -> unchecked_cast(x * 2 as _ \ IO)) |>
            DelayList.toList)

    @Test
    def mapMap04(): Unit \ Assert + IO =
        assertEq(expected = 4 :: 6 :: 8 :: Nil, (1 :: 2 :: 3 :: Nil)                     |>
            List.toDelayList                      |>
            DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |>
            DelayList.map(x -> unchecked_cast(x * 2 as _ \ IO)) |>
            DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(x -> { Ref.put("a" :: Ref.get(l), l); x }) |>
        DelayList.map(x -> { Ref.put("b" :: Ref.get(l), l); x });
        assertEq(expected = ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    @Test
    def mapFusion02(): Unit \ Assert + IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); x } as _ \ {})) |>
        DelayList.map(x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); x } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        assertEq(expected = ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterPure01(): Unit \ Assert =
        assertEq(expected = Nil, Nil |> List.toDelayList |> DelayList.filter(x -> x > 100) |> DelayList.toList)

    @Test
    def filterPure02(): Unit \ Assert =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 0) |> DelayList.toList)

    @Test
    def filterPure03(): Unit \ Assert =
        assertEq(expected = 2 :: 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 1) |> DelayList.toList)

    @Test
    def filterPure04(): Unit \ Assert =
        assertEq(expected = 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 2) |> DelayList.toList)

    @Test
    def filterPure05(): Unit \ Assert =
        assertEq(expected = Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 3) |> DelayList.toList)

    @Test
    def filterPure06(): Unit \ Assert =
        assertEq(expected = Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 100) |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterImpure01(): Unit \ Assert + IO =
        assertEq(expected = Nil, Nil |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 100 as _ \ IO)) |> DelayList.toList)

    @Test
    def filterImpure02(): Unit \ Assert + IO =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 0 as _ \ IO)) |> DelayList.toList)

    @Test
    def filterImpure03(): Unit \ Assert + IO =
        assertEq(expected = 2 :: 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 1 as _ \ IO)) |> DelayList.toList)

    @Test
    def filterImpure04(): Unit \ Assert + IO =
        assertEq(expected = 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 2 as _ \ IO)) |> DelayList.toList)

    @Test
    def filterImpure05(): Unit \ Assert + IO =
        assertEq(expected = Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 3 as _ \ IO)) |> DelayList.toList)

    @Test
    def filterImpure06(): Unit \ Assert + IO =
        assertEq(expected = Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 100 as _ \ IO)) |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterFilter01(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, (1 :: 2 :: 3 :: Nil)            |>
            List.toDelayList             |>
            DelayList.filter(x -> x > 1) |>
            DelayList.filter(x -> x < 3) |>
            DelayList.toList)

    @Test
    def filterFilter02(): Unit \ Assert + IO =
        assertEq(expected = 2 :: Nil, (1 :: 2 :: 3 :: Nil)                                 |>
            List.toDelayList                                 |>
            DelayList.filter(x -> unchecked_cast(x > 1 as _ \ IO)) |>
            DelayList.filter(x -> x < 3)                     |>
            DelayList.toList)

    @Test
    def filterFilter03(): Unit \ Assert + IO =
        assertEq(expected = 2 :: Nil, (1 :: 2 :: 3 :: Nil)                                 |>
            List.toDelayList                                 |>
            DelayList.filter(x -> x > 1)                     |>
            DelayList.filter(x -> unchecked_cast(x < 3 as _ \ IO)) |>
            DelayList.toList)

    @Test
    def filterFilter04(): Unit \ Assert + IO =
        assertEq(expected = 2 :: Nil, (1 :: 2 :: 3 :: Nil)                                 |>
            List.toDelayList                                 |>
            DelayList.filter(x -> unchecked_cast(x > 1 as _ \ IO)) |>
            DelayList.filter(x -> unchecked_cast(x < 3 as _ \ IO)) |>
            DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList     |>
        DelayList.filter(_ -> { Ref.put("a" :: Ref.get(l), l); true }) |>
        DelayList.filter(_ -> { Ref.put("b" :: Ref.get(l), l); true });
        assertEq(expected = ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    @Test
    def filterFusion02(): Unit \ Assert + IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList  |>
        DelayList.filter(_x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.filter(_x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        assertEq(expected = ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapFilterFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(   x -> { Ref.put("a" :: Ref.get(l), l); x }) |>
        DelayList.filter(_ -> { Ref.put("b" :: Ref.get(l), l); true });
        assertEq(expected = ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    @Test
    def mapFilterFusion02(): Unit \ Assert + IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(   x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); x }    as _ \ {})) |>
        DelayList.filter(_ -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        assertEq(expected = ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterThenMapFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filter(_ -> { Ref.put("a" :: Ref.get(l), l); true }) |>
        DelayList.map(   x -> { Ref.put("b" :: Ref.get(l), l); x    });
        assertEq(expected = ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    @Test
    def filterThenMapFusion02(): Unit \ Assert + IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filter(_ -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.map(   x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); x    } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        assertEq(expected = ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert =
    	assertEq(expected = 100, DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, ENil))

    @Test
    def foldLeft02(): Unit \ Assert =
    	assertEq(expected = 100, DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, LList(lazy ENil)))

    @Test
    def foldLeft03(): Unit \ Assert =
    	assertEq(expected = 198, DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy ENil)))

    @Test
    def foldLeft04(): Unit \ Assert =
    	assertEq(expected = 196, DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))))

    @Test
    def foldLeft05(): Unit \ Assert =
    	assertEq(expected = 386, DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))))

    @Test
    def foldLeft06(): Unit \ Assert =
    	assertEq(expected = 3 :: 2 :: 1 :: Nil, DelayList.foldLeft((acc, x) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))))


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert =
    	assertEq(expected = 100, DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, ENil))

    @Test
    def foldRight02(): Unit \ Assert =
    	assertEq(expected = 100, DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, LList(lazy ENil)))

    @Test
    def foldRight03(): Unit \ Assert =
    	assertEq(expected = 198, DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy LList(lazy ENil))))

    @Test
    def foldRight04(): Unit \ Assert =
    	assertEq(expected = 194, DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, ECons(1, LCons(2, lazy ENil))))

    @Test
    def foldRight05(): Unit \ Assert =
    	assertEq(expected = 382, DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))

    @Test
    def foldRight06(): Unit \ Assert =
    	assertEq(expected = 1 :: 2 :: 3 :: Nil, DelayList.foldRight((x, acc) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))))

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Unit \ Assert =
    	assertEq(expected = 100, DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, ENil))

    @Test
    def foldRightWithCont02(): Unit \ Assert =
    	assertEq(expected = 100, DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, LList(lazy ENil)))

    @Test
    def foldRightWithCont03(): Unit \ Assert =
    	assertEq(expected = 198, DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy LList(lazy ENil))))

    @Test
    def foldRightWithCont04(): Unit \ Assert =
    	assertEq(expected = 194, DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, ECons(1, LCons(2, lazy ENil))))

    @Test
    def foldRightWithCont05(): Unit \ Assert =
    	assertEq(expected = 382, DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))

    @Test
    def foldRightWithCont06(): Unit \ Assert =
    	assertEq(expected = 1 :: 2 :: 3 :: Nil, DelayList.foldRightWithCont((x, k) -> x :: k(), Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))))


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Unit \ Assert =
    	assertEq(expected = 0, DelayList.foldMap(x -> 2 * x, ENil))

    @Test
    def foldMap02(): Unit \ Assert =
    	assertEq(expected = 0, DelayList.foldMap(x -> 2 * x, LList(lazy ENil)))

    @Test
    def foldMap03(): Unit \ Assert =
    	assertEq(expected = 2, DelayList.foldMap(x -> 2 * x, LCons(1, lazy LList(lazy ENil))))

    @Test
    def foldMap04(): Unit \ Assert =
    	assertEq(expected = 6, DelayList.foldMap(x -> 2 * x, ECons(1, LCons(2, lazy ENil))))

    @Test
    def foldMap05(): Unit \ Assert =
    	assertEq(expected = 12, DelayList.foldMap(x -> 2 * x, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))

    @Test
    def foldMap06(): Unit \ Assert =
    	assertEq(expected = "123", DelayList.foldMap(Int32.toString, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))))

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Unit \ Assert = region rc {
        let l = ENil;
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        DelayList.forEach(fn, l);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def forEach02(): Unit \ Assert = region rc {
        let l = LList(lazy ENil);
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        DelayList.forEach(fn, l);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def forEach03(): Unit \ Assert = region rc {
        let l = LCons(1, lazy LList(lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        DelayList.forEach(fn, l);
        assertEq(expected = "T", StringBuilder.toString(sb))
    }

    @Test
    def forEach04(): Unit \ Assert = region rc {
        let l = LCons(-1, lazy LList(lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        DelayList.forEach(fn, l);
        assertEq(expected = "F", StringBuilder.toString(sb))
    }

    @Test
    def forEach05(): Unit \ Assert = region rc {
        let l = ECons(1, LCons(-1, lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        DelayList.forEach(fn, l);
        assertEq(expected = "TF", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Unit \ Assert = region rc {
        let l = ENil;
        let sb = StringBuilder.empty(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        DelayList.forEachWithIndex(fn, l);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex02(): Unit \ Assert = region rc {
        let l = LList(lazy ENil);
        let sb = StringBuilder.empty(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        DelayList.forEachWithIndex(fn, l);
        assertEq(expected = "", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex03(): Unit \ Assert = region rc {
        let l = LCons(1, lazy LList(lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        DelayList.forEachWithIndex(fn, l);
        assertEq(expected = "F", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex04(): Unit \ Assert = region rc {
        let l = LCons(-1, lazy LList(lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        DelayList.forEachWithIndex(fn, l);
        assertEq(expected = "F", StringBuilder.toString(sb))
    }

    @Test
    def forEachWithIndex05(): Unit \ Assert = region rc {
        let l = ECons(1, LCons(-1, lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append('T', sb) else StringBuilder.append('F', sb);
        DelayList.forEachWithIndex(fn, l);
        assertEq(expected = "FT", StringBuilder.toString(sb))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
    	assertTrue(DelayList.toArray(rc, (ENil: DelayList[Unit]))`Array.sameElements` Array#{} @ rc)
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
    	assertTrue(DelayList.toArray(rc, (LList(lazy ENil): DelayList[Unit]))`Array.sameElements` Array#{} @ rc)
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        assertTrue(DelayList.toArray(rc, DelayList.range(-100, 1000))`Array.sameElements` Array.range(rc, -100, 1000))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
    	assertTrue(DelayList.toArray(rc, LCons(1, lazy ENil))`Array.sameElements` Array#{1} @ rc)
    }

    @Test
    def toArray05(): Unit \ Assert = region rc {
    	assertTrue(DelayList.toArray(rc, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))))`Array.sameElements` Array#{1, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert =
        assertTrue(((DelayList.toVector(ENil)) : Vector[Int32]) `Vector.equals` Vector.empty())

    @Test
    def toVector02(): Unit \ Assert =
        assertTrue(DelayList.toVector(LCons(0, lazy ENil)) `Vector.equals` Vector.singleton(0))

    @Test
    def toVector03(): Unit \ Assert =
        assertTrue(DelayList.toVector(DelayList.range(1, 4)) `Vector.equals` Vector.range(1, 4))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert =
    	assertEq(expected = Nil, (DelayList.toList(ENil): List[Unit]))

    @Test
    def toList02(): Unit \ Assert =
    	assertEq(expected = Nil, (DelayList.toList(LList(lazy ENil)): List[Unit]))

    @Test
    def toList03(): Unit \ Assert =
        assertEq(expected = List.range(-1000, 1000), DelayList.range(-1000, 1000) |> DelayList.toList)

    @Test
    def toList04(): Unit \ Assert =
        assertEq(expected = List.range(-1000, 0), DelayList.range(-1000, 1000000) |> DelayList.take(1000) |> DelayList.toList)

    @Test
    def toList05(): Unit \ Assert =
        assertEq(expected = (1 :: 2 :: 3 :: Nil), (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.toList)

    @Test
    def toList06(): Unit \ Assert =
    	assertEq(expected = 1 :: 2 :: 3 :: Nil, DelayList.toList(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))))


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Unit \ Assert = region rc {
    	let l = (ENil: DelayList[Int32]);
        let d1 = DelayList.toMutDeque(rc, l);

        let d2 = MutDeque.empty(rc);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque02(): Unit \ Assert = region rc {
    	let l = (LList(lazy ENil): DelayList[Int32]);
        let d1 = DelayList.toMutDeque(rc, l);

        let d2 = MutDeque.empty(rc);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }

    @Test
    def toMutDeque03(): Unit \ Assert = region rc {
    	let l = LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))));
        let d1 = DelayList.toMutDeque(rc, l);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d2);
        MutDeque.pushBack(2, d2);
        MutDeque.pushBack(3, d2);

        assertTrue(d1 `MutDeque.sameElements` d2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Unit \ Assert = assertEq(expected = Map#{}, DelayList.toMap((ENil: DelayList[(Unit, Unit)])))

    @Test
    def toMap02(): Unit \ Assert = assertEq(expected = Map#{}, DelayList.toMap((LList(lazy ENil): DelayList[(Unit, Unit)])))

    @Test
    def toMap03(): Unit \ Assert = assertEq(expected = Map#{1 => true}, DelayList.toMap(ECons((1, true), ENil)))

    @Test
    def toMap04(): Unit \ Assert = assertEq(expected = Map#{1 => true, 2 => false}, DelayList.toMap(LList(lazy LCons((1, true), lazy LList(lazy LCons((2, false), lazy ENil))))))

    @Test
    def toMap05(): Unit \ Assert = assertEq(expected = Map#{1 => false}, DelayList.toMap(ECons((1, true), ECons((1, false), ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Unit \ Assert =
    	assertEq(expected = Set.empty(), (DelayList.toSet(ENil): Set[Unit]))

    @Test
    def toSet02(): Unit \ Assert =
    	assertEq(expected = Set.empty(), (DelayList.toSet(LList(lazy ENil)): Set[Unit]))

    @Test
    def toSet03(): Unit \ Assert =
        assertEq(expected = Set.range(-1000, 1000), DelayList.range(-1000, 1000) |> DelayList.toSet)

    @Test
    def toSet04(): Unit \ Assert =
        assertEq(expected = Set.range(-1000, 0), DelayList.range(-1000, 1000000) |> DelayList.take(1000) |> DelayList.toSet)

    @Test
    def toSet05(): Unit \ Assert =
    	assertEq(expected = Set#{1, 2, 3}, DelayList.toSet(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))))


    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Unit \ Assert =
    	assertEq(expected = ENil, DelayList.append((ENil: DelayList[Unit]), (ENil: DelayList[Unit])))

    @Test
    def append02(): Unit \ Assert =
    	assertEq(expected = ENil, DelayList.append((LList(lazy ENil): DelayList[Unit]), (ENil: DelayList[Unit])))

    @Test
    def append03(): Unit \ Assert =
    	assertEq(expected = ECons(1, ENil), DelayList.append(ECons(1, ENil), ENil))

    @Test
    def append04(): Unit \ Assert =
    	assertEq(expected = ECons(1, ENil), DelayList.append(ENil, LCons(1, lazy ENil)))

    @Test
    def append05(): Unit \ Assert =
    	assertEq(expected = ECons(1, ECons(2, ECons(3, ECons(4, ENil)))), DelayList.append(ECons(1, ECons(2, ECons(3, ENil))), ECons(4, LList(lazy ENil))))

    @Test
    def append06(): Unit \ Assert =
    	assertEq(expected = ECons(4, ECons(1, ECons(2, ECons(3, ENil)))), DelayList.append(ECons(4, ENil), ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Unit \ Assert =
        assertEq(expected = 0, DelayList.count(x -> x > 3, ENil))

    @Test
    def count02(): Unit \ Assert =
    	assertEq(expected = 0, DelayList.count(x -> x > 3, LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy ENil))))))))

    @Test
    def count03(): Unit \ Assert =
    	assertEq(expected = 1, DelayList.count(x -> x > 3, ECons(4, ENil)))

    @Test
    def count04(): Unit \ Assert =
        assertEq(expected = 1000, DelayList.count(_ -> true, DelayList.range(1000, 2000)))

    @Test
    def count05(): Unit \ Assert =
        assertEq(expected = 0, DelayList.count(_ -> false, DelayList.range(1000, 2000)))

    @Test
    def count06(): Unit \ Assert =
        assertEq(expected = 0, DelayList.count(x -> x > 3, ECons(1, ECons(2, ENil))))

    @Test
    def count07(): Unit \ Assert =
        assertEq(expected = 1, DelayList.count(x -> x > 3, ECons(1, ECons(8, ENil))))

    @Test
    def count08(): Unit \ Assert =
        assertEq(expected = 1, DelayList.count(x -> x > 3, ECons(8, ECons(1, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def drop01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.drop(-1, (ENil: DelayList[Unit])))

    @Test
    def drop02(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.drop(0, (ENil: DelayList[Unit])))

    @Test
    def drop03(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.drop(1, (ENil: DelayList[Unit])))

    @Test
    def drop04(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), DelayList.drop(-1, ECons(1, ENil)))

    @Test
    def drop05(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), DelayList.drop(0, ECons(1, ENil)))

    @Test
    def drop06(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.drop(1, ECons(1, ENil)))

    @Test
    def drop07(): Unit \ Assert =
    	assertEq(expected = ENil, DelayList.drop(1, LList(lazy ECons(1, LList(lazy ENil)))))

    @Test
    def drop08(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.drop(2, ECons(1, ENil)))

    @Test
    def drop09(): Unit \ Assert =
    	assertEq(expected = DelayList.range(500, 1000), DelayList.range(0, 1000) |> DelayList.drop(500))


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.flatten((ENil: DelayList[DelayList[Unit]])))

    @Test
    def flatten02(): Unit \ Assert =
        assertEq(expected = ENil, (DelayList.flatten(ECons(ENil, ENil)): DelayList[Unit]))

    @Test
    def flatten03(): Unit \ Assert =
        assertEq(expected = ENil, (DelayList.flatten(ECons(ENil, ECons(ENil, ENil))): DelayList[Unit]))

    @Test
    def flatten04(): Unit \ Assert =
    	assertEq(expected = ECons(1, ENil), DelayList.flatten(ECons(LCons(1, lazy ENil), ENil)))

    @Test
    def flatten05(): Unit \ Assert =
    	assertEq(expected = ECons(1, ENil), DelayList.flatten(ECons(LList(lazy ECons(1, LList(lazy ENil))), ENil)))

    @Test
    def flatten06(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(2, ENil)), DelayList.flatten(ECons((ECons(1, ECons(2, ENil))), ENil)))

    @Test
    def flatten07(): Unit \ Assert =
    	assertEq(expected = ECons(1, ECons(2, ENil)), DelayList.flatten(ECons(ENil, ECons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ENil))))

    @Test
    def flatten08(): Unit \ Assert =
    	assertEq(expected = ECons(1, ECons(2, ECons(1, ECons(2, ENil)))), DelayList.flatten(ECons(ECons(1, LCons(2, lazy ENil)), ECons((ECons(1, ECons(2, ENil))), ENil))))

    @Test
    def flatten09(): Unit \ Assert =
    	assertEq(expected = ECons(1, ECons(2, ECons(3, ECons(4, ENil)))), DelayList.flatten(ECons((ECons(1, ECons(2, ENil))), ECons(ENil, ECons(ENil, ECons(LList(lazy LCons(3, lazy LCons(4, lazy ENil))), ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert =
        assertFalse(DelayList.exists(i -> i > 3, ENil))

    @Test
    def exists02(): Unit \ Assert =
        assertFalse((1 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3))

    @Test
    def exists03(): Unit \ Assert =
        assertTrue((5 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3))

    @Test
    def exists04(): Unit \ Assert =
        assertFalse((1 :: 2 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3))

    @Test
    def exists05(): Unit \ Assert =
        assertTrue((1 :: 6 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3))

    @Test
    def exists06(): Unit \ Assert =
        assertTrue((6 :: 1 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3))

    @Test
    def exists07(): Unit \ Assert =
        assertTrue((16 :: 6 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3))

    @Test
    def exists08(): Unit \ Assert =
        assertFalse((1 :: -9 :: 3 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3))

    @Test
    def exists09(): Unit \ Assert =
        assertTrue((1 :: 9 :: 3 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3))


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert =
        assertTrue(DelayList.forAll(i -> i > 3, ENil))

    @Test
    def forAll02(): Unit \ Assert =
        assertFalse((1 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3))

    @Test
    def forAll03(): Unit \ Assert =
        assertTrue((5 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3))

    @Test
    def forAll04(): Unit \ Assert =
        assertFalse((1 :: 2 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3))

    @Test
    def forAll05(): Unit \ Assert =
        assertFalse((1 :: 6 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3))

    @Test
    def forAll06(): Unit \ Assert =
        assertFalse((6 :: 1 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3))

    @Test
    def forAll07(): Unit \ Assert =
        assertTrue((16 :: 6 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3))

    @Test
    def forAll08(): Unit \ Assert =
        assertFalse((1 :: -9 :: 3 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3))

    @Test
    def forAll09(): Unit \ Assert =
        assertFalse((1 :: 9 :: 3 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3))

    @Test
    def forAll10(): Unit \ Assert =
        assertTrue((11 :: 9 :: 31 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3))


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.repeat(1) |> DelayList.take(-1))

    @Test
    def repeat02(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.repeat(1) |> DelayList.take(0))

    @Test
    def repeat03(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), DelayList.repeat(1) |> DelayList.take(1))

    @Test
    def repeat04(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(1, ENil)), DelayList.repeat(1) |> DelayList.take(2))

    @Test
    def repeat05(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(1, ECons(1, ENil))), DelayList.repeat(1) |> DelayList.take(3))

    @Test
    def repeat07(): Unit \ Assert =
        assertEq(expected = ECons(3, ECons(3, ECons(3, ENil))), DelayList.repeat(3) |> DelayList.take(3))

    @Test
    def repeat08(): Unit \ Assert =
        assertEq(expected = ECons("a", ECons("a", ECons("a", ENil))), DelayList.repeat("a") |> DelayList.take(3))


    /////////////////////////////////////////////////////////////////////////////
    // from                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def startFrom01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.startFrom(-1) |> DelayList.take(0))

    @Test
    def startFrom02(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.startFrom(0) |> DelayList.take(0))

    @Test
    def startFrom04(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.startFrom(100) |> DelayList.take(-1))

    @Test
    def startFrom05(): Unit \ Assert =
    	assertEq(expected = ECons(-1, ENil), DelayList.startFrom(-1) |> DelayList.take(1))

    @Test
    def startFrom06(): Unit \ Assert =
    	assertEq(expected = ECons(5, ENil), DelayList.startFrom(5) |> DelayList.take(1))

    @Test
    def startFrom07(): Unit \ Assert =
    	assertEq(expected = ECons(1, ECons(2, ECons(3, ENil))), DelayList.startFrom(1) |> DelayList.take(3))

    @Test
    def startFrom08(): Unit \ Assert =
    	assertEq(expected = DelayList.range(-1000, 0), DelayList.startFrom(-1000) |> DelayList.take(1000))

    @Test
    def startFrom09(): Unit \ Assert =
    	assertEq(expected = DelayList.range(-1000, 1000), DelayList.startFrom(-1000) |> DelayList.take(2000))

    @Test
    def startFrom10(): Unit \ Assert =
    	assertEq(expected = DelayList.range(0, 2000), DelayList.startFrom(0) |> DelayList.take(2000))


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Unit \ Assert =
        assertEq(expected = None, DelayList.last((ENil: DelayList[Unit])))

    @Test
    def last02(): Unit \ Assert =
        assertEq(expected = None, DelayList.last((LList(lazy ENil): DelayList[Unit])))

    @Test
    def last03(): Unit \ Assert =
        assertEq(expected = Some(0), DelayList.range(0, 1) |> DelayList.last)

    @Test
    def last06(): Unit \ Assert =
    	assertEq(expected = Some(1), ECons(1, ENil) |> DelayList.last)

    @Test
    def last11(): Unit \ Assert =
    	assertEq(expected = Some(3), LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> DelayList.last)

    @Test
    def last12(): Unit \ Assert =
    	assertEq(expected = Some(999), DelayList.range(0, 1000) |> DelayList.last)


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toIter01(): Unit \ Assert = region rc {
    	assertEq(expected = Nil, DelayList.iterator(rc, (ENil: DelayList[Unit])) |> Iterator.toList)
    }

    @Test
    def toIter02(): Unit \ Assert = region rc {
    	assertEq(expected = Nil, DelayList.iterator(rc, (LList(lazy ENil): DelayList[Unit])) |> Iterator.toList)
    }

    @Test
    def toIter06(): Unit \ Assert = region rc {
    	assertEq(expected = 1 :: 2 :: 3 :: Nil, DelayList.iterator(rc, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) |> Iterator.toList)
    }

    @Test
    def toIter09(): Unit \ Assert = region rc {
    	assertEq(expected = List.range(-100, 100), DelayList.range(-100, 100) |> DelayList.iterator(rc) |> Iterator.toList)
    }


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.zip((ENil: DelayList[Int32]), (ENil: DelayList[String])))

    @Test
    def zip02(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.zip((LList(lazy ENil): DelayList[Int32]), (ENil: DelayList[String])))

    @Test
    def zip03(): Unit \ Assert =
    	assertEq(expected = ENil, DelayList.zip((ENil: DelayList[Int32]),( LList(lazy ENil): DelayList[String])))

    @Test
    def zip04(): Unit \ Assert =
    	assertEq(expected = ENil, DelayList.zip((LList(lazy ENil): DelayList[Int32]), (LList(lazy ENil): DelayList[String])))

    @Test
    def zip05(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.zip(ECons(1, ECons(2, ENil)), (ENil: DelayList[String])))

    @Test
    def zip06(): Unit \ Assert =
    	assertEq(expected = ENil, DelayList.zip((ENil: DelayList[String]), ECons(1, ECons(2, ENil))))

    @Test
    def zip07(): Unit \ Assert =
    	assertEq(expected = ECons(("a", 1), ECons(("b", 2), ENil)), DelayList.zip(LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil)))), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))))

    @Test
    def zip08(): Unit \ Assert =
    	assertEq(expected = ECons((1, "a"), ECons((2, "b"), ECons((3, "c"), ENil))), DelayList.zip(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))), LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil))))))

    @Test
    def zip09(): Unit \ Assert =
        assertEq(expected = ECons((1, 1), ECons((2, 2), ECons((3, 3), ENil))), DelayList.zip(DelayList.range(1, 10), DelayList.range(1, 10)) |> DelayList.take(3))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithPure01(): Bool =
        (ENil: DelayList[Int32]) |> DelayList.zipWith((x, y) -> x + y, (ENil: DelayList[Int32])) == ENil

    @test
    def zipWithPure02(): Bool =
    	LList(lazy ((ENil: DelayList[Int32]))) |> DelayList.zipWith((x, y) -> x + y, (ENil: DelayList[Int32])) == ENil

    @test
    def zipWithPure03(): Bool =
    	ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> x + y, (ENil: DelayList[Int32])) == ENil

    @test
    def zipWithPure04(): Bool =
    	(ENil: DelayList[Int32]) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ENil

    @test
    def zipWithPure05(): Bool =
    	ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithPure06(): Bool =
    	ECons(1, ENil) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @test
    def zipWithPure07(): Bool =
    	 LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> DelayList.zipWith((x, y) -> x + y, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(2, ECons(4, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithImpure01(): Bool \ IO =
        (ENil: DelayList[Int32]) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), (ENil: DelayList[Int32])) == ENil

    @test
    def zipWithImpure02(): Bool \ IO =
    	LList(lazy ((ENil: DelayList[Int32]))) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), (ENil: DelayList[Int32])) == ENil

    @test
    def zipWithImpure03(): Bool \ IO =
    	ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), (ENil: DelayList[Int32])) == ENil

    @test
    def zipWithImpure04(): Bool \ IO =
    	(ENil: DelayList[Int32]) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ENil))) == ENil

    @test
    def zipWithImpure05(): Bool \ IO =
    	ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithImpure06(): Bool \ IO =
    	ECons(1, ENil) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @test
    def zipWithImpure07(): Bool \ IO =
    	 LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(2, ECons(4, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith zipWith                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithZipWith01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))

    @test
    def zipWithZipWith02(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))

    @test
    def zipWithZipWith03(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))

    @test
    def zipWithZipWith04(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith zipWith fusion                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.zipWith((x, y) -> { Ref.put("a" :: Ref.get(l), l); (x, y) }, ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.zipWith((x, y) -> { Ref.put("b" :: Ref.get(l), l); (x, y) }, ECons(1, ECons(2, ECons(3, ENil))));
        assertEq(expected = ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    @Test
    def zipWithFusion02(): Unit \ Assert + IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.zipWith((x, y) -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); (x, y) } as _ \ {}), ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.zipWith((x, y) -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); (x, y) } as _ \ {}), ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.toList as _ \ IO);
        assertEq(expected = ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithIndex01(): Bool =
        DelayList.zipWithIndex((ENil: DelayList[Unit])) == (ENil: DelayList[(Int32, Unit)])

    @test
    def zipWithIndex02(): Bool =
        DelayList.zipWithIndex(ECons(1, ENil)) == ECons((0, 1), ENil)

    @test
    def zipWithIndex03(): Bool =
        DelayList.zipWithIndex(LCons(1, lazy ENil)) == LCons((0, 1), lazy ENil)

    @test
    def zipWithIndex04(): Bool =
        DelayList.zipWithIndex(LList(lazy ECons(1, ENil))) == LList(lazy ECons((0, 1), ENil))

    @test
    def zipWithIndex05(): Bool =
        let l = DelayList.zipWithIndex(LList(lazy LList(lazy LCons(5, lazy ECons(7, ENil)))));
        l == LList(lazy LList(lazy LCons((0, 5), lazy ECons((1, 7), ENil))))

    @test
    def zipWithIndex0(): Bool =
        let l = DelayList.zipWithIndex(LList(lazy LCons(0, lazy LList(lazy
            LCons(1, lazy LList(lazy LCons(2, lazy
            LList(lazy LCons(3, lazy LList(lazy
            LCons(4, lazy LList(lazy LCons(5, lazy
            LList(lazy LCons(10, lazy LList(lazy ENil))))))))))))))));
        l == LList(lazy LCons((0, 0), lazy LList(lazy
            LCons((1, 1), lazy LList(lazy LCons((2, 2), lazy
            LList(lazy LCons((3, 3), lazy LList(lazy
            LCons((4, 4), lazy LList(lazy LCons((5, 5), lazy
            LList(lazy LCons((6, 10), lazy LList(lazy ENil)))))))))))))))

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool =
        DelayList.reduceLeft((a, b) -> a + b, (ENil: DelayList[Int32])) == None

    @test
    def reduceLeft02(): Bool =
    	DelayList.reduceLeft((a, b) -> a + b, ECons(1, ENil)) == Some(1)

    @test
    def reduceLeft03(): Bool =
    	DelayList.reduceLeft((a, b) -> a + b, ECons(1, ECons(2, ENil))) == Some(3)

    @test
    def reduceLeft04(): Bool =
    	DelayList.reduceLeft((a, b) -> a + b, ECons(1, ECons(2, ECons(3, ENil)))) == Some(6)

    @test
    def reduceLeft05(): Bool =
    	DelayList.reduceLeft((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == Some(-8)

    @test
    def reduceLeft06(): Bool =
        DelayList.reduceLeft((a, b) -> b - a, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == Some(2)


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool =
        DelayList.reduceRight((a, b) -> a + b, (ENil: DelayList[Int32])) == None

    @test
    def reduceRight02(): Bool =
        DelayList.reduceRight((a, b) -> a + b, (LList(lazy ENil): DelayList[Int32])) == None

    @test
    def reduceRight03(): Bool =
    	DelayList.reduceRight((a, b) -> a + b, ECons(1, ENil)) == Some(1)

    @test
    def reduceRight04(): Bool =
    	DelayList.reduceRight((a, b) -> a + b, ECons(1, ECons(2, ENil))) == Some(3)

    @test
    def reduceRight05(): Bool =
    	DelayList.reduceRight((a, b) -> a + b, ECons(1, ECons(2, ECons(3, ENil)))) == Some(6)

    @test
    def reduceRight06(): Bool =
        DelayList.reduceRight((a, b) -> b - a, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == Some(-2)

    @test
    def reduceRight07(): Bool =
    	DelayList.reduceRight((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == Some(-2)


    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimum01(): Bool =
        DelayList.minimum((ENil: DelayList[Int32])) == None

    @test
    def minimum02(): Bool =
        DelayList.minimum(ECons(1, ENil)) == Some(1)

    @test
    def minimum03(): Bool =
        DelayList.minimum(ECons(3, ECons(2, ECons(1, ENil)))) == Some(1)

    @test
    def minimum04(): Bool =
        DelayList.minimum(ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(0)

    @test
    def minimum05(): Bool =
        DelayList.minimum(DelayList.range(0, 99)) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool =
        DelayList.minimumBy((x, y) -> x <=> y, (ENil: DelayList[Int32])) == None

    @test
    def minimumBy02(): Bool =
        DelayList.minimumBy((x, y) -> x <=> y, ECons(1, ENil)) == Some(1)

    @test
    def minimumBy03(): Bool =
        DelayList.minimumBy((x, y) -> x <=> y, ECons(3, ECons(2, ECons(1, ENil)))) == Some(1)

    @test
    def minimumBy04(): Bool =
        DelayList.minimumBy((x, y) -> x <=> y, ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(0)

    @test
    def minimumBy05(): Bool =
        DelayList.minimumBy((_, _) -> Comparison.LessThan, DelayList.range(0, 99)) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximum01(): Bool =
        DelayList.maximum((ENil: DelayList[Int32])) == None

    @test
    def maximum02(): Bool =
        DelayList.maximum(ECons(1, ENil)) == Some(1)

    @test
    def maximum03(): Bool =
        DelayList.maximum(ECons(3, ECons(2, ECons(1, ENil)))) == Some(3)

    @test
    def maximum04(): Bool =
        DelayList.maximum(ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(3)

    @test
    def maximum05(): Bool =
        DelayList.maximum(DelayList.range(0, 99)) == Some(98)


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool =
        DelayList.maximumBy((x, y) -> x <=> y, (ENil: DelayList[Int32])) == None

    @test
    def maximumBy02(): Bool =
        DelayList.maximumBy((x, y) -> x <=> y, ECons(1, ENil)) == Some(1)

    @test
    def maximumBy03(): Bool =
        DelayList.maximumBy((x, y) -> x <=> y, ECons(3, ECons(2, ECons(1, ENil)))) == Some(3)

    @test
    def maximumBy04(): Bool =
        DelayList.maximumBy((x, y) -> x <=> y, ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(3)

    @test
    def maximumBy05(): Bool =
        DelayList.maximumBy((_, _) -> Comparison.LessThan, DelayList.range(0, 99)) == Some(98)


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapPure01(): Bool =
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), (ENil: DelayList[Int32])) == ENil

    @test
    def flatMapPure02(): Bool =
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), ECons(1, ENil)) == ECons(1, ENil)

    @test
    def flatMapPure03(): Bool =
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure04(): Bool =
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapImpure01(): Bool \ IO =
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO), (ENil: DelayList[Int32])) == ENil

    @test
    def flatMapImpure02(): Bool \ IO =
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO), ECons(1, ENil)) == ECons(1, ENil)

    @test
    def flatMapImpure03(): Bool \ IO =
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO), ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure04(): Bool \ IO =
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO), ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapFlatMap01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil)))                            |>
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i)) |>
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i)) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @test
    def flatMapFlatMap02(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil)))                                                   |>
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO)) |>
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i))             ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @test
    def flatMapFlatMap03(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil)))                                           |>
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i))             |>
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO)) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @test
    def flatMapFlatMap04(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil)))                                                   |>
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO)) |>
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO)) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap fusion                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapFusion01(): Bool = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.flatMap(i -> { Ref.put("a" :: Ref.get(l), l); ECons(i, ENil) }) |>
        DelayList.flatMap(i -> { Ref.put("b" :: Ref.get(l), l); ECons(i, ENil) });
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def flatMapFusion02(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.flatMap(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); ECons(i, ENil) } as _ \ {})) |>
        DelayList.flatMap(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); ECons(i, ENil) } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool =
        not (DelayList.memberOf(0, ENil))

    @test
    def memberOf02(): Bool =
        not (DelayList.memberOf(0, ECons(1, ENil)))

    @test
    def memberOf03(): Bool =
        not (DelayList.memberOf(0, ECons(1, ECons(2, ECons(3, ENil)))))

    @test
    def memberOf04(): Bool =
        DelayList.memberOf(0, ECons(1, ECons(2, ECons(3, ECons(0, ENil)))))

    @test
    def memberOf05(): Bool =
        DelayList.memberOf(0, ECons(0, ECons(1, ECons(2, ECons(3, ENil)))))

    @test
    def memberOf06(): Bool =
        DelayList.memberOf(0, ECons(1, ECons(1, ECons(1, ECons(0, ENil)))))

    @test
    def memberOf07(): Bool =
        DelayList.memberOf(1, ECons(1, ECons(1, ECons(1, ECons(1, ENil)))))

    @test
    def memberOf08(): Bool =
        DelayList.memberOf(0, ECons(1, ECons(1, ECons(0, ECons(1, ENil)))))

    @test
    def memberOf09(): Bool =
        DelayList.memberOf(-2, ECons(1, ECons(1000, ECons(-2, ECons(1, ENil)))))

    @test
    def memberOf10(): Bool =
        DelayList.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil)))))))


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool =
        DelayList.replace(src = 3, dst = 4, ENil) == ENil

    @test
    def replace02(): Bool =
        DelayList.replace(src = 3, dst = 4, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def replace03(): Bool =
        DelayList.replace(src = 3, dst = 4, ECons(3, ENil)) == ECons(4, ENil)

    @test
    def replace04(): Bool =
        DelayList.replace(src = 3, dst = 4, ECons(4, ECons(4, ECons(4, ENil)))) == ECons(4, ECons(4, ECons(4, ENil)))

    @test
    def replace05(): Bool =
        DelayList.replace(src = 3, dst = 4, ECons(2, ECons(2, ECons(2, ENil)))) == ECons(2, ECons(2, ECons(2, ENil)))

    @test
    def replace06(): Bool =
        DelayList.replace(src = 3, dst = 4, ECons(2, ECons(3, ECons(2, ENil)))) == ECons(2, ECons(4, ECons(2, ENil)))

    @test
    def replace07(): Bool =
        DelayList.replace(src = 0, dst = 1, ECons(0, ECons(0, ECons(0, ENil)))) == ECons(1, ECons(1, ECons(1, ENil)))

    @test
    def replace08(): Bool =
        DelayList.replace(src = 3, dst = 4, LList(lazy ECons(3, LList(lazy LCons(3, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(4, ECons(4, ECons(4, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool =
        DelayList.findLeft(i -> i > 2, (ENil: DelayList[Int32])) == None

    @test
    def findLeft02(): Bool =
        DelayList.findLeft(i -> i > 2, ECons(1, ENil)) == None

    @test
    def findLeft03(): Bool =
        DelayList.findLeft(i -> i > 2, ECons(3, ENil)) == Some(3)

    @test
    def findLeft04(): Bool =
        DelayList.findLeft(i -> i > 2, ECons(1, ECons(2, ENil))) == None

    @test
    def findLeft05(): Bool =
        DelayList.findLeft(i -> i > 2, ECons(6, ECons(-6, ENil))) == Some(6)

    @test
    def findLeft06(): Bool =
        DelayList.findLeft(i -> i > 2, ECons(7, ECons(6, ENil))) == Some(7)

    @test
    def findLeft07(): Bool =
        DelayList.findLeft(i -> i > 2, LList(lazy ECons(1, LList(lazy LCons(1, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == Some(4)


    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool =
        DelayList.findRight(i -> i > 2, (ENil: DelayList[Int32])) == None

    @test
    def findRight02(): Bool =
        DelayList.findRight(i -> i > 2, ECons(1, ENil)) == None

    @test
    def findRight03(): Bool =
        DelayList.findRight(i -> i > 2, ECons(3, ENil)) == Some(3)

    @test
    def findRight04(): Bool =
        DelayList.findRight(i -> i > 2, ECons(1, ECons(2, ENil))) == None

    @test
    def findRight05(): Bool =
        DelayList.findRight(i -> i > 2, ECons(6, ECons(-6, ENil))) == Some(6)

    @test
    def findRight06(): Bool =
        DelayList.findRight(i -> i > 2, ECons(7, ECons(6, ENil))) == Some(6)

    @test
    def findRight07(): Bool =
        DelayList.findRight(i -> i > 2, LList(lazy ECons(4, LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy LList(lazy ENil)))))))) == Some(4)


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex (pure)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexPure01(): Bool =
    	ENil |> DelayList.mapWithIndex((i, x) -> x + i) == ENil

    @test
    def mapWithIndexPure02(): Bool =
    	ECons(1, ENil) |> DelayList.mapWithIndex((i, x) -> x + i) == ECons(1, ENil)

    @test
    def mapWithIndexPure03(): Bool =
    	ECons(1, ECons(2, ENil)) |> DelayList.mapWithIndex((i, x) -> x + i) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexPure04(): Bool =
    	ECons(1, ECons(2, ECons(3, ENil))) |> DelayList.mapWithIndex((i, x) -> x + i) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexPure05(): Bool =
        ECons(1, ECons(2, ECons(3, ECons(4, ENil)))) |> DelayList.mapWithIndex((i, x) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexPure06(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) |> DelayList.mapWithIndex((i, x) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex (impure)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexImpure01(): Bool \ IO =
    	ENil |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ENil

    @test
    def mapWithIndexImpure02(): Bool \ IO =
    	ECons(1, ENil) |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ECons(1, ENil)

    @test
    def mapWithIndexImpure03(): Bool \ IO =
    	ECons(1, ECons(2, ENil)) |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexImpure04(): Bool \ IO =
    	ECons(1, ECons(2, ECons(3, ENil))) |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexImpure05(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ECons(4, ENil)))) |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexImpure06(): Bool \ IO =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex mapWithIndex                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexMapWithIndex01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> x + i) |>
            DelayList.mapWithIndex((i, x) -> x * i) == ECons(0, ECons(3, ECons(10, ENil)))

    @test
    def mapWithIndexMapWithIndex02(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) |>
            DelayList.mapWithIndex((i, x) -> x * i) == ECons(0, ECons(3, ECons(10, ENil)))

    @test
    def mapWithIndexMapWithIndex03(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> x + i) |>
            DelayList.mapWithIndex((i, x) -> unchecked_cast(x * i as _ \ IO)) == ECons(0, ECons(3, ECons(10, ENil)))

    @test
    def mapWithIndexMapWithIndex04(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) |>
            DelayList.mapWithIndex((i, x) -> unchecked_cast(x * i as _ \ IO)) == ECons(0, ECons(3, ECons(10, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex mapWithIndex fusion                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexFusion01(): Bool = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.mapWithIndex((_, x) -> { Ref.put("a" :: Ref.get(l), l); x }) |>
        DelayList.mapWithIndex((_, x) -> { Ref.put("b" :: Ref.get(l), l); x }) |>
        DelayList.toList;
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def mapWithIndexFusion02(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.mapWithIndex((_, x) -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); x } as _ \ {})) |>
        DelayList.mapWithIndex((_, x) -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); x } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool =
        DelayList.intercalate(ECons(1, ECons(2, ENil)), ENil) == ENil

    @test
    def intercalate02(): Bool =
        DelayList.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(ECons(1, ENil), ENil)) == ECons(1, ENil)

    @test
    def intercalate03(): Bool =
        DelayList.intercalate(
            ECons(10, ECons(11, ECons(12, ENil))),
                ECons(ECons(1, ECons(2, ECons(3, ENil))),
                ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil)))
            == ECons(1, ECons(2, ECons(3,
                ECons(10, ECons(11, ECons(12,
                ECons(100, ECons(101, ECons(102, ENil)))))))))

    @test
    def intercalate04(): Bool =
        DelayList.intercalate(
            ECons(10, ECons(11, ECons(12, ENil))),
            ECons(ECons(1, ECons(2, ECons(3, ENil))),
                ECons(ECons(100, ECons(101, ECons(102, ENil))),
                ECons(ECons(200, ECons(201, ECons(202, ENil))), ENil))))
            == ECons(1, ECons(2, ECons(3,
                ECons(10, ECons(11, ECons(12,
                ECons(100, ECons(101, ECons(102,
                ECons(10, ECons(11, ECons(12,
                ECons(200, ECons(201, ECons(202, ENil)))))))))))))))

    @test
    def intercalate05(): Bool =
        DelayList.intercalate(
            ECons(12, ECons(11, ECons(10, ENil))),
            ECons(ECons(1, ECons(2, ECons(3, ENil))),
                ECons(ECons(100, ECons(101, ECons(102, ENil))),
                ECons(ECons(200, ECons(201, ECons(202, ENil))), ENil))))
            == ECons(1, ECons(2, ECons(3,
                ECons(12, ECons(11, ECons(10,
                ECons(100, ECons(101, ECons(102,
                ECons(12, ECons(11, ECons(10,
                ECons(200, ECons(201, ECons(202, ENil)))))))))))))))

    @test
    def intercalate06(): Bool =
        DelayList.intercalate(
            ECons(-1, ECons(-2, ECons(0, ENil))),
            ECons(ECons(1, ECons(2, ECons(3, ENil))),
                ECons(ECons(100, ECons(101, ECons(102, ENil))),
                ECons(ECons(200, ECons(201, ECons(202, ENil))), ENil))))
            == ECons(1, ECons(2, ECons(3,
                ECons(-1, ECons(-2, ECons(0,
                ECons(100, ECons(101, ECons(102,
                ECons(-1, ECons(-2, ECons(0,
                ECons(200, ECons(201, ECons(202, ENil)))))))))))))))

    @test
    def intercalate07(): Bool =
        DelayList.intercalate(
            LList(lazy LCons(10, lazy LList(lazy LCons(11, lazy LCons(12, lazy LList(lazy ENil)))))),
                LList(lazy LCons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))))),
                lazy LList(lazy LCons(LList(lazy ECons(100, LList(lazy LCons(101, lazy LList(lazy LCons(102, lazy ENil)))))),
                lazy LList(lazy ENil))))))
            == ECons(1, ECons(2, ECons(3,
                ECons(10, ECons(11, ECons(12,
                ECons(100, ECons(101, ECons(102, ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool =
        DelayList.intersperse(-11, ENil) == ENil

    @test
    def intersperse03(): Bool =
        DelayList.intersperse(-11, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def intersperse04(): Bool =
        DelayList.intersperse(-11, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

    @test
    def intersperse05(): Bool =
        DelayList.intersperse(-11, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

    @test
    def intersperse06(): Bool =
        DelayList.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhilePure01(): Bool =
        DelayList.takeWhile(i -> i > 3, ENil) == ENil

    @test
    def takeWhilePure02(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(1, ENil)) == ENil

    @test
    def takeWhilePure03(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(4, ENil)) == ECons(4, ENil)

    @test
    def takeWhilePure04(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(1, ECons(2, ENil))) == ENil

    @test
    def takeWhilePure05(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(1, ECons(5, ENil))) == ENil

    @test
    def takeWhilePure06(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(5, ECons(1, ENil))) == ECons(5, ENil)

    @test
    def takeWhilePure07(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(5, ECons(8, ENil))) == ECons(5, ECons(8, ENil))

    @test
    def takeWhilePure08(): Bool =
        DelayList.takeWhile(i -> i > 3, LList(lazy ECons(5, LList(lazy LCons(8, lazy ENil))))) == ECons(5, ECons(8, ENil))

    @test
    def takeWhilePure09(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(4, ECons(6, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileImpure01(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ENil) == ENil

    @test
    def takeWhileImpure02(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ENil)) == ENil

    @test
    def takeWhileImpure03(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(4, ENil)) == ECons(4, ENil)

    @test
    def takeWhileImpure04(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ECons(2, ENil))) == ENil

    @test
    def takeWhileImpure05(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ECons(5, ENil))) == ENil

    @test
    def takeWhileImpure06(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(5, ECons(1, ENil))) == ECons(5, ENil)

    @test
    def takeWhileImpure07(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(5, ECons(8, ENil))) == ECons(5, ECons(8, ENil))

    @test
    def takeWhileImpure08(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), LList(lazy ECons(5, LList(lazy LCons(8, lazy ENil))))) == ECons(5, ECons(8, ENil))

    @test
    def takeWhileImpure09(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(4, ECons(6, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile takeWhile                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileTakeWhile01(): Bool =
        ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.takeWhile(i -> i > 3) |>
            DelayList.takeWhile(i -> i < 5) == ECons(4, ENil)

    @test
    def takeWhileTakeWhile02(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil)))                      |>
            DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO)) |>
            DelayList.takeWhile(i -> i < 5) == ECons(4, ENil)

    @test
    def takeWhileTakeWhile03(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.takeWhile(i -> i > 3) |>
            DelayList.takeWhile(i -> unchecked_cast(i < 5 as _ \ IO)) == ECons(4, ENil)

    @test
    def takeWhileTakeWhile04(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil)))             |>
            DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO)) |>
            DelayList.takeWhile(i -> unchecked_cast(i < 5 as _ \ IO)) == ECons(4, ENil)


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile takeWhile fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileFusion01(): Bool = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> { Ref.put("a" :: Ref.get(l), l); i < 4 }) |>
        DelayList.takeWhile(i -> { Ref.put("b" :: Ref.get(l), l); i < 4 });
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def takeWhileFusion02(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 4 } as _ \ {})) |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 4 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    @test
    def takeWhileFusion03(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 2 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: Nil)
    }

    @test
    def takeWhileFusion04(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhilePure01(): Bool =
        DelayList.dropWhile(i -> i > 3, ENil) == ENil

    @test
    def dropWhilePure02(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def dropWhilePure03(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(4, ENil)) == ENil

    @test
    def dropWhilePure04(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(1, ECons(2, ENil))) == ECons(1, ECons(2, ENil))

    @test
    def dropWhilePure05(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(1, ECons(5, ENil))) == ECons(1, ECons(5, ENil))

    @test
    def dropWhilePure06(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(5, ECons(1, ENil))) == ECons(1, ENil)

    @test
    def dropWhilePure07(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(5, ECons(8, ENil))) == ENil

    @test
    def dropWhilePure08(): Bool =
        DelayList.dropWhile(i -> i > 3, LList(lazy ECons(5, LCons(8, lazy LList(lazy LCons(9, lazy LList(lazy ECons(10, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))))))))))) == ECons(1, ECons(2, ENil))

    @test
    def dropWhilePure09(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileImpure01(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ENil) == ENil

    @test
    def dropWhileImpure02(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ENil)) == ECons(1, ENil)

    @test
    def dropWhileImpure03(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(4, ENil)) == ENil

    @test
    def dropWhileImpure04(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ECons(2, ENil))) == ECons(1, ECons(2, ENil))

    @test
    def dropWhileImpure05(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ECons(5, ENil))) == ECons(1, ECons(5, ENil))

    @test
    def dropWhileImpure06(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(5, ECons(1, ENil))) == ECons(1, ENil)

    @test
    def dropWhileImpure07(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(5, ECons(8, ENil))) == ENil

    @test
    def dropWhileImpure08(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), LList(lazy ECons(5, LCons(8, lazy LList(lazy LCons(9, lazy LList(lazy ECons(10, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))))))))))) == ECons(1, ECons(2, ENil))

    @test
    def dropWhileImpure09(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile dropWhile                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileDropWhile01(): Bool =
        ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.dropWhile(i -> i < 5) |>
            DelayList.dropWhile(i -> i > 1) == ECons(1, ENil)

    @test
    def dropWhileDropWhile02(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil)))             |>
            DelayList.dropWhile(i -> unchecked_cast(i < 5 as _ \ IO)) |>
            DelayList.dropWhile(i -> i > 1) == ECons(1, ENil)

    @test
    def dropWhileDropWhile03(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil)))     |>
            DelayList.dropWhile(i -> i < 5)     |>
            DelayList.dropWhile(i -> unchecked_cast(i > 1 as _ \ IO)) == ECons(1, ENil)

    @test
    def dropWhileDropWhile04(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil)))             |>
            DelayList.dropWhile(i -> unchecked_cast(i < 5 as _ \ IO)) |>
            DelayList.dropWhile(i -> unchecked_cast(i > 1 as _ \ IO)) == ECons(1, ENil)


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile dropWhile fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileFusion01(): Bool = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> { Ref.put("a" :: Ref.get(l), l); i < 3 }) |>
        DelayList.dropWhile(i -> { Ref.put("b" :: Ref.get(l), l); i < 4 });
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: Nil)
    }

    @test
    def dropWhileFusion02(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 4 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: Nil)
    }

    @test
    def dropWhileFusion03(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 1 } as _ \ {})) |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def dropWhileFusion04(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 2 } as _ \ {})) |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "b" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool =
        DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ENil) == None

    @test
    def findMap02(): Bool =
        DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ENil)) == None

    @test
    def findMap03(): Bool =
        DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(2, ENil)) == Some(1)

	@test
	def findMap04(): Bool =
		DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, ENil))) == None

	@test
	def findMap05(): Bool =
		DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))) == Some(2)

	@test
	def findMap06(): Bool =
		DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))) == Some(3)

	@test
	def findMap07(): Bool =
		DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))) == Some(4)

    @test
	def findMap08(): Bool =
		DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == Some(0)

    @test
	def findMap09(): Bool =
		DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None,
		    LList(lazy LCons(-1, lazy LList(lazy LCons(1, lazy
		    LList(lazy LCons(9, lazy LList(lazy LCons(3, lazy
		    LList(lazy LCons(5, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == Some(5)


    /////////////////////////////////////////////////////////////////////////////
    // filterMap (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapPure01(): Bool =
        DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ENil) == ENil

    @test
    def filterMapPure02(): Bool =
        DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ENil)) == ENil

    @test
    def filterMapPure03(): Bool =
        DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(2, ENil)) == ECons(1, ENil)

	@test
	def filterMapPure04(): Bool =
		DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, ENil))) == ENil

	@test
	def filterMapPure05(): Bool =
		DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@test
	def filterMapPure06(): Bool =
		DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@test
	def filterMapPure07(): Bool =
		DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))

    @test
	def filterMapPure08(): Bool =
		DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))

    @test
	def filterMapPure09(): Bool =
		DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None,
		    LList(lazy LCons(0, lazy LList(lazy
		    LCons(1, lazy LList(lazy LCons(2, lazy
		    LList(lazy LCons(3, lazy LList(lazy
		    LCons(4, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapImpure01(): Bool \ IO =
        DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO), ENil) == ENil

    @test
    def filterMapImpure02(): Bool \ IO =
        DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(1, ENil)) == ENil

    @test
    def filterMapImpure03(): Bool \ IO =
        DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(2, ENil)) == ECons(1, ENil)

	@test
	def filterMapImpure04(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(1, ECons(3, ENil))) == ENil

	@test
	def filterMapImpure05(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@test
	def filterMapImpure06(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@test
	def filterMapImpure07(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))

    @test
	def filterMapImpure08(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))

    @test
	def filterMapImpure09(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO),
		    LList(lazy LCons(0, lazy LList(lazy
		    LCons(1, lazy LList(lazy LCons(2, lazy
		    LList(lazy LCons(3, lazy LList(lazy
		    LCons(4, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap filterMap                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFilterMap01(): Bool =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None) |>
            DelayList.filterMap(i -> if (i > 1)                Some(i * 3) else None) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap02(): Bool \ IO =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO)) |>
            DelayList.filterMap(i ->             if (i > 1)                Some(i * 3) else None) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap03(): Bool \ IO =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            DelayList.filterMap(i ->             if (i `Int32.remainder` 2 == 0) Some(i / 2) else None) |>
            DelayList.filterMap(i -> unchecked_cast(if (i > 1)                Some(i * 3) else None as _ \ IO)) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap04(): Bool \ IO =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            DelayList.filterMap(i -> unchecked_cast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None as _ \ IO)) |>
            DelayList.filterMap(i -> unchecked_cast(if (i > 1)                Some(i * 3) else None as _ \ IO)) == ECons(6, ECons(9, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap filterMap fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFusion01(): Bool = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filterMap(x -> { Ref.put("a" :: Ref.get(l), l); Some(x) }) |>
        DelayList.filterMap(x -> { Ref.put("b" :: Ref.get(l), l); Some(x) });
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def filterMapFusion02(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filterMap(x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); Some(x) } as _ \ {})) |>
        DelayList.filterMap(x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); Some(x) } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def partition01(): Bool =
        DelayList.partition(i -> i > 3, ENil) == (ENil, ENil)

    @test
    def partition02(): Bool =
        DelayList.partition(i -> i > 3, ECons(1, ENil)) == (ENil, ECons(1, ENil))

    @test
    def partition03(): Bool =
        DelayList.partition(i -> i > 3, ECons(4, ENil)) == (ECons(4, ENil), ENil)

    @test
    def partition04(): Bool =
        DelayList.partition(i -> i > 3, ECons(1, ECons(2, ENil))) == (ENil, ECons(1, ECons(2, ENil)))

    @test
    def partition05(): Bool =
        DelayList.partition(i -> i > 3, ECons(1, ECons(5, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @test
    def partition06(): Bool =
        DelayList.partition(i -> i > 3, ECons(5, ECons(1, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @test
    def partition07(): Bool =
        DelayList.partition(i -> i > 3, ECons(5, ECons(8, ENil))) == (ECons(5, ECons(8, ENil)), ENil)

    @test
    def partition08(): Bool =
        DelayList.partition(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))))
            == (ECons(4, ECons(6, ECons(11, ECons(16, ECons(7, ECons(7, ENil)))))), ECons(-3, ECons(-5, ECons(1, ECons(2, ECons(1, ENil))))))

    @test
    def partition09(): Bool =
        DelayList.partition(i -> i > 3,
            LList(lazy LCons(4, lazy LList(lazy
                ECons(6, LCons(-3, lazy LList(lazy
                LCons(11, lazy LList(lazy LCons(-5, lazy
                LList(lazy LCons(1, lazy LList(lazy
                LCons(2, lazy LList(lazy LCons(16, lazy
                LList(lazy LCons(7, lazy LList(lazy
                LCons(1, lazy LList(lazy LCons(7, lazy LList(lazy ENil)))))))))))))))))))))))
            == (ECons(4, ECons(6, ECons(11, ECons(16, ECons(7, ECons(7, ENil)))))),
                    ECons(-3, ECons(-5, ECons(1, ECons(2, ECons(1, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // span                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def span01(): Bool =
        DelayList.span(i -> i > 3, ENil) == (ENil, ENil)

    @test
    def span02(): Bool =
        DelayList.span(i -> i > 3, ECons(1, ENil)) == (ENil, ECons(1, ENil))

    @test
    def span03(): Bool =
        DelayList.span(i -> i > 3, ECons(4, ENil)) == (ECons(4, ENil), ENil)

    @test
    def span04(): Bool =
        DelayList.span(i -> i > 3, ECons(1, ECons(2, ENil))) == (ENil, ECons(1, ECons(2, ENil)))

    @test
    def span05(): Bool =
        DelayList.span(i -> i > 3, ECons(1, ECons(5, ENil))) == (ENil, ECons(1, ECons(5, ENil)))

    @test
    def span06(): Bool =
        DelayList.span(i -> i > 3, ECons(5, ECons(1, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @test
    def span07(): Bool =
        DelayList.span(i -> i > 3, ECons(5, ECons(8, ENil))) == (ECons(5, ECons(8, ENil)), ENil)

    @test
    def span08(): Bool =
        DelayList.span(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == (ECons(4, ECons(6, ENil)), ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))

    @test
    def span09(): Bool =
        DelayList.span(i -> i > 3,
            LList(lazy LCons(4, lazy LList(lazy
                LCons(6, lazy LList(lazy LCons(-3, lazy
                LList(lazy LCons(11, lazy LList(lazy
                LCons(-5, lazy LList(lazy LCons(1, lazy
                LList(lazy LCons(2, lazy LList(lazy
                LCons(16, lazy LList(lazy LCons(7, lazy
                LList(lazy LCons(1, lazy LList(lazy
                LCons(7, lazy LList(lazy ENil))))))))))))))))))))))))
            == (ECons(4, ECons(6, ENil)),
                    ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))


    /////////////////////////////////////////////////////////////////////////////
    // span span fusion                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def spanSpanFusion01(): Bool = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.span(x -> { Ref.put("a" :: Ref.get(l), l); x < 3 }) |> fst |>
        DelayList.span(x -> { Ref.put("b" :: Ref.get(l), l); x < 3 }); // The working list is `1 :: 2 :: Nil`
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: "b" :: Nil)
    }

    //@test
    //def spanSpanFusion02(): Bool = region rc {
    //    let l = Ref.fresh(rc, Nil);
    //    let _ =
    //    (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
    //    DelayList.span(x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); x < 3 } as _ \ {})) |> fst |>
    //    DelayList.span(x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); x < 3 } as _ \ {})) |> fst |>
    //    DelayList.toList;
    //    List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: Nil)
    //}

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        List.toDelayList(Nil) |> DelayList.sum == 0

    @test
    def sum02(): Bool =
        List.toDelayList(1 :: Nil) |> DelayList.sum == 1

    @test
    def sum03(): Bool =
        List.toDelayList(1 :: 2 :: 3 :: Nil) |> DelayList.sum == 6

    @test
    def sum04(): Bool =
        List.toDelayList(1 :: 2 :: 3 :: -3 :: Nil) |> DelayList.sum == 3

    @test
    def sum05(): Bool =
        List.toDelayList(-1 :: -2 :: -3 :: -4 :: Nil) |> DelayList.sum == -10

    @test
    def sum06(): Bool =
        List.toDelayList(10 :: -10 :: Nil) |> DelayList.sum == 0

    @test
    def sum07(): Bool =
        List.range(1, 101) |> List.toDelayList |> DelayList.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        List.toDelayList(Nil) |> DelayList.sumWith(x -> x + 1) == 0

    @test
    def sumWith02(): Bool =
        List.toDelayList(1 :: Nil) |> DelayList.sumWith(x -> x + 1) == 2

    @test
    def sumWith03(): Bool =
        List.toDelayList(1 :: 2 :: 3 :: Nil) |> DelayList.sumWith(x -> x + 1) == 9

    @test
    def sumWith04(): Bool =
        List.toDelayList(1 :: 2 :: 3 :: -3 :: Nil) |> DelayList.sumWith(x -> x + 1) == 7

    @test
    def sumWith05(): Bool =
        List.toDelayList(-1 :: -2 :: -3 :: -4 :: Nil) |> DelayList.sumWith(x -> x + 1) == -6

    @test
    def sumWith06(): Bool =
        List.toDelayList(10 :: -10 :: Nil) |> DelayList.sumWith(x -> x + 1) == 2


    /////////////////////////////////////////////////////////////////////////////
    // order                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def order01(): Bool =
        ((ENil: DelayList[Unit]) <=> (ENil: DelayList[Unit])) == Comparison.EqualTo

    @test
    def order02(): Bool =
        (((1 :: Nil) |> List.toDelayList) <=> (ENil: DelayList[Int32])) == Comparison.GreaterThan

    @test
    def order03(): Bool =
        ((ENil: DelayList[Int32]) <=> ((1 :: Nil) |> List.toDelayList)) == Comparison.LessThan

    @test
    def order04(): Bool =
        (((1 :: Nil) |> List.toDelayList) <=> ((1 :: Nil) |> List.toDelayList)) == Comparison.EqualTo

    @test
    def order05(): Bool =
        (((2 :: 1 :: Nil) |> List.toDelayList) <=> ((1 :: 1 :: Nil) |> List.toDelayList)) == Comparison.GreaterThan

    @test
    def order06(): Bool =
        (((1 :: 1 :: Nil) |> List.toDelayList) <=> ((2 :: 1 :: Nil) |> List.toDelayList)) == Comparison.LessThan

    @test
    def order07(): Bool =
        (((1 :: Nil) |> List.toDelayList) <=> ((1 :: 1 :: Nil) |> List.toDelayList)) == Comparison.LessThan


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool =
        (Nil: List[Int32]) |> List.toDelayList |>
            DelayList.join(",") == ""

    @test
    def join02(): Bool =
        (1 :: Nil) |> List.toDelayList |>
            DelayList.join(",") == "1"

    @test
    def join03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
            DelayList.join(",") == "1,2,3"

    @test
    def join04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toDelayList |>
            DelayList.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool =
        (Nil: List[Int32]) |> List.toDelayList |>
            DelayList.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool =
        (1 :: Nil) |> List.toDelayList |>
            DelayList.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
            DelayList.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toDelayList |>
            DelayList.joinWith(x -> x + x, ",") == "11,22,33"


    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def ap01(): Bool =
        DelayList.ap(ENil, ENil) == (ENil: DelayList[Int32])

    @test
    def ap02(): Bool =
        DelayList.ap(ECons((x -> x + 1), ENil), ENil) == ENil

    @test
    def ap03(): Bool =
        DelayList.ap(ENil, ECons(5, ENil)) == (ENil: DelayList[Int32])

    @test
    def ap04(): Bool =
        DelayList.ap(ECons((x -> x + 1), ENil), ECons(5, ENil)) |> DelayList.toList == 6 :: Nil

    @test
    def ap05(): Bool =
        let f = List.toDelayList((x -> x + 1) :: Nil);
        let l = List.toDelayList(0 :: 5 :: Nil);
        DelayList.ap(f, l) |> DelayList.toList == 1 :: 6 :: Nil

    @test
    def ap06(): Bool =
        let f = List.toDelayList((x -> x + 1) :: (x -> x * 2) :: Nil);
        let l = List.toDelayList(0 :: 4 :: Nil);
        DelayList.ap(f, l) |> DelayList.toList == 1 :: 5 :: 0 :: 8 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sequence01(): Bool =
        let l: DelayList[Identity[Int32]] = ENil;
        DelayList.sequence(l) == Identity.Identity(ENil)

    @test
    def sequence02(): Bool =
        let l = List.toDelayList(Identity.Identity(1) :: Nil);
        DelayList.sequence(l) == Identity.Identity(List.toDelayList(1 :: Nil))

    @test
    def sequence03(): Bool =
        let l = List.toDelayList(Identity.Identity(1) :: Identity.Identity(2) :: Nil);
        DelayList.sequence(l) == Identity.Identity(List.toDelayList(1 :: 2 :: Nil))

    @test
    def sequence04(): Bool =
        let l = List.toDelayList(Identity.Identity(1) :: Identity.Identity(2) :: Identity.Identity(3) :: Nil);
        DelayList.sequence(l) == Identity.Identity(List.toDelayList(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def traverse01(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let l = ENil;
        let ans = DelayList.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        ans == Identity.Identity(ENil) and Ref.get(st) == 0
    }

    @test
    def traverse02(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let l = List.toDelayList(1 :: Nil);
        let ans = DelayList.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        ans == Identity.Identity(List.toDelayList(1 :: Nil)) and Ref.get(st) == 1
    }

    @test
    def traverse03(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let l = List.toDelayList(1 :: 2 :: Nil);
        let ans = DelayList.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        ans == Identity.Identity(List.toDelayList(1 :: 2 :: Nil)) and Ref.get(st) == 2
    }

    @test
    def traverse04(): Bool = region rc {
        let st = Ref.fresh(rc, 0);
        let l = List.toDelayList(1 :: 2 :: 3 :: Nil);
        let ans = DelayList.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        ans == Identity.Identity(List.toDelayList(1 :: 2 :: 3 :: Nil)) and Ref.get(st) == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def shuffle01(): Bool \ NonDet =
        let l1: List[Int32] = Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle;

        DelayList.length(l2) == 0 and DelayList.toSet(l2) == Set#{}

    @test
    def shuffle02(): Bool \ NonDet =
        let l1 = 1 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle;

        DelayList.length(l2) == 1 and DelayList.toSet(l2) == Set#{1}

    @test
    def shuffle03(): Bool \ NonDet =
        let l1 = 1 :: 2 :: 3 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle;

        DelayList.length(l2) == 3 and DelayList.toSet(l2) == Set#{1, 2, 3}

    @test
    def shuffle04(): Bool \ NonDet =
        let l1 = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle;

        DelayList.length(l2) == 10 and DelayList.toSet(l2) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        ToString.toString(List.toDelayList(1 :: Nil)) == "DelayList(1)"

    @test
    def toString02(): Bool =
        ToString.toString(List.toDelayList(1 :: 2 :: Nil)) == "DelayList(1, 2)"

    @test
    def toString03(): Bool =
        ToString.toString(List.toDelayList(93 :: 3 :: 4 :: Nil)) == "DelayList(93, 3, 4)"

    @test
    def toString04(): Bool =
        ToString.toString(List.toDelayList('a' :: 'b' :: 'c' :: Nil)) == "DelayList(a, b, c)"

    @test
    def toString05(): Bool =
        ToString.toString(List.toDelayList(true :: false :: true :: true :: Nil)) == "DelayList(true, false, true, true)"

    @test
    def toString06(): Bool =
        ToString.toString(List.toDelayList(List.toDelayList(1 :: 2 :: Nil) :: List.toDelayList(2 :: 3 :: Nil) :: List.toDelayList(4 :: 7 :: Nil) :: Nil)) == "DelayList(DelayList(1, 2), DelayList(2, 3), DelayList(4, 7))"

}
