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
        assertEq(expected = Nil, Nil |> List.toDelayList |> DelayList.map(x -> checked_ecast(x + 1)) |> DelayList.toList)

    @Test
    def mapImpure02(): Unit \ Assert + IO =
        assertEq(expected = 2 :: Nil, (1 :: Nil) |> List.toDelayList |> DelayList.map(x -> checked_ecast(x + 1)) |> DelayList.toList)

    @Test
    def mapImpure03(): Unit \ Assert + IO =
        assertEq(expected = 2 :: 3 :: Nil, (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.map(x -> checked_ecast(x + 1)) |> DelayList.toList)

    @Test
    def mapImpure04(): Unit \ Assert + IO =
        assertEq(expected = 2 :: 3 :: 4 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.map(x -> checked_ecast(x + 1)) |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapMap01(): Unit \ Assert =
        assertEq(expected = 4 :: 6 :: 8 :: Nil, (1 :: 2 :: 3 :: Nil) |>
            List.toDelayList |>
            DelayList.map(x -> x + 1) |>
            DelayList.map(x -> x * 2) |>
            DelayList.toList)

    @Test
    def mapMap02(): Unit \ Assert + IO =
        assertEq(expected = 4 :: 6 :: 8 :: Nil, (1 :: 2 :: 3 :: Nil) |>
            List.toDelayList |>
            DelayList.map(x -> checked_ecast(x + 1)) |>
            DelayList.map(x -> x * 2) |>
            DelayList.toList)

    @Test
    def mapMap03(): Unit \ Assert + IO =
        assertEq(expected = 4 :: 6 :: 8 :: Nil, (1 :: 2 :: 3 :: Nil) |>
            List.toDelayList |>
            DelayList.map(x -> x + 1) |>
            DelayList.map(x -> checked_ecast(x * 2)) |>
            DelayList.toList)

    @Test
    def mapMap04(): Unit \ Assert + IO =
        assertEq(expected = 4 :: 6 :: 8 :: Nil, (1 :: 2 :: 3 :: Nil) |>
            List.toDelayList |>
            DelayList.map(x -> checked_ecast(x + 1)) |>
            DelayList.map(x -> checked_ecast(x * 2)) |>
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
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); x } as _ \ {})) |>
        DelayList.map(x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); x } as _ \ {})) |>
        DelayList.toList);
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
        assertEq(expected = Nil, Nil |> List.toDelayList |> DelayList.filter(x -> checked_ecast(x > 100)) |> DelayList.toList)

    @Test
    def filterImpure02(): Unit \ Assert + IO =
        assertEq(expected = 1 :: 2 :: 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> checked_ecast(x > 0)) |> DelayList.toList)

    @Test
    def filterImpure03(): Unit \ Assert + IO =
        assertEq(expected = 2 :: 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> checked_ecast(x > 1)) |> DelayList.toList)

    @Test
    def filterImpure04(): Unit \ Assert + IO =
        assertEq(expected = 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> checked_ecast(x > 2)) |> DelayList.toList)

    @Test
    def filterImpure05(): Unit \ Assert + IO =
        assertEq(expected = Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> checked_ecast(x > 3)) |> DelayList.toList)

    @Test
    def filterImpure06(): Unit \ Assert + IO =
        assertEq(expected = Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> checked_ecast(x > 100)) |> DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterFilter01(): Unit \ Assert =
        assertEq(expected = 2 :: Nil, (1 :: 2 :: 3 :: Nil) |>
            List.toDelayList |>
            DelayList.filter(x -> x > 1) |>
            DelayList.filter(x -> x < 3) |>
            DelayList.toList)

    @Test
    def filterFilter02(): Unit \ Assert + IO =
        assertEq(expected = 2 :: Nil, (1 :: 2 :: 3 :: Nil) |>
            List.toDelayList |>
            DelayList.filter(x -> checked_ecast(x > 1)) |>
            DelayList.filter(x -> x < 3) |>
            DelayList.toList)

    @Test
    def filterFilter03(): Unit \ Assert + IO =
        assertEq(expected = 2 :: Nil, (1 :: 2 :: 3 :: Nil) |>
            List.toDelayList |>
            DelayList.filter(x -> x > 1) |>
            DelayList.filter(x -> checked_ecast(x < 3)) |>
            DelayList.toList)

    @Test
    def filterFilter04(): Unit \ Assert + IO =
        assertEq(expected = 2 :: Nil, (1 :: 2 :: 3 :: Nil) |>
            List.toDelayList |>
            DelayList.filter(x -> checked_ecast(x > 1)) |>
            DelayList.filter(x -> checked_ecast(x < 3)) |>
            DelayList.toList)


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filter(_ -> { Ref.put("a" :: Ref.get(l), l); true }) |>
        DelayList.filter(_ -> { Ref.put("b" :: Ref.get(l), l); true });
        assertEq(expected = ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    @Test
    def filterFusion02(): Unit \ Assert + IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filter(_x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.filter(_x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.toList);
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
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(   x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); x }    as _ \ {})) |>
        DelayList.filter(_ -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.toList);
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
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filter(_ -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.map(   x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); x    } as _ \ {})) |>
        DelayList.toList);
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
    	assertEq(expected = true, DelayList.toArray(rc, (ENil: DelayList[Unit]))`Array.sameElements` Array#{} @ rc)
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
    	assertEq(expected = true, DelayList.toArray(rc, (LList(lazy ENil): DelayList[Unit]))`Array.sameElements` Array#{} @ rc)
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        assertEq(expected = true, DelayList.toArray(rc, DelayList.range(-100, 1000))`Array.sameElements` Array.range(rc, -100, 1000))
    }

    @Test
    def toArray04(): Unit \ Assert = region rc {
    	assertEq(expected = true, DelayList.toArray(rc, LCons(1, lazy ENil))`Array.sameElements` Array#{1} @ rc)
    }

    @Test
    def toArray05(): Unit \ Assert = region rc {
    	assertEq(expected = true, DelayList.toArray(rc, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))))`Array.sameElements` Array#{1, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert =
        assertEq(expected = true, ((DelayList.toVector(ENil)) : Vector[Int32]) `Vector.equals` Vector.empty())

    @Test
    def toVector02(): Unit \ Assert =
        assertEq(expected = true, DelayList.toVector(LCons(0, lazy ENil)) `Vector.equals` Vector.singleton(0))

    @Test
    def toVector03(): Unit \ Assert =
        assertEq(expected = true, DelayList.toVector(DelayList.range(1, 4)) `Vector.equals` Vector.range(1, 4))

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
        assertEq(expected = 1 :: 2 :: 3 :: Nil, (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.toList)

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
        assertEq(expected = ENil, DelayList.zip((ENil: DelayList[Int32]), (LList(lazy ENil): DelayList[String])))

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

    @Test
    def zipWithPure01(): Unit \ Assert =
        assertEq(expected = ENil, (ENil: DelayList[Int32]) |> DelayList.zipWith((x, y) -> x + y, (ENil: DelayList[Int32])))

    @Test
    def zipWithPure02(): Unit \ Assert =
        assertEq(expected = ENil, LList(lazy ((ENil: DelayList[Int32]))) |> DelayList.zipWith((x, y) -> x + y, (ENil: DelayList[Int32])))

    @Test
    def zipWithPure03(): Unit \ Assert =
        assertEq(expected = ENil, ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> x + y, (ENil: DelayList[Int32])))

    @Test
    def zipWithPure04(): Unit \ Assert =
        assertEq(expected = ENil, (ENil: DelayList[Int32]) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))))

    @Test
    def zipWithPure05(): Unit \ Assert =
        assertEq(expected = ECons(2, ECons(4, ENil)), ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))))

    @Test
    def zipWithPure06(): Unit \ Assert =
        assertEq(expected = ECons(2, ENil), ECons(1, ENil) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))))

    @Test
    def zipWithPure07(): Unit \ Assert =
        assertEq(expected = ECons(2, ECons(4, ENil)), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> DelayList.zipWith((x, y) -> x + y, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithImpure01(): Unit \ Assert =
        assertEq(expected = ENil, (ENil: DelayList[Int32]) |> DelayList.zipWith((x, y) -> x + y, (ENil: DelayList[Int32])))

    @Test
    def zipWithImpure02(): Unit \ Assert =
        assertEq(expected = ENil, LList(lazy ((ENil: DelayList[Int32]))) |> DelayList.zipWith((x, y) -> x + y, (ENil: DelayList[Int32])))

    @Test
    def zipWithImpure03(): Unit \ Assert =
        assertEq(expected = ENil, ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> x + y, (ENil: DelayList[Int32])))

    @Test
    def zipWithImpure04(): Unit \ Assert =
        assertEq(expected = ENil, (ENil: DelayList[Int32]) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))))

    @Test
    def zipWithImpure05(): Unit \ Assert =
        assertEq(expected = ECons(2, ECons(4, ENil)), ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))))

    @Test
    def zipWithImpure06(): Unit \ Assert =
        assertEq(expected = ECons(2, ENil), ECons(1, ENil) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))))

    @Test
    def zipWithImpure07(): Unit \ Assert =
        assertEq(expected = ECons(2, ECons(4, ENil)), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> DelayList.zipWith((x, y) -> x + y, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith zipWith                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithZipWith01(): Unit \ Assert =
        assertEq(expected = ECons(3, ECons(6, ECons(9, ENil))), ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def zipWithZipWith02(): Unit \ Assert =
        assertEq(expected = ECons(3, ECons(6, ECons(9, ENil))), ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def zipWithZipWith03(): Unit \ Assert =
        assertEq(expected = ECons(3, ECons(6, ECons(9, ENil))), ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def zipWithZipWith04(): Unit \ Assert =
        assertEq(expected = ECons(3, ECons(6, ECons(9, ENil))), ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith zipWith fusion                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.zipWith((x, y) -> { Ref.put("a" :: Ref.get(l), l); (x, y) }, ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.zipWith((x, y) -> { Ref.put("b" :: Ref.get(l), l); (x, y) }, ECons(1, ECons(2, ECons(3, ENil))));
        assertEq(expected = "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil, List.reverse(Ref.get(l)))
    }

    @Test
    def zipWithFusion02(): Unit \ Assert + IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.zipWith((x, y) -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); (x, y) } as _ \ {}), ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.zipWith((x, y) -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); (x, y) } as _ \ {}), ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.toList);
        assertEq(expected = ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithIndex01(): Unit \ Assert =
        assertEq(expected = (ENil: DelayList[(Int32, Unit)]), DelayList.zipWithIndex((ENil: DelayList[Unit])))

    @Test
    def zipWithIndex02(): Unit \ Assert =
        assertEq(expected = ECons((0, 1), ENil), DelayList.zipWithIndex(ECons(1, ENil)))

    @Test
    def zipWithIndex03(): Unit \ Assert =
        assertEq(expected = LCons((0, 1), lazy ENil), DelayList.zipWithIndex(LCons(1, lazy ENil)))

    @Test
    def zipWithIndex04(): Unit \ Assert =
        assertEq(expected = LList(lazy ECons((0, 1), ENil)), DelayList.zipWithIndex(LList(lazy ECons(1, ENil))))

    @Test
    def zipWithIndex05(): Unit \ Assert =
        let l = DelayList.zipWithIndex(LList(lazy LList(lazy LCons(5, lazy ECons(7, ENil)))));
        assertEq(expected = LList(lazy LList(lazy LCons((0, 5), lazy ECons((1, 7), ENil)))), l)

    @test
    def zipWithIndex06(): Unit \ Assert =
        let l = DelayList.zipWithIndex(LList(lazy LCons(0, lazy LList(lazy
            LCons(1, lazy LList(lazy LCons(2, lazy
            LList(lazy LCons(3, lazy LList(lazy
            LCons(4, lazy LList(lazy LCons(5, lazy
            LList(lazy LCons(10, lazy LList(lazy ENil))))))))))))))));
        assertEq(expected = LList(lazy LCons((0, 0), lazy LList(lazy
                                        LCons((1, 1), lazy LList(lazy LCons((2, 2), lazy
                                        LList(lazy LCons((3, 3), lazy LList(lazy
                                        LCons((4, 4), lazy LList(lazy LCons((5, 5), lazy
                                        LList(lazy LCons((6, 10), lazy LList(lazy ENil))))))))))))))), l)

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Unit \ Assert =
        assertEq(expected = None, DelayList.reduceLeft((a, b) -> a + b, (ENil: DelayList[Int32])))

    @Test
    def reduceLeft02(): Unit \ Assert =
    	assertEq(expected = Some(1), DelayList.reduceLeft((a, b) -> a + b, ECons(1, ENil)))

    @Test
    def reduceLeft03(): Unit \ Assert =
    	assertEq(expected = Some(3), DelayList.reduceLeft((a, b) -> a + b, ECons(1, ECons(2, ENil))))

    @Test
    def reduceLeft04(): Unit \ Assert =
    	assertEq(expected = Some(6), DelayList.reduceLeft((a, b) -> a + b, ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def reduceLeft05(): Unit \ Assert =
    	assertEq(expected = Some(-8), DelayList.reduceLeft((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))))

    @Test
    def reduceLeft06(): Unit \ Assert =
        assertEq(expected = Some(2), DelayList.reduceLeft((a, b) -> b - a, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Unit \ Assert =
        assertEq(expected = None, DelayList.reduceRight((a, b) -> a + b, (ENil: DelayList[Int32])))

    @Test
    def reduceRight02(): Unit \ Assert =
        assertEq(expected = None, DelayList.reduceRight((a, b) -> a + b, (LList(lazy ENil): DelayList[Int32])))

    @Test
    def reduceRight03(): Unit \ Assert =
    	assertEq(expected = Some(1), DelayList.reduceRight((a, b) -> a + b, ECons(1, ENil)))

    @Test
    def reduceRight04(): Unit \ Assert =
    	assertEq(expected = Some(3), DelayList.reduceRight((a, b) -> a + b, ECons(1, ECons(2, ENil))))

    @Test
    def reduceRight05(): Unit \ Assert =
    	assertEq(expected = Some(6), DelayList.reduceRight((a, b) -> a + b, ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def reduceRight06(): Unit \ Assert =
        assertEq(expected = Some(-2), DelayList.reduceRight((a, b) -> b - a, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))))

    @Test
    def reduceRight07(): Unit \ Assert =
    	assertEq(expected = Some(-2), DelayList.reduceRight((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))))


    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimum01(): Unit \ Assert =
        assertEq(expected = None, DelayList.minimum((ENil: DelayList[Int32])))

    @Test
    def minimum02(): Unit \ Assert =
        assertEq(expected = Some(1), DelayList.minimum(ECons(1, ENil)))

    @Test
    def minimum03(): Unit \ Assert =
        assertEq(expected = Some(1), DelayList.minimum(ECons(3, ECons(2, ECons(1, ENil)))))

    @Test
    def minimum04(): Unit \ Assert =
        assertEq(expected = Some(0), DelayList.minimum(ECons(0, ECons(3, ECons(2, ECons(1, ENil))))))

    @Test
    def minimum05(): Unit \ Assert =
        assertEq(expected = Some(0), DelayList.minimum(DelayList.range(0, 99)))


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Unit \ Assert =
        assertEq(expected = None, DelayList.minimumBy((x, y) -> x <=> y, (ENil: DelayList[Int32])))

    @Test
    def minimumBy02(): Unit \ Assert =
        assertEq(expected = Some(1), DelayList.minimumBy((x, y) -> x <=> y, ECons(1, ENil)))

    @Test
    def minimumBy03(): Unit \ Assert =
        assertEq(expected = Some(1), DelayList.minimumBy((x, y) -> x <=> y, ECons(3, ECons(2, ECons(1, ENil)))))

    @Test
    def minimumBy04(): Unit \ Assert =
        assertEq(expected = Some(0), DelayList.minimumBy((x, y) -> x <=> y, ECons(0, ECons(3, ECons(2, ECons(1, ENil))))))

    @Test
    def minimumBy05(): Unit \ Assert =
        assertEq(expected = Some(0), DelayList.minimumBy((_, _) -> Comparison.LessThan, DelayList.range(0, 99)))


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximum01(): Unit \ Assert =
        assertEq(expected = None, DelayList.maximum((ENil: DelayList[Int32])))

    @Test
    def maximum02(): Unit \ Assert =
        assertEq(expected = Some(1), DelayList.maximum(ECons(1, ENil)))

    @Test
    def maximum03(): Unit \ Assert =
        assertEq(expected = Some(3), DelayList.maximum(ECons(3, ECons(2, ECons(1, ENil)))))

    @Test
    def maximum04(): Unit \ Assert =
        assertEq(expected = Some(3), DelayList.maximum(ECons(0, ECons(3, ECons(2, ECons(1, ENil))))))

    @Test
    def maximum05(): Unit \ Assert =
        assertEq(expected = Some(98), DelayList.maximum(DelayList.range(0, 99)))


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Unit \ Assert =
        assertEq(expected = None, DelayList.maximumBy((x, y) -> x <=> y, (ENil: DelayList[Int32])))

    @Test
    def maximumBy02(): Unit \ Assert =
        assertEq(expected = Some(1), DelayList.maximumBy((x, y) -> x <=> y, ECons(1, ENil)))

    @Test
    def maximumBy03(): Unit \ Assert =
        assertEq(expected = Some(3), DelayList.maximumBy((x, y) -> x <=> y, ECons(3, ECons(2, ECons(1, ENil)))))

    @Test
    def maximumBy04(): Unit \ Assert =
        assertEq(expected = Some(3), DelayList.maximumBy((x, y) -> x <=> y, ECons(0, ECons(3, ECons(2, ECons(1, ENil))))))

    @Test
    def maximumBy05(): Unit \ Assert =
        assertEq(expected = Some(98), DelayList.maximumBy((_, _) -> Comparison.LessThan, DelayList.range(0, 99)))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMapPure01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), (ENil: DelayList[Int32])))

    @Test
    def flatMapPure02(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), ECons(1, ENil)))

    @Test
    def flatMapPure03(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil)))))), DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def flatMapPure04(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil)))))), DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMapImpure01(): Unit \ Assert + IO =
        assertEq(expected = ENil, DelayList.flatMap(i -> checked_ecast(DelayList.repeat(i) |> DelayList.take(i)), (ENil: DelayList[Int32])))

    @Test
    def flatMapImpure02(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ENil), DelayList.flatMap(i -> checked_ecast(DelayList.repeat(i) |> DelayList.take(i)), ECons(1, ENil)))

    @Test
    def flatMapImpure03(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil)))))), DelayList.flatMap(i -> checked_ecast(DelayList.repeat(i) |> DelayList.take(i)), ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def flatMapImpure04(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil)))))), DelayList.flatMap(i -> checked_ecast(DelayList.repeat(i) |> DelayList.take(i)), ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapFlatMap01(): Unit \ Assert =
        let l = ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i)) |>
            DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i));
        assertEq(expected = ECons(1,
            ECons(2, ECons(2, ECons(2, ECons(2,
                ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil)))))))))))))), l)

    @test
    def flatMapFlatMap02(): Unit \ Assert + IO =
        let l = ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.flatMap(i -> checked_ecast(DelayList.repeat(i) |> DelayList.take(i))) |>
            DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i));
        assertEq(expected = ECons(1,
            ECons(2, ECons(2, ECons(2, ECons(2,
                ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil)))))))))))))), l)

    @test
    def flatMapFlatMap03(): Unit \ Assert + IO =
        let l = ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i)) |>
            DelayList.flatMap(i -> checked_ecast(DelayList.repeat(i) |> DelayList.take(i)));
        assertEq(expected = ECons(1,
            ECons(2, ECons(2, ECons(2, ECons(2,
                ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil)))))))))))))), l)

    @test
    def flatMapFlatMap04(): Unit \ Assert + IO =
        let l = ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.flatMap(i -> checked_ecast(DelayList.repeat(i) |> DelayList.take(i))) |>
            DelayList.flatMap(i -> checked_ecast(DelayList.repeat(i) |> DelayList.take(i)));
        assertEq(expected = ECons(1,
            ECons(2, ECons(2, ECons(2, ECons(2,
                ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil)))))))))))))), l)


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap fusion                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.flatMap(i -> { Ref.put("a" :: Ref.get(l), l); ECons(i, ENil) }) |>
        DelayList.flatMap(i -> { Ref.put("b" :: Ref.get(l), l); ECons(i, ENil) });
        assertEq(expected = ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    @test
    def flatMapFusion02(): Unit \ Assert + IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.flatMap(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); ECons(i, ENil) } as _ \ {})) |>
        DelayList.flatMap(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); ECons(i, ENil) } as _ \ {})) |>
        DelayList.toList);
        assertEq(expected = ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Unit \ Assert =
        assertFalse(DelayList.memberOf(0, ENil))

    @Test
    def memberOf02(): Unit \ Assert =
        assertFalse(DelayList.memberOf(0, ECons(1, ENil)))

    @Test
    def memberOf03(): Unit \ Assert =
        assertFalse(DelayList.memberOf(0, ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def memberOf04(): Unit \ Assert =
        assertTrue(DelayList.memberOf(0, ECons(1, ECons(2, ECons(3, ECons(0, ENil))))))

    @Test
    def memberOf05(): Unit \ Assert =
        assertTrue(DelayList.memberOf(0, ECons(0, ECons(1, ECons(2, ECons(3, ENil))))))

    @Test
    def memberOf06(): Unit \ Assert =
        assertTrue(DelayList.memberOf(0, ECons(1, ECons(1, ECons(1, ECons(0, ENil))))))

    @Test
    def memberOf07(): Unit \ Assert =
        assertTrue(DelayList.memberOf(1, ECons(1, ECons(1, ECons(1, ECons(1, ENil))))))

    @Test
    def memberOf08(): Unit \ Assert =
        assertTrue(DelayList.memberOf(0, ECons(1, ECons(1, ECons(0, ECons(1, ENil))))))

    @Test
    def memberOf09(): Unit \ Assert =
        assertTrue(DelayList.memberOf(-2, ECons(1, ECons(1000, ECons(-2, ECons(1, ENil))))))

    @Test
    def memberOf10(): Unit \ Assert =
        assertTrue(DelayList.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil))))))))


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.replace(src = 3, dst = 4, ENil))

    @Test
    def replace02(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), DelayList.replace(src = 3, dst = 4, ECons(1, ENil)))

    @Test
    def replace03(): Unit \ Assert =
        assertEq(expected = ECons(4, ENil), DelayList.replace(src = 3, dst = 4, ECons(3, ENil)))

    @Test
    def replace04(): Unit \ Assert =
        assertEq(expected = ECons(4, ECons(4, ECons(4, ENil))), DelayList.replace(src = 3, dst = 4, ECons(4, ECons(4, ECons(4, ENil)))))

    @Test
    def replace05(): Unit \ Assert =
        assertEq(expected = ECons(2, ECons(2, ECons(2, ENil))), DelayList.replace(src = 3, dst = 4, ECons(2, ECons(2, ECons(2, ENil)))))

    @Test
    def replace06(): Unit \ Assert =
        assertEq(expected = ECons(2, ECons(4, ECons(2, ENil))), DelayList.replace(src = 3, dst = 4, ECons(2, ECons(3, ECons(2, ENil)))))

    @Test
    def replace07(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(1, ECons(1, ENil))), DelayList.replace(src = 0, dst = 1, ECons(0, ECons(0, ECons(0, ENil)))))

    @Test
    def replace08(): Unit \ Assert =
        assertEq(expected = ECons(4, ECons(4, ECons(4, ENil))), DelayList.replace(src = 3, dst = 4, LList(lazy ECons(3, LList(lazy LCons(3, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Unit \ Assert =
        assertEq(expected = None, DelayList.findLeft(i -> i > 2, (ENil: DelayList[Int32])))

    @Test
    def findLeft02(): Unit \ Assert =
        assertEq(expected = None, DelayList.findLeft(i -> i > 2, ECons(1, ENil)))

    @test
    def findLeft03(): Unit \ Assert =
        assertEq(expected = Some(3), DelayList.findLeft(i -> i > 2, ECons(3, ENil)))

    @test
    def findLeft04(): Unit \ Assert =
        assertEq(expected = None, DelayList.findLeft(i -> i > 2, ECons(1, ECons(2, ENil))))

    @test
    def findLeft05(): Unit \ Assert =
        assertEq(expected = Some(6), DelayList.findLeft(i -> i > 2, ECons(6, ECons(-6, ENil))))

    @test
    def findLeft06(): Unit \ Assert =
        assertEq(expected = Some(7), DelayList.findLeft(i -> i > 2, ECons(7, ECons(6, ENil))))

    @test
    def findLeft07(): Unit \ Assert =
        assertEq(expected = Some(4), DelayList.findLeft(i -> i > 2, LList(lazy ECons(1, LList(lazy LCons(1, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Unit \ Assert =
        assertEq(expected = None, DelayList.findRight(i -> i > 2, (ENil: DelayList[Int32])))

    @test
    def findRight02(): Unit \ Assert =
        assertEq(expected = None, DelayList.findRight(i -> i > 2, ECons(1, ENil)))

    @test
    def findRight03(): Unit \ Assert =
        assertEq(expected = Some(3), DelayList.findRight(i -> i > 2, ECons(3, ENil)))

    @test
    def findRight04(): Unit \ Assert =
        assertEq(expected = None, DelayList.findRight(i -> i > 2, ECons(1, ECons(2, ENil))))

    @test
    def findRight05(): Unit \ Assert =
        assertEq(expected = Some(6), DelayList.findRight(i -> i > 2, ECons(6, ECons(-6, ENil))))

    @test
    def findRight06(): Unit \ Assert =
        assertEq(expected = Some(6), DelayList.findRight(i -> i > 2, ECons(7, ECons(6, ENil))))

    @test
    def findRight07(): Unit \ Assert =
        assertEq(expected = Some(4), DelayList.findRight(i -> i > 2, LList(lazy ECons(4, LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy LList(lazy ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex (pure)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexPure01(): Unit \ Assert =
        assertEq(expected = ENil, ENil |> DelayList.mapWithIndex((i, x) -> x + i))

    @test
    def mapWithIndexPure02(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), ECons(1, ENil) |> DelayList.mapWithIndex((i, x) -> x + i))

    @test
    def mapWithIndexPure03(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(3, ENil)), ECons(1, ECons(2, ENil)) |> DelayList.mapWithIndex((i, x) -> x + i))

    @test
    def mapWithIndexPure04(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(3, ECons(5, ENil))), ECons(1, ECons(2, ECons(3, ENil))) |> DelayList.mapWithIndex((i, x) -> x + i))

    @test
    def mapWithIndexPure05(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(3, ECons(5, ECons(7, ENil)))), ECons(1, ECons(2, ECons(3, ECons(4, ENil)))) |> DelayList.mapWithIndex((i, x) -> x + i))

    @test
    def mapWithIndexPure06(): Unit \ Assert =
        let l = LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) |> DelayList.mapWithIndex((i, x) -> x + i);
        assertEq(expected = ECons(1, ECons(3, ECons(5, ECons(7, ENil)))), l)


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex (impure)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndexImpure01(): Unit \ Assert + IO =
    	assertEq(expected = ENil, ENil |> DelayList.mapWithIndex((i, x) -> checked_ecast(x + i)))

    @Test
    def mapWithIndexImpure02(): Unit \ Assert + IO =
    	assertEq(expected = ECons(1, ENil), ECons(1, ENil) |> DelayList.mapWithIndex((i, x) -> checked_ecast(x + i)))

    @Test
    def mapWithIndexImpure03(): Unit \ Assert + IO =
    	assertEq(expected = ECons(1, ECons(3, ENil)), ECons(1, ECons(2, ENil)) |> DelayList.mapWithIndex((i, x) -> checked_ecast(x + i)))

    @Test
    def mapWithIndexImpure04(): Unit \ Assert + IO =
    	assertEq(expected = ECons(1, ECons(3, ECons(5, ENil))), ECons(1, ECons(2, ECons(3, ENil))) |> DelayList.mapWithIndex((i, x) -> checked_ecast(x + i)))

    @Test
    def mapWithIndexImpure05(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ECons(3, ECons(5, ECons(7, ENil)))), ECons(1, ECons(2, ECons(3, ECons(4, ENil)))) |> DelayList.mapWithIndex((i, x) -> checked_ecast(x + i)))

    @test
    def mapWithIndexImpure06(): Bool \ IO =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) |> DelayList.mapWithIndex((i, x) -> checked_ecast(x + i)) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex mapWithIndex                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndexMapWithIndex01(): Unit \ Assert =
        assertEq(expected = ECons(0, ECons(3, ECons(10, ENil))), ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> x + i) |>
            DelayList.mapWithIndex((i, x) -> x * i))

    @Test
    def mapWithIndexMapWithIndex02(): Unit \ Assert + IO =
        assertEq(expected = ECons(0, ECons(3, ECons(10, ENil))), ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> checked_ecast(x + i)) |>
            DelayList.mapWithIndex((i, x) -> x * i))

    @Test
    def mapWithIndexMapWithIndex03(): Unit \ Assert + IO =
        assertEq(expected = ECons(0, ECons(3, ECons(10, ENil))), ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> x + i) |>
            DelayList.mapWithIndex((i, x) -> checked_ecast(x * i)))

    @Test
    def mapWithIndexMapWithIndex04(): Unit \ Assert + IO =
        assertEq(expected = ECons(0, ECons(3, ECons(10, ENil))), ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> checked_ecast(x + i)) |>
            DelayList.mapWithIndex((i, x) -> checked_ecast(x * i)))


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
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.mapWithIndex((_, x) -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); x } as _ \ {})) |>
        DelayList.mapWithIndex((_, x) -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); x } as _ \ {})) |>
        DelayList.toList);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalate01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.intercalate(ECons(1, ECons(2, ENil)), ENil))

    @Test
    def intercalate02(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), DelayList.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(ECons(1, ENil), ENil)))

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

    @Test
    def intersperse01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.intersperse(-11, ENil))

    @Test
    def intersperse03(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), DelayList.intersperse(-11, ECons(1, ENil)))

    @Test
    def intersperse04(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil))))), DelayList.intersperse(-11, ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def intersperse05(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil))))))), DelayList.intersperse(-11, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))))

    @test
    def intersperse06(): Bool =
        DelayList.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhilePure01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.takeWhile(i -> i > 3, ENil))

    @Test
    def takeWhilePure02(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.takeWhile(i -> i > 3, ECons(1, ENil)))

    @Test
    def takeWhilePure03(): Unit \ Assert =
        assertEq(expected = ECons(4, ENil), DelayList.takeWhile(i -> i > 3, ECons(4, ENil)))

    @Test
    def takeWhilePure04(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.takeWhile(i -> i > 3, ECons(1, ECons(2, ENil))))

    @Test
    def takeWhilePure05(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.takeWhile(i -> i > 3, ECons(1, ECons(5, ENil))))

    @Test
    def takeWhilePure06(): Unit \ Assert =
        assertEq(expected = ECons(5, ENil), DelayList.takeWhile(i -> i > 3, ECons(5, ECons(1, ENil))))

    @Test
    def takeWhilePure07(): Unit \ Assert =
        assertEq(expected = ECons(5, ECons(8, ENil)), DelayList.takeWhile(i -> i > 3, ECons(5, ECons(8, ENil))))

    @Test
    def takeWhilePure08(): Unit \ Assert =
        assertEq(expected = ECons(5, ECons(8, ENil)), DelayList.takeWhile(i -> i > 3, LList(lazy ECons(5, LList(lazy LCons(8, lazy ENil))))))

    @test
    def takeWhilePure09(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(4, ECons(6, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileImpure01(): Unit \ Assert + IO =
        assertEq(expected = ENil, DelayList.takeWhile(i -> checked_ecast(i > 3), ENil))

    @Test
    def takeWhileImpure02(): Unit \ Assert + IO =
        assertEq(expected = ENil, DelayList.takeWhile(i -> checked_ecast(i > 3), ECons(1, ENil)))

    @Test
    def takeWhileImpure03(): Unit \ Assert + IO =
        assertEq(expected = ECons(4, ENil), DelayList.takeWhile(i -> checked_ecast(i > 3), ECons(4, ENil)))

    @Test
    def takeWhileImpure04(): Unit \ Assert + IO =
        assertEq(expected = ENil, DelayList.takeWhile(i -> checked_ecast(i > 3), ECons(1, ECons(2, ENil))))

    @Test
    def takeWhileImpure05(): Unit \ Assert + IO =
        assertEq(expected = ENil, DelayList.takeWhile(i -> checked_ecast(i > 3), ECons(1, ECons(5, ENil))))

    @Test
    def takeWhileImpure06(): Unit \ Assert + IO =
        assertEq(expected = ECons(5, ENil), DelayList.takeWhile(i -> checked_ecast(i > 3), ECons(5, ECons(1, ENil))))

    @Test
    def takeWhileImpure07(): Unit \ Assert + IO =
        assertEq(expected = ECons(5, ECons(8, ENil)), DelayList.takeWhile(i -> checked_ecast(i > 3), ECons(5, ECons(8, ENil))))

    @Test
    def takeWhileImpure08(): Unit \ Assert + IO =
        assertEq(expected = ECons(5, ECons(8, ENil)), DelayList.takeWhile(i -> checked_ecast(i > 3), LList(lazy ECons(5, LList(lazy LCons(8, lazy ENil))))))

    @test
    def takeWhileImpure09(): Bool \ IO =
        DelayList.takeWhile(i -> checked_ecast(i > 3), ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(4, ECons(6, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile takeWhile                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileTakeWhile01(): Unit \ Assert =
        assertEq(expected = ECons(4, ENil), ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.takeWhile(i -> i > 3) |>
            DelayList.takeWhile(i -> i < 5))

    @Test
    def takeWhileTakeWhile02(): Unit \ Assert + IO =
        assertEq(expected = ECons(4, ENil), ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.takeWhile(i -> checked_ecast(i > 3)) |>
            DelayList.takeWhile(i -> i < 5))

    @Test
    def takeWhileTakeWhile03(): Unit \ Assert + IO =
        assertEq(expected = ECons(4, ENil), ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.takeWhile(i -> i > 3) |>
            DelayList.takeWhile(i -> checked_ecast(i < 5)))

    @Test
    def takeWhileTakeWhile04(): Unit \ Assert + IO =
        assertEq(expected = ECons(4, ENil), ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.takeWhile(i -> checked_ecast(i > 3)) |>
            DelayList.takeWhile(i -> checked_ecast(i < 5)))


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
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 4 } as _ \ {})) |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 4 } as _ \ {})) |>
        DelayList.toList);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    @test
    def takeWhileFusion03(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 2 } as _ \ {})) |>
        DelayList.toList);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: Nil)
    }

    @test
    def takeWhileFusion04(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.takeWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.toList);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhilePure01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.dropWhile(i -> i > 3, ENil))

    @Test
    def dropWhilePure02(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), DelayList.dropWhile(i -> i > 3, ECons(1, ENil)))

    @Test
    def dropWhilePure03(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.dropWhile(i -> i > 3, ECons(4, ENil)))

    @Test
    def dropWhilePure04(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(2, ENil)), DelayList.dropWhile(i -> i > 3, ECons(1, ECons(2, ENil))))

    @Test
    def dropWhilePure05(): Unit \ Assert =
        assertEq(expected = ECons(1, ECons(5, ENil)), DelayList.dropWhile(i -> i > 3, ECons(1, ECons(5, ENil))))

    @Test
    def dropWhilePure06(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), DelayList.dropWhile(i -> i > 3, ECons(5, ECons(1, ENil))))

    @Test
    def dropWhilePure07(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.dropWhile(i -> i > 3, ECons(5, ECons(8, ENil))))

    @test
    def dropWhilePure08(): Bool =
        DelayList.dropWhile(i -> i > 3, LList(lazy ECons(5, LCons(8, lazy LList(lazy LCons(9, lazy LList(lazy ECons(10, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))))))))))) == ECons(1, ECons(2, ENil))

    @test
    def dropWhilePure09(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileImpure01(): Unit \ Assert + IO =
        assertEq(expected = ENil, DelayList.dropWhile(i -> checked_ecast(i > 3), ENil))

    @Test
    def dropWhileImpure02(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ENil), DelayList.dropWhile(i -> checked_ecast(i > 3), ECons(1, ENil)))

    @Test
    def dropWhileImpure03(): Unit \ Assert + IO =
        assertEq(expected = ENil, DelayList.dropWhile(i -> checked_ecast(i > 3), ECons(4, ENil)))

    @Test
    def dropWhileImpure04(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ECons(2, ENil)), DelayList.dropWhile(i -> checked_ecast(i > 3), ECons(1, ECons(2, ENil))))

    @Test
    def dropWhileImpure05(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ECons(5, ENil)), DelayList.dropWhile(i -> checked_ecast(i > 3), ECons(1, ECons(5, ENil))))

    @Test
    def dropWhileImpure06(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ENil), DelayList.dropWhile(i -> checked_ecast(i > 3), ECons(5, ECons(1, ENil))))

    @Test
    def dropWhileImpure07(): Unit \ Assert + IO =
        assertEq(expected = ENil, DelayList.dropWhile(i -> checked_ecast(i > 3), ECons(5, ECons(8, ENil))))

    @test
    def dropWhileImpure08(): Bool \ IO =
        DelayList.dropWhile(i -> checked_ecast(i > 3), LList(lazy ECons(5, LCons(8, lazy LList(lazy LCons(9, lazy LList(lazy ECons(10, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))))))))))) == ECons(1, ECons(2, ENil))

    @test
    def dropWhileImpure09(): Bool \ IO =
        DelayList.dropWhile(i -> checked_ecast(i > 3), ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile dropWhile                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileDropWhile01(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.dropWhile(i -> i < 5) |>
            DelayList.dropWhile(i -> i > 1))

    @Test
    def dropWhileDropWhile02(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ENil), ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.dropWhile(i -> checked_ecast(i < 5)) |>
            DelayList.dropWhile(i -> i > 1))

    @Test
    def dropWhileDropWhile03(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ENil), ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.dropWhile(i -> i < 5) |>
            DelayList.dropWhile(i -> checked_ecast(i > 1)))

    @Test
    def dropWhileDropWhile04(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ENil), ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.dropWhile(i -> checked_ecast(i < 5)) |>
            DelayList.dropWhile(i -> checked_ecast(i > 1)))


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
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 4 } as _ \ {})) |>
        DelayList.toList);
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: Nil)
    }

    @test
    def dropWhileFusion03(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 1 } as _ \ {})) |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.toList);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def dropWhileFusion04(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); i < 2 } as _ \ {})) |>
        DelayList.dropWhile(i -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); i < 3 } as _ \ {})) |>
        DelayList.toList);
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "b" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findMap01(): Unit \ Assert =
        assertEq(expected = None, DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ENil))

    @Test
    def findMap02(): Unit \ Assert =
        assertEq(expected = None, DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ENil)))

    @Test
    def findMap03(): Unit \ Assert =
        assertEq(expected = Some(1), DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(2, ENil)))

	@Test
	def findMap04(): Unit \ Assert =
		assertEq(expected = None, DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, ENil))))

	@Test
	def findMap05(): Unit \ Assert =
		assertEq(expected = Some(2), DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))))

	@Test
	def findMap06(): Unit \ Assert =
		assertEq(expected = Some(3), DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))))

	@Test
	def findMap07(): Unit \ Assert =
		assertEq(expected = Some(4), DelayList.findMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))))

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

    @Test
    def filterMapPure01(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ENil))

    @Test
    def filterMapPure02(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ENil)))

    @Test
    def filterMapPure03(): Unit \ Assert =
        assertEq(expected = ECons(1, ENil), DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(2, ENil)))

	@Test
	def filterMapPure04(): Unit \ Assert =
		assertEq(expected = ENil, DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, ENil))))

	@Test
	def filterMapPure05(): Unit \ Assert =
		assertEq(expected = ECons(2, ENil), DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))))

	@Test
	def filterMapPure06(): Unit \ Assert =
		assertEq(expected = ECons(3, ENil), DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))))

	@Test
	def filterMapPure07(): Unit \ Assert =
		assertEq(expected = ECons(4, ECons(3, ENil)), DelayList.filterMap(i -> if (i `Int32.remainder` 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))))

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

    @Test
    def filterMapImpure01(): Unit \ Assert + IO =
        assertEq(expected = ENil, DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None), ENil))

    @Test
    def filterMapImpure02(): Unit \ Assert + IO =
        assertEq(expected = ENil, DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None), ECons(1, ENil)))

    @Test
    def filterMapImpure03(): Unit \ Assert + IO =
        assertEq(expected = ECons(1, ENil), DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None), ECons(2, ENil)))

	@Test
	def filterMapImpure04(): Unit \ Assert + IO =
		assertEq(expected = ENil, DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None), ECons(1, ECons(3, ENil))))

	@Test
	def filterMapImpure05(): Unit \ Assert + IO =
		assertEq(expected = ECons(2, ENil), DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None), ECons(1, ECons(4, ENil))))

	@Test
	def filterMapImpure06(): Unit \ Assert + IO =
		assertEq(expected = ECons(3, ENil), DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None), ECons(6, ECons(-1, ENil))))

	@Test
	def filterMapImpure07(): Unit \ Assert + IO =
		assertEq(expected = ECons(4, ECons(3, ENil)), DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None), ECons(8, ECons(6, ENil))))

    @test
	def filterMapImpure08(): Bool \ IO =
		DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None), ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))

    @test
	def filterMapImpure09(): Bool \ IO =
		DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None),
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
            DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None)) |>
            DelayList.filterMap(i ->             if (i > 1)                Some(i * 3) else None) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap03(): Bool \ IO =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            DelayList.filterMap(i ->             if (i `Int32.remainder` 2 == 0) Some(i / 2) else None) |>
            DelayList.filterMap(i -> checked_ecast(if (i > 1)                Some(i * 3) else None)) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap04(): Bool \ IO =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            DelayList.filterMap(i -> checked_ecast(if (i `Int32.remainder` 2 == 0) Some(i / 2) else None)) |>
            DelayList.filterMap(i -> checked_ecast(if (i > 1)                Some(i * 3) else None)) == ECons(6, ECons(9, ENil))


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
        discard checked_ecast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filterMap(x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); Some(x) } as _ \ {})) |>
        DelayList.filterMap(x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); Some(x) } as _ \ {})) |>
        DelayList.toList);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def partition01(): Unit \ Assert =
        assertEq(expected = (ENil, ENil), DelayList.partition(i -> i > 3, ENil))

    @Test
    def partition02(): Unit \ Assert =
        assertEq(expected = (ENil, ECons(1, ENil)), DelayList.partition(i -> i > 3, ECons(1, ENil)))

    @Test
    def partition03(): Unit \ Assert =
        assertEq(expected = (ECons(4, ENil), ENil), DelayList.partition(i -> i > 3, ECons(4, ENil)))

    @Test
    def partition04(): Unit \ Assert =
        assertEq(expected = (ENil, ECons(1, ECons(2, ENil))), DelayList.partition(i -> i > 3, ECons(1, ECons(2, ENil))))

    @Test
    def partition05(): Unit \ Assert =
        assertEq(expected = (ECons(5, ENil), ECons(1, ENil)), DelayList.partition(i -> i > 3, ECons(1, ECons(5, ENil))))

    @Test
    def partition06(): Unit \ Assert =
        assertEq(expected = (ECons(5, ENil), ECons(1, ENil)), DelayList.partition(i -> i > 3, ECons(5, ECons(1, ENil))))

    @Test
    def partition07(): Unit \ Assert =
        assertEq(expected = (ECons(5, ECons(8, ENil)), ENil), DelayList.partition(i -> i > 3, ECons(5, ECons(8, ENil))))

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

    @Test
    def span01(): Unit \ Assert =
        assertEq(expected = (ENil, ENil), DelayList.span(i -> i > 3, ENil))

    @Test
    def span02(): Unit \ Assert =
        assertEq(expected = (ENil, ECons(1, ENil)), DelayList.span(i -> i > 3, ECons(1, ENil)))

    @Test
    def span03(): Unit \ Assert =
        assertEq(expected = (ECons(4, ENil), ENil), DelayList.span(i -> i > 3, ECons(4, ENil)))

    @Test
    def span04(): Unit \ Assert =
        assertEq(expected = (ENil, ECons(1, ECons(2, ENil))), DelayList.span(i -> i > 3, ECons(1, ECons(2, ENil))))

    @Test
    def span05(): Unit \ Assert =
        assertEq(expected = (ENil, ECons(1, ECons(5, ENil))), DelayList.span(i -> i > 3, ECons(1, ECons(5, ENil))))

    @Test
    def span06(): Unit \ Assert =
        assertEq(expected = (ECons(5, ENil), ECons(1, ENil)), DelayList.span(i -> i > 3, ECons(5, ECons(1, ENil))))

    @Test
    def span07(): Unit \ Assert =
        assertEq(expected = (ECons(5, ECons(8, ENil)), ENil), DelayList.span(i -> i > 3, ECons(5, ECons(8, ENil))))

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

    @Test
    def spanSpanFusion01(): Unit \ Assert = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.span(x -> { Ref.put("a" :: Ref.get(l), l); x < 3 }) |> fst |>
        DelayList.span(x -> { Ref.put("b" :: Ref.get(l), l); x < 3 }); // The working list is `1 :: 2 :: Nil`
        assertEq(expected = ("a" :: "a" :: "a" :: "b" :: "b" :: Nil), List.reverse(Ref.get(l)))
    }

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Unit \ Assert =
        assertEq(expected = 0, List.toDelayList(Nil) |> DelayList.sum)

    @Test
    def sum02(): Unit \ Assert =
        assertEq(expected = 1, List.toDelayList(1 :: Nil) |> DelayList.sum)

    @Test
    def sum03(): Unit \ Assert =
        assertEq(expected = 6, List.toDelayList(1 :: 2 :: 3 :: Nil) |> DelayList.sum)

    @Test
    def sum04(): Unit \ Assert =
        assertEq(expected = 3, List.toDelayList(1 :: 2 :: 3 :: -3 :: Nil) |> DelayList.sum)

    @Test
    def sum05(): Unit \ Assert =
        assertEq(expected = -10, List.toDelayList(-1 :: -2 :: -3 :: -4 :: Nil) |> DelayList.sum)

    @Test
    def sum06(): Unit \ Assert =
        assertEq(expected = 0, List.toDelayList(10 :: -10 :: Nil) |> DelayList.sum)

    @Test
    def sum07(): Unit \ Assert =
        assertEq(expected = 5050, List.range(1, 101) |> List.toDelayList |> DelayList.sum)


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Unit \ Assert =
        assertEq(expected = 0, List.toDelayList(Nil) |> DelayList.sumWith(x -> x + 1))

    @Test
    def sumWith02(): Unit \ Assert =
        assertEq(expected = 2, List.toDelayList(1 :: Nil) |> DelayList.sumWith(x -> x + 1))

    @Test
    def sumWith03(): Unit \ Assert =
        assertEq(expected = 9, List.toDelayList(1 :: 2 :: 3 :: Nil) |> DelayList.sumWith(x -> x + 1))

    @Test
    def sumWith04(): Unit \ Assert =
        assertEq(expected = 7, List.toDelayList(1 :: 2 :: 3 :: -3 :: Nil) |> DelayList.sumWith(x -> x + 1))

    @Test
    def sumWith05(): Unit \ Assert =
        assertEq(expected = -6, List.toDelayList(-1 :: -2 :: -3 :: -4 :: Nil) |> DelayList.sumWith(x -> x + 1))

    @Test
    def sumWith06(): Unit \ Assert =
        assertEq(expected = 2, List.toDelayList(10 :: -10 :: Nil) |> DelayList.sumWith(x -> x + 1))


    /////////////////////////////////////////////////////////////////////////////
    // order                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def order01(): Unit \ Assert =
        assertEq(expected = Comparison.EqualTo, (ENil: DelayList[Unit]) <=> (ENil: DelayList[Unit]))

    @Test
    def order02(): Unit \ Assert =
        assertEq(expected = Comparison.GreaterThan, ((1 :: Nil) |> List.toDelayList) <=> (ENil: DelayList[Int32]))

    @Test
    def order03(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, (ENil: DelayList[Int32]) <=> ((1 :: Nil) |> List.toDelayList))

    @Test
    def order04(): Unit \ Assert =
        assertEq(expected = Comparison.EqualTo, ((1 :: Nil) |> List.toDelayList) <=> ((1 :: Nil) |> List.toDelayList))

    @Test
    def order05(): Unit \ Assert =
        assertEq(expected = Comparison.GreaterThan, ((2 :: 1 :: Nil) |> List.toDelayList) <=> ((1 :: 1 :: Nil) |> List.toDelayList))

    @Test
    def order06(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, ((1 :: 1 :: Nil) |> List.toDelayList) <=> ((2 :: 1 :: Nil) |> List.toDelayList))

    @Test
    def order07(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, ((1 :: Nil) |> List.toDelayList) <=> ((1 :: 1 :: Nil) |> List.toDelayList))


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Unit \ Assert =
        assertEq(expected = "", (Nil: List[Int32]) |> List.toDelayList |>
            DelayList.join(","))

    @Test
    def join02(): Unit \ Assert =
        assertEq(expected = "1", (1 :: Nil) |> List.toDelayList |>
            DelayList.join(","))

    @Test
    def join03(): Unit \ Assert =
        assertEq(expected = "1,2,3", (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
            DelayList.join(","))

    @Test
    def join04(): Unit \ Assert =
        assertEq(expected = "1,2,3", ("1" :: "2" :: "3" :: Nil) |> List.toDelayList |>
            DelayList.join(","))


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Unit \ Assert =
        assertEq(expected = "", (Nil: List[Int32]) |> List.toDelayList |>
            DelayList.joinWith(x -> "${x + 1}", ","))

    @Test
    def joinWith02(): Unit \ Assert =
        assertEq(expected = "2", (1 :: Nil) |> List.toDelayList |>
            DelayList.joinWith(x -> "${x + 1}", ","))

    @Test
    def joinWith03(): Unit \ Assert =
        assertEq(expected = "2,3,4", (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
            DelayList.joinWith(x -> "${x + 1}", ","))

    @Test
    def joinWith04(): Unit \ Assert =
        assertEq(expected = "11,22,33", ("1" :: "2" :: "3" :: Nil) |> List.toDelayList |>
            DelayList.joinWith(x -> x + x, ","))


    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Unit \ Assert =
        assertEq(expected = (ENil: DelayList[Int32]), DelayList.ap(ENil, ENil))

    @Test
    def ap02(): Unit \ Assert =
        assertEq(expected = ENil, DelayList.ap(ECons((x -> x + 1), ENil), ENil))

    @Test
    def ap03(): Unit \ Assert =
        assertEq(expected = (ENil: DelayList[Int32]), DelayList.ap(ENil, ECons(5, ENil)))

    @Test
    def ap04(): Unit \ Assert =
        assertEq(expected = 6 :: Nil, DelayList.ap(ECons((x -> x + 1), ENil), ECons(5, ENil)) |> DelayList.toList)

    @Test
    def ap05(): Unit \ Assert =
        let f = List.toDelayList((x -> x + 1) :: Nil);
        let l = List.toDelayList(0 :: 5 :: Nil);
        assertEq(expected = 1 :: 6 :: Nil, DelayList.ap(f, l) |> DelayList.toList)

    @Test
    def ap06(): Unit \ Assert =
        let f = List.toDelayList((x -> x + 1) :: (x -> x * 2) :: Nil);
        let l = List.toDelayList(0 :: 4 :: Nil);
        assertEq(expected = 1 :: 5 :: 0 :: 8 :: Nil, DelayList.ap(f, l) |> DelayList.toList)

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Unit \ Assert =
        let l: DelayList[Identity[Int32]] = ENil;
        assertEq(expected = Identity.Identity(ENil), DelayList.sequence(l))

    @Test
    def sequence02(): Unit \ Assert =
        let l = List.toDelayList(Identity.Identity(1) :: Nil);
        assertEq(expected = Identity.Identity(List.toDelayList(1 :: Nil)), DelayList.sequence(l))

    @Test
    def sequence03(): Unit \ Assert =
        let l = List.toDelayList(Identity.Identity(1) :: Identity.Identity(2) :: Nil);
        assertEq(expected = Identity.Identity(List.toDelayList(1 :: 2 :: Nil)), DelayList.sequence(l))

    @Test
    def sequence04(): Unit \ Assert =
        let l = List.toDelayList(Identity.Identity(1) :: Identity.Identity(2) :: Identity.Identity(3) :: Nil);
        assertEq(expected = Identity.Identity(List.toDelayList(1 :: 2 :: 3 :: Nil)), DelayList.sequence(l))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = ENil;
        let ans = DelayList.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(ENil), ans);
        assertEq(expected = 0, Ref.get(st))
    }

    @Test
    def traverse02(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = List.toDelayList(1 :: Nil);
        let ans = DelayList.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(List.toDelayList(1 :: Nil)), ans);
        assertEq(expected = 1, Ref.get(st))
    }

    @Test
    def traverse03(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = List.toDelayList(1 :: 2 :: Nil);
        let ans = DelayList.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(List.toDelayList(1 :: 2 :: Nil)), ans);
        assertEq(expected = 2, Ref.get(st))
    }

    @Test
    def traverse04(): Unit \ Assert = region rc {
        let st = Ref.fresh(rc, 0);
        let l = List.toDelayList(1 :: 2 :: 3 :: Nil);
        let ans = DelayList.traverse(x -> {Ref.put(x, st); Identity.Identity(x)}, l);
        assertEq(expected = Identity.Identity(List.toDelayList(1 :: 2 :: 3 :: Nil)), ans);
        assertEq(expected = 3, Ref.get(st))
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Unit \ Assert + NonDet =
        let l1: List[Int32] = Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle;

        assertEq(expected = 0, DelayList.length(l2));
        assertEq(expected = Set#{}, DelayList.toSet(l2))

    @Test
    def shuffle02(): Unit \ Assert + NonDet =
        let l1 = 1 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle;

        assertEq(expected = 1, DelayList.length(l2));
        assertEq(expected = Set#{1}, DelayList.toSet(l2))

    @Test
    def shuffle03(): Unit \ Assert + NonDet =
        let l1 = 1 :: 2 :: 3 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle;

        assertEq(expected = 3, DelayList.length(l2));
        assertEq(expected = Set#{1, 2, 3}, DelayList.toSet(l2))

    @Test
    def shuffle04(): Unit \ Assert + NonDet =
        let l1 = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle;

        assertEq(expected = 10, DelayList.length(l2));
        assertEq(expected = Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}, DelayList.toSet(l2))


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert =
        assertEq(expected = "DelayList(1)", ToString.toString(List.toDelayList(1 :: Nil)))

    @Test
    def toString02(): Unit \ Assert =
        assertEq(expected = "DelayList(1, 2)", ToString.toString(List.toDelayList(1 :: 2 :: Nil)))

    @Test
    def toString03(): Unit \ Assert =
        assertEq(expected = "DelayList(93, 3, 4)", ToString.toString(List.toDelayList(93 :: 3 :: 4 :: Nil)))

    @Test
    def toString04(): Unit \ Assert =
        assertEq(expected = "DelayList(a, b, c)", ToString.toString(List.toDelayList('a' :: 'b' :: 'c' :: Nil)))

    @Test
    def toString05(): Unit \ Assert =
        assertEq(expected = "DelayList(true, false, true, true)", ToString.toString(List.toDelayList(true :: false :: true :: true :: Nil)))

    @Test
    def toString06(): Unit \ Assert =
        assertEq(expected = "DelayList(DelayList(1, 2), DelayList(2, 3), DelayList(4, 7))", ToString.toString(List.toDelayList(List.toDelayList(1 :: 2 :: Nil) :: List.toDelayList(2 :: 3 :: Nil) :: List.toDelayList(4 :: 7 :: Nil) :: Nil)))

}
