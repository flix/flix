/*
 * Copyright 2021 Jakob Schneider Villumsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

mod TestDelayList {
    use DelayList.{ENil, ECons, LCons, LList}

    /////////////////////////////////////////////////////////////////////////////
    // Eq                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def eq01(): Bool =
        ((ENil: DelayList[Unit])) == ((ENil: DelayList[Unit]))

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
        (DelayList.empty(): DelayList[Int32]) |> DelayList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        (DelayList.empty(): DelayList[Int32]) |> DelayList.isEmpty

    @test
    def isEmpty02(): Bool =
        not (DelayList.range(0, 1000) |> DelayList.isEmpty)

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def nonEmpty01(): Bool =
        not ((DelayList.empty(): DelayList[Int32]) |> DelayList.nonEmpty)

    @test
    def nonEmpty02(): Bool =
        DelayList.range(0, 1000) |> DelayList.nonEmpty


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool =
        DelayList.range(0, 0) |> DelayList.isEmpty

    @test
    def range02(): Bool =
        DelayList.range(100, 100) |> DelayList.isEmpty

    @test
    def range03(): Bool =
        DelayList.range(1, 0) |> DelayList.isEmpty

    @test
    def range04(): Bool =
        not (DelayList.range(0, 100) |> DelayList.isEmpty)

    @test
    def range05(): Bool =
        DelayList.range(0, 100) |> DelayList.toList == List.range(0, 100)

    @test
    def range06(): Bool =
        not (DelayList.range(-100, 100) |> DelayList.isEmpty)

    @test
    def range07(): Bool =
        DelayList.range(-100, 100) |> DelayList.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool =
        (DelayList.empty(): DelayList[Int32]) |> DelayList.head == None

    @test
    def head02(): Bool =
        DelayList.range(0, 1000) |> DelayList.head == Some(0)

    @test
    def head03(): Bool =
        let l = DelayList.range(0, 1000);
        DelayList.head(l) == Some(0)
            and DelayList.head(l) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool =
        DelayList.tail((ENil: DelayList[Unit])) == ENil

    @test
    def tail02(): Bool =
        DelayList.tail((LList(lazy ENil): DelayList[Unit])) == ENil

    @test
    def tail03(): Bool =
        DelayList.range(0, 1) |> DelayList.tail == ENil

    @test
    def tail04(): Bool =
    	LCons(1, lazy ENil) |> DelayList.tail == ENil

    @test
    def tail05(): Bool =
    	LList(lazy ECons(1, LList(lazy ENil))) |> DelayList.tail == ENil

    @test
    def tail06(): Bool =
    	ECons(1, LList(lazy ENil)) |> DelayList.tail == ENil

    @test
    def tail07(): Bool =
    	LCons(1, lazy LList(lazy ENil)) |> DelayList.tail == ENil

    @test
    def tail08(): Bool =
    	LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))) |> DelayList.tail == DelayList.range(2, 4)

    @test
    def tail09(): Bool =
    	LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))) |> DelayList.tail == DelayList.range(2, 4)

    @test
    def tail10(): Bool =
    	LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))) |> DelayList.tail == DelayList.range(2, 4)

    @test
    def tail11(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> DelayList.tail == DelayList.range(2, 4)

    @test
    def tail12(): Bool =
    	DelayList.range(0, 1000) |> DelayList.tail == DelayList.range(1, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        (DelayList.empty(): DelayList[Int32]) |> DelayList.take(1000) |> DelayList.isEmpty

    @test
    def take02(): Bool =
        (DelayList.empty(): DelayList[Int32]) |> DelayList.take(0) |> DelayList.isEmpty

    @test
    def take03(): Bool =
        (DelayList.range(0, 1000): DelayList[Int32]) |> DelayList.take(0) |> DelayList.isEmpty

    @test
    def take04(): Bool =
        (DelayList.range(0, 1000): DelayList[Int32])
            |> DelayList.take(500)
            |> DelayList.toList == List.range(0, 500)

    @test
    def take05(): Bool =
        (DelayList.range(0, 1000): DelayList[Int32])
            |> DelayList.take(1)
            |> DelayList.toList == List.range(0, 1)


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool =
        (DelayList.empty(): DelayList[Int32]) |> DelayList.length == 0

    @test
    def length02(): Bool =
        DelayList.range(0, 100000) |> DelayList.length == 100000


    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool =
        (DelayList.empty(): DelayList[Int32]) |> DelayList.size == 0

    @test
    def size02(): Bool =
        DelayList.range(0, 100000) |> DelayList.size == 100000


    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool =
        DelayList.reverse((DelayList.empty(): DelayList[Unit])) |> DelayList.toList == Nil

    @test
    def reverse02(): Bool =
        (1 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList == 1 :: Nil

    @test
    def reverse03(): Bool =
        (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList == 2 :: 1 :: Nil

    @test
    def reverse04(): Bool =
        (1 :: 1 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList == 1 :: 1 :: Nil

    @test
    def reverse05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList == 3 :: 2 :: 1 :: Nil

    @test
    def reverse06(): Bool =
        (1 :: 2 :: 3 :: 4 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList == 4 :: 3 :: 2 :: 1 :: Nil

    @test
    def reverse07(): Bool =
        DelayList.range(-100, 1000) |> DelayList.reverse |> DelayList.toList == List.range(-100, 1000) |> List.reverse


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool =
        Nil |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList == Nil

    @test
    def mapPure02(): Bool =
        (1 :: Nil) |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList == 2 :: Nil

    @test
    def mapPure03(): Bool =
        (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList == 2 :: 3 :: Nil

    @test
    def mapPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool \ IO =
        Nil |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList == Nil

    @test
    def mapImpure02(): Bool \ IO =
        (1 :: Nil) |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList == 2 :: Nil

    @test
    def mapImpure03(): Bool \ IO =
        (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList == 2 :: 3 :: Nil

    @test
    def mapImpure04(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapMap01(): Bool =
        (1 :: 2 :: 3 :: Nil)         |>
            List.toDelayList          |>
            DelayList.map(x -> x + 1) |>
            DelayList.map(x -> x * 2) |>
            DelayList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap02(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                     |>
            List.toDelayList                      |>
            DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |>
            DelayList.map(x -> x * 2)             |>
            DelayList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap03(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                     |>
            List.toDelayList                      |>
            DelayList.map(x -> x + 1)             |>
            DelayList.map(x -> unchecked_cast(x * 2 as _ \ IO)) |>
            DelayList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap04(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                     |>
            List.toDelayList                      |>
            DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |>
            DelayList.map(x -> unchecked_cast(x * 2 as _ \ IO)) |>
            DelayList.toList == 4 :: 6 :: 8 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(x -> { Ref.put("a" :: Ref.get(l), l); x }) |>
        DelayList.map(x -> { Ref.put("b" :: Ref.get(l), l); x });
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def mapFusion02(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); x } as _ \ {})) |>
        DelayList.map(x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); x } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool =
        Nil |> List.toDelayList |> DelayList.filter(x -> x > 100) |> DelayList.toList == Nil

    @test
    def filterPure02(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 0) |> DelayList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterPure03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 1) |> DelayList.toList == 2 :: 3 :: Nil

    @test
    def filterPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 2) |> DelayList.toList == 3 :: Nil

    @test
    def filterPure05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 3) |> DelayList.toList == Nil

    @test
    def filterPure06(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 100) |> DelayList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool \ IO =
        Nil |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 100 as _ \ IO)) |> DelayList.toList == Nil

    @test
    def filterImpure02(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 0 as _ \ IO)) |> DelayList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterImpure03(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 1 as _ \ IO)) |> DelayList.toList == 2 :: 3 :: Nil

    @test
    def filterImpure04(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 2 as _ \ IO)) |> DelayList.toList == 3 :: Nil

    @test
    def filterImpure05(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 3 as _ \ IO)) |> DelayList.toList == Nil

    @test
    def filterImpure06(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 100 as _ \ IO)) |> DelayList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFilter01(): Bool =
        (1 :: 2 :: 3 :: Nil)            |>
            List.toDelayList             |>
            DelayList.filter(x -> x > 1) |>
            DelayList.filter(x -> x < 3) |>
            DelayList.toList == 2 :: Nil

    @test
    def filterFilter02(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                                 |>
            List.toDelayList                                 |>
            DelayList.filter(x -> unchecked_cast(x > 1 as _ \ IO)) |>
            DelayList.filter(x -> x < 3)                     |>
            DelayList.toList == 2 :: Nil

    @test
    def filterFilter03(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                                 |>
            List.toDelayList                                 |>
            DelayList.filter(x -> x > 1)                     |>
            DelayList.filter(x -> unchecked_cast(x < 3 as _ \ IO)) |>
            DelayList.toList == 2 :: Nil

    @test
    def filterFilter04(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                                 |>
            List.toDelayList                                 |>
            DelayList.filter(x -> unchecked_cast(x > 1 as _ \ IO)) |>
            DelayList.filter(x -> unchecked_cast(x < 3 as _ \ IO)) |>
            DelayList.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFusion01(): Bool= region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList     |>
        DelayList.filter(_ -> { Ref.put("a" :: Ref.get(l), l); true }) |>
        DelayList.filter(_ -> { Ref.put("b" :: Ref.get(l), l); true });
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def filterFusion02(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList  |>
        DelayList.filter(_x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.filter(_x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFilterFusion01(): Bool = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(   x -> { Ref.put("a" :: Ref.get(l), l); x }) |>
        DelayList.filter(_ -> { Ref.put("b" :: Ref.get(l), l); true });
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def mapFilterFusion02(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(   x -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); x }    as _ \ {})) |>
        DelayList.filter(_ -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterThenMapFusion01(): Bool = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filter(_ -> { Ref.put("a" :: Ref.get(l), l); true }) |>
        DelayList.map(   x -> { Ref.put("b" :: Ref.get(l), l); x    });
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def filterThenMapFusion02(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filter(_ -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); true } as _ \ {})) |>
        DelayList.map(   x -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); x    } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
    	DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, ENil) == 100

    @test
    def foldLeft02(): Bool =
    	DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, LList(lazy ENil)) == 100

    @test
    def foldLeft03(): Bool =
    	DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy ENil)) == 198

    @test
    def foldLeft04(): Bool =
    	DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == 196

    @test
    def foldLeft05(): Bool =
    	DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 386

    @test
    def foldLeft06(): Bool =
    	DelayList.foldLeft((acc, x) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
    	DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, ENil) == 100

    @test
    def foldRight02(): Bool =
    	DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, LList(lazy ENil)) == 100

    @test
    def foldRight03(): Bool =
    	DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy LList(lazy ENil))) == 198

    @test
    def foldRight04(): Bool =
    	DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, ECons(1, LCons(2, lazy ENil))) == 194

    @test
    def foldRight05(): Bool =
    	DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 382

    @test
    def foldRight06(): Bool =
    	DelayList.foldRight((x, acc) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool =
    	DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, ENil) == 100

    @test
    def foldRightWithCont02(): Bool =
    	DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, LList(lazy ENil)) == 100

    @test
    def foldRightWithCont03(): Bool =
    	DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy LList(lazy ENil))) == 198

    @test
    def foldRightWithCont04(): Bool =
    	DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, ECons(1, LCons(2, lazy ENil))) == 194

    @test
    def foldRightWithCont05(): Bool =
    	DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 382

    @test
    def foldRightWithCont06(): Bool =
    	DelayList.foldRightWithCont((x, k) -> x :: k(), Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool =
    	DelayList.foldMap(x -> 2 * x, ENil) == 0

    @test
    def foldMap02(): Bool =
    	DelayList.foldMap(x -> 2 * x, LList(lazy ENil)) == 0

    @test
    def foldMap03(): Bool =
    	DelayList.foldMap(x -> 2 * x, LCons(1, lazy LList(lazy ENil))) == 2

    @test
    def foldMap04(): Bool =
    	DelayList.foldMap(x -> 2 * x, ECons(1, LCons(2, lazy ENil))) == 6

    @test
    def foldMap05(): Bool =
    	DelayList.foldMap(x -> 2 * x, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 12

    @test
    def foldMap06(): Bool =
    	DelayList.foldMap(Int32.toString, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == "123"

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let l = ENil;
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEach(fn, l);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEach02(): Bool = region rc {
        let l = LList(lazy ENil);
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEach(fn, l);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEach03(): Bool = region rc {
        let l = LCons(1, lazy LList(lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEach(fn, l);
        StringBuilder.toString(sb) == "T"
    }

    @test
    def forEach04(): Bool = region rc {
        let l = LCons(-1, lazy LList(lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEach(fn, l);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def forEach05(): Bool = region rc {
        let l = ECons(1, LCons(-1, lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEach(fn, l);
        StringBuilder.toString(sb) == "TF"
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region rc {
        let l = ENil;
        let sb = StringBuilder.empty(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEachWithIndex(fn, l);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEachWithIndex02(): Bool = region rc {
        let l = LList(lazy ENil);
        let sb = StringBuilder.empty(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEachWithIndex(fn, l);
        StringBuilder.toString(sb) == ""
    }

    @test
    def forEachWithIndex03(): Bool = region rc {
        let l = LCons(1, lazy LList(lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEachWithIndex(fn, l);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def forEachWithIndex04(): Bool = region rc {
        let l = LCons(-1, lazy LList(lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEachWithIndex(fn, l);
        StringBuilder.toString(sb) == "F"
    }

    @test
    def forEachWithIndex05(): Bool = region rc {
        let l = ECons(1, LCons(-1, lazy ENil));
        let sb = StringBuilder.empty(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEachWithIndex(fn, l);
        StringBuilder.toString(sb) == "FT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region rc {
    	DelayList.toArray(rc, (ENil: DelayList[Unit]))`Array.sameElements` Array#{} @ rc
    }

    @test
    def toArray02(): Bool = region rc {
    	DelayList.toArray(rc, (LList(lazy ENil): DelayList[Unit]))`Array.sameElements` Array#{} @ rc
    }

    @test
    def toArray03(): Bool = region rc {
        DelayList.toArray(rc, DelayList.range(-100, 1000))`Array.sameElements` Array.range(rc, -100, 1000)
    }

    @test
    def toArray04(): Bool = region rc {
    	DelayList.toArray(rc, LCons(1, lazy ENil))`Array.sameElements` Array#{1} @ rc
    }

    @test
    def toArray05(): Bool = region rc {
    	DelayList.toArray(rc, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))))`Array.sameElements` Array#{1, 2, 3} @ rc
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool =
        ((DelayList.toVector(ENil)) : Vector[Int32]) `Vector.equals` Vector.empty()

    @test
    def toVector02(): Bool =
        DelayList.toVector(LCons(0, lazy ENil)) `Vector.equals` Vector.singleton(0)

    @test
    def toVector03(): Bool =
        DelayList.toVector(DelayList.range(1, 4)) `Vector.equals` Vector.range(1, 4)

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool =
    	(DelayList.toList(ENil): List[Unit]) == Nil

    @test
    def toList02(): Bool =
    	(DelayList.toList(LList(lazy ENil)): List[Unit]) == Nil

    @test
    def toList03(): Bool =
        DelayList.range(-1000, 1000) |> DelayList.toList == List.range(-1000, 1000)

    @test
    def toList04(): Bool =
        DelayList.range(-1000, 1000000) |> DelayList.take(1000) |> DelayList.toList == List.range(-1000, 0)

    @test
    def toList05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.toList == (1 :: 2 :: 3 :: Nil)

    @test
    def toList06(): Bool =
    	DelayList.toList(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
    	let l = (ENil: DelayList[Int32]);
        let d1 = DelayList.toMutDeque(rc, l);

        let d2 = MutDeque.empty(rc);

        d1 `MutDeque.sameElements` d2

    }

    @test
    def toMutDeque02(): Bool = region rc {
    	let l = (LList(lazy ENil): DelayList[Int32]);
        let d1 = DelayList.toMutDeque(rc, l);

        let d2 = MutDeque.empty(rc);

        d1 `MutDeque.sameElements` d2

    }

    @test
    def toMutDeque03(): Bool = region rc {
    	let l = LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))));
        let d1 = DelayList.toMutDeque(rc, l);

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d2);
        MutDeque.pushBack(2, d2);
        MutDeque.pushBack(3, d2);

        d1 `MutDeque.sameElements` d2
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = DelayList.toMap((ENil: DelayList[(Unit, Unit)])) == Map#{}

    @test
    def toMap02(): Bool = DelayList.toMap((LList(lazy ENil): DelayList[(Unit, Unit)])) == Map#{}

    @test
    def toMap03(): Bool = DelayList.toMap(ECons((1, true), ENil)) == Map#{1 => true}

    @test
    def toMap04(): Bool = DelayList.toMap(LList(lazy LCons((1, true), lazy LList(lazy LCons((2, false), lazy ENil))))) == Map#{1 => true, 2 => false}

    @test
    def toMap05(): Bool = DelayList.toMap(ECons((1, true), ECons((1, false), ENil))) == Map#{1 => false}


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool =
    	(DelayList.toSet(ENil): Set[Unit]) == Set.empty()

    @test
    def toSet02(): Bool =
    	(DelayList.toSet(LList(lazy ENil)): Set[Unit]) == Set.empty()

    @test
    def toSet03(): Bool =
        DelayList.range(-1000, 1000) |> DelayList.toSet == Set.range(-1000, 1000)

    @test
    def toSet04(): Bool =
        DelayList.range(-1000, 1000000) |> DelayList.take(1000) |> DelayList.toSet == Set.range(-1000, 0)

    @test
    def toSet05(): Bool =
    	DelayList.toSet(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == Set#{1, 2, 3}


    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
    	DelayList.append((ENil: DelayList[Unit]), (ENil: DelayList[Unit])) == ENil

    @test
    def append02(): Bool =
    	DelayList.append((LList(lazy ENil): DelayList[Unit]), (ENil: DelayList[Unit])) == ENil

    @test
    def append03(): Bool =
    	DelayList.append(ECons(1, ENil), ENil) == ECons(1, ENil)

    @test
    def append04(): Bool =
    	DelayList.append(ENil, LCons(1, lazy ENil)) == ECons(1, ENil)

    @test
    def append05(): Bool =
    	DelayList.append(ECons(1, ECons(2, ECons(3, ENil))), ECons(4, LList(lazy ENil))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))

    @test
    def append06(): Bool =
    	DelayList.append(ECons(4, ENil), ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(4, ECons(1, ECons(2, ECons(3, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        DelayList.count(x -> x > 3, ENil) == 0

    @test
    def count02(): Bool =
    	DelayList.count(x -> x > 3, LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy ENil))))))) == 0

    @test
    def count03(): Bool =
    	DelayList.count(x -> x > 3, ECons(4, ENil)) == 1

    @test
    def count04(): Bool =
        DelayList.count(_ -> true, DelayList.range(1000, 2000)) == 1000

    @test
    def count05(): Bool =
        DelayList.count(_ -> false, DelayList.range(1000, 2000)) == 0

    @test
    def count06(): Bool =
        DelayList.count(x -> x > 3, ECons(1, ECons(2, ENil))) == 0

    @test
    def count07(): Bool =
        DelayList.count(x -> x > 3, ECons(1, ECons(8, ENil))) == 1

    @test
    def count08(): Bool =
        DelayList.count(x -> x > 3, ECons(8, ECons(1, ENil))) == 1


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool =
        DelayList.drop(-1, (ENil: DelayList[Unit])) == ENil

    @test
    def drop02(): Bool =
        DelayList.drop(0, (ENil: DelayList[Unit])) == ENil

    @test
    def drop03(): Bool =
        DelayList.drop(1, (ENil: DelayList[Unit])) == ENil

    @test
    def drop04(): Bool =
        DelayList.drop(-1, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def drop05(): Bool =
        DelayList.drop(0, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def drop06(): Bool =
        DelayList.drop(1, ECons(1, ENil)) == ENil

    @test
    def drop07(): Bool =
    	DelayList.drop(1, LList(lazy ECons(1, LList(lazy ENil)))) == ENil

    @test
    def drop08(): Bool =
        DelayList.drop(2, ECons(1, ENil)) == ENil

    @test
    def drop09(): Bool =
    	DelayList.range(0, 1000) |> DelayList.drop(500) == DelayList.range(500, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool =
        DelayList.flatten((ENil: DelayList[DelayList[Unit]])) == ENil

    @test
    def flatten02(): Bool =
        (DelayList.flatten(ECons(ENil, ENil)): DelayList[Unit]) == ENil

    @test
    def flatten03(): Bool =
        (DelayList.flatten(ECons(ENil, ECons(ENil, ENil))): DelayList[Unit]) == ENil

    @test
    def flatten04(): Bool =
    	DelayList.flatten(ECons(LCons(1, lazy ENil), ENil)) == ECons(1, ENil)

    @test
    def flatten05(): Bool =
    	DelayList.flatten(ECons(LList(lazy ECons(1, LList(lazy ENil))), ENil)) == ECons(1, ENil)

    @test
    def flatten06(): Bool =
        DelayList.flatten(ECons((ECons(1, ECons(2, ENil))), ENil)) == ECons(1, ECons(2, ENil))

    @test
    def flatten07(): Bool =
    	DelayList.flatten(ECons(ENil, ECons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ENil))) == ECons(1, ECons(2, ENil))

    @test
    def flatten08(): Bool =
    	DelayList.flatten(ECons(ECons(1, LCons(2, lazy ENil)), ECons((ECons(1, ECons(2, ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @test
    def flatten09(): Bool =
    	DelayList.flatten(ECons((ECons(1, ECons(2, ENil))), ECons(ENil, ECons(ENil, ECons(LList(lazy LCons(3, lazy LCons(4, lazy ENil))), ENil))))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        DelayList.exists(i -> i > 3, ENil) == false

    @test
    def exists02(): Bool =
        (1 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == false

    @test
    def exists03(): Bool =
        (5 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == true

    @test
    def exists04(): Bool =
        (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == false

    @test
    def exists05(): Bool =
        (1 :: 6 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == true

    @test
    def exists06(): Bool =
        (6 :: 1 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == true

    @test
    def exists07(): Bool =
        (16 :: 6 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == true

    @test
    def exists08(): Bool =
        (1 :: -9 :: 3 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == false

    @test
    def exists09(): Bool =
        (1 :: 9 :: 3 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == true


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool =
        DelayList.forAll(i -> i > 3, ENil) == true

    @test
    def forAll02(): Bool =
        (1 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @test
    def forAll03(): Bool =
        (5 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == true

    @test
    def forAll04(): Bool =
        (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @test
    def forAll05(): Bool =
        (1 :: 6 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @test
    def forAll06(): Bool =
        (6 :: 1 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @test
    def forAll07(): Bool =
        (16 :: 6 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == true

    @test
    def forAll08(): Bool =
        (1 :: -9 :: 3 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @test
    def forAll09(): Bool =
        (1 :: 9 :: 3 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @test
    def forAll10(): Bool =
        (11 :: 9 :: 31 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == true


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool =
        DelayList.repeat(1) |> DelayList.take(-1) == ENil

    @test
    def repeat02(): Bool =
        DelayList.repeat(1) |> DelayList.take(0) == ENil

    @test
    def repeat03(): Bool =
        DelayList.repeat(1) |> DelayList.take(1) == ECons(1, ENil)

    @test
    def repeat04(): Bool =
        DelayList.repeat(1) |> DelayList.take(2) == ECons(1, ECons(1, ENil))

    @test
    def repeat05(): Bool =
        DelayList.repeat(1) |> DelayList.take(3) == ECons(1, ECons(1, ECons(1, ENil)))

    @test
    def repeat07(): Bool =
        DelayList.repeat(3) |> DelayList.take(3) == ECons(3, ECons(3, ECons(3, ENil)))

    @test
    def repeat08(): Bool =
        DelayList.repeat("a") |> DelayList.take(3) == ECons("a", ECons("a", ECons("a", ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // from                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def startFrom01(): Bool =
        DelayList.startFrom(-1) |> DelayList.take(0) == ENil

    @test
    def startFrom02(): Bool =
        DelayList.startFrom(0) |> DelayList.take(0) == ENil

    @test
    def startFrom04(): Bool =
        DelayList.startFrom(100) |> DelayList.take(-1) == ENil

    @test
    def startFrom05(): Bool =
    	DelayList.startFrom(-1) |> DelayList.take(1) == ECons(-1, ENil)

    @test
    def startFrom06(): Bool =
    	DelayList.startFrom(5) |> DelayList.take(1) == ECons(5, ENil)

    @test
    def startFrom07(): Bool =
    	DelayList.startFrom(1) |> DelayList.take(3) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    def startFrom08(): Bool =
    	DelayList.startFrom(-1000) |> DelayList.take(1000) == DelayList.range(-1000, 0)

    @test
    def startFrom09(): Bool =
    	DelayList.startFrom(-1000) |> DelayList.take(2000) == DelayList.range(-1000, 1000)

    @test
    def startFrom10(): Bool =
    	DelayList.startFrom(0) |> DelayList.take(2000) == DelayList.range(0, 2000)


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool =
        DelayList.last((ENil: DelayList[Unit])) == None

    @test
    def last02(): Bool =
        DelayList.last((LList(lazy ENil): DelayList[Unit])) == None

    @test
    def last03(): Bool =
        DelayList.range(0, 1) |> DelayList.last == Some(0)

    @test
    def last06(): Bool =
    	ECons(1, ENil) |> DelayList.last == Some(1)

    @test
    def last11(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> DelayList.last == Some(3)

    @test
    def last12(): Bool =
    	DelayList.range(0, 1000) |> DelayList.last == Some(999)


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toIter01(): Bool = region rc {
    	DelayList.iterator(rc, (ENil: DelayList[Unit])) |> Iterator.toList == Nil
    }

    @test
    def toIter02(): Bool = region rc {
    	DelayList.iterator(rc, (LList(lazy ENil): DelayList[Unit])) |> Iterator.toList == Nil
    }

    @test
    def toIter06(): Bool = region rc {
    	DelayList.iterator(rc, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def toIter09(): Bool = region rc {
    	DelayList.range(-100, 100) |> DelayList.iterator(rc) |> Iterator.toList == List.range(-100, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool =
        DelayList.zip((ENil: DelayList[Int32]), (ENil: DelayList[String])) == ENil

    @test
    def zip02(): Bool =
        DelayList.zip((LList(lazy ENil): DelayList[Int32]), (ENil: DelayList[String])) == ENil

    @test
    def zip03(): Bool =
    	DelayList.zip((ENil: DelayList[Int32]),( LList(lazy ENil): DelayList[String])) == ENil

    @test
    def zip04(): Bool =
    	DelayList.zip((LList(lazy ENil): DelayList[Int32]), (LList(lazy ENil): DelayList[String])) == ENil

    @test
    def zip05(): Bool =
        DelayList.zip(ECons(1, ECons(2, ENil)), (ENil: DelayList[String])) == ENil

    @test
    def zip06(): Bool =
    	DelayList.zip((ENil: DelayList[String]), ECons(1, ECons(2, ENil))) == ENil

    @test
    def zip07(): Bool =
    	DelayList.zip(LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil)))), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(("a", 1), ECons(("b", 2), ENil))

    @test
    def zip08(): Bool =
    	DelayList.zip(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))), LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil))))) == ECons((1, "a"), ECons((2, "b"), ECons((3, "c"), ENil)))

    @test
    def zip09(): Bool =
        DelayList.zip(DelayList.range(1, 10), DelayList.range(1, 10)) |> DelayList.take(3) == ECons((1, 1), ECons((2, 2), ECons((3, 3), ENil)))


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

    @test
    def zipWithFusion01(): Bool = region rc {
        let l = Ref.fresh(rc, Nil);
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.zipWith((x, y) -> { Ref.put("a" :: Ref.get(l), l); (x, y) }, ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.zipWith((x, y) -> { Ref.put("b" :: Ref.get(l), l); (x, y) }, ECons(1, ECons(2, ECons(3, ENil))));
        List.reverse(Ref.get(l)) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @test
    def zipWithFusion02(): Bool \ IO = region rc {
        let l = Ref.fresh(rc, Nil);
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.zipWith((x, y) -> unchecked_cast({ Ref.put("a" :: Ref.get(l), l); (x, y) } as _ \ {}), ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.zipWith((x, y) -> unchecked_cast({ Ref.put("b" :: Ref.get(l), l); (x, y) } as _ \ {}), ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.toList as _ \ IO);
        List.reverse(Ref.get(l)) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
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
    def shuffle01(): Bool \ IO =
        let l1: List[Int32] = Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle(Random.fresh());

        DelayList.length(l2) == 0 and DelayList.toSet(l2) == Set#{}

    @test
    def shuffle02(): Bool \ IO =
        let l1 = 1 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle(Random.fresh());

        DelayList.length(l2) == 1 and DelayList.toSet(l2) == Set#{1}

    @test
    def shuffle03(): Bool \ IO =
        let l1 = 1 :: 2 :: 3 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle(Random.fresh());

        DelayList.length(l2) == 3 and DelayList.toSet(l2) == Set#{1, 2, 3}

    @test
    def shuffle04(): Bool \ IO =
        let l1 = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle(Random.fresh());

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
