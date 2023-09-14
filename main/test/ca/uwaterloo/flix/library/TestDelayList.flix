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

    @Test
    def eq01(): Bool =
        ENil: DelayList[Unit] == ENil: DelayList[Unit]

    @Test
    def eq02(): Bool =
        ECons(1, ENil) == ECons(1, ENil)

    @Test
    def eq03(): Bool =
        LCons(1, lazy ENil) == LCons(1, lazy ENil)

    @Test
    def eq04(): Bool =
        LList(lazy ECons(1, ENil)) == LList(lazy ECons(1, ENil))

    @Test
    def eq05(): Bool =
        ECons(1, ENil) == LCons(1, lazy ENil)

    @Test
    def eq06(): Bool =
        LCons(1, lazy ENil) == ECons(1, ENil)

    @Test
    def eq07(): Bool =
        ECons(1, ENil) == LList(lazy ECons(1, ENil))

    @Test
    def eq08(): Bool =
        LList(lazy ECons(1, ENil)) == ECons(1, ENil)

    @Test
    def eq09(): Bool =
        ECons(1, ENil) == LList(lazy LCons(1, lazy ENil))

    @Test
    def eq10(): Bool =
        LList(lazy LCons(1, lazy ENil)) == ECons(1, ENil)

    @Test
    def eq11(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @Test
    def eq12(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @Test
    def eq13(): Bool =
        LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @Test
    def eq14(): Bool =
        ECons(1, LCons(2, lazy ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @Test
    def eq15(): Bool =
        LCons(1, lazy ECons(2, LCons(3, lazy ENil))) == ECons(1, ECons(2, ECons(3, ENil)))

    @Test
    def eq16(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) == LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil)))

    @Test
    def eq17(): Bool =
        LList(lazy ECons(1, ECons(2, ECons(3, ENil)))) == LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @Test
    def eq18(): Bool =
        LList(lazy ECons(1, ECons(2, ECons(3, ENil)))) == LList(lazy LCons(1, lazy LCons(2, lazy LCons(3, lazy ENil))))

    @Test
    def eq19(): Bool =
        ECons(0, ENil) != ECons(1, ENil)

    @Test
    def eq20(): Bool =
        ECons(0, ENil) != LCons(1, lazy ENil)

    @Test
    def eq21(): Bool =
        LCons(0, lazy ENil) != ECons(1, ENil)

    @Test
    def eq22(): Bool =
        LCons(0, lazy ENil) != LCons(1, lazy ENil)

    @Test
    def eq23(): Bool =
        LList(lazy ECons(0, ENil)) != LCons(1, lazy ENil)

    @Test
    def eq24(): Bool =
        LCons(0, lazy ENil) != LList(lazy ECons(1, ENil))

    @Test
    def eq25(): Bool =
        ECons(0, ENil) != ECons(0, ECons(0, ENil))

    @Test
    def eq26(): Bool =
        ECons(0, ENil) != ECons(0, ECons(1, ENil))

    @Test
    def eq27(): Bool =
        ECons(0, ENil) != ENil

    @Test
    def eq28(): Bool =
        ENil != ECons(0, ENil)

    @Test
    def eq29(): Bool =
        LCons(0, lazy ENil) != ENil

    @Test
    def eq30(): Bool =
        ENil != LCons(0, lazy ENil)


    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def empty01(): Bool =
        (DelayList.empty()): DelayList[Int32] |> DelayList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Bool =
        (DelayList.empty()): DelayList[Int32] |> DelayList.isEmpty

    @Test
    def isEmpty02(): Bool =
        not (DelayList.range(0, 1000) |> DelayList.isEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Bool =
        DelayList.range(0, 0) |> DelayList.isEmpty

    @Test
    def range02(): Bool =
        DelayList.range(100, 100) |> DelayList.isEmpty

    @Test
    def range03(): Bool =
        DelayList.range(1, 0) |> DelayList.isEmpty

    @Test
    def range04(): Bool =
        not (DelayList.range(0, 100) |> DelayList.isEmpty)

    @Test
    def range05(): Bool =
        DelayList.range(0, 100) |> DelayList.toList == List.range(0, 100)

    @Test
    def range06(): Bool =
        not (DelayList.range(-100, 100) |> DelayList.isEmpty)

    @Test
    def range07(): Bool =
        DelayList.range(-100, 100) |> DelayList.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Bool =
        (DelayList.empty()): DelayList[Int32] |> DelayList.head == None

    @Test
    def head02(): Bool =
        DelayList.range(0, 1000) |> DelayList.head == Some(0)

    @Test
    def head03(): Bool =
        let l = DelayList.range(0, 1000);
        DelayList.head(l) == Some(0)
            and DelayList.head(l) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tail01(): Bool =
        DelayList.tail(ENil: DelayList[Unit]) == ENil

    @Test
    def tail02(): Bool =
        DelayList.tail(LList(lazy ENil): DelayList[Unit]) == ENil

    @Test
    def tail03(): Bool =
        DelayList.range(0, 1) |> DelayList.tail == ENil

    @Test
    def tail04(): Bool =
    	LCons(1, lazy ENil) |> DelayList.tail == ENil

    @Test
    def tail05(): Bool =
    	LList(lazy ECons(1, LList(lazy ENil))) |> DelayList.tail == ENil

    @Test
    def tail06(): Bool =
    	ECons(1, LList(lazy ENil)) |> DelayList.tail == ENil

    @Test
    def tail07(): Bool =
    	LCons(1, lazy LList(lazy ENil)) |> DelayList.tail == ENil

    @Test
    def tail08(): Bool =
    	LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))) |> DelayList.tail == DelayList.range(2, 4)

    @Test
    def tail09(): Bool =
    	LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))) |> DelayList.tail == DelayList.range(2, 4)

    @Test
    def tail10(): Bool =
    	LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))) |> DelayList.tail == DelayList.range(2, 4)

    @Test
    def tail11(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> DelayList.tail == DelayList.range(2, 4)

    @Test
    def tail12(): Bool =
    	DelayList.range(0, 1000) |> DelayList.tail == DelayList.range(1, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def take01(): Bool =
        (DelayList.empty()): DelayList[Int32] |> DelayList.take(1000) |> DelayList.isEmpty

    @Test
    def take02(): Bool =
        (DelayList.empty()): DelayList[Int32] |> DelayList.take(0) |> DelayList.isEmpty

    @Test
    def take03(): Bool =
        (DelayList.range(0, 1000)): DelayList[Int32] |> DelayList.take(0) |> DelayList.isEmpty

    @Test
    def take04(): Bool =
        (DelayList.range(0, 1000)): DelayList[Int32]
            |> DelayList.take(500)
            |> DelayList.toList == List.range(0, 500)

    @Test
    def take05(): Bool =
        (DelayList.range(0, 1000)): DelayList[Int32]
            |> DelayList.take(1)
            |> DelayList.toList == List.range(0, 1)


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Bool =
        (DelayList.empty()): DelayList[Int32] |> DelayList.length == 0

    @Test
    def length02(): Bool =
        DelayList.range(0, 100000) |> DelayList.length == 100000


    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Bool =
        DelayList.reverse(DelayList.empty(): DelayList[Unit]) |> DelayList.toList == Nil

    @Test
    def reverse02(): Bool =
        (1 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList == 1 :: Nil

    @Test
    def reverse03(): Bool =
        (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList == 2 :: 1 :: Nil

    @Test
    def reverse04(): Bool =
        (1 :: 1 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList == 1 :: 1 :: Nil

    @Test
    def reverse05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList == 3 :: 2 :: 1 :: Nil

    @Test
    def reverse06(): Bool =
        (1 :: 2 :: 3 :: 4 :: Nil) |> List.toDelayList |> DelayList.reverse |> DelayList.toList == 4 :: 3 :: 2 :: 1 :: Nil

    @Test
    def reverse07(): Bool =
        DelayList.range(-100, 1000) |> DelayList.reverse |> DelayList.toList == List.range(-100, 1000) |> List.reverse


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapPure01(): Bool =
        Nil |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList == Nil

    @Test
    def mapPure02(): Bool =
        (1 :: Nil) |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList == 2 :: Nil

    @Test
    def mapPure03(): Bool =
        (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList == 2 :: 3 :: Nil

    @Test
    def mapPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.map(x -> x + 1) |> DelayList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapImpure01(): Bool \ IO =
        Nil |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList == Nil

    @Test
    def mapImpure02(): Bool \ IO =
        (1 :: Nil) |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList == 2 :: Nil

    @Test
    def mapImpure03(): Bool \ IO =
        (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList == 2 :: 3 :: Nil

    @Test
    def mapImpure04(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |> DelayList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapMap01(): Bool =
        (1 :: 2 :: 3 :: Nil)         |>
            List.toDelayList          |>
            DelayList.map(x -> x + 1) |>
            DelayList.map(x -> x * 2) |>
            DelayList.toList == 4 :: 6 :: 8 :: Nil

    @Test
    def mapMap02(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                     |>
            List.toDelayList                      |>
            DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |>
            DelayList.map(x -> x * 2)             |>
            DelayList.toList == 4 :: 6 :: 8 :: Nil

    @Test
    def mapMap03(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                     |>
            List.toDelayList                      |>
            DelayList.map(x -> x + 1)             |>
            DelayList.map(x -> unchecked_cast(x * 2 as _ \ IO)) |>
            DelayList.toList == 4 :: 6 :: 8 :: Nil

    @Test
    def mapMap04(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                     |>
            List.toDelayList                      |>
            DelayList.map(x -> unchecked_cast(x + 1 as _ \ IO)) |>
            DelayList.map(x -> unchecked_cast(x * 2 as _ \ IO)) |>
            DelayList.toList == 4 :: 6 :: 8 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(x -> { l := "a" :: deref l; x }) |>
        DelayList.map(x -> { l := "b" :: deref l; x });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @Test
    def mapFusion02(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(x -> unchecked_cast({ l := "a" :: deref l; x } as _ \ {})) |>
        DelayList.map(x -> unchecked_cast({ l := "b" :: deref l; x } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterPure01(): Bool =
        Nil |> List.toDelayList |> DelayList.filter(x -> x > 100) |> DelayList.toList == Nil

    @Test
    def filterPure02(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 0) |> DelayList.toList == 1 :: 2 :: 3 :: Nil

    @Test
    def filterPure03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 1) |> DelayList.toList == 2 :: 3 :: Nil

    @Test
    def filterPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 2) |> DelayList.toList == 3 :: Nil

    @Test
    def filterPure05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 3) |> DelayList.toList == Nil

    @Test
    def filterPure06(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> x > 100) |> DelayList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterImpure01(): Bool \ IO =
        Nil |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 100 as _ \ IO)) |> DelayList.toList == Nil

    @Test
    def filterImpure02(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 0 as _ \ IO)) |> DelayList.toList == 1 :: 2 :: 3 :: Nil

    @Test
    def filterImpure03(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 1 as _ \ IO)) |> DelayList.toList == 2 :: 3 :: Nil

    @Test
    def filterImpure04(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 2 as _ \ IO)) |> DelayList.toList == 3 :: Nil

    @Test
    def filterImpure05(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 3 as _ \ IO)) |> DelayList.toList == Nil

    @Test
    def filterImpure06(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.filter(x -> unchecked_cast(x > 100 as _ \ IO)) |> DelayList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterFilter01(): Bool =
        (1 :: 2 :: 3 :: Nil)            |>
            List.toDelayList             |>
            DelayList.filter(x -> x > 1) |>
            DelayList.filter(x -> x < 3) |>
            DelayList.toList == 2 :: Nil

    @Test
    def filterFilter02(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                                 |>
            List.toDelayList                                 |>
            DelayList.filter(x -> unchecked_cast(x > 1 as _ \ IO)) |>
            DelayList.filter(x -> x < 3)                     |>
            DelayList.toList == 2 :: Nil

    @Test
    def filterFilter03(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                                 |>
            List.toDelayList                                 |>
            DelayList.filter(x -> x > 1)                     |>
            DelayList.filter(x -> unchecked_cast(x < 3 as _ \ IO)) |>
            DelayList.toList == 2 :: Nil

    @Test
    def filterFilter04(): Bool \ IO =
        (1 :: 2 :: 3 :: Nil)                                 |>
            List.toDelayList                                 |>
            DelayList.filter(x -> unchecked_cast(x > 1 as _ \ IO)) |>
            DelayList.filter(x -> unchecked_cast(x < 3 as _ \ IO)) |>
            DelayList.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterFusion01(): Bool= region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList     |>
        DelayList.filter(_ -> { l := "a" :: deref l; true }) |>
        DelayList.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @Test
    def filterFusion02(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList  |>
        DelayList.filter(_x -> unchecked_cast({ l := "a" :: deref l; true } as _ \ {})) |>
        DelayList.filter(_x -> unchecked_cast({ l := "b" :: deref l; true } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapFilterFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(   x -> { l := "a" :: deref l; x }) |>
        DelayList.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @Test
    def mapFilterFusion02(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.map(   x -> unchecked_cast({ l := "a" :: deref l; x }    as _ \ {})) |>
        DelayList.filter(_ -> unchecked_cast({ l := "b" :: deref l; true } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterThenMapFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filter(_ -> { l := "a" :: deref l; true }) |>
        DelayList.map(   x -> { l := "b" :: deref l; x    });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @Test
    def filterThenMapFusion02(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filter(_ -> unchecked_cast({ l := "a" :: deref l; true } as _ \ {})) |>
        DelayList.map(   x -> unchecked_cast({ l := "b" :: deref l; x    } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool =
    	DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, ENil) == 100

    @Test
    def foldLeft02(): Bool =
    	DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, LList(lazy ENil)) == 100

    @Test
    def foldLeft03(): Bool =
    	DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, LCons(1, lazy ENil)) == 198

    @Test
    def foldLeft04(): Bool =
    	DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == 196

    @Test
    def foldLeft05(): Bool =
    	DelayList.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 386

    @Test
    def foldLeft06(): Bool =
    	DelayList.foldLeft((acc, x) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool =
    	DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, ENil) == 100

    @Test
    def foldRight02(): Bool =
    	DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, LList(lazy ENil)) == 100

    @Test
    def foldRight03(): Bool =
    	DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, LCons(1, lazy LList(lazy ENil))) == 198

    @Test
    def foldRight04(): Bool =
    	DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, ECons(1, LCons(2, lazy ENil))) == 194

    @Test
    def foldRight05(): Bool =
    	DelayList.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 382

    @Test
    def foldRight06(): Bool =
    	DelayList.foldRight((x, acc) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool =
    	DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, ENil) == 100

    @Test
    def foldRightWithCont02(): Bool =
    	DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, LList(lazy ENil)) == 100

    @Test
    def foldRightWithCont03(): Bool =
    	DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, LCons(1, lazy LList(lazy ENil))) == 198

    @Test
    def foldRightWithCont04(): Bool =
    	DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, ECons(1, LCons(2, lazy ENil))) == 194

    @Test
    def foldRightWithCont05(): Bool =
    	DelayList.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 382

    @Test
    def foldRightWithCont06(): Bool =
    	DelayList.foldRightWithCont((x, k) -> x :: k(), Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool =
    	DelayList.foldMap(x -> 2 * x, ENil) == 0

    @Test
    def foldMap02(): Bool =
    	DelayList.foldMap(x -> 2 * x, LList(lazy ENil)) == 0

    @Test
    def foldMap03(): Bool =
    	DelayList.foldMap(x -> 2 * x, LCons(1, lazy LList(lazy ENil))) == 2

    @Test
    def foldMap04(): Bool =
    	DelayList.foldMap(x -> 2 * x, ECons(1, LCons(2, lazy ENil))) == 6

    @Test
    def foldMap05(): Bool =
    	DelayList.foldMap(x -> 2 * x, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 12

    @Test
    def foldMap06(): Bool =
    	DelayList.foldMap(Int32.toString, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == "123"

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let l = ENil;
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEach(fn, l);
        StringBuilder.toString(sb) == ""
    }

    @Test
    def forEach02(): Bool = region rc {
        let l = LList(lazy ENil);
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEach(fn, l);
        StringBuilder.toString(sb) == ""
    }

    @Test
    def forEach03(): Bool = region rc {
        let l = LCons(1, lazy LList(lazy ENil));
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEach(fn, l);
        StringBuilder.toString(sb) == "T"
    }

    @Test
    def forEach04(): Bool = region rc {
        let l = LCons(-1, lazy LList(lazy ENil));
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEach(fn, l);
        StringBuilder.toString(sb) == "F"
    }

    @Test
    def forEach05(): Bool = region rc {
        let l = ECons(1, LCons(-1, lazy ENil));
        let sb = StringBuilder.new(rc);
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEach(fn, l);
        StringBuilder.toString(sb) == "TF"
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let l = ENil;
        let sb = StringBuilder.new(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEachWithIndex(fn, l);
        StringBuilder.toString(sb) == ""
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let l = LList(lazy ENil);
        let sb = StringBuilder.new(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEachWithIndex(fn, l);
        StringBuilder.toString(sb) == ""
    }

    @Test
    def forEachWithIndex03(): Bool = region rc {
        let l = LCons(1, lazy LList(lazy ENil));
        let sb = StringBuilder.new(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEachWithIndex(fn, l);
        StringBuilder.toString(sb) == "F"
    }

    @Test
    def forEachWithIndex04(): Bool = region rc {
        let l = LCons(-1, lazy LList(lazy ENil));
        let sb = StringBuilder.new(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEachWithIndex(fn, l);
        StringBuilder.toString(sb) == "F"
    }

    @Test
    def forEachWithIndex05(): Bool = region rc {
        let l = ECons(1, LCons(-1, lazy ENil));
        let sb = StringBuilder.new(rc);
        let fn = (i, _) -> if (i > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        DelayList.forEachWithIndex(fn, l);
        StringBuilder.toString(sb) == "FT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Bool = region rc {
    	DelayList.toArray(rc, ENil: DelayList[Unit])`Array.sameElements` Array#{} @ rc
    }

    @Test
    def toArray02(): Bool = region rc {
    	DelayList.toArray(rc, LList(lazy ENil): DelayList[Unit])`Array.sameElements` Array#{} @ rc
    }

    @Test
    def toArray03(): Bool = region rc {
        DelayList.toArray(rc, DelayList.range(-100, 1000))`Array.sameElements` Array.range(rc, -100, 1000)
    }

    @Test
    def toArray04(): Bool = region rc {
    	DelayList.toArray(rc, LCons(1, lazy ENil))`Array.sameElements` Array#{1} @ rc
    }

    @Test
    def toArray05(): Bool = region rc {
    	DelayList.toArray(rc, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))))`Array.sameElements` Array#{1, 2, 3} @ rc
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Bool =
        ((DelayList.toVector(ENil)) : Vector[Int32]) `Vector.equals` Vector.empty()

    @Test
    def toVector02(): Bool =
        DelayList.toVector(LCons(0, lazy ENil)) `Vector.equals` Vector.singleton(0)

    @Test
    def toVector03(): Bool =
        DelayList.toVector(DelayList.range(1, 4)) `Vector.equals` Vector.range(1, 4)

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Bool =
    	DelayList.toList(ENil): List[Unit] == Nil

    @Test
    def toList02(): Bool =
    	DelayList.toList(LList(lazy ENil)): List[Unit] == Nil

    @Test
    def toList03(): Bool =
        DelayList.range(-1000, 1000) |> DelayList.toList == List.range(-1000, 1000)

    @Test
    def toList04(): Bool =
        DelayList.range(-1000, 1000000) |> DelayList.take(1000) |> DelayList.toList == List.range(-1000, 0)

    @Test
    def toList05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |> DelayList.toList == (1 :: 2 :: 3 :: Nil)

    @Test
    def toList06(): Bool =
    	DelayList.toList(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
    	let l = ENil: DelayList[Int32];
        let d1 = DelayList.toMutDeque(rc, l);

        let d2 = MutDeque.new(rc);

        d1 `MutDeque.sameElements` d2

    }

    @Test
    def toMutDeque02(): Bool = region rc {
    	let l = LList(lazy ENil): DelayList[Int32];
        let d1 = DelayList.toMutDeque(rc, l);

        let d2 = MutDeque.new(rc);

        d1 `MutDeque.sameElements` d2

    }

    @Test
    def toMutDeque03(): Bool = region rc {
    	let l = LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))));
        let d1 = DelayList.toMutDeque(rc, l);

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack(1, d2);
        MutDeque.pushBack(2, d2);
        MutDeque.pushBack(3, d2);

        d1 `MutDeque.sameElements` d2
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Bool = DelayList.toMap(ENil: DelayList[(Unit, Unit)]) == Map#{}

    @Test
    def toMap02(): Bool = DelayList.toMap(LList(lazy ENil): DelayList[(Unit, Unit)]) == Map#{}

    @Test
    def toMap03(): Bool = DelayList.toMap(ECons((1, true), ENil)) == Map#{1 => true}

    @Test
    def toMap04(): Bool = DelayList.toMap(LList(lazy LCons((1, true), lazy LList(lazy LCons((2, false), lazy ENil))))) == Map#{1 => true, 2 => false}

    @Test
    def toMap05(): Bool = DelayList.toMap(ECons((1, true), ECons((1, false), ENil))) == Map#{1 => false}


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Bool =
    	DelayList.toSet(ENil): Set[Unit] == Set.empty()

    @Test
    def toSet02(): Bool =
    	DelayList.toSet(LList(lazy ENil)): Set[Unit] == Set.empty()

    @Test
    def toSet03(): Bool =
        DelayList.range(-1000, 1000) |> DelayList.toSet == Set.range(-1000, 1000)

    @Test
    def toSet04(): Bool =
        DelayList.range(-1000, 1000000) |> DelayList.take(1000) |> DelayList.toSet == Set.range(-1000, 0)

    @Test
    def toSet05(): Bool =
    	DelayList.toSet(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == Set#{1, 2, 3}


    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Bool =
    	DelayList.append(ENil: DelayList[Unit], ENil: DelayList[Unit]) == ENil

    @Test
    def append02(): Bool =
    	DelayList.append(LList(lazy ENil): DelayList[Unit], ENil: DelayList[Unit]) == ENil

    @Test
    def append03(): Bool =
    	DelayList.append(ECons(1, ENil), ENil) == ECons(1, ENil)

    @Test
    def append04(): Bool =
    	DelayList.append(ENil, LCons(1, lazy ENil)) == ECons(1, ENil)

    @Test
    def append05(): Bool =
    	DelayList.append(ECons(1, ECons(2, ECons(3, ENil))), ECons(4, LList(lazy ENil))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))

    @Test
    def append06(): Bool =
    	DelayList.append(ECons(4, ENil), ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(4, ECons(1, ECons(2, ECons(3, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Bool =
        DelayList.count(x -> x > 3, ENil) == 0

    @Test
    def count02(): Bool =
    	DelayList.count(x -> x > 3, LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy ENil))))))) == 0

    @Test
    def count03(): Bool =
    	DelayList.count(x -> x > 3, ECons(4, ENil)) == 1

    @Test
    def count04(): Bool =
        DelayList.count(_ -> true, DelayList.range(1000, 2000)) == 1000

    @Test
    def count05(): Bool =
        DelayList.count(_ -> false, DelayList.range(1000, 2000)) == 0

    @Test
    def count06(): Bool =
        DelayList.count(x -> x > 3, ECons(1, ECons(2, ENil))) == 0

    @Test
    def count07(): Bool =
        DelayList.count(x -> x > 3, ECons(1, ECons(8, ENil))) == 1

    @Test
    def count08(): Bool =
        DelayList.count(x -> x > 3, ECons(8, ECons(1, ENil))) == 1


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def drop01(): Bool =
        DelayList.drop(-1, ENil: DelayList[Unit]) == ENil

    @Test
    def drop02(): Bool =
        DelayList.drop(0, ENil: DelayList[Unit]) == ENil

    @Test
    def drop03(): Bool =
        DelayList.drop(1, ENil: DelayList[Unit]) == ENil

    @Test
    def drop04(): Bool =
        DelayList.drop(-1, ECons(1, ENil)) == ECons(1, ENil)

    @Test
    def drop05(): Bool =
        DelayList.drop(0, ECons(1, ENil)) == ECons(1, ENil)

    @Test
    def drop06(): Bool =
        DelayList.drop(1, ECons(1, ENil)) == ENil

    @Test
    def drop07(): Bool =
    	DelayList.drop(1, LList(lazy ECons(1, LList(lazy ENil)))) == ENil

    @Test
    def drop08(): Bool =
        DelayList.drop(2, ECons(1, ENil)) == ENil

    @Test
    def drop09(): Bool =
    	DelayList.range(0, 1000) |> DelayList.drop(500) == DelayList.range(500, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Bool =
        DelayList.flatten(ENil: DelayList[DelayList[Unit]]) == ENil

    @Test
    def flatten02(): Bool =
        DelayList.flatten(ECons(ENil, ENil)): DelayList[Unit] == ENil

    @Test
    def flatten03(): Bool =
        DelayList.flatten(ECons(ENil, ECons(ENil, ENil))): DelayList[Unit] == ENil

    @Test
    def flatten04(): Bool =
    	DelayList.flatten(ECons(LCons(1, lazy ENil), ENil)) == ECons(1, ENil)

    @Test
    def flatten05(): Bool =
    	DelayList.flatten(ECons(LList(lazy ECons(1, LList(lazy ENil))), ENil)) == ECons(1, ENil)

    @Test
    def flatten06(): Bool =
        DelayList.flatten(ECons((ECons(1, ECons(2, ENil))), ENil)) == ECons(1, ECons(2, ENil))

    @Test
    def flatten07(): Bool =
    	DelayList.flatten(ECons(ENil, ECons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ENil))) == ECons(1, ECons(2, ENil))

    @Test
    def flatten08(): Bool =
    	DelayList.flatten(ECons(ECons(1, LCons(2, lazy ENil)), ECons((ECons(1, ECons(2, ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @Test
    def flatten09(): Bool =
    	DelayList.flatten(ECons((ECons(1, ECons(2, ENil))), ECons(ENil, ECons(ENil, ECons(LList(lazy LCons(3, lazy LCons(4, lazy ENil))), ENil))))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool =
        DelayList.exists(i -> i > 3, ENil) == false

    @Test
    def exists02(): Bool =
        (1 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == false

    @Test
    def exists03(): Bool =
        (5 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == true

    @Test
    def exists04(): Bool =
        (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == false

    @Test
    def exists05(): Bool =
        (1 :: 6 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == true

    @Test
    def exists06(): Bool =
        (6 :: 1 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == true

    @Test
    def exists07(): Bool =
        (16 :: 6 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == true

    @Test
    def exists08(): Bool =
        (1 :: -9 :: 3 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == false

    @Test
    def exists09(): Bool =
        (1 :: 9 :: 3 :: Nil) |> List.toDelayList |> DelayList.exists(i -> i > 3) == true


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Bool =
        DelayList.forAll(i -> i > 3, ENil) == true

    @Test
    def forAll02(): Bool =
        (1 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @Test
    def forAll03(): Bool =
        (5 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == true

    @Test
    def forAll04(): Bool =
        (1 :: 2 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @Test
    def forAll05(): Bool =
        (1 :: 6 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @Test
    def forAll06(): Bool =
        (6 :: 1 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @Test
    def forAll07(): Bool =
        (16 :: 6 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == true

    @Test
    def forAll08(): Bool =
        (1 :: -9 :: 3 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @Test
    def forAll09(): Bool =
        (1 :: 9 :: 3 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == false

    @Test
    def forAll10(): Bool =
        (11 :: 9 :: 31 :: Nil) |> List.toDelayList |> DelayList.forAll(i -> i > 3) == true


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Bool =
        DelayList.repeat(1) |> DelayList.take(-1) == ENil

    @Test
    def repeat02(): Bool =
        DelayList.repeat(1) |> DelayList.take(0) == ENil

    @Test
    def repeat03(): Bool =
        DelayList.repeat(1) |> DelayList.take(1) == ECons(1, ENil)

    @Test
    def repeat04(): Bool =
        DelayList.repeat(1) |> DelayList.take(2) == ECons(1, ECons(1, ENil))

    @Test
    def repeat05(): Bool =
        DelayList.repeat(1) |> DelayList.take(3) == ECons(1, ECons(1, ECons(1, ENil)))

    @Test
    def repeat07(): Bool =
        DelayList.repeat(3) |> DelayList.take(3) == ECons(3, ECons(3, ECons(3, ENil)))

    @Test
    def repeat08(): Bool =
        DelayList.repeat("a") |> DelayList.take(3) == ECons("a", ECons("a", ECons("a", ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // from                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def from01(): Bool =
        DelayList.from(-1) |> DelayList.take(0) == ENil

    @Test
    def from02(): Bool =
        DelayList.from(0) |> DelayList.take(0) == ENil

    @Test
    def from04(): Bool =
        DelayList.from(100) |> DelayList.take(-1) == ENil

    @Test
    def from05(): Bool =
    	DelayList.from(-1) |> DelayList.take(1) == ECons(-1, ENil)

    @Test
    def from06(): Bool =
    	DelayList.from(5) |> DelayList.take(1) == ECons(5, ENil)

    @Test
    def from07(): Bool =
    	DelayList.from(1) |> DelayList.take(3) == ECons(1, ECons(2, ECons(3, ENil)))

    @Test
    def from08(): Bool =
    	DelayList.from(-1000) |> DelayList.take(1000) == DelayList.range(-1000, 0)

    @Test
    def from09(): Bool =
    	DelayList.from(-1000) |> DelayList.take(2000) == DelayList.range(-1000, 1000)

    @Test
    def from10(): Bool =
    	DelayList.from(0) |> DelayList.take(2000) == DelayList.range(0, 2000)


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Bool =
        DelayList.last(ENil: DelayList[Unit]) == None

    @Test
    def last02(): Bool =
        DelayList.last(LList(lazy ENil): DelayList[Unit]) == None

    @Test
    def last03(): Bool =
        DelayList.range(0, 1) |> DelayList.last == Some(0)

    @Test
    def last06(): Bool =
    	ECons(1, ENil) |> DelayList.last == Some(1)

    @Test
    def last11(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> DelayList.last == Some(3)

    @Test
    def last12(): Bool =
    	DelayList.range(0, 1000) |> DelayList.last == Some(999)


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toIter01(): Bool = region rc {
    	DelayList.iterator(rc, ENil: DelayList[Unit]) |> Iterator.toList == Nil
    }

    @Test
    def toIter02(): Bool = region rc {
    	DelayList.iterator(rc, LList(lazy ENil): DelayList[Unit]) |> Iterator.toList == Nil
    }

    @Test
    def toIter06(): Bool = region rc {
    	DelayList.iterator(rc, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def toIter09(): Bool = region rc {
    	DelayList.range(-100, 100) |> DelayList.iterator(rc) |> Iterator.toList == List.range(-100, 100)
    }


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Bool =
        DelayList.zip(ENil: DelayList[Int32], ENil: DelayList[String]) == ENil

    @Test
    def zip02(): Bool =
        DelayList.zip(LList(lazy ENil): DelayList[Int32], ENil: DelayList[String]) == ENil

    @Test
    def zip03(): Bool =
    	DelayList.zip(ENil: DelayList[Int32], LList(lazy ENil): DelayList[String]) == ENil

    @Test
    def zip04(): Bool =
    	DelayList.zip(LList(lazy ENil): DelayList[Int32], LList(lazy ENil): DelayList[String]) == ENil

    @Test
    def zip05(): Bool =
        DelayList.zip(ECons(1, ECons(2, ENil)), ENil: DelayList[String]) == ENil

    @Test
    def zip06(): Bool =
    	DelayList.zip(ENil: DelayList[String], ECons(1, ECons(2, ENil))) == ENil

    @Test
    def zip07(): Bool =
    	DelayList.zip(LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil)))), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(("a", 1), ECons(("b", 2), ENil))

    @Test
    def zip08(): Bool =
    	DelayList.zip(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))), LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil))))) == ECons((1, "a"), ECons((2, "b"), ECons((3, "c"), ENil)))

    @Test
    def zip09(): Bool =
        DelayList.zip(DelayList.range(1, 10), DelayList.range(1, 10)) |> DelayList.take(3) == ECons((1, 1), ECons((2, 2), ECons((3, 3), ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithPure01(): Bool =
        ENil: DelayList[Int32] |> DelayList.zipWith((x, y) -> x + y, ENil: DelayList[Int32]) == ENil

    @Test
    def zipWithPure02(): Bool =
    	LList(lazy (ENil: DelayList[Int32])) |> DelayList.zipWith((x, y) -> x + y, ENil: DelayList[Int32]) == ENil

    @Test
    def zipWithPure03(): Bool =
    	ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> x + y, ENil: DelayList[Int32]) == ENil

    @Test
    def zipWithPure04(): Bool =
    	ENil: DelayList[Int32] |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ENil

    @Test
    def zipWithPure05(): Bool =
    	ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @Test
    def zipWithPure06(): Bool =
    	ECons(1, ENil) |> DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @Test
    def zipWithPure07(): Bool =
    	 LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> DelayList.zipWith((x, y) -> x + y, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(2, ECons(4, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithImpure01(): Bool \ IO =
        ENil: DelayList[Int32] |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ENil: DelayList[Int32]) == ENil

    @Test
    def zipWithImpure02(): Bool \ IO =
    	LList(lazy (ENil: DelayList[Int32])) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ENil: DelayList[Int32]) == ENil

    @Test
    def zipWithImpure03(): Bool \ IO =
    	ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ENil: DelayList[Int32]) == ENil

    @Test
    def zipWithImpure04(): Bool \ IO =
    	ENil: DelayList[Int32] |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ENil))) == ENil

    @Test
    def zipWithImpure05(): Bool \ IO =
    	ECons(1, ECons(2, ENil)) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @Test
    def zipWithImpure06(): Bool \ IO =
    	ECons(1, ENil) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @Test
    def zipWithImpure07(): Bool \ IO =
    	 LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(2, ECons(4, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith zipWith                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithZipWith01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))

    @Test
    def zipWithZipWith02(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))

    @Test
    def zipWithZipWith03(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))

    @Test
    def zipWithZipWith04(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ECons(3, ENil)))) |>
            DelayList.zipWith((x, y) -> unchecked_cast(x + y as _ \ IO), ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith zipWith fusion                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.zipWith((x, y) -> { l := "a" :: deref l; (x, y) }, ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.zipWith((x, y) -> { l := "b" :: deref l; (x, y) }, ECons(1, ECons(2, ECons(3, ENil))));
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @Test
    def zipWithFusion02(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.zipWith((x, y) -> unchecked_cast({ l := "a" :: deref l; (x, y) } as _ \ {}), ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.zipWith((x, y) -> unchecked_cast({ l := "b" :: deref l; (x, y) } as _ \ {}), ECons(1, ECons(2, ECons(3, ENil)))) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWithIndex01(): Bool =
        DelayList.zipWithIndex(ENil: DelayList[Unit]) == ENil: DelayList[(Int32, Unit)]

    @Test
    def zipWithIndex02(): Bool =
        DelayList.zipWithIndex(ECons(1, ENil)) == ECons((0, 1), ENil)

    @Test
    def zipWithIndex03(): Bool =
        DelayList.zipWithIndex(LCons(1, lazy ENil)) == LCons((0, 1), lazy ENil)

    @Test
    def zipWithIndex04(): Bool =
        DelayList.zipWithIndex(LList(lazy ECons(1, ENil))) == LList(lazy ECons((0, 1), ENil))

    @Test
    def zipWithIndex05(): Bool =
        let l = DelayList.zipWithIndex(LList(lazy LList(lazy LCons(5, lazy ECons(7, ENil)))));
        l == LList(lazy LList(lazy LCons((0, 5), lazy ECons((1, 7), ENil))))

    @Test
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

    @Test
    def reduceLeft01(): Bool =
        DelayList.reduceLeft((a, b) -> a + b, ENil: DelayList[Int32]) == None

    @Test
    def reduceLeft02(): Bool =
    	DelayList.reduceLeft((a, b) -> a + b, ECons(1, ENil)) == Some(1)

    @Test
    def reduceLeft03(): Bool =
    	DelayList.reduceLeft((a, b) -> a + b, ECons(1, ECons(2, ENil))) == Some(3)

    @Test
    def reduceLeft04(): Bool =
    	DelayList.reduceLeft((a, b) -> a + b, ECons(1, ECons(2, ECons(3, ENil)))) == Some(6)

    @Test
    def reduceLeft05(): Bool =
    	DelayList.reduceLeft((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == Some(-8)

    @Test
    def reduceLeft06(): Bool =
        DelayList.reduceLeft((a, b) -> b - a, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == Some(2)


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceRight01(): Bool =
        DelayList.reduceRight((a, b) -> a + b, ENil: DelayList[Int32]) == None

    @Test
    def reduceRight02(): Bool =
        DelayList.reduceRight((a, b) -> a + b, LList(lazy ENil): DelayList[Int32]) == None

    @Test
    def reduceRight03(): Bool =
    	DelayList.reduceRight((a, b) -> a + b, ECons(1, ENil)) == Some(1)

    @Test
    def reduceRight04(): Bool =
    	DelayList.reduceRight((a, b) -> a + b, ECons(1, ECons(2, ENil))) == Some(3)

    @Test
    def reduceRight05(): Bool =
    	DelayList.reduceRight((a, b) -> a + b, ECons(1, ECons(2, ECons(3, ENil)))) == Some(6)

    @Test
    def reduceRight06(): Bool =
        DelayList.reduceRight((a, b) -> b - a, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == Some(-2)

    @Test
    def reduceRight07(): Bool =
    	DelayList.reduceRight((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == Some(-2)


    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimum01(): Bool =
        DelayList.minimum(ENil: DelayList[Int32]) == None

    @Test
    def minimum02(): Bool =
        DelayList.minimum(ECons(1, ENil)) == Some(1)

    @Test
    def minimum03(): Bool =
        DelayList.minimum(ECons(3, ECons(2, ECons(1, ENil)))) == Some(1)

    @Test
    def minimum04(): Bool =
        DelayList.minimum(ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(0)

    @Test
    def minimum05(): Bool =
        DelayList.minimum(DelayList.range(0, 99)) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Bool =
        DelayList.minimumBy((x, y) -> x <=> y, ENil: DelayList[Int32]) == None

    @Test
    def minimumBy02(): Bool =
        DelayList.minimumBy((x, y) -> x <=> y, ECons(1, ENil)) == Some(1)

    @Test
    def minimumBy03(): Bool =
        DelayList.minimumBy((x, y) -> x <=> y, ECons(3, ECons(2, ECons(1, ENil)))) == Some(1)

    @Test
    def minimumBy04(): Bool =
        DelayList.minimumBy((x, y) -> x <=> y, ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(0)

    @Test
    def minimumBy05(): Bool =
        DelayList.minimumBy((_, _) -> Comparison.LessThan, DelayList.range(0, 99)) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximum01(): Bool =
        DelayList.maximum(ENil: DelayList[Int32]) == None

    @Test
    def maximum02(): Bool =
        DelayList.maximum(ECons(1, ENil)) == Some(1)

    @Test
    def maximum03(): Bool =
        DelayList.maximum(ECons(3, ECons(2, ECons(1, ENil)))) == Some(3)

    @Test
    def maximum04(): Bool =
        DelayList.maximum(ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(3)

    @Test
    def maximum05(): Bool =
        DelayList.maximum(DelayList.range(0, 99)) == Some(98)


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Bool =
        DelayList.maximumBy((x, y) -> x <=> y, ENil: DelayList[Int32]) == None

    @Test
    def maximumBy02(): Bool =
        DelayList.maximumBy((x, y) -> x <=> y, ECons(1, ENil)) == Some(1)

    @Test
    def maximumBy03(): Bool =
        DelayList.maximumBy((x, y) -> x <=> y, ECons(3, ECons(2, ECons(1, ENil)))) == Some(3)

    @Test
    def maximumBy04(): Bool =
        DelayList.maximumBy((x, y) -> x <=> y, ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(3)

    @Test
    def maximumBy05(): Bool =
        DelayList.maximumBy((_, _) -> Comparison.LessThan, DelayList.range(0, 99)) == Some(98)


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMapPure01(): Bool =
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), ENil: DelayList[Int32]) == ENil

    @Test
    def flatMapPure02(): Bool =
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), ECons(1, ENil)) == ECons(1, ENil)

    @Test
    def flatMapPure03(): Bool =
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @Test
    def flatMapPure04(): Bool =
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i), ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMapImpure01(): Bool \ IO =
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO), ENil: DelayList[Int32]) == ENil

    @Test
    def flatMapImpure02(): Bool \ IO =
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO), ECons(1, ENil)) == ECons(1, ENil)

    @Test
    def flatMapImpure03(): Bool \ IO =
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO), ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @Test
    def flatMapImpure04(): Bool \ IO =
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO), ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMapFlatMap01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil)))                            |>
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i)) |>
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i)) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @Test
    def flatMapFlatMap02(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil)))                                                   |>
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO)) |>
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i))             ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @Test
    def flatMapFlatMap03(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil)))                                           |>
        DelayList.flatMap(i -> DelayList.repeat(i) |> DelayList.take(i))             |>
        DelayList.flatMap(i -> unchecked_cast(DelayList.repeat(i) |> DelayList.take(i) as _ \ IO)) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @Test
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

    @Test
    def flatMapFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.flatMap(i -> { l := "a" :: deref l; ECons(i, ENil) }) |>
        DelayList.flatMap(i -> { l := "b" :: deref l; ECons(i, ENil) });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @Test
    def flatMapFusion02(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.flatMap(i -> unchecked_cast({ l := "a" :: deref l; ECons(i, ENil) } as _ \ {})) |>
        DelayList.flatMap(i -> unchecked_cast({ l := "b" :: deref l; ECons(i, ENil) } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Bool =
        not (DelayList.memberOf(0, ENil))

    @Test
    def memberOf02(): Bool =
        not (DelayList.memberOf(0, ECons(1, ENil)))

    @Test
    def memberOf03(): Bool =
        not (DelayList.memberOf(0, ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def memberOf04(): Bool =
        DelayList.memberOf(0, ECons(1, ECons(2, ECons(3, ECons(0, ENil)))))

    @Test
    def memberOf05(): Bool =
        DelayList.memberOf(0, ECons(0, ECons(1, ECons(2, ECons(3, ENil)))))

    @Test
    def memberOf06(): Bool =
        DelayList.memberOf(0, ECons(1, ECons(1, ECons(1, ECons(0, ENil)))))

    @Test
    def memberOf07(): Bool =
        DelayList.memberOf(1, ECons(1, ECons(1, ECons(1, ECons(1, ENil)))))

    @Test
    def memberOf08(): Bool =
        DelayList.memberOf(0, ECons(1, ECons(1, ECons(0, ECons(1, ENil)))))

    @Test
    def memberOf09(): Bool =
        DelayList.memberOf(-2, ECons(1, ECons(1000, ECons(-2, ECons(1, ENil)))))

    @Test
    def memberOf10(): Bool =
        DelayList.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil)))))))


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Bool =
        DelayList.replace(from = 3, to = 4, ENil) == ENil

    @Test
    def replace02(): Bool =
        DelayList.replace(from = 3, to = 4, ECons(1, ENil)) == ECons(1, ENil)

    @Test
    def replace03(): Bool =
        DelayList.replace(from = 3, to = 4, ECons(3, ENil)) == ECons(4, ENil)

    @Test
    def replace04(): Bool =
        DelayList.replace(from = 3, to = 4, ECons(4, ECons(4, ECons(4, ENil)))) == ECons(4, ECons(4, ECons(4, ENil)))

    @Test
    def replace05(): Bool =
        DelayList.replace(from = 3, to = 4, ECons(2, ECons(2, ECons(2, ENil)))) == ECons(2, ECons(2, ECons(2, ENil)))

    @Test
    def replace06(): Bool =
        DelayList.replace(from = 3, to = 4, ECons(2, ECons(3, ECons(2, ENil)))) == ECons(2, ECons(4, ECons(2, ENil)))

    @Test
    def replace07(): Bool =
        DelayList.replace(from = 0, to = 1, ECons(0, ECons(0, ECons(0, ENil)))) == ECons(1, ECons(1, ECons(1, ENil)))

    @Test
    def replace08(): Bool =
        DelayList.replace(from = 3, to = 4, LList(lazy ECons(3, LList(lazy LCons(3, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(4, ECons(4, ECons(4, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Bool =
        DelayList.findLeft(i -> i > 2, ENil: DelayList[Int32]) == None

    @Test
    def findLeft02(): Bool =
        DelayList.findLeft(i -> i > 2, ECons(1, ENil)) == None

    @Test
    def findLeft03(): Bool =
        DelayList.findLeft(i -> i > 2, ECons(3, ENil)) == Some(3)

    @Test
    def findLeft04(): Bool =
        DelayList.findLeft(i -> i > 2, ECons(1, ECons(2, ENil))) == None

    @Test
    def findLeft05(): Bool =
        DelayList.findLeft(i -> i > 2, ECons(6, ECons(-6, ENil))) == Some(6)

    @Test
    def findLeft06(): Bool =
        DelayList.findLeft(i -> i > 2, ECons(7, ECons(6, ENil))) == Some(7)

    @Test
    def findLeft07(): Bool =
        DelayList.findLeft(i -> i > 2, LList(lazy ECons(1, LList(lazy LCons(1, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == Some(4)


    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Bool =
        DelayList.findRight(i -> i > 2, ENil: DelayList[Int32]) == None

    @Test
    def findRight02(): Bool =
        DelayList.findRight(i -> i > 2, ECons(1, ENil)) == None

    @Test
    def findRight03(): Bool =
        DelayList.findRight(i -> i > 2, ECons(3, ENil)) == Some(3)

    @Test
    def findRight04(): Bool =
        DelayList.findRight(i -> i > 2, ECons(1, ECons(2, ENil))) == None

    @Test
    def findRight05(): Bool =
        DelayList.findRight(i -> i > 2, ECons(6, ECons(-6, ENil))) == Some(6)

    @Test
    def findRight06(): Bool =
        DelayList.findRight(i -> i > 2, ECons(7, ECons(6, ENil))) == Some(6)

    @Test
    def findRight07(): Bool =
        DelayList.findRight(i -> i > 2, LList(lazy ECons(4, LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy LList(lazy ENil)))))))) == Some(4)


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex (pure)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndexPure01(): Bool =
    	ENil |> DelayList.mapWithIndex((i, x) -> x + i) == ENil

    @Test
    def mapWithIndexPure02(): Bool =
    	ECons(1, ENil) |> DelayList.mapWithIndex((i, x) -> x + i) == ECons(1, ENil)

    @Test
    def mapWithIndexPure03(): Bool =
    	ECons(1, ECons(2, ENil)) |> DelayList.mapWithIndex((i, x) -> x + i) == ECons(1, ECons(3, ENil))

    @Test
    def mapWithIndexPure04(): Bool =
    	ECons(1, ECons(2, ECons(3, ENil))) |> DelayList.mapWithIndex((i, x) -> x + i) == ECons(1, ECons(3, ECons(5, ENil)))

    @Test
    def mapWithIndexPure05(): Bool =
        ECons(1, ECons(2, ECons(3, ECons(4, ENil)))) |> DelayList.mapWithIndex((i, x) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @Test
    def mapWithIndexPure06(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) |> DelayList.mapWithIndex((i, x) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex (impure)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndexImpure01(): Bool \ IO =
    	ENil |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ENil

    @Test
    def mapWithIndexImpure02(): Bool \ IO =
    	ECons(1, ENil) |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ECons(1, ENil)

    @Test
    def mapWithIndexImpure03(): Bool \ IO =
    	ECons(1, ECons(2, ENil)) |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ECons(1, ECons(3, ENil))

    @Test
    def mapWithIndexImpure04(): Bool \ IO =
    	ECons(1, ECons(2, ECons(3, ENil))) |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ECons(1, ECons(3, ECons(5, ENil)))

    @Test
    def mapWithIndexImpure05(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ECons(4, ENil)))) |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @Test
    def mapWithIndexImpure06(): Bool \ IO =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) |> DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex mapWithIndex                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndexMapWithIndex01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> x + i) |>
            DelayList.mapWithIndex((i, x) -> x * i) == ECons(0, ECons(3, ECons(10, ENil)))

    @Test
    def mapWithIndexMapWithIndex02(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) |>
            DelayList.mapWithIndex((i, x) -> x * i) == ECons(0, ECons(3, ECons(10, ENil)))

    @Test
    def mapWithIndexMapWithIndex03(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> x + i) |>
            DelayList.mapWithIndex((i, x) -> unchecked_cast(x * i as _ \ IO)) == ECons(0, ECons(3, ECons(10, ENil)))

    @Test
    def mapWithIndexMapWithIndex04(): Bool \ IO =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            DelayList.mapWithIndex((i, x) -> unchecked_cast(x + i as _ \ IO)) |>
            DelayList.mapWithIndex((i, x) -> unchecked_cast(x * i as _ \ IO)) == ECons(0, ECons(3, ECons(10, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex mapWithIndex fusion                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndexFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.mapWithIndex((_, x) -> { l := "a" :: deref l; x }) |>
        DelayList.mapWithIndex((_, x) -> { l := "b" :: deref l; x }) |>
        DelayList.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @Test
    def mapWithIndexFusion02(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.mapWithIndex((_, x) -> unchecked_cast({ l := "a" :: deref l; x } as _ \ {})) |>
        DelayList.mapWithIndex((_, x) -> unchecked_cast({ l := "b" :: deref l; x } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalate01(): Bool =
        DelayList.intercalate(ECons(1, ECons(2, ENil)), ENil) == ENil

    @Test
    def intercalate02(): Bool =
        DelayList.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(ECons(1, ENil), ENil)) == ECons(1, ENil)

    @Test
    def intercalate03(): Bool =
        DelayList.intercalate(
            ECons(10, ECons(11, ECons(12, ENil))),
                ECons(ECons(1, ECons(2, ECons(3, ENil))),
                ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil)))
            == ECons(1, ECons(2, ECons(3,
                ECons(10, ECons(11, ECons(12,
                ECons(100, ECons(101, ECons(102, ENil)))))))))

    @Test
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

    @Test
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

    @Test
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

    @Test
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
    def intersperse01(): Bool =
        DelayList.intersperse(-11, ENil) == ENil

    @Test
    def intersperse03(): Bool =
        DelayList.intersperse(-11, ECons(1, ENil)) == ECons(1, ENil)

    @Test
    def intersperse04(): Bool =
        DelayList.intersperse(-11, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

    @Test
    def intersperse05(): Bool =
        DelayList.intersperse(-11, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

    @Test
    def intersperse06(): Bool =
        DelayList.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhilePure01(): Bool =
        DelayList.takeWhile(i -> i > 3, ENil) == ENil

    @Test
    def takeWhilePure02(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(1, ENil)) == ENil

    @Test
    def takeWhilePure03(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(4, ENil)) == ECons(4, ENil)

    @Test
    def takeWhilePure04(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(1, ECons(2, ENil))) == ENil

    @Test
    def takeWhilePure05(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(1, ECons(5, ENil))) == ENil

    @Test
    def takeWhilePure06(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(5, ECons(1, ENil))) == ECons(5, ENil)

    @Test
    def takeWhilePure07(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(5, ECons(8, ENil))) == ECons(5, ECons(8, ENil))

    @Test
    def takeWhilePure08(): Bool =
        DelayList.takeWhile(i -> i > 3, LList(lazy ECons(5, LList(lazy LCons(8, lazy ENil))))) == ECons(5, ECons(8, ENil))

    @Test
    def takeWhilePure09(): Bool =
        DelayList.takeWhile(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(4, ECons(6, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileImpure01(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ENil) == ENil

    @Test
    def takeWhileImpure02(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ENil)) == ENil

    @Test
    def takeWhileImpure03(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(4, ENil)) == ECons(4, ENil)

    @Test
    def takeWhileImpure04(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ECons(2, ENil))) == ENil

    @Test
    def takeWhileImpure05(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ECons(5, ENil))) == ENil

    @Test
    def takeWhileImpure06(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(5, ECons(1, ENil))) == ECons(5, ENil)

    @Test
    def takeWhileImpure07(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(5, ECons(8, ENil))) == ECons(5, ECons(8, ENil))

    @Test
    def takeWhileImpure08(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), LList(lazy ECons(5, LList(lazy LCons(8, lazy ENil))))) == ECons(5, ECons(8, ENil))

    @Test
    def takeWhileImpure09(): Bool \ IO =
        DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(4, ECons(6, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile takeWhile                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileTakeWhile01(): Bool =
        ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.takeWhile(i -> i > 3) |>
            DelayList.takeWhile(i -> i < 5) == ECons(4, ENil)

    @Test
    def takeWhileTakeWhile02(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil)))                      |>
            DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO)) |>
            DelayList.takeWhile(i -> i < 5) == ECons(4, ENil)

    @Test
    def takeWhileTakeWhile03(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.takeWhile(i -> i > 3) |>
            DelayList.takeWhile(i -> unchecked_cast(i < 5 as _ \ IO)) == ECons(4, ENil)

    @Test
    def takeWhileTakeWhile04(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil)))             |>
            DelayList.takeWhile(i -> unchecked_cast(i > 3 as _ \ IO)) |>
            DelayList.takeWhile(i -> unchecked_cast(i < 5 as _ \ IO)) == ECons(4, ENil)


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile takeWhile fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> { l := "a" :: deref l; i < 4 }) |>
        DelayList.takeWhile(i -> { l := "b" :: deref l; i < 4 });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @Test
    def takeWhileFusion02(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> unchecked_cast({ l := "a" :: deref l; i < 4 } as _ \ {})) |>
        DelayList.takeWhile(i -> unchecked_cast({ l := "b" :: deref l; i < 4 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    @Test
    def takeWhileFusion03(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> unchecked_cast({ l := "a" :: deref l; i < 3 } as _ \ {})) |>
        DelayList.takeWhile(i -> unchecked_cast({ l := "b" :: deref l; i < 2 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: Nil)
    }

    @Test
    def takeWhileFusion04(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.takeWhile(i -> unchecked_cast({ l := "a" :: deref l; i < 3 } as _ \ {})) |>
        DelayList.takeWhile(i -> unchecked_cast({ l := "b" :: deref l; i < 3 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhilePure01(): Bool =
        DelayList.dropWhile(i -> i > 3, ENil) == ENil

    @Test
    def dropWhilePure02(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(1, ENil)) == ECons(1, ENil)

    @Test
    def dropWhilePure03(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(4, ENil)) == ENil

    @Test
    def dropWhilePure04(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(1, ECons(2, ENil))) == ECons(1, ECons(2, ENil))

    @Test
    def dropWhilePure05(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(1, ECons(5, ENil))) == ECons(1, ECons(5, ENil))

    @Test
    def dropWhilePure06(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(5, ECons(1, ENil))) == ECons(1, ENil)

    @Test
    def dropWhilePure07(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(5, ECons(8, ENil))) == ENil

    @Test
    def dropWhilePure08(): Bool =
        DelayList.dropWhile(i -> i > 3, LList(lazy ECons(5, LCons(8, lazy LList(lazy LCons(9, lazy LList(lazy ECons(10, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))))))))))) == ECons(1, ECons(2, ENil))

    @Test
    def dropWhilePure09(): Bool =
        DelayList.dropWhile(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileImpure01(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ENil) == ENil

    @Test
    def dropWhileImpure02(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ENil)) == ECons(1, ENil)

    @Test
    def dropWhileImpure03(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(4, ENil)) == ENil

    @Test
    def dropWhileImpure04(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ECons(2, ENil))) == ECons(1, ECons(2, ENil))

    @Test
    def dropWhileImpure05(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(1, ECons(5, ENil))) == ECons(1, ECons(5, ENil))

    @Test
    def dropWhileImpure06(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(5, ECons(1, ENil))) == ECons(1, ENil)

    @Test
    def dropWhileImpure07(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(5, ECons(8, ENil))) == ENil

    @Test
    def dropWhileImpure08(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), LList(lazy ECons(5, LCons(8, lazy LList(lazy LCons(9, lazy LList(lazy ECons(10, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))))))))))) == ECons(1, ECons(2, ENil))

    @Test
    def dropWhileImpure09(): Bool \ IO =
        DelayList.dropWhile(i -> unchecked_cast(i > 3 as _ \ IO), ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile dropWhile                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileDropWhile01(): Bool =
        ECons(4, ECons(5, ECons(1, ENil))) |>
            DelayList.dropWhile(i -> i < 5) |>
            DelayList.dropWhile(i -> i > 1) == ECons(1, ENil)

    @Test
    def dropWhileDropWhile02(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil)))             |>
            DelayList.dropWhile(i -> unchecked_cast(i < 5 as _ \ IO)) |>
            DelayList.dropWhile(i -> i > 1) == ECons(1, ENil)

    @Test
    def dropWhileDropWhile03(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil)))     |>
            DelayList.dropWhile(i -> i < 5)     |>
            DelayList.dropWhile(i -> unchecked_cast(i > 1 as _ \ IO)) == ECons(1, ENil)

    @Test
    def dropWhileDropWhile04(): Bool \ IO =
        ECons(4, ECons(5, ECons(1, ENil)))             |>
            DelayList.dropWhile(i -> unchecked_cast(i < 5 as _ \ IO)) |>
            DelayList.dropWhile(i -> unchecked_cast(i > 1 as _ \ IO)) == ECons(1, ENil)


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile dropWhile fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> { l := "a" :: deref l; i < 3 }) |>
        DelayList.dropWhile(i -> { l := "b" :: deref l; i < 4 });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: Nil)
    }

    @Test
    def dropWhileFusion02(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> unchecked_cast({ l := "a" :: deref l; i < 3 } as _ \ {})) |>
        DelayList.dropWhile(i -> unchecked_cast({ l := "b" :: deref l; i < 4 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: Nil)
    }

    @Test
    def dropWhileFusion03(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> unchecked_cast({ l := "a" :: deref l; i < 1 } as _ \ {})) |>
        DelayList.dropWhile(i -> unchecked_cast({ l := "b" :: deref l; i < 3 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "b" :: "b" :: Nil)
    }

    @Test
    def dropWhileFusion04(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.dropWhile(i -> unchecked_cast({ l := "a" :: deref l; i < 2 } as _ \ {})) |>
        DelayList.dropWhile(i -> unchecked_cast({ l := "b" :: deref l; i < 3 } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "a" :: "b" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findMap01(): Bool =
        DelayList.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ENil) == None

    @Test
    def findMap02(): Bool =
        DelayList.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(1, ENil)) == None

    @Test
    def findMap03(): Bool =
        DelayList.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(2, ENil)) == Some(1)

	@Test
	def findMap04(): Bool =
		DelayList.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, ENil))) == None

	@Test
	def findMap05(): Bool =
		DelayList.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))) == Some(2)

	@Test
	def findMap06(): Bool =
		DelayList.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))) == Some(3)

	@Test
	def findMap07(): Bool =
		DelayList.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))) == Some(4)

    @Test
	def findMap08(): Bool =
		DelayList.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == Some(0)

    @Test
	def findMap09(): Bool =
		DelayList.findMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None,
		    LList(lazy LCons(-1, lazy LList(lazy LCons(1, lazy
		    LList(lazy LCons(9, lazy LList(lazy LCons(3, lazy
		    LList(lazy LCons(5, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == Some(5)


    /////////////////////////////////////////////////////////////////////////////
    // filterMap (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapPure01(): Bool =
        DelayList.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ENil) == ENil

    @Test
    def filterMapPure02(): Bool =
        DelayList.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(1, ENil)) == ENil

    @Test
    def filterMapPure03(): Bool =
        DelayList.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(2, ENil)) == ECons(1, ENil)

	@Test
	def filterMapPure04(): Bool =
		DelayList.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, ENil))) == ENil

	@Test
	def filterMapPure05(): Bool =
		DelayList.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@Test
	def filterMapPure06(): Bool =
		DelayList.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@Test
	def filterMapPure07(): Bool =
		DelayList.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))

    @Test
	def filterMapPure08(): Bool =
		DelayList.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))

    @Test
	def filterMapPure09(): Bool =
		DelayList.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None,
		    LList(lazy LCons(0, lazy LList(lazy
		    LCons(1, lazy LList(lazy LCons(2, lazy
		    LList(lazy LCons(3, lazy LList(lazy
		    LCons(4, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapImpure01(): Bool \ IO =
        DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO), ENil) == ENil

    @Test
    def filterMapImpure02(): Bool \ IO =
        DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(1, ENil)) == ENil

    @Test
    def filterMapImpure03(): Bool \ IO =
        DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(2, ENil)) == ECons(1, ENil)

	@Test
	def filterMapImpure04(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(1, ECons(3, ENil))) == ENil

	@Test
	def filterMapImpure05(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@Test
	def filterMapImpure06(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@Test
	def filterMapImpure07(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))

    @Test
	def filterMapImpure08(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO), ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))

    @Test
	def filterMapImpure09(): Bool \ IO =
		DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO),
		    LList(lazy LCons(0, lazy LList(lazy
		    LCons(1, lazy LList(lazy LCons(2, lazy
		    LList(lazy LCons(3, lazy LList(lazy
		    LCons(4, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap filterMap                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapFilterMap01(): Bool =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            DelayList.filterMap(i -> if (i `Int32.rem` 2 == 0) Some(i / 2) else None) |>
            DelayList.filterMap(i -> if (i > 1)                Some(i * 3) else None) == ECons(6, ECons(9, ENil))

    @Test
    def filterMapFilterMap02(): Bool \ IO =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO)) |>
            DelayList.filterMap(i ->             if (i > 1)                Some(i * 3) else None) == ECons(6, ECons(9, ENil))

    @Test
    def filterMapFilterMap03(): Bool \ IO =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            DelayList.filterMap(i ->             if (i `Int32.rem` 2 == 0) Some(i / 2) else None) |>
            DelayList.filterMap(i -> unchecked_cast(if (i > 1)                Some(i * 3) else None as _ \ IO)) == ECons(6, ECons(9, ENil))

    @Test
    def filterMapFilterMap04(): Bool \ IO =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            DelayList.filterMap(i -> unchecked_cast(if (i `Int32.rem` 2 == 0) Some(i / 2) else None as _ \ IO)) |>
            DelayList.filterMap(i -> unchecked_cast(if (i > 1)                Some(i * 3) else None as _ \ IO)) == ECons(6, ECons(9, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap filterMap fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMapFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filterMap(x -> { l := "a" :: deref l; Some(x) }) |>
        DelayList.filterMap(x -> { l := "b" :: deref l; Some(x) });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)
    }

    @Test
    def filterMapFusion02(): Bool \ IO = region rc {
        let l = ref Nil @ rc;
        discard unchecked_cast((1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.filterMap(x -> unchecked_cast({ l := "a" :: deref l; Some(x) } as _ \ {})) |>
        DelayList.filterMap(x -> unchecked_cast({ l := "b" :: deref l; Some(x) } as _ \ {})) |>
        DelayList.toList as _ \ IO);
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)
    }

    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def partition01(): Bool =
        DelayList.partition(i -> i > 3, ENil) == (ENil, ENil)

    @Test
    def partition02(): Bool =
        DelayList.partition(i -> i > 3, ECons(1, ENil)) == (ENil, ECons(1, ENil))

    @Test
    def partition03(): Bool =
        DelayList.partition(i -> i > 3, ECons(4, ENil)) == (ECons(4, ENil), ENil)

    @Test
    def partition04(): Bool =
        DelayList.partition(i -> i > 3, ECons(1, ECons(2, ENil))) == (ENil, ECons(1, ECons(2, ENil)))

    @Test
    def partition05(): Bool =
        DelayList.partition(i -> i > 3, ECons(1, ECons(5, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @Test
    def partition06(): Bool =
        DelayList.partition(i -> i > 3, ECons(5, ECons(1, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @Test
    def partition07(): Bool =
        DelayList.partition(i -> i > 3, ECons(5, ECons(8, ENil))) == (ECons(5, ECons(8, ENil)), ENil)

    @Test
    def partition08(): Bool =
        DelayList.partition(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))))
            == (ECons(4, ECons(6, ECons(11, ECons(16, ECons(7, ECons(7, ENil)))))), ECons(-3, ECons(-5, ECons(1, ECons(2, ECons(1, ENil))))))

    @Test
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
    def span01(): Bool =
        DelayList.span(i -> i > 3, ENil) == (ENil, ENil)

    @Test
    def span02(): Bool =
        DelayList.span(i -> i > 3, ECons(1, ENil)) == (ENil, ECons(1, ENil))

    @Test
    def span03(): Bool =
        DelayList.span(i -> i > 3, ECons(4, ENil)) == (ECons(4, ENil), ENil)

    @Test
    def span04(): Bool =
        DelayList.span(i -> i > 3, ECons(1, ECons(2, ENil))) == (ENil, ECons(1, ECons(2, ENil)))

    @Test
    def span05(): Bool =
        DelayList.span(i -> i > 3, ECons(1, ECons(5, ENil))) == (ENil, ECons(1, ECons(5, ENil)))

    @Test
    def span06(): Bool =
        DelayList.span(i -> i > 3, ECons(5, ECons(1, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @Test
    def span07(): Bool =
        DelayList.span(i -> i > 3, ECons(5, ECons(8, ENil))) == (ECons(5, ECons(8, ENil)), ENil)

    @Test
    def span08(): Bool =
        DelayList.span(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == (ECons(4, ECons(6, ENil)), ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))

    @Test
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
    def spanSpanFusion01(): Bool = region rc {
        let l = ref Nil @ rc;
        discard (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
        DelayList.span(x -> { l := "a" :: deref l; x < 3 }) |> fst |>
        DelayList.span(x -> { l := "b" :: deref l; x < 3 }); // The working list is `1 :: 2 :: Nil`
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: Nil)
    }

    //@Test
    //def spanSpanFusion02(): Bool = region rc {
    //    let l = ref Nil @ rc;
    //    let _ =
    //    (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
    //    DelayList.span(x -> unchecked_cast({ l := "a" :: deref l; x < 3 } as _ \ {})) |> fst |>
    //    DelayList.span(x -> unchecked_cast({ l := "b" :: deref l; x < 3 } as _ \ {})) |> fst |>
    //    DelayList.toList;
    //    List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: Nil)
    //}

    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool =
        List.toDelayList(Nil) |> DelayList.sum == 0

    @Test
    def sum02(): Bool =
        List.toDelayList(1 :: Nil) |> DelayList.sum == 1

    @Test
    def sum03(): Bool =
        List.toDelayList(1 :: 2 :: 3 :: Nil) |> DelayList.sum == 6

    @Test
    def sum04(): Bool =
        List.toDelayList(1 :: 2 :: 3 :: -3 :: Nil) |> DelayList.sum == 3

    @Test
    def sum05(): Bool =
        List.toDelayList(-1 :: -2 :: -3 :: -4 :: Nil) |> DelayList.sum == -10

    @Test
    def sum06(): Bool =
        List.toDelayList(10 :: -10 :: Nil) |> DelayList.sum == 0

    @Test
    def sum07(): Bool =
        List.range(1, 101) |> List.toDelayList |> DelayList.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool =
        List.toDelayList(Nil) |> DelayList.sumWith(x -> x + 1) == 0

    @Test
    def sumWith02(): Bool =
        List.toDelayList(1 :: Nil) |> DelayList.sumWith(x -> x + 1) == 2

    @Test
    def sumWith03(): Bool =
        List.toDelayList(1 :: 2 :: 3 :: Nil) |> DelayList.sumWith(x -> x + 1) == 9

    @Test
    def sumWith04(): Bool =
        List.toDelayList(1 :: 2 :: 3 :: -3 :: Nil) |> DelayList.sumWith(x -> x + 1) == 7

    @Test
    def sumWith05(): Bool =
        List.toDelayList(-1 :: -2 :: -3 :: -4 :: Nil) |> DelayList.sumWith(x -> x + 1) == -6

    @Test
    def sumWith06(): Bool =
        List.toDelayList(10 :: -10 :: Nil) |> DelayList.sumWith(x -> x + 1) == 2


    /////////////////////////////////////////////////////////////////////////////
    // order                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def order01(): Bool =
        (ENil: DelayList[Unit] <=> ENil: DelayList[Unit]) == Comparison.EqualTo

    @Test
    def order02(): Bool =
        (((1 :: Nil) |> List.toDelayList) <=> ENil: DelayList[Int32]) == Comparison.GreaterThan

    @Test
    def order03(): Bool =
        (ENil: DelayList[Int32] <=> ((1 :: Nil) |> List.toDelayList)) == Comparison.LessThan

    @Test
    def order04(): Bool =
        (((1 :: Nil) |> List.toDelayList) <=> ((1 :: Nil) |> List.toDelayList)) == Comparison.EqualTo

    @Test
    def order05(): Bool =
        (((2 :: 1 :: Nil) |> List.toDelayList) <=> ((1 :: 1 :: Nil) |> List.toDelayList)) == Comparison.GreaterThan

    @Test
    def order06(): Bool =
        (((1 :: 1 :: Nil) |> List.toDelayList) <=> ((2 :: 1 :: Nil) |> List.toDelayList)) == Comparison.LessThan

    @Test
    def order07(): Bool =
        (((1 :: Nil) |> List.toDelayList) <=> ((1 :: 1 :: Nil) |> List.toDelayList)) == Comparison.LessThan


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool =
        (Nil: List[Int32]) |> List.toDelayList |>
            DelayList.join(",") == ""

    @Test
    def join02(): Bool =
        (1 :: Nil) |> List.toDelayList |>
            DelayList.join(",") == "1"

    @Test
    def join03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
            DelayList.join(",") == "1,2,3"

    @Test
    def join04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toDelayList |>
            DelayList.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool =
        (Nil: List[Int32]) |> List.toDelayList |>
            DelayList.joinWith(x -> "${x + 1}", ",") == ""

    @Test
    def joinWith02(): Bool =
        (1 :: Nil) |> List.toDelayList |>
            DelayList.joinWith(x -> "${x + 1}", ",") == "2"

    @Test
    def joinWith03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toDelayList |>
            DelayList.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @Test
    def joinWith04(): Bool =
        ("1" :: "2" :: "3" :: Nil) |> List.toDelayList |>
            DelayList.joinWith(x -> x + x, ",") == "11,22,33"


    /////////////////////////////////////////////////////////////////////////////
    // ap                                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def ap01(): Bool =
        DelayList.ap(ENil, ENil) == ENil: DelayList[Int32]

    @Test
    def ap02(): Bool =
        DelayList.ap(ECons((x -> x + 1), ENil), ENil) == ENil

    @Test
    def ap03(): Bool =
        DelayList.ap(ENil, ECons(5, ENil)) == ENil: DelayList[Int32]

    @Test
    def ap04(): Bool =
        DelayList.ap(ECons((x -> x + 1), ENil), ECons(5, ENil)) |> DelayList.toList == 6 :: Nil

    @Test
    def ap05(): Bool =
        let f = List.toDelayList((x -> x + 1) :: Nil);
        let l = List.toDelayList(0 :: 5 :: Nil);
        DelayList.ap(f, l) |> DelayList.toList == 1 :: 6 :: Nil

    @Test
    def ap06(): Bool =
        let f = List.toDelayList((x -> x + 1) :: (x -> x * 2) :: Nil);
        let l = List.toDelayList(0 :: 4 :: Nil);
        DelayList.ap(f, l) |> DelayList.toList == 1 :: 5 :: 0 :: 8 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // sequence                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sequence01(): Bool =
        let l: DelayList[Identity[Int32]] = ENil;
        DelayList.sequence(l) == Identity.Identity(ENil)

    @Test
    def sequence02(): Bool =
        let l = List.toDelayList(Identity.Identity(1) :: Nil);
        DelayList.sequence(l) == Identity.Identity(List.toDelayList(1 :: Nil))

    @Test
    def sequence03(): Bool =
        let l = List.toDelayList(Identity.Identity(1) :: Identity.Identity(2) :: Nil);
        DelayList.sequence(l) == Identity.Identity(List.toDelayList(1 :: 2 :: Nil))

    @Test
    def sequence04(): Bool =
        let l = List.toDelayList(Identity.Identity(1) :: Identity.Identity(2) :: Identity.Identity(3) :: Nil);
        DelayList.sequence(l) == Identity.Identity(List.toDelayList(1 :: 2 :: 3 :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // traverse                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def traverse01(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = ENil;
        let ans = DelayList.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(ENil) and deref st == 0
    }

    @Test
    def traverse02(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = List.toDelayList(1 :: Nil);
        let ans = DelayList.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(List.toDelayList(1 :: Nil)) and deref st == 1
    }

    @Test
    def traverse03(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = List.toDelayList(1 :: 2 :: Nil);
        let ans = DelayList.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(List.toDelayList(1 :: 2 :: Nil)) and deref st == 2
    }

    @Test
    def traverse04(): Bool = region rc {
        let st = ref 0 @ rc;
        let l = List.toDelayList(1 :: 2 :: 3 :: Nil);
        let ans = DelayList.traverse(x -> {st := x; Identity.Identity(x)}, l);
        ans == Identity.Identity(List.toDelayList(1 :: 2 :: 3 :: Nil)) and deref st == 3
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Bool \ NonDet =
        let l1: List[Int32] = Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle(Random.new());

        DelayList.length(l2) == 0 and DelayList.toSet(l2) == Set#{}

    @Test
    def shuffle02(): Bool \ NonDet =
        let l1 = 1 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle(Random.new());

        DelayList.length(l2) == 1 and DelayList.toSet(l2) == Set#{1}

    @Test
    def shuffle03(): Bool \ NonDet =
        let l1 = 1 :: 2 :: 3 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle(Random.new());

        DelayList.length(l2) == 3 and DelayList.toSet(l2) == Set#{1, 2, 3}

    @Test
    def shuffle04(): Bool \ NonDet =
        let l1 = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let l2 = List.toDelayList(l1) |> DelayList.shuffle(Random.new());

        DelayList.length(l2) == 10 and DelayList.toSet(l2) == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Bool =
        ToString.toString(List.toDelayList(1 :: Nil)) == "DelayList(1)"

    @Test
    def toString02(): Bool =
        ToString.toString(List.toDelayList(1 :: 2 :: Nil)) == "DelayList(1, 2)"

    @Test
    def toString03(): Bool =
        ToString.toString(List.toDelayList(93 :: 3 :: 4 :: Nil)) == "DelayList(93, 3, 4)"

    @Test
    def toString04(): Bool =
        ToString.toString(List.toDelayList('a' :: 'b' :: 'c' :: Nil)) == "DelayList(a, b, c)"

    @Test
    def toString05(): Bool =
        ToString.toString(List.toDelayList(true :: false :: true :: true :: Nil)) == "DelayList(true, false, true, true)"

    @Test
    def toString06(): Bool =
        ToString.toString(List.toDelayList(List.toDelayList(1 :: 2 :: Nil) :: List.toDelayList(2 :: 3 :: Nil) :: List.toDelayList(4 :: 7 :: Nil) :: Nil)) == "DelayList(DelayList(1, 2), DelayList(2, 3), DelayList(4, 7))"

}
