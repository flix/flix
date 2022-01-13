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
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool =
        LazyList.tail(ENil: LazyList[Unit]) == ENil

    @test
    def tail02(): Bool =
        LazyList.tail(LList(lazy ENil): LazyList[Unit]) == ENil

    @test
    def tail03(): Bool =
        LazyList.range(0, 1) |> LazyList.tail == ENil

    @test
    def tail04(): Bool =
    	LCons(1, lazy ENil) |> LazyList.tail == ENil

    @test
    def tail05(): Bool =
    	LList(lazy ECons(1, LList(lazy ENil))) |> LazyList.tail == ENil

    @test
    def tail06(): Bool =
    	ECons(1, LList(lazy ENil)) |> LazyList.tail == ENil

    @test
    def tail07(): Bool =
    	LCons(1, lazy LList(lazy ENil)) |> LazyList.tail == ENil

    @test
    def tail08(): Bool =
    	LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))) |> LazyList.tail == LazyList.range(2, 4)

    @test
    def tail09(): Bool =
    	LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil)))) |> LazyList.tail == LazyList.range(2, 4)

    @test
    def tail10(): Bool =
    	LCons(1, lazy ECons(2, LList(lazy LCons(3, lazy LList(lazy ENil))))) |> LazyList.tail == LazyList.range(2, 4)

    @test
    def tail11(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> LazyList.tail == LazyList.range(2, 4)

    @test
    def tail12(): Bool =
    	LazyList.range(0, 1000) |> LazyList.tail == LazyList.range(1, 1000)


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
        (1 :: Nil) |> List.toLazyList |> LazyList.reverse |> LazyList.toList == 1 :: Nil

    @test
    def reverse03(): Bool =
        (1 :: 2 :: Nil) |> List.toLazyList |> LazyList.reverse |> LazyList.toList == 2 :: 1 :: Nil

    @test
    def reverse04(): Bool =
        (1 :: 1 :: Nil) |> List.toLazyList |> LazyList.reverse |> LazyList.toList == 1 :: 1 :: Nil

    @test
    def reverse05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.reverse |> LazyList.toList == 3 :: 2 :: 1 :: Nil

    @test
    def reverse06(): Bool =
        (1 :: 2 :: 3 :: 4 :: Nil) |> List.toLazyList |> LazyList.reverse |> LazyList.toList == 4 :: 3 :: 2 :: 1 :: Nil

    @test
    def reverse07(): Bool =
        LazyList.range(-100, 1000) |> LazyList.reverse |> LazyList.toList == List.range(-100, 1000) |> List.reverse


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool =
        Nil |> List.toLazyList |> LazyList.map(x -> x + 1) |> LazyList.toList == Nil

    @test
    def mapPure02(): Bool =
        (1 :: Nil) |> List.toLazyList |> LazyList.map(x -> x + 1) |> LazyList.toList == 2 :: Nil

    @test
    def mapPure03(): Bool =
        (1 :: 2 :: Nil) |> List.toLazyList |> LazyList.map(x -> x + 1) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def mapPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.map(x -> x + 1) |> LazyList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Nil |> List.toLazyList |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == Nil

    @test
    def mapImpure02(): Bool & Impure =
        (1 :: Nil) |> List.toLazyList |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == 2 :: Nil

    @test
    def mapImpure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> List.toLazyList |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def mapImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.map(x -> x + 1 as & Impure) |> LazyList.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapMap01(): Bool =
        (1 :: 2 :: 3 :: Nil)         |>
            List.toLazyList          |>
            LazyList.map(x -> x + 1) |>
            LazyList.map(x -> x * 2) |>
            LazyList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil)                     |>
            List.toLazyList                      |>
            LazyList.map(x -> x + 1 as & Impure) |>
            LazyList.map(x -> x * 2)             |>
            LazyList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil)                     |>
            List.toLazyList                      |>
            LazyList.map(x -> x + 1)             |>
            LazyList.map(x -> x * 2 as & Impure) |>
            LazyList.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil)                     |>
            List.toLazyList                      |>
            LazyList.map(x -> x + 1 as & Impure) |>
            LazyList.map(x -> x * 2 as & Impure) |>
            LazyList.toList == 4 :: 6 :: 8 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.map(x -> { l := "a" :: deref l; x }) |>
        LazyList.map(x -> { l := "b" :: deref l; x });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.map(x -> { l := "a" :: deref l; x } as & Pure) |>
        LazyList.map(x -> { l := "b" :: deref l; x } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool =
        Nil |> List.toLazyList |> LazyList.filter(x -> x > 100) |> LazyList.toList == Nil

    @test
    def filterPure02(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.filter(x -> x > 0) |> LazyList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterPure03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.filter(x -> x > 1) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def filterPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.filter(x -> x > 2) |> LazyList.toList == 3 :: Nil

    @test
    def filterPure05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.filter(x -> x > 3) |> LazyList.toList == Nil

    @test
    def filterPure06(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.filter(x -> x > 100) |> LazyList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool & Impure =
        Nil |> List.toLazyList |> LazyList.filter(x -> x > 100 as & Impure) |> LazyList.toList == Nil

    @test
    def filterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.filter(x -> x > 0 as & Impure) |> LazyList.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.filter(x -> x > 1 as & Impure) |> LazyList.toList == 2 :: 3 :: Nil

    @test
    def filterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.filter(x -> x > 2 as & Impure) |> LazyList.toList == 3 :: Nil

    @test
    def filterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.filter(x -> x > 3 as & Impure) |> LazyList.toList == Nil

    @test
    def filterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.filter(x -> x > 100 as & Impure) |> LazyList.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFilter01(): Bool =
        (1 :: 2 :: 3 :: Nil)            |>
            List.toLazyList             |>
            LazyList.filter(x -> x > 1) |>
            LazyList.filter(x -> x < 3) |>
            LazyList.toList == 2 :: Nil

    @test
    def filterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil)                        |>
            List.toLazyList                         |>
            LazyList.filter(x -> x > 1 as & Impure) |>
            LazyList.filter(x -> x < 3)             |>
            LazyList.toList == 2 :: Nil

    @test
    def filterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil)                        |>
            List.toLazyList                         |>
            LazyList.filter(x -> x > 1)             |>
            LazyList.filter(x -> x < 3 as & Impure) |>
            LazyList.toList == 2 :: Nil

    @test
    def filterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil)                        |>
            List.toLazyList                         |>
            LazyList.filter(x -> x > 1 as & Impure) |>
            LazyList.filter(x -> x < 3 as & Impure) |>
            LazyList.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.filter(_ -> { l := "a" :: deref l; true }) |>
        LazyList.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.filter(_x -> { l := "a" :: deref l; true } as & Pure) |>
        LazyList.filter(_x -> { l := "b" :: deref l; true } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.map(   x -> { l := "a" :: deref l; x }) |>
        LazyList.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.map(   x -> { l := "a" :: deref l; x }    as & Pure) |>
        LazyList.filter(_ -> { l := "b" :: deref l; true } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterThenMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.filter(_ -> { l := "a" :: deref l; true }) |>
        LazyList.map(   x -> { l := "b" :: deref l; x    });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterThenMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.filter(_ -> { l := "a" :: deref l; true } as & Pure) |>
        LazyList.map(   x -> { l := "b" :: deref l; x    } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, ENil) == 100

    @test
    def foldLeft02(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, LList(lazy ENil)) == 100

    @test
    def foldLeft03(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, LCons(1, lazy ENil)) == 198

    @test
    def foldLeft04(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil))))) == 196

    @test
    def foldLeft05(): Bool =
    	LazyList.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == 386

    @test
    def foldLeft06(): Bool =
    	LazyList.foldLeft((acc, x) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
    	LazyList.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, ENil) == 100

    @test
    def foldRight02(): Bool =
    	LazyList.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, LList(lazy ENil)) == 100

    @test
    def foldRight03(): Bool =
    	LazyList.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, LCons(1, lazy LList(lazy ENil))) == 198

    @test
    def foldRight04(): Bool =
    	LazyList.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, ECons(1, LCons(2, lazy ENil))) == 194

    @test
    def foldRight05(): Bool =
    	LazyList.foldRight((e, i) -> (i - e) * (e rem 2 + 1), 100, LCons(1, lazy ECons(2, LCons(3, lazy LList(lazy ENil))))) == 382

    @test
    def foldRight06(): Bool =
    	LazyList.foldRight((x, acc) -> x :: acc, Nil, LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure =
    	LazyList.toArray(ENil: LazyList[Unit]) `Array.sameElements` []

    @test
    def toArray02(): Bool & Impure =
    	LazyList.toArray(LList(lazy ENil): LazyList[Unit]) `Array.sameElements` []

    @test
    def toArray03(): Bool & Impure =
    	LazyList.range(-100, 1000) |> LazyList.toArray `Array.sameElements` Array.range(-100, 1000)

    @test
    def toArray04(): Bool & Impure =
    	LazyList.toArray(LCons(1, lazy ENil)) `Array.sameElements` [1]

    @test
    def toArray05(): Bool & Impure =
    	LazyList.toArray(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) `Array.sameElements` [1, 2, 3]


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
    def toList05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |> LazyList.toList == (1 :: 2 :: 3 :: Nil)

    @test
    def toList06(): Bool =
    	LazyList.toList(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = LazyList.toMap(ENil: LazyList[(Unit, Unit)]) == Map#{}

    @test
    def toMap02(): Bool = LazyList.toMap(LList(lazy ENil): LazyList[(Unit, Unit)]) == Map#{}

    @test
    def toMap03(): Bool = LazyList.toMap(ECons((1, true), ENil)) == Map#{1 => true}

    @test
    def toMap04(): Bool = LazyList.toMap(LList(lazy LCons((1, true), lazy LList(lazy LCons((2, false), lazy ENil))))) == Map#{1 => true, 2 => false}

    @test
    def toMap05(): Bool = LazyList.toMap(ECons((1, true), ECons((1, false), ENil))) == Map#{1 => false}


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
    	LazyList.toSet(LList(lazy LCons(1, lazy LList(lazy ECons(2, LList(lazy ECons(3, ENil))))))) == Set#{1, 2, 3}


    /////////////////////////////////////////////////////////////////////////////
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
    	LazyList.append(ECons(1, ENil), ENil) == ECons(1, ENil)

    @test
    def append04(): Bool =
    	LazyList.append(ENil, LCons(1, lazy ENil)) == ECons(1, ENil)

    @test
    def append05(): Bool =
    	LazyList.append(ECons(1, ECons(2, ECons(3, ENil))), ECons(4, LList(lazy ENil))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))

    @test
    def append06(): Bool =
    	LazyList.append(ECons(4, ENil), ECons(1, LCons(2, lazy LList(lazy LCons(3, lazy ENil))))) == ECons(4, ECons(1, ECons(2, ECons(3, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        LazyList.count(x -> x > 3, ENil) == 0

    @test
    def count02(): Bool =
    	LazyList.count(x -> x > 3, LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy ENil))))))) == 0

    @test
    def count03(): Bool =
    	LazyList.count(x -> x > 3, ECons(4, ENil)) == 1

    @test
    def count04(): Bool =
        LazyList.count(_ -> true, LazyList.range(1000, 2000)) == 1000

    @test
    def count05(): Bool =
        LazyList.count(_ -> false, LazyList.range(1000, 2000)) == 0

    @test
    def count06(): Bool =
        LazyList.count(x -> x > 3, ECons(1, ECons(2, ENil))) == 0

    @test
    def count07(): Bool =
        LazyList.count(x -> x > 3, ECons(1, ECons(8, ENil))) == 1

    @test
    def count08(): Bool =
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
        LazyList.drop(1, ENil: LazyList[Unit]) == ENil

    @test
    def drop04(): Bool =
        LazyList.drop(-1, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def drop05(): Bool =
        LazyList.drop(0, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def drop06(): Bool =
        LazyList.drop(1, ECons(1, ENil)) == ENil

    @test
    def drop07(): Bool =
    	LazyList.drop(1, LList(lazy ECons(1, LList(lazy ENil)))) == ENil

    @test
    def drop08(): Bool =
        LazyList.drop(2, ECons(1, ENil)) == ENil

    @test
    def drop09(): Bool =
    	LazyList.range(0, 1000) |> LazyList.drop(500) == LazyList.range(500, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool =
        LazyList.flatten(ENil: LazyList[LazyList[Unit]]) == ENil

    @test
    def flatten02(): Bool =
        LazyList.flatten(ECons(ENil, ENil)): LazyList[Unit] == ENil

    @test
    def flatten03(): Bool =
        LazyList.flatten(ECons(ENil, ECons(ENil, ENil))): LazyList[Unit] == ENil

    @test
    def flatten04(): Bool =
    	LazyList.flatten(ECons(LCons(1, lazy ENil), ENil)) == ECons(1, ENil)

    @test
    def flatten05(): Bool =
    	LazyList.flatten(ECons(LList(lazy ECons(1, LList(lazy ENil))), ENil)) == ECons(1, ENil)

    @test
    def flatten06(): Bool =
        LazyList.flatten(ECons((ECons(1, ECons(2, ENil))), ENil)) == ECons(1, ECons(2, ENil))

    @test
    def flatten07(): Bool =
    	LazyList.flatten(ECons(ENil, ECons(LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))), ENil))) == ECons(1, ECons(2, ENil))

    @test
    def flatten08(): Bool =
    	LazyList.flatten(ECons(ECons(1, LCons(2, lazy ENil)), ECons((ECons(1, ECons(2, ENil))), ENil))) == ECons(1, ECons(2, ECons(1, ECons(2, ENil))))

    @test
    def flatten09(): Bool =
    	LazyList.flatten(ECons((ECons(1, ECons(2, ENil))), ECons(ENil, ECons(ENil, ECons(LList(lazy LCons(3, lazy LCons(4, lazy ENil))), ENil))))) == ECons(1, ECons(2, ECons(3, ECons(4, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        LazyList.exists(i -> i > 3, ENil) == false

    @test
    def exists02(): Bool =
        (1 :: Nil) |> List.toLazyList |> LazyList.exists(i -> i > 3) == false

    @test
    def exists03(): Bool =
        (5 :: Nil) |> List.toLazyList |> LazyList.exists(i -> i > 3) == true

    @test
    def exists04(): Bool =
        (1 :: 2 :: Nil) |> List.toLazyList |> LazyList.exists(i -> i > 3) == false

    @test
    def exists05(): Bool =
        (1 :: 6 :: Nil) |> List.toLazyList |> LazyList.exists(i -> i > 3) == true

    @test
    def exists06(): Bool =
        (6 :: 1 :: Nil) |> List.toLazyList |> LazyList.exists(i -> i > 3) == true

    @test
    def exists07(): Bool =
        (16 :: 6 :: Nil) |> List.toLazyList |> LazyList.exists(i -> i > 3) == true

    @test
    def exists08(): Bool =
        (1 :: -9 :: 3 :: Nil) |> List.toLazyList |> LazyList.exists(i -> i > 3) == false

    @test
    def exists09(): Bool =
        (1 :: 9 :: 3 :: Nil) |> List.toLazyList |> LazyList.exists(i -> i > 3) == true


    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool =
        LazyList.forall(i -> i > 3, ENil) == true

    @test
    def forall02(): Bool =
        (1 :: Nil) |> List.toLazyList |> LazyList.forall(i -> i > 3) == false

    @test
    def forall03(): Bool =
        (5 :: Nil) |> List.toLazyList |> LazyList.forall(i -> i > 3) == true

    @test
    def forall04(): Bool =
        (1 :: 2 :: Nil) |> List.toLazyList |> LazyList.forall(i -> i > 3) == false

    @test
    def forall05(): Bool =
        (1 :: 6 :: Nil) |> List.toLazyList |> LazyList.forall(i -> i > 3) == false

    @test
    def forall06(): Bool =
        (6 :: 1 :: Nil) |> List.toLazyList |> LazyList.forall(i -> i > 3) == false

    @test
    def forall07(): Bool =
        (16 :: 6 :: Nil) |> List.toLazyList |> LazyList.forall(i -> i > 3) == true

    @test
    def forall08(): Bool =
        (1 :: -9 :: 3 :: Nil) |> List.toLazyList |> LazyList.forall(i -> i > 3) == false

    @test
    def forall09(): Bool =
        (1 :: 9 :: 3 :: Nil) |> List.toLazyList |> LazyList.forall(i -> i > 3) == false

    @test
    def forall10(): Bool =
        (11 :: 9 :: 31 :: Nil) |> List.toLazyList |> LazyList.forall(i -> i > 3) == true


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool =
        LazyList.repeat(1) |> LazyList.take(-1) == ENil

    @test
    def repeat02(): Bool =
        LazyList.repeat(1) |> LazyList.take(0) == ENil

    @test
    def repeat03(): Bool =
        LazyList.repeat(1) |> LazyList.take(1) == ECons(1, ENil)

    @test
    def repeat04(): Bool =
        LazyList.repeat(1) |> LazyList.take(2) == ECons(1, ECons(1, ENil))

    @test
    def repeat05(): Bool =
        LazyList.repeat(1) |> LazyList.take(3) == ECons(1, ECons(1, ECons(1, ENil)))

    @test
    def repeat07(): Bool =
        LazyList.repeat(3) |> LazyList.take(3) == ECons(3, ECons(3, ECons(3, ENil)))

    @test
    def repeat08(): Bool =
        LazyList.repeat("a") |> LazyList.take(3) == ECons("a", ECons("a", ECons("a", ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // from                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def from01(): Bool =
        LazyList.from(-1) |> LazyList.take(0) == ENil

    @test
    def from02(): Bool =
        LazyList.from(0) |> LazyList.take(0) == ENil

    @test
    def from04(): Bool =
        LazyList.from(100) |> LazyList.take(-1) == ENil

    @test
    def from05(): Bool =
    	LazyList.from(-1) |> LazyList.take(1) == ECons(-1, ENil)

    @test
    def from06(): Bool =
    	LazyList.from(5) |> LazyList.take(1) == ECons(5, ENil)

    @test
    def from07(): Bool =
    	LazyList.from(1) |> LazyList.take(3) == ECons(1, ECons(2, ECons(3, ENil)))

    @test
    def from08(): Bool =
    	LazyList.from(-1000) |> LazyList.take(1000) == LazyList.range(-1000, 0)

    @test
    def from09(): Bool =
    	LazyList.from(-1000) |> LazyList.take(2000) == LazyList.range(-1000, 1000)

    @test
    def from10(): Bool =
    	LazyList.from(0) |> LazyList.take(2000) == LazyList.range(0, 2000)


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool =
        LazyList.last(ENil: LazyList[Unit]) == None

    @test
    def last02(): Bool =
        LazyList.last(LList(lazy ENil): LazyList[Unit]) == None

    @test
    def last03(): Bool =
        LazyList.range(0, 1) |> LazyList.last == Some(0)

    @test
    def last06(): Bool =
    	ECons(1, ENil) |> LazyList.last == Some(1)

    @test
    def last11(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil)))))) |> LazyList.last == Some(3)

    @test
    def last12(): Bool =
    	LazyList.range(0, 1000) |> LazyList.last == Some(999)


    /////////////////////////////////////////////////////////////////////////////
    // toIterator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toIter01(): Bool & Impure =
    	LazyList.toIterator(ENil: LazyList[Unit]) |> Iterator.toList == Nil

    @test
    def toIter02(): Bool & Impure =
    	LazyList.toIterator(LList(lazy ENil): LazyList[Unit]) |> Iterator.toList == Nil

    @test
    def toIter06(): Bool & Impure =
    	LazyList.toIterator(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toIter09(): Bool & Impure =
    	LazyList.range(-100, 100) |> LazyList.toIterator |> Iterator.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool =
        LazyList.zip(ENil: LazyList[Int32], ENil: LazyList[String]) == ENil

    @test
    def zip02(): Bool =
        LazyList.zip(LList(lazy ENil): LazyList[Int32], ENil: LazyList[String]) == ENil

    @test
    def zip03(): Bool =
    	LazyList.zip(ENil: LazyList[Int32], LList(lazy ENil): LazyList[String]) == ENil

    @test
    def zip04(): Bool =
    	LazyList.zip(LList(lazy ENil): LazyList[Int32], LList(lazy ENil): LazyList[String]) == ENil

    @test
    def zip05(): Bool =
        LazyList.zip(ECons(1, ECons(2, ENil)), ENil: LazyList[String]) == ENil

    @test
    def zip06(): Bool =
    	LazyList.zip(ENil: LazyList[String], ECons(1, ECons(2, ENil))) == ENil

    @test
    def zip07(): Bool =
    	LazyList.zip(LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil)))), LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(("a", 1), ECons(("b", 2), ENil))

    @test
    def zip08(): Bool =
    	LazyList.zip(LCons(1, lazy LCons(2, lazy LList(lazy LCons(3, lazy ENil)))), LCons("a", lazy ECons("b", LList(lazy ECons("c", ENil))))) == ECons((1, "a"), ECons((2, "b"), ECons((3, "c"), ENil)))

    @test
    def zip09(): Bool =
        LazyList.zip(LazyList.range(1, 10), LazyList.range(1, 10)) |> LazyList.take(3) == ECons((1, 1), ECons((2, 2), ECons((3, 3), ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithPure01(): Bool =
        ENil: LazyList[Int32] |> LazyList.zipWith((x, y) -> x + y, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithPure02(): Bool =
    	LList(lazy (ENil: LazyList[Int32])) |> LazyList.zipWith((x, y) -> x + y, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithPure03(): Bool =
    	ECons(1, ECons(2, ENil)) |> LazyList.zipWith((x, y) -> x + y, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithPure04(): Bool =
    	ENil: LazyList[Int32] |> LazyList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ENil

    @test
    def zipWithPure05(): Bool =
    	ECons(1, ECons(2, ENil)) |> LazyList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithPure06(): Bool =
    	ECons(1, ENil) |> LazyList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @test
    def zipWithPure07(): Bool =
    	 LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> LazyList.zipWith((x, y) -> x + y, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(2, ECons(4, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithImpure01(): Bool & Impure =
        ENil: LazyList[Int32] |> LazyList.zipWith((x, y) -> x + y as & Impure, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithImpure02(): Bool & Impure =
    	LList(lazy (ENil: LazyList[Int32])) |> LazyList.zipWith((x, y) -> x + y as & Impure, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithImpure03(): Bool & Impure =
    	ECons(1, ECons(2, ENil)) |> LazyList.zipWith((x, y) -> x + y as & Impure, ENil: LazyList[Int32]) == ENil

    @test
    def zipWithImpure04(): Bool & Impure =
    	ENil: LazyList[Int32] |> LazyList.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ENil

    @test
    def zipWithImpure05(): Bool & Impure =
    	ECons(1, ECons(2, ENil)) |> LazyList.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ECons(2, ECons(4, ENil))

    @test
    def zipWithImpure06(): Bool & Impure =
    	ECons(1, ENil) |> LazyList.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ENil))) == ECons(2, ENil)

    @test
    def zipWithImpure07(): Bool & Impure =
    	 LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil)))) |> LazyList.zipWith((x, y) -> x + y as & Impure, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy ENil))))) == ECons(2, ECons(4, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith zipWith                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithZipWith01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            LazyList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))

    @test
    def zipWithZipWith02(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ECons(3, ENil)))) |>
            LazyList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))

    @test
    def zipWithZipWith03(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList.zipWith((x, y) -> x + y, ECons(1, ECons(2, ECons(3, ENil)))) |>
            LazyList.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))

    @test
    def zipWithZipWith04(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ECons(3, ENil)))) |>
            LazyList.zipWith((x, y) -> x + y as & Impure, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(3, ECons(6, ECons(9, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // zipWith zipWith fusion                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.zipWith((x, y) -> { l := "a" :: deref l; (x, y) }, ECons(1, ECons(2, ECons(3, ENil)))) |>
        LazyList.zipWith((x, y) -> { l := "b" :: deref l; (x, y) }, ECons(1, ECons(2, ECons(3, ENil))));
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def zipWithFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.zipWith((x, y) -> { l := "a" :: deref l; (x, y) } as & Pure, ECons(1, ECons(2, ECons(3, ENil)))) |>
        LazyList.zipWith((x, y) -> { l := "b" :: deref l; (x, y) } as & Pure, ECons(1, ECons(2, ECons(3, ENil)))) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool =
        LazyList.reduceLeft((a, b) -> a + b, ENil: LazyList[Int32]) == None

    @test
    def reduceLeft02(): Bool =
    	LazyList.reduceLeft((a, b) -> a + b, ECons(1, ENil)) == Some(1)

    @test
    def reduceLeft03(): Bool =
    	LazyList.reduceLeft((a, b) -> a + b, ECons(1, ECons(2, ENil))) == Some(3)

    @test
    def reduceLeft04(): Bool =
    	LazyList.reduceLeft((a, b) -> a + b, ECons(1, ECons(2, ECons(3, ENil)))) == Some(6)

    @test
    def reduceLeft05(): Bool =
    	LazyList.reduceLeft((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == Some(-8)

    @test
    def reduceLeft06(): Bool =
        LazyList.reduceLeft((a, b) -> b - a, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == Some(2)


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool =
        LazyList.reduceRight((a, b) -> a + b, ENil: LazyList[Int32]) == None

    @test
    def reduceRight02(): Bool =
        LazyList.reduceRight((a, b) -> a + b, LList(lazy ENil): LazyList[Int32]) == None

    @test
    def reduceRight03(): Bool =
    	LazyList.reduceRight((a, b) -> a + b, ECons(1, ENil)) == Some(1)

    @test
    def reduceRight04(): Bool =
    	LazyList.reduceRight((a, b) -> a + b, ECons(1, ECons(2, ENil))) == Some(3)

    @test
    def reduceRight05(): Bool =
    	LazyList.reduceRight((a, b) -> a + b, ECons(1, ECons(2, ECons(3, ENil)))) == Some(6)

    @test
    def reduceRight06(): Bool =
        LazyList.reduceRight((a, b) -> b - a, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == Some(-2)

    @test
    def reduceRight07(): Bool =
    	LazyList.reduceRight((a, b) -> a - b, LList(lazy ECons(1, LCons(2, lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))) == Some(-2)


    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimum01(): Bool =
        LazyList.minimum(ENil: LazyList[Int32]) == None

    @test
    def minimum02(): Bool =
        LazyList.minimum(ECons(1, ENil)) == Some(1)

    @test
    def minimum03(): Bool =
        LazyList.minimum(ECons(3, ECons(2, ECons(1, ENil)))) == Some(1)

    @test
    def minimum04(): Bool =
        LazyList.minimum(ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(0)

    @test
    def minimum05(): Bool =
        LazyList.minimum(LazyList.range(0, 99)) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool =
        LazyList.minimumBy((x, y) -> x <=> y, ENil: LazyList[Int32]) == None

    @test
    def minimumBy02(): Bool =
        LazyList.minimumBy((x, y) -> x <=> y, ECons(1, ENil)) == Some(1)

    @test
    def minimumBy03(): Bool =
        LazyList.minimumBy((x, y) -> x <=> y, ECons(3, ECons(2, ECons(1, ENil)))) == Some(1)

    @test
    def minimumBy04(): Bool =
        LazyList.minimumBy((x, y) -> x <=> y, ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(0)

    @test
    def minimumBy05(): Bool =
        LazyList.minimumBy((_, _) -> LessThan, LazyList.range(0, 99)) == Some(0)


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximum01(): Bool =
        LazyList.maximum(ENil: LazyList[Int32]) == None

    @test
    def maximum02(): Bool =
        LazyList.maximum(ECons(1, ENil)) == Some(1)

    @test
    def maximum03(): Bool =
        LazyList.maximum(ECons(3, ECons(2, ECons(1, ENil)))) == Some(3)

    @test
    def maximum04(): Bool =
        LazyList.maximum(ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(3)

    @test
    def maximum05(): Bool =
        LazyList.maximum(LazyList.range(0, 99)) == Some(98)


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool =
        LazyList.maximumBy((x, y) -> x <=> y, ENil: LazyList[Int32]) == None

    @test
    def maximumBy02(): Bool =
        LazyList.maximumBy((x, y) -> x <=> y, ECons(1, ENil)) == Some(1)

    @test
    def maximumBy03(): Bool =
        LazyList.maximumBy((x, y) -> x <=> y, ECons(3, ECons(2, ECons(1, ENil)))) == Some(3)

    @test
    def maximumBy04(): Bool =
        LazyList.maximumBy((x, y) -> x <=> y, ECons(0, ECons(3, ECons(2, ECons(1, ENil))))) == Some(3)

    @test
    def maximumBy05(): Bool =
        LazyList.maximumBy((_, _) -> LessThan, LazyList.range(0, 99)) == Some(98)


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (pure)                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapPure01(): Bool =
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ENil: LazyList[Int32]) == ENil

    @test
    def flatMapPure02(): Bool =
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ENil)) == ECons(1, ENil)

    @test
    def flatMapPure03(): Bool =
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapPure04(): Bool =
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i), ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap (impure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapImpure01(): Bool & Impure =
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i) as & Impure, ENil: LazyList[Int32]) == ENil

    @test
    def flatMapImpure02(): Bool & Impure =
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i) as & Impure, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def flatMapImpure03(): Bool & Impure =
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i) as & Impure, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))

    @test
    def flatMapImpure04(): Bool & Impure =
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i) as & Impure, ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy ENil))))))) == ECons(1, ECons(2, ECons(2, ECons(3, ECons(3, ECons(3, ENil))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapFlatMap01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil)))                            |>
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i)) |>
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i)) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @test
    def flatMapFlatMap02(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil)))                                        |>
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i) as & Impure) |>
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i))             ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @test
    def flatMapFlatMap03(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil)))                                        |>
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i))             |>
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i) as & Impure) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))

    @test
    def flatMapFlatMap04(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil)))                                        |>
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i) as & Impure) |>
        LazyList.flatMap(i -> LazyList.repeat(i) |> LazyList.take(i) as & Impure) ==
            ECons(1,
                ECons(2, ECons(2, ECons(2, ECons(2,
                    ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ECons(3, ENil))))))))))))))


    /////////////////////////////////////////////////////////////////////////////
    // flatMap flatMap fusion                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.flatMap(i -> { l := "a" :: deref l; ECons(i, ENil) }) |>
        LazyList.flatMap(i -> { l := "b" :: deref l; ECons(i, ENil) });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def flatMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.flatMap(i -> { l := "a" :: deref l; ECons(i, ENil) } as & Pure) |>
        LazyList.flatMap(i -> { l := "b" :: deref l; ECons(i, ENil) } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool =
        not (LazyList.memberOf(0, ENil))

    @test
    def memberOf02(): Bool =
        not (LazyList.memberOf(0, ECons(1, ENil)))

    @test
    def memberOf03(): Bool =
        not (LazyList.memberOf(0, ECons(1, ECons(2, ECons(3, ENil)))))

    @test
    def memberOf04(): Bool =
        LazyList.memberOf(0, ECons(1, ECons(2, ECons(3, ECons(0, ENil)))))

    @test
    def memberOf05(): Bool =
        LazyList.memberOf(0, ECons(0, ECons(1, ECons(2, ECons(3, ENil)))))

    @test
    def memberOf06(): Bool =
        LazyList.memberOf(0, ECons(1, ECons(1, ECons(1, ECons(0, ENil)))))

    @test
    def memberOf07(): Bool =
        LazyList.memberOf(1, ECons(1, ECons(1, ECons(1, ECons(1, ENil)))))

    @test
    def memberOf08(): Bool =
        LazyList.memberOf(0, ECons(1, ECons(1, ECons(0, ECons(1, ENil)))))

    @test
    def memberOf09(): Bool =
        LazyList.memberOf(-2, ECons(1, ECons(1000, ECons(-2, ECons(1, ENil)))))

    @test
    def memberOf10(): Bool =
        LazyList.memberOf(0, LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LCons(3, lazy LCons(0, lazy ENil)))))))


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool =
        LazyList.replace(3, 4, ENil) == ENil

    @test
    def replace02(): Bool =
        LazyList.replace(3, 4, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def replace03(): Bool =
        LazyList.replace(3, 4, ECons(3, ENil)) == ECons(4, ENil)

    @test
    def replace04(): Bool =
        LazyList.replace(3, 4, ECons(4, ECons(4, ECons(4, ENil)))) == ECons(4, ECons(4, ECons(4, ENil)))

    @test
    def replace05(): Bool =
        LazyList.replace(3, 4, ECons(2, ECons(2, ECons(2, ENil)))) == ECons(2, ECons(2, ECons(2, ENil)))

    @test
    def replace06(): Bool =
        LazyList.replace(3, 4, ECons(2, ECons(3, ECons(2, ENil)))) == ECons(2, ECons(4, ECons(2, ENil)))

    @test
    def replace07(): Bool =
        LazyList.replace(0, 1, ECons(0, ECons(0, ECons(0, ENil)))) == ECons(1, ECons(1, ECons(1, ENil)))

    @test
    def replace08(): Bool =
        LazyList.replace(3, 4, LList(lazy ECons(3, LList(lazy LCons(3, lazy LList(lazy LCons(3, lazy LList(lazy ENil)))))))) == ECons(4, ECons(4, ECons(4, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool =
        LazyList.findLeft(i -> i > 2, ENil: LazyList[Int32]) == None

    @test
    def findLeft02(): Bool =
        LazyList.findLeft(i -> i > 2, ECons(1, ENil)) == None

    @test
    def findLeft03(): Bool =
        LazyList.findLeft(i -> i > 2, ECons(3, ENil)) == Some(3)

    @test
    def findLeft04(): Bool =
        LazyList.findLeft(i -> i > 2, ECons(1, ECons(2, ENil))) == None

    @test
    def findLeft05(): Bool =
        LazyList.findLeft(i -> i > 2, ECons(6, ECons(-6, ENil))) == Some(6)

    @test
    def findLeft06(): Bool =
        LazyList.findLeft(i -> i > 2, ECons(7, ECons(6, ENil))) == Some(7)

    @test
    def findLeft07(): Bool =
        LazyList.findLeft(i -> i > 2, LList(lazy ECons(1, LList(lazy LCons(1, lazy LList(lazy LCons(4, lazy LList(lazy ENil)))))))) == Some(4)


    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool =
        LazyList.findRight(i -> i > 2, ENil: LazyList[Int32]) == None

    @test
    def findRight02(): Bool =
        LazyList.findRight(i -> i > 2, ECons(1, ENil)) == None

    @test
    def findRight03(): Bool =
        LazyList.findRight(i -> i > 2, ECons(3, ENil)) == Some(3)

    @test
    def findRight04(): Bool =
        LazyList.findRight(i -> i > 2, ECons(1, ECons(2, ENil))) == None

    @test
    def findRight05(): Bool =
        LazyList.findRight(i -> i > 2, ECons(6, ECons(-6, ENil))) == Some(6)

    @test
    def findRight06(): Bool =
        LazyList.findRight(i -> i > 2, ECons(7, ECons(6, ENil))) == Some(6)

    @test
    def findRight07(): Bool =
        LazyList.findRight(i -> i > 2, LList(lazy ECons(4, LList(lazy LCons(1, lazy LList(lazy LCons(1, lazy LList(lazy ENil)))))))) == Some(4)


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex (pure)                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexPure01(): Bool =
    	ENil |> LazyList.mapWithIndex((x, i) -> x + i) == ENil

    @test
    def mapWithIndexPure02(): Bool =
    	ECons(1, ENil) |> LazyList.mapWithIndex((x, i) -> x + i) == ECons(1, ENil)

    @test
    def mapWithIndexPure03(): Bool =
    	ECons(1, ECons(2, ENil)) |> LazyList.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexPure04(): Bool =
    	ECons(1, ECons(2, ECons(3, ENil))) |> LazyList.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexPure05(): Bool =
        ECons(1, ECons(2, ECons(3, ECons(4, ENil)))) |> LazyList.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexPure06(): Bool =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) |> LazyList.mapWithIndex((x, i) -> x + i) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex (impure)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexImpure01(): Bool & Impure =
    	ENil |> LazyList.mapWithIndex((x, i) -> x + i as & Impure) == ENil

    @test
    def mapWithIndexImpure02(): Bool & Impure =
    	ECons(1, ENil) |> LazyList.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ENil)

    @test
    def mapWithIndexImpure03(): Bool & Impure =
    	ECons(1, ECons(2, ENil)) |> LazyList.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ENil))

    @test
    def mapWithIndexImpure04(): Bool & Impure =
    	ECons(1, ECons(2, ECons(3, ENil))) |> LazyList.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ENil)))

    @test
    def mapWithIndexImpure05(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ECons(4, ENil)))) |> LazyList.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))

    @test
    def mapWithIndexImpure06(): Bool & Impure =
    	LList(lazy LCons(1, lazy LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy LList(lazy ENil))))))))) |> LazyList.mapWithIndex((x, i) -> x + i as & Impure) == ECons(1, ECons(3, ECons(5, ECons(7, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex mapWithIndex                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexMapWithIndex01(): Bool =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList.mapWithIndex((x, i) -> x + i) |>
            LazyList.mapWithIndex((x, i) -> x * i) == ECons(0, ECons(3, ECons(10, ENil)))

    @test
    def mapWithIndexMapWithIndex02(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList.mapWithIndex((x, i) -> x + i as & Impure) |>
            LazyList.mapWithIndex((x, i) -> x * i) == ECons(0, ECons(3, ECons(10, ENil)))

    @test
    def mapWithIndexMapWithIndex03(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList.mapWithIndex((x, i) -> x + i) |>
            LazyList.mapWithIndex((x, i) -> x * i as & Impure) == ECons(0, ECons(3, ECons(10, ENil)))

    @test
    def mapWithIndexMapWithIndex04(): Bool & Impure =
        ECons(1, ECons(2, ECons(3, ENil))) |>
            LazyList.mapWithIndex((x, i) -> x + i as & Impure) |>
            LazyList.mapWithIndex((x, i) -> x * i as & Impure) == ECons(0, ECons(3, ECons(10, ENil)))


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex mapWithIndex fusion                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndexFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.mapWithIndex((x, _) -> { l := "a" :: deref l; x }) |>
        LazyList.mapWithIndex((x, _) -> { l := "b" :: deref l; x }) |>
        LazyList.toList;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapWithIndexFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.mapWithIndex((x, _) -> { l := "a" :: deref l; x } as & Pure) |>
        LazyList.mapWithIndex((x, _) -> { l := "b" :: deref l; x } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool =
        LazyList.intercalate(ECons(1, ECons(2, ENil)), ENil) == ENil

    @test
    def intercalate02(): Bool =
        LazyList.intercalate(ECons(10, ECons(11, ECons(12, ENil))), ECons(ECons(1, ENil), ENil)) == ECons(1, ENil)

    @test
    def intercalate03(): Bool =
        LazyList.intercalate(
            ECons(10, ECons(11, ECons(12, ENil))),
                ECons(ECons(1, ECons(2, ECons(3, ENil))),
                ECons(ECons(100, ECons(101, ECons(102, ENil))), ENil)))
            == ECons(1, ECons(2, ECons(3,
                ECons(10, ECons(11, ECons(12,
                ECons(100, ECons(101, ECons(102, ENil)))))))))

    @test
    def intercalate04(): Bool =
        LazyList.intercalate(
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
        LazyList.intercalate(
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
        LazyList.intercalate(
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
        LazyList.intercalate(
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
        LazyList.intersperse(-11, ENil) == ENil

    @test
    def intersperse03(): Bool =
        LazyList.intersperse(-11, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def intersperse04(): Bool =
        LazyList.intersperse(-11, ECons(1, ECons(2, ECons(3, ENil)))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ENil)))))

    @test
    def intersperse05(): Bool =
        LazyList.intersperse(-11, ECons(1, ECons(2, ECons(3, ECons(4, ENil))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))

    @test
    def intersperse06(): Bool =
        LazyList.intersperse(-11, LList(lazy ECons(1, LList(lazy LCons(2, lazy LList(lazy ECons(3, LList(lazy LCons(4, lazy ENil))))))))) == ECons(1, ECons(-11, ECons(2, ECons(-11, ECons(3, ECons(-11, ECons(4, ENil)))))))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhilePure01(): Bool =
        LazyList.takeWhile(i -> i > 3, ENil) == ENil

    @test
    def takeWhilePure02(): Bool =
        LazyList.takeWhile(i -> i > 3, ECons(1, ENil)) == ENil

    @test
    def takeWhilePure03(): Bool =
        LazyList.takeWhile(i -> i > 3, ECons(4, ENil)) == ECons(4, ENil)

    @test
    def takeWhilePure04(): Bool =
        LazyList.takeWhile(i -> i > 3, ECons(1, ECons(2, ENil))) == ENil

    @test
    def takeWhilePure05(): Bool =
        LazyList.takeWhile(i -> i > 3, ECons(1, ECons(5, ENil))) == ENil

    @test
    def takeWhilePure06(): Bool =
        LazyList.takeWhile(i -> i > 3, ECons(5, ECons(1, ENil))) == ECons(5, ENil)

    @test
    def takeWhilePure07(): Bool =
        LazyList.takeWhile(i -> i > 3, ECons(5, ECons(8, ENil))) == ECons(5, ECons(8, ENil))

    @test
    def takeWhilePure08(): Bool =
        LazyList.takeWhile(i -> i > 3, LList(lazy ECons(5, LList(lazy LCons(8, lazy ENil))))) == ECons(5, ECons(8, ENil))

    @test
    def takeWhilePure09(): Bool =
        LazyList.takeWhile(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(4, ECons(6, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileImpure01(): Bool & Impure =
        LazyList.takeWhile(i -> i > 3 as & Impure, ENil) == ENil

    @test
    def takeWhileImpure02(): Bool & Impure =
        LazyList.takeWhile(i -> i > 3 as & Impure, ECons(1, ENil)) == ENil

    @test
    def takeWhileImpure03(): Bool & Impure =
        LazyList.takeWhile(i -> i > 3 as & Impure, ECons(4, ENil)) == ECons(4, ENil)

    @test
    def takeWhileImpure04(): Bool & Impure =
        LazyList.takeWhile(i -> i > 3 as & Impure, ECons(1, ECons(2, ENil))) == ENil

    @test
    def takeWhileImpure05(): Bool & Impure =
        LazyList.takeWhile(i -> i > 3 as & Impure, ECons(1, ECons(5, ENil))) == ENil

    @test
    def takeWhileImpure06(): Bool & Impure =
        LazyList.takeWhile(i -> i > 3 as & Impure, ECons(5, ECons(1, ENil))) == ECons(5, ENil)

    @test
    def takeWhileImpure07(): Bool & Impure =
        LazyList.takeWhile(i -> i > 3 as & Impure, ECons(5, ECons(8, ENil))) == ECons(5, ECons(8, ENil))

    @test
    def takeWhileImpure08(): Bool & Impure =
        LazyList.takeWhile(i -> i > 3 as & Impure, LList(lazy ECons(5, LList(lazy LCons(8, lazy ENil))))) == ECons(5, ECons(8, ENil))

    @test
    def takeWhileImpure09(): Bool & Impure =
        LazyList.takeWhile(i -> i > 3 as & Impure, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(4, ECons(6, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile takeWhile                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileTakeWhile01(): Bool =
        ECons(4, ECons(5, ECons(1, ENil))) |>
            LazyList.takeWhile(i -> i > 3) |>
            LazyList.takeWhile(i -> i < 5) == ECons(4, ENil)

    @test
    def takeWhileTakeWhile02(): Bool & Impure =
        ECons(4, ECons(5, ECons(1, ENil)))             |>
            LazyList.takeWhile(i -> i > 3 as & Impure) |>
            LazyList.takeWhile(i -> i < 5) == ECons(4, ENil)

    @test
    def takeWhileTakeWhile03(): Bool & Impure =
        ECons(4, ECons(5, ECons(1, ENil))) |>
            LazyList.takeWhile(i -> i > 3) |>
            LazyList.takeWhile(i -> i < 5 as & Impure) == ECons(4, ENil)

    @test
    def takeWhileTakeWhile04(): Bool & Impure =
        ECons(4, ECons(5, ECons(1, ENil)))             |>
            LazyList.takeWhile(i -> i > 3 as & Impure) |>
            LazyList.takeWhile(i -> i < 5 as & Impure) == ECons(4, ENil)


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile takeWhile fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhileFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.takeWhile(i -> { l := "a" :: deref l; i < 4 }) |>
        LazyList.takeWhile(i -> { l := "b" :: deref l; i < 4 });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def takeWhileFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.takeWhile(i -> { l := "a" :: deref l; i < 4 } as & Pure) |>
        LazyList.takeWhile(i -> { l := "b" :: deref l; i < 4 } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

    @test
    def takeWhileFusion03(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.takeWhile(i -> { l := "a" :: deref l; i < 3 } as & Pure) |>
        LazyList.takeWhile(i -> { l := "b" :: deref l; i < 2 } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: Nil)

    @test
    def takeWhileFusion04(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.takeWhile(i -> { l := "a" :: deref l; i < 3 } as & Pure) |>
        LazyList.takeWhile(i -> { l := "b" :: deref l; i < 3 } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhilePure01(): Bool =
        LazyList.dropWhile(i -> i > 3, ENil) == ENil

    @test
    def dropWhilePure02(): Bool =
        LazyList.dropWhile(i -> i > 3, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def dropWhilePure03(): Bool =
        LazyList.dropWhile(i -> i > 3, ECons(4, ENil)) == ENil

    @test
    def dropWhilePure04(): Bool =
        LazyList.dropWhile(i -> i > 3, ECons(1, ECons(2, ENil))) == ECons(1, ECons(2, ENil))

    @test
    def dropWhilePure05(): Bool =
        LazyList.dropWhile(i -> i > 3, ECons(1, ECons(5, ENil))) == ECons(1, ECons(5, ENil))

    @test
    def dropWhilePure06(): Bool =
        LazyList.dropWhile(i -> i > 3, ECons(5, ECons(1, ENil))) == ECons(1, ENil)

    @test
    def dropWhilePure07(): Bool =
        LazyList.dropWhile(i -> i > 3, ECons(5, ECons(8, ENil))) == ENil

    @test
    def dropWhilePure08(): Bool =
        LazyList.dropWhile(i -> i > 3, LList(lazy ECons(5, LCons(8, lazy LList(lazy LCons(9, lazy LList(lazy ECons(10, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))))))))))) == ECons(1, ECons(2, ENil))

    @test
    def dropWhilePure09(): Bool =
        LazyList.dropWhile(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileImpure01(): Bool & Impure =
        LazyList.dropWhile(i -> i > 3 as & Impure, ENil) == ENil

    @test
    def dropWhileImpure02(): Bool & Impure =
        LazyList.dropWhile(i -> i > 3 as & Impure, ECons(1, ENil)) == ECons(1, ENil)

    @test
    def dropWhileImpure03(): Bool & Impure =
        LazyList.dropWhile(i -> i > 3 as & Impure, ECons(4, ENil)) == ENil

    @test
    def dropWhileImpure04(): Bool & Impure =
        LazyList.dropWhile(i -> i > 3 as & Impure, ECons(1, ECons(2, ENil))) == ECons(1, ECons(2, ENil))

    @test
    def dropWhileImpure05(): Bool & Impure =
        LazyList.dropWhile(i -> i > 3 as & Impure, ECons(1, ECons(5, ENil))) == ECons(1, ECons(5, ENil))

    @test
    def dropWhileImpure06(): Bool & Impure =
        LazyList.dropWhile(i -> i > 3 as & Impure, ECons(5, ECons(1, ENil))) == ECons(1, ENil)

    @test
    def dropWhileImpure07(): Bool & Impure =
        LazyList.dropWhile(i -> i > 3 as & Impure, ECons(5, ECons(8, ENil))) == ENil

    @test
    def dropWhileImpure08(): Bool & Impure =
        LazyList.dropWhile(i -> i > 3 as & Impure, LList(lazy ECons(5, LCons(8, lazy LList(lazy LCons(9, lazy LList(lazy ECons(10, LList(lazy LCons(1, lazy ECons(2, LList(lazy ENil)))))))))))) == ECons(1, ECons(2, ENil))

    @test
    def dropWhileImpure09(): Bool & Impure =
        LazyList.dropWhile(i -> i > 3 as & Impure, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile dropWhile                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileDropWhile01(): Bool =
        ECons(4, ECons(5, ECons(1, ENil))) |>
            LazyList.dropWhile(i -> i < 5) |>
            LazyList.dropWhile(i -> i > 1) == ECons(1, ENil)

    @test
    def dropWhileDropWhile02(): Bool & Impure =
        ECons(4, ECons(5, ECons(1, ENil)))             |>
            LazyList.dropWhile(i -> i < 5 as & Impure) |>
            LazyList.dropWhile(i -> i > 1) == ECons(1, ENil)

    @test
    def dropWhileDropWhile03(): Bool & Impure =
        ECons(4, ECons(5, ECons(1, ENil)))     |>
            LazyList.dropWhile(i -> i < 5)     |>
            LazyList.dropWhile(i -> i > 1 as & Impure) == ECons(1, ENil)

    @test
    def dropWhileDropWhile04(): Bool & Impure =
        ECons(4, ECons(5, ECons(1, ENil)))             |>
            LazyList.dropWhile(i -> i < 5 as & Impure) |>
            LazyList.dropWhile(i -> i > 1 as & Impure) == ECons(1, ENil)


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile dropWhile fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.dropWhile(i -> { l := "a" :: deref l; i < 3 }) |>
        LazyList.dropWhile(i -> { l := "b" :: deref l; i < 4 });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: Nil)

    @test
    def dropWhileFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.dropWhile(i -> { l := "a" :: deref l; i < 3 } as & Pure) |>
        LazyList.dropWhile(i -> { l := "b" :: deref l; i < 4 } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: Nil)

    @test
    def dropWhileFusion03(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.dropWhile(i -> { l := "a" :: deref l; i < 1 } as & Pure) |>
        LazyList.dropWhile(i -> { l := "b" :: deref l; i < 3 } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def dropWhileFusion04(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.dropWhile(i -> { l := "a" :: deref l; i < 2 } as & Pure) |>
        LazyList.dropWhile(i -> { l := "b" :: deref l; i < 3 } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "a" :: "b" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // findMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findMap01(): Bool =
        LazyList.findMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ENil) == None

    @test
    def findMap02(): Bool =
        LazyList.findMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(1, ENil)) == None

    @test
    def findMap03(): Bool =
        LazyList.findMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(2, ENil)) == Some(1)

	@test
	def findMap04(): Bool =
		LazyList.findMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, ENil))) == None

	@test
	def findMap05(): Bool =
		LazyList.findMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))) == Some(2)

	@test
	def findMap06(): Bool =
		LazyList.findMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))) == Some(3)

	@test
	def findMap07(): Bool =
		LazyList.findMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))) == Some(4)

    @test
	def findMap08(): Bool =
		LazyList.findMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == Some(0)

    @test
	def findMap09(): Bool =
		LazyList.findMap(i -> if (i rem 2 == 0) Some(i / 2) else None,
		    LList(lazy LCons(-1, lazy LList(lazy LCons(1, lazy
		    LList(lazy LCons(9, lazy LList(lazy LCons(3, lazy
		    LList(lazy LCons(5, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == Some(5)


    /////////////////////////////////////////////////////////////////////////////
    // filterMap (pure)                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapPure01(): Bool =
        LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ENil) == ENil

    @test
    def filterMapPure02(): Bool =
        LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(1, ENil)) == ENil

    @test
    def filterMapPure03(): Bool =
        LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(2, ENil)) == ECons(1, ENil)

	@test
	def filterMapPure04(): Bool =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(1, ECons(3, ENil))) == ENil

	@test
	def filterMapPure05(): Bool =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@test
	def filterMapPure06(): Bool =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@test
	def filterMapPure07(): Bool =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))

    @test
	def filterMapPure08(): Bool =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))

    @test
	def filterMapPure09(): Bool =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None,
		    LList(lazy LCons(0, lazy LList(lazy
		    LCons(1, lazy LList(lazy LCons(2, lazy
		    LList(lazy LCons(3, lazy LList(lazy
		    LCons(4, lazy LList(lazy LCons(5, lazy
		    LList(lazy LCons(10, lazy LList(lazy ENil)))))))))))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap (impure)                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapImpure01(): Bool & Impure =
        LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure, ENil) == ENil

    @test
    def filterMapImpure02(): Bool & Impure =
        LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure, ECons(1, ENil)) == ENil

    @test
    def filterMapImpure03(): Bool & Impure =
        LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure, ECons(2, ENil)) == ECons(1, ENil)

	@test
	def filterMapImpure04(): Bool & Impure =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure, ECons(1, ECons(3, ENil))) == ENil

	@test
	def filterMapImpure05(): Bool & Impure =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure, ECons(1, ECons(4, ENil))) == ECons(2, ENil)

	@test
	def filterMapImpure06(): Bool & Impure =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure, ECons(6, ECons(-1, ENil))) == ECons(3, ENil)

	@test
	def filterMapImpure07(): Bool & Impure =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure, ECons(8, ECons(6, ENil))) == ECons(4, ECons(3, ENil))

    @test
	def filterMapImpure08(): Bool & Impure =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure, ECons(0, ECons(1, ECons(2, ECons(3, ECons(4, ECons(5, ECons(10, ENil)))))))) == ECons(0, ECons(1, ECons(2, ECons(5, ENil))))

    @test
	def filterMapImpure09(): Bool & Impure =
		LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure,
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
            LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None) |>
            LazyList.filterMap(i -> if (i > 1)      Some(i * 3) else None) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap02(): Bool & Impure =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure) |>
            LazyList.filterMap(i -> if (i > 1)      Some(i * 3) else None) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap03(): Bool & Impure =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None) |>
            LazyList.filterMap(i -> if (i > 1)      Some(i * 3) else None as & Impure) == ECons(6, ECons(9, ENil))

    @test
    def filterMapFilterMap04(): Bool & Impure =
        ECons(2, ECons(4, ECons(6, ECons(7, ENil)))) |>
            LazyList.filterMap(i -> if (i rem 2 == 0) Some(i / 2) else None as & Impure) |>
            LazyList.filterMap(i -> if (i > 1)      Some(i * 3) else None as & Impure) == ECons(6, ECons(9, ENil))


    /////////////////////////////////////////////////////////////////////////////
    // filterMap filterMap fusion                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.filterMap(x -> { l := "a" :: deref l; Some(x) }) |>
        LazyList.filterMap(x -> { l := "b" :: deref l; Some(x) });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toLazyList |>
        LazyList.filterMap(x -> { l := "a" :: deref l; Some(x) } as & Pure) |>
        LazyList.filterMap(x -> { l := "b" :: deref l; Some(x) } as & Pure) |>
        LazyList.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        LazyList.toString(ECons(1, ECons(2, ECons(3, ENil)))) == "1 :: 2 :: 3 :: ENil"

    @test
    def toString04(): Bool =
        LazyList.toString(LList(lazy LCons(1, lazy LList(lazy ECons(2, ECons(3, LList(lazy ENil))))))) == "1 :: 2 :: 3 :: ENil"


    /////////////////////////////////////////////////////////////////////////////
    // partition                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def partition01(): Bool =
        LazyList.partition(i -> i > 3, ENil) == (ENil, ENil)

    @test
    def partition02(): Bool =
        LazyList.partition(i -> i > 3, ECons(1, ENil)) == (ENil, ECons(1, ENil))

    @test
    def partition03(): Bool =
        LazyList.partition(i -> i > 3, ECons(4, ENil)) == (ECons(4, ENil), ENil)

    @test
    def partition04(): Bool =
        LazyList.partition(i -> i > 3, ECons(1, ECons(2, ENil))) == (ENil, ECons(1, ECons(2, ENil)))

    @test
    def partition05(): Bool =
        LazyList.partition(i -> i > 3, ECons(1, ECons(5, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @test
    def partition06(): Bool =
        LazyList.partition(i -> i > 3, ECons(5, ECons(1, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @test
    def partition07(): Bool =
        LazyList.partition(i -> i > 3, ECons(5, ECons(8, ENil))) == (ECons(5, ECons(8, ENil)), ENil)

    @test
    def partition08(): Bool =
        LazyList.partition(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))))
            == (ECons(4, ECons(6, ECons(11, ECons(16, ECons(7, ECons(7, ENil)))))), ECons(-3, ECons(-5, ECons(1, ECons(2, ECons(1, ENil))))))

    @test
    def partition09(): Bool =
        LazyList.partition(i -> i > 3,
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
        LazyList.span(i -> i > 3, ENil) == (ENil, ENil)

    @test
    def span02(): Bool =
        LazyList.span(i -> i > 3, ECons(1, ENil)) == (ENil, ECons(1, ENil))

    @test
    def span03(): Bool =
        LazyList.span(i -> i > 3, ECons(4, ENil)) == (ECons(4, ENil), ENil)

    @test
    def span04(): Bool =
        LazyList.span(i -> i > 3, ECons(1, ECons(2, ENil))) == (ENil, ECons(1, ECons(2, ENil)))

    @test
    def span05(): Bool =
        LazyList.span(i -> i > 3, ECons(1, ECons(5, ENil))) == (ENil, ECons(1, ECons(5, ENil)))

    @test
    def span06(): Bool =
        LazyList.span(i -> i > 3, ECons(5, ECons(1, ENil))) == (ECons(5, ENil), ECons(1, ENil))

    @test
    def span07(): Bool =
        LazyList.span(i -> i > 3, ECons(5, ECons(8, ENil))) == (ECons(5, ECons(8, ENil)), ENil)

    @test
    def span08(): Bool =
        LazyList.span(i -> i > 3, ECons(4, ECons(6, ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil)))))))))))) == (ECons(4, ECons(6, ENil)), ECons(-3, ECons(11, ECons(-5, ECons(1, ECons(2, ECons(16, ECons(7, ECons(1, ECons(7, ENil))))))))))

    @test
    def span09(): Bool =
        LazyList.span(i -> i > 3,
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
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        List.toLazyList(Nil) |> LazyList.sum == 0

    @test
    def sum02(): Bool =
        List.toLazyList(1 :: Nil) |> LazyList.sum == 1

    @test
    def sum03(): Bool =
        List.toLazyList(1 :: 2 :: 3 :: Nil) |> LazyList.sum == 6

    @test
    def sum04(): Bool =
        List.toLazyList(1 :: 2 :: 3 :: -3 :: Nil) |> LazyList.sum == 3

    @test
    def sum05(): Bool =
        List.toLazyList(-1 :: -2 :: -3 :: -4 :: Nil) |> LazyList.sum == -10

    @test
    def sum06(): Bool =
        List.toLazyList(10 :: -10 :: Nil) |> LazyList.sum == 0

    @test
    def sum07(): Bool =
        List.range(1, 101) |> List.toLazyList |> LazyList.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // order                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def order01(): Bool =
        ENil: LazyList[Unit] <=> ENil: LazyList[Unit] == EqualTo

    @test
    def order02(): Bool =
        ((1 :: Nil) |> List.toLazyList) <=> ENil: LazyList[Int32] == GreaterThan

    @test
    def order03(): Bool =
        ENil: LazyList[Int32] <=> ((1 :: Nil) |> List.toLazyList) == LessThan

    @test
    def order04(): Bool =
        ((1 :: Nil) |> List.toLazyList) <=> ((1 :: Nil) |> List.toLazyList) == EqualTo

    @test
    def order05(): Bool =
        ((2 :: 1 :: Nil) |> List.toLazyList) <=> ((1 :: 1 :: Nil) |> List.toLazyList) == GreaterThan

    @test
    def order06(): Bool =
        ((1 :: 1 :: Nil) |> List.toLazyList) <=> ((2 :: 1 :: Nil) |> List.toLazyList) == LessThan

    @test
    def order07(): Bool =
        ((1 :: Nil) |> List.toLazyList) <=> ((1 :: 1 :: Nil) |> List.toLazyList) == LessThan

}
