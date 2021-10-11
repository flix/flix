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

namespace TestStream {

    /////////////////////////////////////////////////////////////////////////////
    // empty                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def empty01(): Bool =
        Stream.empty(): Stream[Unit, Unit] |> Stream.toList == Nil

    @test
    def empty02(): Bool =
        not (Stream.empty(): Stream[Int32, Int32] |> Stream.toList == 1 :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool =
        Stream.empty(): Stream[Int32, Unit] |> Stream.isEmpty

    @test
    def isEmpty02(): Bool =
        not (List.toStream(1 :: Nil) |> Stream.isEmpty)


    /////////////////////////////////////////////////////////////////////////////
    // toStream & toList                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toStream01(): Bool =
        List.toStream(Nil): Stream[Unit, List[Unit]] |> Stream.toList == Nil

    @test
    def toStream02(): Bool =
        List.toStream(1 :: Nil) |> Stream.toList == 1 :: Nil

    @test
    def toStream03(): Bool =
        List.toStream(1 :: 2 :: 3 :: Nil) |> Stream.toList == 1 :: 2 :: 3 :: Nil

    @test
    def toStream04(): Bool =
        List.range(-1000, 1000) |>
            List.toStream |>
            Stream.toList == List.range(-1000, 1000)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool =
        Stream.range(0, 0) |> Stream.isEmpty

    @test
    def range02(): Bool =
        Stream.range(100, 100) |> Stream.isEmpty

    @test
    def range03(): Bool =
        Stream.range(1, 0) |> Stream.isEmpty

    @test
    def range04(): Bool =
        not (Stream.range(0, 100) |> Stream.isEmpty)

    @test
    def range05(): Bool =
        Stream.range(0, 100) |> Stream.toList == List.range(0, 100)

    @test
    def range06(): Bool =
        not (Stream.range(-100, 100) |> Stream.isEmpty)

    @test
    def range07(): Bool =
        Stream.range(-100, 100) |> Stream.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool =
        Nil |> List.toStream |> Stream.filter(x -> x > 100) |> Stream.toList == Nil

    @test
    def filterPure02(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 0) |> Stream.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterPure03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 1) |> Stream.toList == 2 :: 3 :: Nil

    @test
    def filterPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 2) |> Stream.toList == 3 :: Nil

    @test
    def filterPure05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 3) |> Stream.toList == Nil

    @test
    def filterPure06(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 100) |> Stream.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool & Impure =
        Nil |> List.toStream |> Stream.filter(x -> x > 100 as & Impure) |> Stream.toList == Nil

    @test
    def filterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 0 as & Impure) |> Stream.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 1 as & Impure) |> Stream.toList == 2 :: 3 :: Nil

    @test
    def filterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 2 as & Impure) |> Stream.toList == 3 :: Nil

    @test
    def filterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 3 as & Impure) |> Stream.toList == Nil

    @test
    def filterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 100 as & Impure) |> Stream.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFilter01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 1) |> Stream.filter(x -> x < 3) |> Stream.toList == 2 :: Nil

    @test
    def filterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 1 as & Impure) |> Stream.filter(x -> x < 3) |> Stream.toList == 2 :: Nil

    @test
    def filterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 1) |> Stream.filter(x -> x < 3 as & Impure) |> Stream.toList == 2 :: Nil

    @test
    def filterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.filter(x -> x > 1 as & Impure) |> Stream.filter(x -> x < 3 as & Impure) |> Stream.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream.filter(_ -> { l := "a" :: deref l; true }) |>
        Stream.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream.filter(_x -> { l := "a" :: deref l; true } as & Pure) |>
        Stream.filter(_x -> { l := "b" :: deref l; true } as & Pure) |>
        Stream.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool =
        Nil |> List.toStream |> Stream.map(x -> x + 1) |> Stream.toList == Nil

    @test
    def mapPure02(): Bool =
        (1 :: Nil) |> List.toStream |> Stream.map(x -> x + 1) |> Stream.toList == 2 :: Nil

    @test
    def mapPure03(): Bool =
        (1 :: 2 :: Nil) |> List.toStream |> Stream.map(x -> x + 1) |> Stream.toList == 2 :: 3 :: Nil

    @test
    def mapPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.map(x -> x + 1) |> Stream.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Nil |> List.toStream |> Stream.map(x -> x + 1 as & Impure) |> Stream.toList == Nil

    @test
    def mapImpure02(): Bool & Impure =
        (1 :: Nil) |> List.toStream |> Stream.map(x -> x + 1 as & Impure) |> Stream.toList == 2 :: Nil

    @test
    def mapImpure03(): Bool & Impure =
        (1 :: 2 :: Nil) |> List.toStream |> Stream.map(x -> x + 1 as & Impure) |> Stream.toList == 2 :: 3 :: Nil

    @test
    def mapImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.map(x -> x + 1 as & Impure) |> Stream.toList == 2 :: 3 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapMap01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.map(x -> x + 1) |> Stream.map(x -> x * 2) |> Stream.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.map(x -> x + 1 as & Impure) |> Stream.map(x -> x * 2) |> Stream.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.map(x -> x + 1) |> Stream.map(x -> x * 2 as & Impure) |> Stream.toList == 4 :: 6 :: 8 :: Nil

    @test
    def mapMap04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.map(x -> x + 1 as & Impure) |> Stream.map(x -> x * 2 as & Impure) |> Stream.toList == 4 :: 6 :: 8 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map map fusion                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream.map(x -> { l := "a" :: deref l; x }) |>
        Stream.map(x -> { l := "b" :: deref l; x });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream.map(x -> { l := "a" :: deref l; x } as & Pure) |>
        Stream.map(x -> { l := "b" :: deref l; x } as & Pure) |>
        Stream.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // map filter fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapFilterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream.map(   x -> { l := "a" :: deref l; x }) |>
        Stream.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def mapFilterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream.map(   x -> { l := "a" :: deref l; x }    as & Pure) |>
        Stream.filter(_ -> { l := "b" :: deref l; true } as & Pure) |>
        Stream.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // filter map fusion                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream.filter(_ -> { l := "a" :: deref l; true }) |>
        Stream.map(   x -> { l := "b" :: deref l; x    });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterMapFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream.filter(_ -> { l := "a" :: deref l; true } as & Pure) |>
        Stream.map(   x -> { l := "b" :: deref l; x    } as & Pure) |>
        Stream.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
    	Stream.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100, Stream.empty(): Stream[Int32, Int32]) == 100

    @test
    def foldLeft02(): Bool =
    	(1 :: Nil) |> List.toStream |> Stream.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 198

    @test
    def foldLeft03(): Bool =
    	(1 :: 2 :: Nil) |> List.toStream |> Stream.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 196

    @test
    def foldLeft04(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.foldLeft((i, e) -> (i - e) * (e % 2 + 1), 100) == 386

    @test
    def foldLeft05(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.foldLeft((acc, x) -> x :: acc, Nil) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
    	Stream.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100, Stream.empty(): Stream[Int32, Int32]) == 100

    @test
    def foldRight02(): Bool =
    	(1 :: Nil) |> List.toStream |> Stream.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 198

    @test
    def foldRight03(): Bool =
    	(1 :: 2 :: Nil) |> List.toStream |> Stream.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 194

    @test
    def foldRight04(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.foldRight((e, i) -> (i - e) * (e % 2 + 1), 100) == 382

    @test
    def foldRight05(): Bool =
    	(1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream.foldRight((x, acc) -> x :: acc, Nil) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        not (Stream.exists(i -> i > 3, Stream.empty()))

    @test
    def exists02(): Bool =
        not ((1 :: Nil) |> List.toStream |> Stream.exists(i -> i > 3))

    @test
    def exists03(): Bool =
        (5 :: Nil) |> List.toStream |> Stream.exists(i -> i > 3)

    @test
    def exists04(): Bool =
        not ((1 :: 2 :: Nil) |> List.toStream |> Stream.exists(i -> i > 3))

    @test
    def exists05(): Bool =
        (1 :: 6 :: Nil) |> List.toStream |> Stream.exists(i -> i > 3)

    @test
    def exists06(): Bool =
        (6 :: 1 :: Nil) |> List.toStream |> Stream.exists(i -> i > 3)

    @test
    def exists07(): Bool =
        (16 :: 6 :: Nil) |> List.toStream |> Stream.exists(i -> i > 3)

    @test
    def exists08(): Bool =
        not ((1 :: -9 :: 3 :: Nil) |> List.toStream |> Stream.exists(i -> i > 3))

    @test
    def exists09(): Bool =
        (1 :: 9 :: 3 :: Nil) |> List.toStream |> Stream.exists(i -> i > 3)


    /////////////////////////////////////////////////////////////////////////////
    // forall                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forall01(): Bool =
        Stream.forall(i -> i > 3, Stream.empty())

    @test
    def forall02(): Bool =
        not ((1 :: Nil) |> List.toStream |> Stream.forall(i -> i > 3))

    @test
    def forall03(): Bool =
        (5 :: Nil) |> List.toStream |> Stream.forall(i -> i > 3)

    @test
    def forall04(): Bool =
        not ((1 :: 2 :: Nil) |> List.toStream |> Stream.forall(i -> i > 3))

    @test
    def forall05(): Bool =
        not ((1 :: 6 :: Nil) |> List.toStream |> Stream.forall(i -> i > 3))

    @test
    def forall06(): Bool =
        not ((6 :: 1 :: Nil) |> List.toStream |> Stream.forall(i -> i > 3))

    @test
    def forall07(): Bool =
        (16 :: 6 :: Nil) |> List.toStream |> Stream.forall(i -> i > 3)

    @test
    def forall08(): Bool =
        not ((1 :: -9 :: 3 :: Nil) |> List.toStream |> Stream.forall(i -> i > 3))

    @test
    def forall09(): Bool =
        not ((1 :: 9 :: 3 :: Nil) |> List.toStream |> Stream.forall(i -> i > 3))

    @test
    def forall10(): Bool =
        (11 :: 9 :: 31 :: Nil) |> List.toStream |> Stream.forall(i -> i > 3)


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool =
        (Stream.empty()): Stream[Int32, Unit] |> Stream.take(1000) |> Stream.isEmpty

    @test
    def take02(): Bool =
        (Stream.empty()): Stream[Int32, Unit] |> Stream.take(0) |> Stream.isEmpty

    @test
    def take03(): Bool =
        List.range(0, 1000) |> List.toStream |> Stream.take(0) |> Stream.isEmpty

    @test
    def take04(): Bool =
        List.range(0, 1000)
            |> List.toStream
            |> Stream.take(500)
            |> Stream.toList == List.range(0, 500)

    @test
    def take05(): Bool =
        List.range(0, 1000)
            |> List.toStream
            |> Stream.take(1)
            |> Stream.toList == List.range(0, 1)


    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool =
        Stream.append(Stream.empty(), Stream.empty()) |> Stream.toList == Nil

    @test
    def append02(): Bool =
        Stream.append(
            Stream.empty(),
            List.toStream(1 :: Nil)) |> Stream.toList == 1 :: Nil

    @test
    def append03(): Bool =
        Stream.append(
            Stream.empty(),
            List.toStream(1 :: 2 :: Nil)) |> Stream.toList == 1 :: 2 :: Nil

    @test
    def append04(): Bool =
        Stream.append(
            List.toStream(1 :: Nil),
            Stream.empty()) |> Stream.toList == 1 :: Nil

    @test
    def append05(): Bool =
        Stream.append(
            List.toStream(1 :: 2 :: Nil),
            Stream.empty()) |> Stream.toList == 1 :: 2 :: Nil

    @test
    def append06(): Bool =
        Stream.append(
            List.toStream(1 :: Nil),
            List.toStream(2 :: Nil)) |> Stream.toList == 1 :: 2 :: Nil

    @test
    def append07(): Bool =
        Stream.append(
            List.toStream(1 :: 2 :: Nil),
            List.toStream(3 :: Nil)) |> Stream.toList == 1 :: 2 :: 3 :: Nil

    @test
    def append08(): Bool =
        Stream.append(
            List.toStream(1 :: Nil),
            List.toStream(2 :: 3 :: Nil)) |> Stream.toList == 1 :: 2 :: 3 :: Nil

}