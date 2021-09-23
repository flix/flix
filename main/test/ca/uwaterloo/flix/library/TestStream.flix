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
        Stream1.range(0, 0) |> Stream1.isEmpty

    @test
    def range02(): Bool =
        Stream1.range(100, 100) |> Stream1.isEmpty

    @test
    def range03(): Bool =
        Stream1.range(1, 0) |> Stream1.isEmpty

    @test
    def range04(): Bool =
        not (Stream1.range(0, 100) |> Stream1.isEmpty)

    @test
    def range05(): Bool =
        Stream1.range(0, 100) |> Stream1.toList == List.range(0, 100)

    @test
    def range06(): Bool =
        not (Stream1.range(-100, 100) |> Stream1.isEmpty)

    @test
    def range07(): Bool =
        Stream1.range(-100, 100) |> Stream1.toList == List.range(-100, 100)


    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool =
        Nil |> List.toStream |> Stream1.filter(x -> x > 100) |> Stream1.toList == Nil

    @test
    def filterPure02(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 0) |> Stream1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterPure03(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1) |> Stream1.toList == 2 :: 3 :: Nil

    @test
    def filterPure04(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 2) |> Stream1.toList == 3 :: Nil

    @test
    def filterPure05(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 3) |> Stream1.toList == Nil

    @test
    def filterPure06(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 100) |> Stream1.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool & Impure =
        Nil |> List.toStream |> Stream1.filter(x -> x > 100 as & Impure) |> Stream1.toList == Nil

    @test
    def filterImpure02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 0 as & Impure) |> Stream1.toList == 1 :: 2 :: 3 :: Nil

    @test
    def filterImpure03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1 as & Impure) |> Stream1.toList == 2 :: 3 :: Nil

    @test
    def filterImpure04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 2 as & Impure) |> Stream1.toList == 3 :: Nil

    @test
    def filterImpure05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 3 as & Impure) |> Stream1.toList == Nil

    @test
    def filterImpure06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 100 as & Impure) |> Stream1.toList == Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFilter01(): Bool =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1) |> Stream1.filter(x -> x < 3) |> Stream1.toList == 2 :: Nil

    @test
    def filterFilter02(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1 as & Impure) |> Stream1.filter(x -> x < 3) |> Stream1.toList == 2 :: Nil

    @test
    def filterFilter03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1) |> Stream1.filter(x -> x < 3 as & Impure) |> Stream1.toList == 2 :: Nil

    @test
    def filterFilter04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toStream |> Stream1.filter(x -> x > 1 as & Impure) |> Stream1.filter(x -> x < 3 as & Impure) |> Stream1.toList == 2 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter filter fusion                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterFusion01(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream1.filter(_ -> { l := "a" :: deref l; true }) |>
        Stream1.filter(_ -> { l := "b" :: deref l; true });
        List.reverse(deref l) == ("a" :: "a" :: "a" :: "b" :: "b" :: "b" :: Nil)

    @test
    def filterFusion02(): Bool & Impure =
        let l = ref Nil;
        (1 :: 2 :: 3 :: Nil) |> List.toStream |>
        Stream1.filter(_x -> { l := "a" :: deref l; true } as & Pure) |>
        Stream1.filter(_x -> { l := "b" :: deref l; true } as & Pure) |>
        Stream1.toList as & Impure;
        List.reverse(deref l) == ("a" :: "b" :: "a" :: "b" :: "a" :: "b" :: Nil)

}