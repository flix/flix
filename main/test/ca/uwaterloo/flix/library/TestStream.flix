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

}