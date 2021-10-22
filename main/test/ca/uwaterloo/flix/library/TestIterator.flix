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

namespace TestIterator {

    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool & Impure =
        Iterator.range(0, 0) |> Iterator.toList == Nil

    @test
    def range02(): Bool & Impure =
        Iterator.range(10, 0) |> Iterator.toList == Nil

    @test
    def range03(): Bool & Impure =
        Iterator.range(1, 4) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def range04(): Bool & Impure =
        Iterator.range(-1, 2) |> Iterator.toList == -1 :: 0 :: 1 :: Nil

    @test
    def range05(): Bool & Impure =
        Iterator.range(-100, 200) |> Iterator.toList == List.range(-100, 200)



    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool & Impure =
        Iterator.repeat(10, 0) |> Iterator.toList == Nil

    @test
    def repeat02(): Bool & Impure =
        Iterator.repeat(2, -100) |> Iterator.toList == Nil

    @test
    def repeat03(): Bool & Impure =
        Iterator.repeat(2, 100) |> Iterator.toList == List.repeat(2, 100)

    @test
    def repeat04(): Bool & Impure =
        Iterator.repeat(1, 3) |> Iterator.toList == 1 :: 1 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure =
        (Nil: List[Int32] |> List.toIterator |> Iterator.toArray) == []

    @test
    def toArray02(): Bool & Impure =
        Iterator.range(-10, -5) |> Iterator.toArray == [-10, -9, -8, -7, -6]

    @test
    def toArray03(): Bool & Impure =
        Iterator.repeat(1, 3) |> Iterator.toArray == [1, 1, 1]

    @test
    def toArray04(): Bool & Impure =
        Iterator.range(-100, 200) |> Iterator.toArray == Array.range(-100, 200)


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool & Impure =
        Nil: List[(String, Int32)] |> List.toIterator |> Iterator.toMap == Map.empty()

    @test
    def toMap02(): Bool & Impure =
        (("A", 1) :: ("B", 2) :: ("C", 3) :: ("D", 4) :: Nil)|>
        List.toIterator |>
        Iterator.toMap == Map#{"A" -> 1, "B" -> 2, "C" -> 3, "D" -> 4}

    @test
    def toMap03(): Bool & Impure =
        let map = Map#{"1" -> 2, "2" -> 3, "3" -> 4};
        map |> Map.toList |> List.toIterator |> Iterator.toMap == map

}
