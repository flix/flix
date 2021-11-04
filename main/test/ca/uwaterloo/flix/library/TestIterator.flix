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
        Iterator.repeat(0, 10) |> Iterator.toList == Nil

    @test
    def repeat02(): Bool & Impure =
        Iterator.repeat(-100, 2) |> Iterator.toList == Nil

    @test
    def repeat03(): Bool & Impure =
        Iterator.repeat(100, 2) |> Iterator.toList == List.repeat(100, 2)

    @test
    def repeat04(): Bool & Impure =
        Iterator.repeat(3, 1) |> Iterator.toList == 1 :: 1 :: 1 :: Nil

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
        Iterator.repeat(3, 1) |> Iterator.toArray == [1, 1, 1]

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


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool & Impure =
        List.toIterator(Nil: List[Int32]) |> Iterator.toSet == Set.empty()

    @test
    def toSet02(): Bool & Impure =
        Set#{} |> Set.toList |> List.toIterator |> Iterator.toSet == Set.empty()

    @test
    def toSet03(): Bool & Impure =
        Set#{"a", "b", "c", "d"} |> Set.toList |> List.toIterator |> Iterator.toSet == Set#{"a", "b", "c", "d"}

    @test
    def toSet04(): Bool & Impure =
        Set.range(0, 10000) |> Set.toList |> List.toIterator |> Iterator.toSet == Set.range(0, 10000)


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool & Impure =
        Nil |> List.toIterator |> Iterator.toNel == None

    @test
    def toNel02(): Bool & Impure =
        (1 :: Nil) |> List.toIterator |> Iterator.toNel == Some(Nel(1, Nil))

    @test
    def toNel03(): Bool & Impure =
        Iterator.range(0, 1000) |> Iterator.toNel == Some(Nel(0, List.range(1, 1000)))


    /////////////////////////////////////////////////////////////////////////////
    // filter (pure)                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterPure01(): Bool & Impure =
        Nil |> List.toIterator |> Iterator.filter(i -> i > 3) == Nil

    @test
    def filterPure02(): Bool & Impure =
        (2 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3) == Nil

    @test
    def filterPure03(): Bool & Impure =
        (4 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3) == 4 :: Nil

    @test
    def filterPure04(): Bool & Impure =
        (1 :: 3 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3) == Nil

    @test
    def filterPure05(): Bool & Impure =
        (1 :: 8 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3) == 8 :: Nil

    @test
    def filterPure06(): Bool & Impure =
        (8 :: 1 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3) == 8 :: Nil

    @test
    def filterPure07(): Bool & Impure =
        (8 :: 9 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3) == 8 :: 9 :: Nil

    @test
    def filterPure08(): Bool & Impure =
        (1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3) == 4 :: 11 :: 17 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // filter (impure)                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterImpure01(): Bool & Impure =
        Nil |> List.toIterator |> Iterator.filter(i -> i > 3 as & Impure) == Nil

    @test
    def filterImpure02(): Bool & Impure =
        (2 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3 as & Impure) == Nil

    @test
    def filterImpure03(): Bool & Impure =
        (4 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3 as & Impure) == 4 :: Nil

    @test
    def filterImpure04(): Bool & Impure =
        (1 :: 3 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3 as & Impure) == Nil

    @test
    def filterImpure05(): Bool & Impure =
        (1 :: 8 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3 as & Impure) == 8 :: Nil

    @test
    def filterImpure06(): Bool & Impure =
        (8 :: 1 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3 as & Impure) == 8 :: Nil

    @test
    def filterImpure07(): Bool & Impure =
        (8 :: 9 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3 as & Impure) == 8 :: 9 :: Nil

    @test
    def filterImpure08(): Bool & Impure =
        (1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> List.toIterator |> Iterator.filter(i -> i > 3 as & Impure) == 4 :: 11 :: 17 :: Nil

}
