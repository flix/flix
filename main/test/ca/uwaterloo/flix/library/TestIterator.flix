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


    /////////////////////////////////////////////////////////////////////////////
    // map (pure)                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapPure01(): Bool & Impure =
        Nil |> List.toIterator |>
            Iterator.map(i -> i > 2) == Nil

    @test
    def mapPure02(): Bool & Impure =
        (1 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2) == false :: Nil

    @test
    def mapPure03(): Bool & Impure =
        (3 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2) == true :: Nil

    @test
    def mapPure04(): Bool & Impure =
        (1 :: 2 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2) == false :: false :: Nil

    @test
    def mapPure05(): Bool & Impure =
        (1 :: 8 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2) == false :: true :: Nil

    @test
    def mapPure06(): Bool & Impure =
        (8 :: 1 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2) == true :: false :: Nil

    @test
    def mapPure07(): Bool & Impure =
        (7 :: 8 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2) == true :: true :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // map (impure)                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapImpure01(): Bool & Impure =
        Nil |> List.toIterator |>
            Iterator.map(i -> i > 2 as & Impure) == Nil

    @test
    def mapImpure02(): Bool & Impure =
        (1 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2 as & Impure) == false :: Nil

    @test
    def mapImpure03(): Bool & Impure =
        (3 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2 as & Impure) == true :: Nil

    @test
    def mapImpure04(): Bool & Impure =
        (1 :: 2 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2 as & Impure) == false :: false :: Nil

    @test
    def mapImpure05(): Bool & Impure =
        (1 :: 8 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2 as & Impure) == false :: true :: Nil

    @test
    def mapImpure06(): Bool & Impure =
        (8 :: 1 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2 as & Impure) == true :: false :: Nil

    @test
    def mapImpure07(): Bool & Impure =
        (7 :: 8 :: Nil) |> List.toIterator |>
            Iterator.map(i -> i > 2 as & Impure) == true :: true :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool & Impure =
        let r = ref 21;
        Nil |> List.toIterator |>
            Iterator.foreach(x -> r := x);
        21 == deref r

    @test
    def foreach02(): Bool & Impure =
        let r = ref 21;
        (1 :: 2 :: 3 :: Nil) |> List.toIterator |>
            Iterator.foreach(x -> r := x);
        3 == deref r

    /////////////////////////////////////////////////////////////////////////////
    // lazyFilter                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lazyFilter01(): Bool & Impure =
        List.toIterator(Nil) |> Iterator.lazyFilter(i -> i > 3) |> Iterator.toList == Nil

    @test
    def lazyFilter02(): Bool & Impure =
        List.toIterator(2 :: Nil) |> Iterator.lazyFilter(i -> i > 3) |> Iterator.toList == Nil

    @test
    def lazyFilter03(): Bool & Impure =
        List.toIterator(4 :: Nil) |> Iterator.lazyFilter(i -> i > 3) |> Iterator.toList == 4 :: Nil

    @test
    def lazyFilter04(): Bool & Impure =
        List.toIterator(1 :: 3 :: Nil) |> Iterator.lazyFilter(i -> i > 3) |> Iterator.toList == Nil

    @test
    def lazyFilter05(): Bool & Impure =
        List.toIterator(1 :: 8 :: Nil) |> Iterator.lazyFilter(i -> i > 3) |> Iterator.toList == 8 :: Nil

    @test
    def lazyFilter06(): Bool & Impure =
        List.toIterator(8 :: 1 :: Nil) |> Iterator.lazyFilter(i -> i > 3) |> Iterator.toList == 8 :: Nil

    @test
    def lazyFilter07(): Bool & Impure =
        List.toIterator(8 :: 9 :: Nil) |> Iterator.lazyFilter(i -> i > 3) |> Iterator.toList == 8 :: 9 :: Nil

    @test
    def lazyFilter08(): Bool & Impure =
        List.toIterator(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> Iterator.lazyFilter(i -> i > 3) |> Iterator.toList == 4 :: 11 :: 17 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // lazyMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lazyMap01(): Bool & Impure =
        List.toIterator(Nil) |> Iterator.lazyMap(i -> i > 2) |> Iterator.toList == Nil

    @test
    def lazyMap02(): Bool & Impure =
        List.toIterator(1 :: Nil) |> Iterator.lazyMap(i -> i > 2) |> Iterator.toList == false :: Nil

    @test
    def lazyMap03(): Bool & Impure =
        List.toIterator(3 :: Nil) |> Iterator.lazyMap(i -> i > 2) |> Iterator.toList == true :: Nil

    @test
    def lazyMap04(): Bool & Impure =
        List.toIterator(1 :: 2 :: Nil) |> Iterator.lazyMap(i -> i > 2) |> Iterator.toList == false :: false :: Nil

    @test
    def lazyMap05(): Bool & Impure =
        List.toIterator(1 :: 8 :: Nil) |> Iterator.lazyMap(i -> i > 2) |> Iterator.toList == false :: true :: Nil

    @test
    def lazyMap06(): Bool & Impure =
        List.toIterator(8 :: 1 :: Nil) |> Iterator.lazyMap(i -> i > 2) |> Iterator.toList == true :: false :: Nil

    @test
    def lazyMap07(): Bool & Impure =
        List.toIterator(7 :: 8 :: Nil) |> Iterator.lazyMap(i -> i > 2) |> Iterator.toList == true :: true :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool & Impure =
        List.toIterator(Nil) |>
            Iterator.replace(3, 4) |> Iterator.toList == Nil

    @test
    def replace02(): Bool & Impure =
        List.toIterator(1 :: Nil) |>
            Iterator.replace(3, 4) |> Iterator.toList == 1 :: Nil

    @test
    def replace03(): Bool & Impure =
        List.toIterator(3 :: Nil) |>
            Iterator.replace(3, 4) |> Iterator.toList == 4 :: Nil

    @test
    def replace04(): Bool & Impure =
        List.toIterator(4 :: Nil) |>
            Iterator.replace(3, 4) |> Iterator.toList == 4 :: Nil

    @test
    def replace05(): Bool & Impure =
        List.toIterator(1 :: 2 :: Nil) |>
            Iterator.replace(3, 4) |> Iterator.toList == 1 :: 2 :: Nil

    @test
    def replace06(): Bool & Impure =
        List.toIterator(1 :: 3 :: Nil) |>
            Iterator.replace(3, 4) |> Iterator.toList == 1 :: 4 :: Nil

    @test
    def replace07(): Bool & Impure =
        List.toIterator(3 :: 4 :: Nil) |>
            Iterator.replace(3, 4) |> Iterator.toList == 4 :: 4 :: Nil

    @test
    def replace08(): Bool & Impure =
        List.toIterator(3 :: 3 :: Nil) |>
            Iterator.replace(3, 4) |> Iterator.toList == 4 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool & Impure =
        let iter1 = List.toIterator(Nil: List[Unit]);
        let iter2 = List.toIterator(Nil: List[Unit]);
        Iterator.append(iter1, iter2) |> Iterator.toList == Nil

    @test
    def append02(): Bool & Impure =
        let iter1 = List.toIterator(Nil);
        let iter2 = List.toIterator(1 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: Nil

    @test
    def append03(): Bool & Impure =
        let iter1 = List.toIterator(Nil);
        let iter2 = List.toIterator(1 :: 2 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil

    @test
    def append04(): Bool & Impure =
        let iter1 = List.toIterator(1 :: Nil);
        let iter2 = List.toIterator(Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: Nil

    @test
    def append05(): Bool & Impure =
        let iter1 = List.toIterator(1 :: 2 :: Nil);
        let iter2 = List.toIterator(Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil

    @test
    def append06(): Bool & Impure =
        let iter1 = List.toIterator(1 :: Nil);
        let iter2 = List.toIterator(2 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil

    @test
    def append07(): Bool & Impure =
        let iter1 = List.toIterator(1 :: 2 :: Nil);
        let iter2 = List.toIterator(3 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def append08(): Bool & Impure =
        let iter1 = List.toIterator(1 :: Nil);
        let iter2 = List.toIterator(2 :: 3 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool & Impure =
        let iterA = Nil: List[Int32] |> List.toIterator;
        let iterB = Iterator.repeat(1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == Nil

    @test
    def zip02(): Bool & Impure =
        let iterA = Nil: List[Int32] |> List.toIterator;
        let iterB = Iterator.repeat(1000, "b");
        Iterator.zip(iterB, iterA) |> Iterator.toList == Nil

    @test
    def zip03(): Bool & Impure =
        let iterA = Iterator.repeat(500,  "a");
        let iterB = Iterator.repeat(1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(500, ("a", "b"))

    @test
    def zip04(): Bool & Impure =
        let iterA = Iterator.repeat(1000, "a");
        let iterB = Iterator.repeat(500,  "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(500, ("a", "b"))

    @test
    def zip05(): Bool & Impure =
        let iterA = Iterator.repeat(1000, "a");
        let iterB = Iterator.repeat(1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(1000, ("a", "b"))

    @test
    def zip06(): Bool & Impure =
        let iterA = Iterator.range(0, 500);
        let iterB = Iterator.repeat(1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.zip(List.range(0, 500), List.repeat(500, "b"))

}
