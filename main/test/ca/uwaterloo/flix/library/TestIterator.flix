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
    // next                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def next01(): Bool & Impure =
        Iterator.empty() |> Iterator.next == None: Option[Int32]

    @test
    def next02(): Bool & Impure =
        let iter = List.toIterator(1 :: Nil);
        Iterator.next(iter) == Some(1)

    @test
    def next03(): Bool & Impure =
        let iter = List.toIterator(1 :: Nil);
        Iterator.next(iter);
        Iterator.next(iter) == None

    @test
    def next04(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: Nil);
        Iterator.next(iter);
        Iterator.next(iter) == Some(2)


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty (idempotency)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool & Impure = // empty
        Iterator.empty() |> Iterator.isEmpty

    @test
    def isEmpty02(): Bool & Impure = // singleton
        let iter = Iterator.singleton(1);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == 1 :: Nil

    @test
    def isEmpty03(): Bool & Impure = // range
        let iter = Iterator.range(0, 100);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == List.range(0, 100)

    @test
    def isEmpty04(): Bool & Impure = // repeat
        let iter = Iterator.repeat(2, 10);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == 10 :: 10 :: Nil

    @test
    def isEmpty05(): Bool & Impure = // lazyFilter
        let iter = (1 :: 2 :: 3 :: Nil) |> List.toIterator |> Iterator.filterL(i -> i < 3);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == 1 :: 2 :: Nil

    @test
    def isEmpty06(): Bool & Impure = // lazyMap
        let iter = (1 :: 2 :: 3 :: Nil) |> List.toIterator |> Iterator.mapL(i -> i + 1);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == 2 :: 3 :: 4 :: Nil

    @test
    def isEmpty07(): Bool & Impure = // append
        let i1 = (1 :: 2 :: 3 :: Nil) |> List.toIterator;
        let i2 = (4 :: 5 :: 6 :: Nil) |> List.toIterator;
        let iter = Iterator.append(i1, i2);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil

    @test
    def isEmpty08(): Bool & Impure = // zip
        let i1 = (1 :: 2 :: 3 :: Nil) |> List.toIterator;
        let i2 = (4 :: 5 :: 6 :: Nil) |> List.toIterator;
        let iter = Iterator.zip(i1, i2);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == (1, 4) :: (2, 5) :: (3, 6) :: Nil

    @test
    def isEmpty09(): Bool & Impure = // lazyZipWith
        let i1 = (1 :: 2 :: 3 :: Nil) |> List.toIterator;
        let i2 = (4 :: 5 :: 6 :: Nil) |> List.toIterator;
        let iter = Iterator.zipWithL((x, y) -> x + y, i1, i2);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == 5 :: 7 :: 9 :: Nil

    @test
    def isEmpty10(): Bool & Impure = // flatMap
        let i = (1 :: 2 :: 3 :: Nil) |> List.toIterator;
        let iter = Iterator.flatMap(x -> Iterator.singleton(x + 1), i);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == 2 :: 3 :: 4 :: Nil

    @test
    def isEmpty11(): Bool & Impure = // intersperse
        let i = (1 :: 2 :: 3 :: Nil) |> List.toIterator;
        let iter = Iterator.intersperse(5, i);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == 1 :: 5 :: 2 :: 5 :: 3 :: Nil

    @test
    def isEmpty12(): Bool & Impure = // intercalate
        let iterA  = (11 :: 12 :: 13 :: Nil) |> List.toIterator;
        let iterB0 = (1 :: Nil) |> List.toIterator;
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator;
        let iterB  = (iterB0 :: iterB1 :: Nil) |> List.toIterator;
        let iter = Iterator.intercalate(iterA, iterB);
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: Nil

    @test
    def isEmpty13(): Bool & Impure = // unfoldWithOk
        let Iterator(d, n) = List.toIterator(1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        let iter = Iterator.unfoldWithOk(() -> if (d()) Err("Done") else Ok(n()));
        let _ = invokeIsEmpty(100, iter);
        Iterator.toList(iter) == 1 :: 2 :: 3 :: 4 :: 5 :: Nil

    ///
    /// Helper function that invokes `Iterator.isEmpty` `n` times.
    ///
    def invokeIsEmpty(n: Int32, iter: Iterator[a]): Bool & Impure =
        def loop(m) =
            if (m <= 0)
                true
            else {
                Iterator.isEmpty(iter);
                loop(m - 1)
            };
        loop(n)

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
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool & Impure =
        Iterator.empty() |> Iterator.product == 0

    @test
    def product02(): Bool & Impure =
        List.toIterator(1 :: Nil) |> Iterator.product == 1

    @test
    def product03(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: Nil) |> Iterator.product == 6

    @test
    def product04(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.product == -18

    @test
    def product05(): Bool & Impure =
        List.toIterator(-1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.product == 24

    @test
    def product06(): Bool & Impure =
        List.toIterator(10 :: -10 :: Nil) |> Iterator.product == -100


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool & Impure =
        Iterator.empty() |> Iterator.productWith(x -> x + 1) == 0

    @test
    def productWith02(): Bool & Impure =
        List.toIterator(1 :: Nil) |> Iterator.productWith(x -> x + 1) == 2

    @test
    def productWith03(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: Nil) |> Iterator.productWith(x -> x + 1) == 24

    @test
    def productWith04(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.productWith(x -> x + 1) == -48

    @test
    def productWith05(): Bool & Impure =
        List.toIterator(-2 :: -3 :: -4 :: -5 :: Nil) |> Iterator.productWith(x -> x + 1) == 24

    @test
    def productWith06(): Bool & Impure =
        List.toIterator(10 :: -10 :: Nil) |> Iterator.productWith(x -> x + 1) == -99


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool & Impure = region r {
        (Nil: List[Int32] |> List.toIterator |> Iterator.toArray)(r) `Array.sameElements` [] @ r
    }

    @test
    def toArray02(): Bool & Impure = region r {
        (Iterator.range(-10, -5) |> Iterator.toArray)(r) `Array.sameElements` [-10, -9, -8, -7, -6] @ r
    }

    @test
    def toArray03(): Bool & Impure = region r {
        (Iterator.repeat(3, 1) |> Iterator.toArray)(r) `Array.sameElements` [1, 1, 1] @ r
    }

    @test
    def toArray04(): Bool & Impure = region r {
        (Iterator.range(-100, 200) |> Iterator.toArray)(r) `Array.sameElements` Array.range(-100, 200, r)
    }


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
        Iterator.toMap == Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4}

    @test
    def toMap03(): Bool & Impure =
        let map = Map#{"1" => 2, "2" => 3, "3" => 4};
        map |> Map.toList |> List.toIterator |> Iterator.toMap == map


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool & Impure =
        List.toIterator(Nil: List[Int32]) |> Iterator.toSet == Set.empty()

    @test
    def toSet02(): Bool & Impure =
        Set#{} |> Set.toList |> List.toIterator |> Iterator.toSet == Set.empty(): Set[String]

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
        Nil |> List.toIterator |> Iterator.toNel == None: Option[Nel[Int32]]

    @test
    def toNel02(): Bool & Impure =
        (1 :: Nil) |> List.toIterator |> Iterator.toNel == Some(Nel(1, Nil))

    @test
    def toNel03(): Bool & Impure =
        Iterator.range(0, 1000) |> Iterator.toNel == Some(Nel(0, List.range(1, 1000)))


    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool & Impure =
        let iter = List.toIterator(Nil: List[Unit]);
        Iterator.toNec(iter) == None

    @test
    def toNec02(): Bool & Impure =
        let iter = List.toIterator(1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.singleton(1))

    @test
    def toNec03(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(1, Nec.singleton(2)))

    @test
    def toNec04(): Bool & Impure =
        let iter = List.toIterator(1 :: 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(1, Nec.singleton(1)))

    @test
    def toNec05(): Bool & Impure =
        let iter = List.toIterator(2 :: 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(2, Nec.singleton(1)))


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
        List.toIterator(Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == Nil

    @test
    def lazyFilter02(): Bool & Impure =
        List.toIterator(2 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == Nil

    @test
    def lazyFilter03(): Bool & Impure =
        List.toIterator(4 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == 4 :: Nil

    @test
    def lazyFilter04(): Bool & Impure =
        List.toIterator(1 :: 3 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == Nil

    @test
    def lazyFilter05(): Bool & Impure =
        List.toIterator(1 :: 8 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == 8 :: Nil

    @test
    def lazyFilter06(): Bool & Impure =
        List.toIterator(8 :: 1 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == 8 :: Nil

    @test
    def lazyFilter07(): Bool & Impure =
        List.toIterator(8 :: 9 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == 8 :: 9 :: Nil

    @test
    def lazyFilter08(): Bool & Impure =
        List.toIterator(1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == 4 :: 11 :: 17 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // lazyMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lazyMap01(): Bool & Impure =
        List.toIterator(Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == Nil

    @test
    def lazyMap02(): Bool & Impure =
        List.toIterator(1 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == false :: Nil

    @test
    def lazyMap03(): Bool & Impure =
        List.toIterator(3 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == true :: Nil

    @test
    def lazyMap04(): Bool & Impure =
        List.toIterator(1 :: 2 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == false :: false :: Nil

    @test
    def lazyMap05(): Bool & Impure =
        List.toIterator(1 :: 8 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == false :: true :: Nil

    @test
    def lazyMap06(): Bool & Impure =
        List.toIterator(8 :: 1 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == true :: false :: Nil

    @test
    def lazyMap07(): Bool & Impure =
        List.toIterator(7 :: 8 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == true :: true :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool & Impure =
        List.toIterator(Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == Nil

    @test
    def replace02(): Bool & Impure =
        List.toIterator(1 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: Nil

    @test
    def replace03(): Bool & Impure =
        List.toIterator(3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: Nil

    @test
    def replace04(): Bool & Impure =
        List.toIterator(4 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: Nil

    @test
    def replace05(): Bool & Impure =
        List.toIterator(1 :: 2 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: 2 :: Nil

    @test
    def replace06(): Bool & Impure =
        List.toIterator(1 :: 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: 4 :: Nil

    @test
    def replace07(): Bool & Impure =
        List.toIterator(3 :: 4 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: 4 :: Nil

    @test
    def replace08(): Bool & Impure =
        List.toIterator(3 :: 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: 4 :: Nil


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


    /////////////////////////////////////////////////////////////////////////////
    // lazyZipWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lazyZipWith01(): Bool & Impure =
        let iterA = List.toIterator(Nil);
        let iterB = List.toIterator(Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil

    @test
    def lazyZipWith02(): Bool & Impure =
        let iterA = List.toIterator(1 :: Nil);
        let iterB = List.toIterator(Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil

    @test
    def lazyZipWith03(): Bool & Impure =
        let iterA = List.toIterator(Nil);
        let iterB = List.toIterator(true :: Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil

    @test
    def lazyZipWith04(): Bool & Impure =
        let iterA = List.toIterator(1 :: Nil);
        let iterB = List.toIterator(true :: Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 2 :: Nil

    @test
    def lazyZipWith05(): Bool & Impure =
        let iterA = List.toIterator(1 :: Nil);
        let iterB = List.toIterator(false :: Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 1 :: Nil

    @test
    def lazyZipWith06(): Bool & Impure =
        let iterA = List.toIterator(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        let iterB = List.toIterator(false :: true :: true :: false :: false :: true :: true :: true :: Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |>
            Iterator.toList == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool & Impure =
        let iterA = List.toIterator(Nil);
        let iterB = List.toIterator(Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == Nil

    @test
    def zipWith02(): Bool & Impure =
        let iterA = List.toIterator(1 :: Nil);
        let iterB = List.toIterator(Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == Nil

    @test
    def zipWith03(): Bool & Impure =
        let iterA = List.toIterator(Nil);
        let iterB = List.toIterator(true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == Nil

    @test
    def zipWith04(): Bool & Impure =
        let iterA = List.toIterator(1 :: Nil);
        let iterB = List.toIterator(true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 2 :: Nil

    @test
    def zipWith05(): Bool & Impure =
        let iterA = List.toIterator(1 :: Nil);
        let iterB = List.toIterator(false :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 1 :: Nil

    @test
    def zipWith06(): Bool & Impure =
        let iterA = List.toIterator(1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        let iterB = List.toIterator(false :: true :: true :: false :: false :: true :: true :: true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool & Impure =
        let iter = List.toIterator(Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 100

    @test
    def foldLeft02(): Bool & Impure =
        let iter = List.toIterator(1 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 198

    @test
    def foldLeft03(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 196

    @test
    def foldLeft04(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: 3 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 386


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool & Impure =
        let iter = List.toIterator(Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 100

    @test
    def foldRight02(): Bool & Impure =
        let iter = List.toIterator(1 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 198

    @test
    def foldRight03(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 194

    @test
    def foldRight04(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: 3 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 382

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool & Impure =
        let iter = List.toIterator(Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 100

    @test
    def foldRightWithCont02(): Bool & Impure =
        let iter = List.toIterator(1 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 198

    @test
    def foldRightWithCont03(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 194

    @test
    def foldRightWithCont04(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: 3 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 382


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = Iterator.foldMap(x -> 2 * x, List.toIterator(Nil)) == 0

    @test
    def foldMap02(): Bool = Iterator.foldMap(x -> 2 * x, List.toIterator(1 :: 2 :: Nil)) == 6

    @test
    def foldMap03(): Bool = Iterator.foldMap(x -> if(x == "a") "b" else x, List.toIterator("a" :: Nil)) == "b"

    @test
    def foldMap04(): Bool = Iterator.foldMap(x -> if(x == "a") "b" else x, List.toIterator("a" :: "b" :: Nil)) == "bb"


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool & Impure =
        let iter = List.toIterator(Nil: List[Int32]);
        Iterator.reduceLeft((a, b) -> a - b, iter) == None

    @test
    def reduceLeft02(): Bool & Impure =
        let iter = List.toIterator(1 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(1)

    @test
    def reduceLeft03(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-1)

    @test
    def reduceLeft04(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: 3 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-4)

    @test
    def reduceLeft05(): Bool & Impure =
        let iter = List.toIterator(1 :: 2 :: 3 :: 4 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-8)


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool & Impure =
        List.toIterator(Nil: List[Int32]) |>
            Iterator.takeWhile(i -> i > 3) == Nil

    @test
    def takeWhile02(): Bool & Impure =
        List.toIterator(1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == Nil

    @test
    def takeWhile03(): Bool & Impure =
        List.toIterator(4 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == 4 :: Nil

    @test
    def takeWhile04(): Bool & Impure =
        List.toIterator(1 :: 2 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == Nil

    @test
    def takeWhile05(): Bool & Impure =
        List.toIterator(1 :: 5 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == Nil

    @test
    def takeWhile06(): Bool & Impure =
        List.toIterator(5 :: 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == 5 :: Nil

    @test
    def takeWhile07(): Bool & Impure =
        List.toIterator(5 :: 8 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == 5 :: 8 :: Nil

    @test
    def takeWhile08(): Bool & Impure =
        List.toIterator(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == 4 :: 6 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool & Impure =
        List.toIterator(Nil) |>
            Iterator.dropWhile(i -> i > 3) == Nil

    @test
    def dropWhile02(): Bool & Impure =
        List.toIterator(1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == 1 :: Nil

    @test
    def dropWhile03(): Bool & Impure =
        List.toIterator(4 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == Nil

    @test
    def dropWhile04(): Bool & Impure =
        List.toIterator(1 :: 2 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == 1 :: 2 :: Nil

    @test
    def dropWhile05(): Bool & Impure =
        List.toIterator(1 :: 5 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == 1 :: 5 :: Nil

    @test
    def dropWhile06(): Bool & Impure =
        List.toIterator(5 :: 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == 1 :: Nil

    @test
    def dropWhile07(): Bool & Impure =
        List.toIterator(5 :: 8 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == Nil

    @test
    def dropWhile08(): Bool & Impure =
        List.toIterator(4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool & Impure =
        List.toIterator(Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(i, i)) |>
            Iterator.toList == Nil

    @test
    def flatMap02(): Bool & Impure =
        List.toIterator(0 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(i, i)) |>
            Iterator.toList == Nil

    @test
    def flatMap03(): Bool & Impure =
        List.toIterator(1 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(i, i)) |>
            Iterator.toList == 1 :: Nil

    @test
    def flatMap04(): Bool & Impure =
        List.toIterator(2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(i, i)) |>
            Iterator.toList == 2 :: 2 :: Nil

    @test
    def flatMap05(): Bool & Impure =
        List.toIterator(1 :: 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(i, i)) |>
            Iterator.toList == 1 :: 2 :: 2 :: Nil

    @test
    def flatMap06(): Bool & Impure =
        List.toIterator(2 :: 3 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(i, i)) |>
            Iterator.toList == 2 :: 2 :: 3 :: 3 :: 3 :: Nil

    @test
    def flatMap07(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.flatMap(i -> if (i mod 2 == 0) Iterator.repeat(i, i) else Iterator.empty()) |>
            Iterator.toList == 2 :: 2 :: 4 :: 4 :: 4 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool & Impure =
        List.toIterator(Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == Nil

    @test
    def intersperse02(): Bool & Impure =
        List.toIterator(1 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: Nil

    @test
    def intersperse03(): Bool & Impure =
        List.toIterator(1 :: 2 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: Nil

    @test
    def intersperse04(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: 11 :: 3 :: Nil

    @test
    def intersperse05(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool & Impure =
        let iterA = Nil |> List.toIterator;
        let iterB = Nil |> List.toIterator;
        Iterator.intercalate(iterA, iterB) |> Iterator.toList == Nil: List[Int32]

    @test
    def intercalate02(): Bool & Impure =
        let iterA  = Nil |> List.toIterator;
        let iterB0 = (1 :: Nil) |> List.toIterator;
        let iterB  = (iterB0 :: Nil) |> List.toIterator;
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: Nil

    @test
    def intercalate03(): Bool & Impure =
        let iterA  = (11 :: 12 :: 13 :: Nil) |> List.toIterator;
        let iterB  = Nil |> List.toIterator;
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList ==  Nil

    @test
    def intercalate04(): Bool & Impure =
        let iterA  = Nil |> List.toIterator;
        let iterB0 = (1 :: Nil) |> List.toIterator;
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator;
        let iterB  = (iterB0 :: iterB1 :: Nil) |> List.toIterator;
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: 2 :: 3 :: Nil

    @test
    def intercalate05(): Bool & Impure =
        let iterA  = (11 :: 12 :: 13 :: Nil) |> List.toIterator;
        let iterB0 = (1 :: Nil) |> List.toIterator;
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator;
        let iterB  = (iterB0 :: iterB1 :: Nil) |> List.toIterator;
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: Nil

    @test
    def intercalate06(): Bool & Impure =
        let iterA  = Nil |> List.toIterator;
        let iterB0 = (1 :: Nil) |> List.toIterator;
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator;
        let iterB2 = (4 :: Nil) |> List.toIterator;
        let iterB  = (iterB0 :: iterB1 :: iterB2 :: Nil) |> List.toIterator;
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: 2 :: 3 :: 4 :: Nil

    @test
    def intercalate07(): Bool & Impure =
        let iterA  = (11 :: 12 :: 13 :: Nil) |> List.toIterator;
        let iterB0 = (1 :: Nil) |> List.toIterator;
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator;
        let iterB2 = (4 :: Nil) |> List.toIterator;
        let iterB  = (iterB0 :: iterB1 :: iterB2 :: Nil) |> List.toIterator;
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: 11 :: 12 :: 13 :: 4 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // unfoldWithOk                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unfoldWithOk01(): Bool & Impure =
        let iter = Iterator.unfoldWithOk(() -> Err(0));
        Iterator.toList(iter) == Nil: List[Int32]

    @test
    def unfoldWithOk02(): Bool & Impure =
        let Iterator(d, n) = List.toIterator(1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        Iterator.unfoldWithOk(() -> if (d()) Err("Done") else Ok(n())) |>
            Iterator.toList == 1 :: 2 :: 3 :: 4 :: 5 :: Nil

    @test
    def unfoldWithOk03(): Bool & Impure =
        let Iterator(d, n) = (1 :: 2 :: 3 :: 4 :: 5 :: Nil)   |>
            List.map(x -> if (x == 4) Err("Done") else Ok(x)) |>
            List.toIterator;
        Iterator.unfoldWithOk(() -> { d(); n() }) |>
            Iterator.toList == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool & Impure =
        List.toIterator(Nil) |> Iterator.sum == 0

    @test
    def sum02(): Bool & Impure =
        List.toIterator(1 :: Nil) |> Iterator.sum == 1

    @test
    def sum03(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: Nil) |> Iterator.sum == 6

    @test
    def sum04(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sum == 3

    @test
    def sum05(): Bool & Impure =
        List.toIterator(-1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sum == -10

    @test
    def sum06(): Bool & Impure =
        List.toIterator(10 :: -10 :: Nil) |> Iterator.sum == 0

    @test
    def sum07(): Bool & Impure =
        Iterator.range(1, 101) |> Iterator.sum == 5050


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool & Impure =
        List.toIterator(Nil) |> Iterator.sumWith(x -> x + 1) == 0

    @test
    def sumWith02(): Bool & Impure =
        List.toIterator(1 :: Nil) |> Iterator.sumWith(x -> x + 1) == 2

    @test
    def sumWith03(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: Nil) |> Iterator.sumWith(x -> x + 1) == 9

    @test
    def sumWith04(): Bool & Impure =
        List.toIterator(1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sumWith(x -> x + 1) == 7

    @test
    def sumWith05(): Bool & Impure =
        List.toIterator(-1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sumWith(x -> x + 1) == -6

    @test
    def sumWith06(): Bool & Impure =
        List.toIterator(10 :: -10 :: Nil) |> Iterator.sumWith(x -> x + 1) == 2


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool & Impure =
        (Nil: List[Int32]) |> List.toIterator |>
            Iterator.join(",") == ""

    @test
    def join02(): Bool & Impure =
        (1 :: Nil) |> List.toIterator |>
            Iterator.join(",") == "1"

    @test
    def join03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toIterator |>
            Iterator.join(",") == "1,2,3"

    @test
    def join04(): Bool & Impure =
        ("1" :: "2" :: "3" :: Nil) |> List.toIterator |>
            Iterator.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool & Impure =
        (Nil: List[Int32]) |> List.toIterator |>
            Iterator.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool & Impure =
        (1 :: Nil) |> List.toIterator |>
            Iterator.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toIterator |>
            Iterator.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool & Impure =
        ("1" :: "2" :: "3" :: Nil) |> List.toIterator |>
            Iterator.joinWith(x -> x + x, ",") == "11,22,33"

    /////////////////////////////////////////////////////////////////////////////
    // filterMapL                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapL01(): Bool & Impure =
        (Nil: List[Int32]) |> List.toIterator |>
            Iterator.filterMapL(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == Nil
    @test
    def filterMapL02(): Bool & Impure =
        (1 :: Nil) |> List.toIterator |>
            Iterator.filterMapL(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == Nil

    @test
    def filterMapL03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toIterator |>
            Iterator.filterMapL(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == 2 :: Nil

    @test
    def filterMapL04(): Bool & Impure =
        (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> List.toIterator |>
            Iterator.filterMapL(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == 2 :: 4 :: 6 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // dropL                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropL01(): Bool & Impure =
        (Nil: List[Int32]) |> List.toIterator |>
            Iterator.dropL(1) |> Iterator.toList == Nil
    @test
    def dropL02(): Bool & Impure =
        (1 :: Nil) |> List.toIterator |>
            Iterator.dropL(1) |> Iterator.toList == Nil

    @test
    def dropL03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toIterator |>
            Iterator.dropL(2) |>  Iterator.toList == 3 :: Nil

    @test
    def dropL04(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toIterator |>
            Iterator.dropL(3) |>  Iterator.toList == Nil

    @test
    def dropL05(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toIterator |>
            Iterator.dropL(4) |>  Iterator.toList == Nil

    @test
    def dropL06(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toIterator |>
            Iterator.dropL(0) |>  Iterator.toList == 1 :: 2 :: 3 :: Nil

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileL                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileL01(): Bool & Impure =
        (Nil: List[Int32]) |> List.toIterator |>
            Iterator.dropWhileL(x -> x < 3) |> Iterator.toList == Nil
    @test
    def dropWhileL02(): Bool & Impure =
        (1 :: Nil) |> List.toIterator |>
            Iterator.dropWhileL(x -> x < 3) |> Iterator.toList == Nil

    @test
    def dropWhileL03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toIterator |>
            Iterator.dropWhileL(x -> x < 3) |>  Iterator.toList == 3 :: Nil

    @test
    def dropWhileL04(): Bool & Impure =
        (1 :: 2 :: 3 :: 4 :: Nil) |> List.toIterator |>
            Iterator.dropWhileL(x -> x < 3) |>  Iterator.toList == 3 :: 4 :: Nil

    @test
    def dropWhileL05(): Bool & Impure =
        (1 :: 2 :: 3 :: 4 :: 0 :: Nil) |> List.toIterator |>
            Iterator.dropWhileL(x -> x < 3) |>  Iterator.toList == 3 :: 4 :: 0 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toResultList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toResultList01(): Bool & Impure =
        (Nil: List[Result[Int32, String]]) |> List.toIterator |>
            Iterator.toResultList == Ok(Nil)
    @test
    def toResultList02(): Bool & Impure =
        ((Ok(1) :: Nil): List[Result[Int32, String]]) |> List.toIterator |>
            Iterator.toResultList == Ok(1 :: Nil)

    @test
    def toResultList03(): Bool & Impure =
        ((Ok(1) :: Ok(2) :: Ok(3) :: Nil): List[Result[Int32, String]]) |> List.toIterator |>
            Iterator.toResultList == Ok(1 :: 2 :: 3 :: Nil)

    @test
    def toResultList04(): Bool & Impure =
        (Ok(1) :: Ok(2) :: Ok(3) :: Err("four") :: Nil) |> List.toIterator |>
            Iterator.toResultList == Err("four")

    @test
    def toResultList05(): Bool & Impure =
        (Ok(1) :: Ok(2) :: Ok(3) :: Err("four") :: Err("zero") :: Nil) |> List.toIterator |>
            Iterator.toResultList == Err("four")

}
