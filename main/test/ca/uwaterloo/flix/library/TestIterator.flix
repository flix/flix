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
    def next01(): Bool = region r {
        new Iterator(r) |> Iterator.next == None: Option[Int32]
    }

    @test
    def next02(): Bool = region r {
        let iter = List.toIterator(r, 1 :: Nil);
        Iterator.next(iter) == Some(1)
    }

    @test
    def next03(): Bool = region r {
        let iter = List.toIterator(r, 1 :: Nil);
        discard Iterator.next(iter);
        Iterator.next(iter) == None
    }

    @test
    def next04(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: Nil);
        discard Iterator.next(iter);
        Iterator.next(iter) == Some(2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty (idempotency)                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool = region r { // empty
        let iter = new Iterator(r);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == Nil: List[Int32]
    }

    @test
    def isEmpty02(): Bool = region r { // singleton
        let iter = Iterator.singleton(r, 1);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == 1 :: Nil
    }

    @test
    def isEmpty03(): Bool = region r { // range
        let iter = Iterator.range(r, 0, 100);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == List.range(0, 100)
    }

    @test
    def isEmpty04(): Bool = region r { // repeat
        let iter = Iterator.repeat(r, 2, 10);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == 10 :: 10 :: Nil
    }

    @test
    def isEmpty05(): Bool = region r { // lazyFilter
        let iter = (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |> Iterator.filterL(i -> i < 3);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == 1 :: 2 :: Nil
    }

    @test
    def isEmpty06(): Bool = region r { // lazyMap
        let iter = (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |> Iterator.mapL(i -> i + 1);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == 2 :: 3 :: 4 :: Nil
    }

    @test
    def isEmpty07(): Bool = region r { // append
        let i1 = (1 :: 2 :: 3 :: Nil) |> List.toIterator(r);
        let i2 = (4 :: 5 :: 6 :: Nil) |> List.toIterator(r);
        let iter = Iterator.append(i1, i2);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil
    }

    @test
    def isEmpty08(): Bool = region r { // zip
        let i1 = (1 :: 2 :: 3 :: Nil) |> List.toIterator(r);
        let i2 = (4 :: 5 :: 6 :: Nil) |> List.toIterator(r);
        let iter = Iterator.zip(i1, i2);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == (1, 4) :: (2, 5) :: (3, 6) :: Nil
    }

    @test
    def isEmpty09(): Bool = region r { // lazyZipWith
        let i1 = (1 :: 2 :: 3 :: Nil) |> List.toIterator(r);
        let i2 = (4 :: 5 :: 6 :: Nil) |> List.toIterator(r);
        let iter = Iterator.zipWithL((x, y) -> x + y, i1, i2);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == 5 :: 7 :: 9 :: Nil
    }

    @test
    def isEmpty10(): Bool = region r { // flatMap
        let i = (1 :: 2 :: 3 :: Nil) |> List.toIterator(r);
        let iter = Iterator.flatMap(x -> Iterator.singleton(r, x + 1), i);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == 2 :: 3 :: 4 :: Nil
    }

    @test
    def isEmpty11(): Bool = region r { // intersperse
        let i = (1 :: 2 :: 3 :: Nil) |> List.toIterator(r);
        let iter = Iterator.intersperse(5, i);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == 1 :: 5 :: 2 :: 5 :: 3 :: Nil
    }

    @test
    def isEmpty12(): Bool = region r { // intercalate
        let iterA  = (11 :: 12 :: 13 :: Nil) |> List.toIterator(r);
        let iterB0 = (1 :: Nil) |> List.toIterator(r);
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator(r);
        let iterB  = (iterB0 :: iterB1 :: Nil) |> List.toIterator(r);
        let iter = Iterator.intercalate(iterA, iterB);
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: Nil
    }

    @test
    def isEmpty13(): Bool = region r { // unfoldWithOk
        let Iterator(d, n) = List.toIterator(r, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        let iter = Iterator.unfoldWithOk(r, () -> if (d()) Err("Done") else Ok(n()));
        invokeIsEmptyRegion(100, iter);
        Iterator.toList(iter) == 1 :: 2 :: 3 :: 4 :: 5 :: Nil
    }

    ///
    /// Helper function that invokes `Iterator.isEmpty` `n` times.
    ///
    def invokeIsEmptyRegion(n: Int32, iter: Iterator[a, r]): Unit \ Read(r) =
        def loop(m) =
            if (m <= 0)
                ()
            else {
                discard Iterator.isEmpty(iter);
                loop(m - 1)
            };
        loop(n)


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = region r {
        Iterator.range(r, 0, 0) |> Iterator.toList == Nil
    }

    @test
    def range02(): Bool = region r {
        Iterator.range(r, 10, 0) |> Iterator.toList == Nil
    }

    @test
    def range03(): Bool = region r {
        Iterator.range(r, 1, 4) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def range04(): Bool = region r {
        Iterator.range(r, -1, 2) |> Iterator.toList == -1 :: 0 :: 1 :: Nil
    }

    @test
    def range05(): Bool = region r {
        Iterator.range(r, -100, 200) |> Iterator.toList == List.range(-100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool = region r {
        Iterator.repeat(r, 0, 10) |> Iterator.toList == Nil
    }

    @test
    def repeat02(): Bool = region r {
        Iterator.repeat(r, -100, 2) |> Iterator.toList == Nil
    }

    @test
    def repeat03(): Bool = region r {
        Iterator.repeat(r, 100, 2) |> Iterator.toList == List.repeat(100, 2)
    }

    @test
    def repeat04(): Bool = region r {
        Iterator.repeat(r, 3, 1) |> Iterator.toList == 1 :: 1 :: 1 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region r {
        List.toIterator(r, Nil) |> Iterator.sum == 0
    }

    @test
    def sum02(): Bool = region r {
        List.toIterator(r, 1 :: Nil) |> Iterator.sum == 1
    }

    @test
    def sum03(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: Nil) |> Iterator.sum == 6
    }

    @test
    def sum04(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sum == 3
    }

    @test
    def sum05(): Bool = region r {
        List.toIterator(r, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sum == -10
    }

    @test
    def sum06(): Bool = region r {
        List.toIterator(r, 10 :: -10 :: Nil) |> Iterator.sum == 0
    }

    @test
    def sum07(): Bool = region r {
        Iterator.range(r, 1, 101) |> Iterator.sum == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region r {
        List.toIterator(r, Nil) |> Iterator.sumWith(x -> x + 1) == 0
    }

    @test
    def sumWith02(): Bool = region r {
        List.toIterator(r, 1 :: Nil) |> Iterator.sumWith(x -> x + 1) == 2
    }

    @test
    def sumWith03(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: Nil) |> Iterator.sumWith(x -> x + 1) == 9
    }

    @test
    def sumWith04(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sumWith(x -> x + 1) == 7
    }

    @test
    def sumWith05(): Bool = region r {
        List.toIterator(r, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sumWith(x -> x + 1) == -6
    }

    @test
    def sumWith06(): Bool = region r {
        List.toIterator(r, 10 :: -10 :: Nil) |> Iterator.sumWith(x -> x + 1) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool = region r {
        new Iterator(r) |> Iterator.product == 0
    }

    @test
    def product02(): Bool = region r {
        List.toIterator(r, 1 :: Nil) |> Iterator.product == 1
    }

    @test
    def product03(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: Nil) |> Iterator.product == 6
    }

    @test
    def product04(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.product == -18
    }

    @test
    def product05(): Bool = region r {
        List.toIterator(r, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.product == 24
    }

    @test
    def product06(): Bool = region r {
        List.toIterator(r, 10 :: -10 :: Nil) |> Iterator.product == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = region r {
        new Iterator(r) |> Iterator.productWith(x -> x + 1) == 0
    }

    @test
    def productWith02(): Bool = region r {
        List.toIterator(r, 1 :: Nil) |> Iterator.productWith(x -> x + 1) == 2
    }

    @test
    def productWith03(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: Nil) |> Iterator.productWith(x -> x + 1) == 24
    }

    @test
    def productWith04(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.productWith(x -> x + 1) == -48
    }

    @test
    def productWith05(): Bool = region r {
        List.toIterator(r, -2 :: -3 :: -4 :: -5 :: Nil) |> Iterator.productWith(x -> x + 1) == 24
    }

    @test
    def productWith06(): Bool = region r {
        List.toIterator(r, 10 :: -10 :: Nil) |> Iterator.productWith(x -> x + 1) == -99
    }


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region r {
        Nil: List[Int32] |> List.toIterator(r) |> Iterator.toArray(r) `Array.sameElements` [] @ r
    }

    @test
    def toArray02(): Bool = region r {
        Iterator.range(r, -10, -5) |> Iterator.toArray(r) `Array.sameElements` [-10, -9, -8, -7, -6] @ r
    }

    @test
    def toArray03(): Bool = region r {
        Iterator.repeat(r, 3, 1) |> Iterator.toArray(r) `Array.sameElements` [1, 1, 1] @ r
    }

    @test
    def toArray04(): Bool = region r {
        Iterator.range(r, -100, 200) |> Iterator.toArray(r) `Array.sameElements` Array.range(-100, 200, r)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = region r {
        Map.empty() |> Map.toIterator(r) |> Iterator.toMap == Map.empty(): Map[Int32, Int32]
    }

    @test
    def toMap02(): Bool = region r {
        Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4} |> Map.toIterator(r) |> Iterator.toMap == Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4}
    }

    @test
    def toMap03(): Bool = region r {
        Map#{1 => 2, 2 => 3, 3 => 4} |> Map.toIterator(r) |> Iterator.toMap == Map#{1 => 2, 2 => 3, 3 => 4}
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region r {
        let iter = new Iterator(r): Iterator[Int32, r];
        let d = new MutDeque(r): MutDeque[Int32, _];
        Iterator.toMutDeque(r, iter) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque02(): Bool = region r {
        let iter = List.toIterator(r, 1 :: Nil);
        let d = new MutDeque(r);
        List.foreach(x -> MutDeque.pushBack(x, d), 1 :: Nil);
        Iterator.toMutDeque(r, iter) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque03(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: Nil);
        let d = new MutDeque(r);
        List.foreach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: Nil);
        Iterator.toMutDeque(r, iter) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque04(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: 3 :: Nil);
        let d = new MutDeque(r);
        List.foreach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        Iterator.toMutDeque(r, iter) `MutDeque.sameElements` d
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool = region r {
        Nil |> List.toIterator(r) |> Iterator.toNel == None: Option[Nel[Int32]]
    }

    @test
    def toNel02(): Bool = region r {
        (1 :: Nil) |> List.toIterator(r) |> Iterator.toNel == Some(Nel(1, Nil))
    }

    @test
    def toNel03(): Bool = region r {
        Iterator.range(r, 0, 1000) |> Iterator.toNel == Some(Nel(0, List.range(1, 1000)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool = region r {
        let iter = List.toIterator(r, Nil: List[Unit]);
        Iterator.toNec(iter) == None
    }

    @test
    def toNec02(): Bool = region r {
        let iter = List.toIterator(r, 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.singleton(1))
    }

    @test
    def toNec03(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(1, Nec.singleton(2)))
    }

    @test
    def toNec04(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(1, Nec.singleton(1)))
    }

    @test
    def toNec05(): Bool = region r {
        let iter = List.toIterator(r, 2 :: 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(2, Nec.singleton(1)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool = region r {
        Set.empty() |> Set.toIterator(r) |> Iterator.toSet == Set.empty(): Set[Int32]
    }

    @test
    def toSet02(): Bool = region r {
        Set#{"a", "b", "c", "d"} |> Set.toIterator(r) |> Iterator.toSet == Set#{"a", "b", "c", "d"}
    }

    @test
    def toSet03(): Bool = region r {
        Set.range(0, 10000) |> Set.toIterator(r) |> Iterator.toSet == Set.range(0, 10000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = region r {
        Nil |> List.toIterator(r) |> Iterator.filter(i -> i > 3) == Nil
    }

    @test
    def filter02(): Bool = region r {
        (2 :: Nil) |> List.toIterator(r) |> Iterator.filter(i -> i > 3) == Nil
    }

    @test
    def filter03(): Bool = region r {
        (4 :: Nil) |> List.toIterator(r) |> Iterator.filter(i -> i > 3) == 4 :: Nil
    }

    @test
    def filter04(): Bool = region r {
        (1 :: 3 :: Nil) |> List.toIterator(r) |> Iterator.filter(i -> i > 3) == Nil
    }

    @test
    def filter05(): Bool = region r {
        (1 :: 8 :: Nil) |> List.toIterator(r) |> Iterator.filter(i -> i > 3) == 8 :: Nil
    }

    @test
    def filter06(): Bool = region r {
        (8 :: 1 :: Nil) |> List.toIterator(r) |> Iterator.filter(i -> i > 3) == 8 :: Nil
    }

    @test
    def filter07(): Bool = region r {
        (8 :: 9 :: Nil) |> List.toIterator(r) |> Iterator.filter(i -> i > 3) == 8 :: 9 :: Nil
    }

    @test
    def filter08(): Bool = region r {
        (1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> List.toIterator(r) |> Iterator.filter(i -> i > 3) == 4 :: 11 :: 17 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = region r {
        Nil |> List.toIterator(r) |>
            Iterator.map(i -> i > 2) == Nil
    }

    @test
    def map02(): Bool = region r {
        (1 :: Nil) |> List.toIterator(r) |>
            Iterator.map(i -> i > 2) == false :: Nil
    }

    @test
    def map03(): Bool = region r {
        (3 :: Nil) |> List.toIterator(r) |>
            Iterator.map(i -> i > 2) == true :: Nil
    }

    @test
    def map04(): Bool = region r {
        (1 :: 2 :: Nil) |> List.toIterator(r) |>
            Iterator.map(i -> i > 2) == false :: false :: Nil
    }

    @test
    def map05(): Bool = region r {
        (1 :: 8 :: Nil) |> List.toIterator(r) |>
            Iterator.map(i -> i > 2) == false :: true :: Nil
    }

    @test
    def map06(): Bool = region r {
        (8 :: 1 :: Nil) |> List.toIterator(r) |>
            Iterator.map(i -> i > 2) == true :: false :: Nil
    }

    @test
    def map07(): Bool = region r {
        (7 :: 8 :: Nil) |> List.toIterator(r) |>
            Iterator.map(i -> i > 2) == true :: true :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool = region r {
        let res = ref 21 @ r;
        Nil |> List.toIterator(r) |>
            Iterator.foreach(x -> res := x);
        21 == deref res
    }

    @test
    def foreach02(): Bool = region r {
        let res = ref 21 @ r;
        (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |>
            Iterator.foreach(x -> res := x);
        3 == deref res
    }


    /////////////////////////////////////////////////////////////////////////////
    // lazyFilter                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lazyFilter01(): Bool = region r {
        List.toIterator(r, Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def lazyFilter02(): Bool = region r {
        List.toIterator(r, 2 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def lazyFilter03(): Bool = region r {
        List.toIterator(r, 4 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == 4 :: Nil
    }

    @test
    def lazyFilter04(): Bool = region r {
        List.toIterator(r, 1 :: 3 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def lazyFilter05(): Bool = region r {
        List.toIterator(r, 1 :: 8 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == 8 :: Nil
    }

    @test
    def lazyFilter06(): Bool = region r {
        List.toIterator(r, 8 :: 1 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == 8 :: Nil
    }

    @test
    def lazyFilter07(): Bool = region r {
        List.toIterator(r, 8 :: 9 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == 8 :: 9 :: Nil
    }

    @test
    def lazyFilter08(): Bool = region r {
        List.toIterator(r, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> Iterator.filterL(i -> i > 3) |> Iterator.toList == 4 :: 11 :: 17 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // lazyMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lazyMap01(): Bool = region r {
        List.toIterator(r, Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == Nil
    }

    @test
    def lazyMap02(): Bool = region r {
        List.toIterator(r, 1 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == false :: Nil
    }

    @test
    def lazyMap03(): Bool = region r {
        List.toIterator(r, 3 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == true :: Nil
    }

    @test
    def lazyMap04(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == false :: false :: Nil
    }

    @test
    def lazyMap05(): Bool = region r {
        List.toIterator(r, 1 :: 8 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == false :: true :: Nil
    }

    @test
    def lazyMap06(): Bool = region r {
        List.toIterator(r, 8 :: 1 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == true :: false :: Nil
    }

    @test
    def lazyMap07(): Bool = region r {
        List.toIterator(r, 7 :: 8 :: Nil) |> Iterator.mapL(i -> i > 2) |> Iterator.toList == true :: true :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool = region r {
        List.toIterator(r, Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == Nil
    }

    @test
    def replace02(): Bool = region r {
        List.toIterator(r, 1 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: Nil
    }

    @test
    def replace03(): Bool = region r {
        List.toIterator(r, 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: Nil
    }

    @test
    def replace04(): Bool = region r {
        List.toIterator(r, 4 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: Nil
    }

    @test
    def replace05(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def replace06(): Bool = region r {
        List.toIterator(r, 1 :: 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: 4 :: Nil
    }

    @test
    def replace07(): Bool = region r {
        List.toIterator(r, 3 :: 4 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: 4 :: Nil
    }

    @test
    def replace08(): Bool = region r {
        List.toIterator(r, 3 :: 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: 4 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = region r {
        let iter1 = List.toIterator(r, Nil: List[Unit]);
        let iter2 = List.toIterator(r, Nil: List[Unit]);
        Iterator.append(iter1, iter2) |> Iterator.toList == Nil
    }

    @test
    def append02(): Bool = region r {
        let iter1 = List.toIterator(r, Nil);
        let iter2 = List.toIterator(r, 1 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: Nil
    }

    @test
    def append03(): Bool = region r {
        let iter1 = List.toIterator(r, Nil);
        let iter2 = List.toIterator(r, 1 :: 2 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def append04(): Bool = region r {
        let iter1 = List.toIterator(r, 1 :: Nil);
        let iter2 = List.toIterator(r, Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: Nil
    }

    @test
    def append05(): Bool = region r {
        let iter1 = List.toIterator(r, 1 :: 2 :: Nil);
        let iter2 = List.toIterator(r, Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def append06(): Bool = region r {
        let iter1 = List.toIterator(r, 1 :: Nil);
        let iter2 = List.toIterator(r, 2 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def append07(): Bool = region r {
        let iter1 = List.toIterator(r, 1 :: 2 :: Nil);
        let iter2 = List.toIterator(r, 3 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def append08(): Bool = region r {
        let iter1 = List.toIterator(r, 1 :: Nil);
        let iter2 = List.toIterator(r, 2 :: 3 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def append09(): Bool = region r1 {
        let iter1 = List.toIterator(r1, 1 :: Nil);
        region r2 {
            let iter2 = List.toIterator(r2, 2 :: 3 :: Nil);
            Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = region r {
        let iterA = Nil: List[Int32] |> List.toIterator(r);
        let iterB = Iterator.repeat(r, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def zip02(): Bool = region r {
        let iterA = Nil: List[Int32] |> List.toIterator(r);
        let iterB = Iterator.repeat(r, 1000, "b");
        Iterator.zip(iterB, iterA) |> Iterator.toList == Nil
    }

    @test
    def zip03(): Bool = region r {
        let iterA = Iterator.repeat(r, 500,  "a");
        let iterB = Iterator.repeat(r, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(500, ("a", "b"))
    }

    @test
    def zip04(): Bool = region r {
        let iterA = Iterator.repeat(r, 1000, "a");
        let iterB = Iterator.repeat(r, 500,  "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(500, ("a", "b"))
    }

    @test
    def zip05(): Bool = region r {
        let iterA = Iterator.repeat(r, 1000, "a");
        let iterB = Iterator.repeat(r, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(1000, ("a", "b"))
    }

    @test
    def zip06(): Bool = region r {
        let iterA = Iterator.range(r, 0, 500);
        let iterB = Iterator.repeat(r, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.zip(List.range(0, 500), List.repeat(500, "b"))
    }

    @test
    def zip07(): Bool = region r1 {
        let iterA = Iterator.range(r1, 0, 500);
        region r2 {
            let iterB = Iterator.repeat(r2, 1000, "b");
            Iterator.zip(iterA, iterB) |> Iterator.toList == List.zip(List.range(0, 500), List.repeat(500, "b"))
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // lazyZipWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def lazyZipWith01(): Bool = region r {
        let iterA = List.toIterator(r, Nil);
        let iterB = List.toIterator(r, Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def lazyZipWith02(): Bool = region r {
        let iterA = List.toIterator(r, 1 :: Nil);
        let iterB = List.toIterator(r, Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def lazyZipWith03(): Bool = region r {
        let iterA = List.toIterator(r, Nil);
        let iterB = List.toIterator(r, true :: Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def lazyZipWith04(): Bool = region r {
        let iterA = List.toIterator(r, 1 :: Nil);
        let iterB = List.toIterator(r, true :: Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 2 :: Nil
    }

    @test
    def lazyZipWith05(): Bool = region r {
        let iterA = List.toIterator(r, 1 :: Nil);
        let iterB = List.toIterator(r, false :: Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 1 :: Nil
    }

    @test
    def lazyZipWith06(): Bool = region r {
        let iterA = List.toIterator(r, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        let iterB = List.toIterator(r, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
        Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |>
            Iterator.toList == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
    }

    @test
    def lazyZipWith07(): Bool = region r1 {
        let iterA = List.toIterator(r1, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        region r2 {
            let iterB = List.toIterator(r2, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
            Iterator.zipWithL((a, b) -> if (b) a + 1 else a, iterA, iterB) |>
                Iterator.toList == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = region r {
        let iterA = List.toIterator(r, Nil);
        let iterB = List.toIterator(r, Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == Nil
    }

    @test
    def zipWith02(): Bool = region r {
        let iterA = List.toIterator(r, 1 :: Nil);
        let iterB = List.toIterator(r, Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == Nil
    }

    @test
    def zipWith03(): Bool = region r {
        let iterA = List.toIterator(r, Nil);
        let iterB = List.toIterator(r, true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == Nil
    }

    @test
    def zipWith04(): Bool = region r {
        let iterA = List.toIterator(r, 1 :: Nil);
        let iterB = List.toIterator(r, true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 2 :: Nil
    }

    @test
    def zipWith05(): Bool = region r {
        let iterA = List.toIterator(r, 1 :: Nil);
        let iterB = List.toIterator(r, false :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 1 :: Nil
    }

    @test
    def zipWith06(): Bool = region r {
        let iterA = List.toIterator(r, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        let iterB = List.toIterator(r, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
    }

    @test
    def zipWith07(): Bool = region r1 {
        let iterA = List.toIterator(r1, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        region r2 {
            let iterB = List.toIterator(r2, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
            Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithIndex01(): Bool = region r {
        Iterator.zipWithIndex(new Iterator(r): Iterator[Int32, r]) |> Iterator.toList == Nil
    }

    @test
    def zipWithIndex02(): Bool = region r {
        Iterator.zipWithIndex(Iterator.singleton(r, 2)) |> Iterator.toList == (2, 0) :: Nil
    }

    @test
    def zipWithIndex03(): Bool = region r {
        Iterator.zipWithIndex(List.toIterator(r, 1 :: 8 :: Nil)) |> Iterator.toList == (1, 0) :: (8, 1) :: Nil
    }

    @test
    def zipWithIndex04(): Bool = region r {
        Iterator.zipWithIndex(List.toIterator(r, 8 :: 1 :: Nil)) |> Iterator.toList == (8, 0) :: (1, 1) :: Nil
    }

    @test
    def zipWithIndex05(): Bool = region r {
        Iterator.zipWithIndex(List.toIterator(r, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) |> Iterator.toList == (1, 0) :: (4, 1) :: (11, 2) :: (2, 3) :: (-22, 4) :: (17, 5) :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region r {
        let iter = List.toIterator(r, Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 100
    }

    @test
    def foldLeft02(): Bool = region r {
        let iter = List.toIterator(r, 1 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 198
    }

    @test
    def foldLeft03(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 196
    }

    @test
    def foldLeft04(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: 3 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 386
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region r {
        let iter = List.toIterator(r, Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 100
    }

    @test
    def foldRight02(): Bool = region r {
        let iter = List.toIterator(r, 1 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 198
    }

    @test
    def foldRight03(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 194
    }

    @test
    def foldRight04(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: 3 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = region r {
        let iter = List.toIterator(r, Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 100
    }

    @test
    def foldRightWithCont02(): Bool = region r {
        let iter = List.toIterator(r, 1 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 198
    }

    @test
    def foldRightWithCont03(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 194
    }

    @test
    def foldRightWithCont04(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: 3 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = region r {
        Iterator.foldMap(x -> 2 * x, List.toIterator(r, Nil)) == 0
    }

    @test
    def foldMap02(): Bool = region r {
        Iterator.foldMap(x -> 2 * x, List.toIterator(r, 1 :: 2 :: Nil)) == 6
    }

    @test
    def foldMap03(): Bool = region r {
        Iterator.foldMap(x -> if (x == "a") "b" else x, List.toIterator(r, "a" :: Nil)) == "b"
    }

    @test
    def foldMap04(): Bool = region r {
        Iterator.foldMap(x -> if (x == "c") "b" else x, List.toIterator(r, "a" :: "b" :: "c" :: Nil)) == "abb"
    }

    @test
    def foldMap05(): Bool = region r {
        Iterator.foldMap(Int32.toString, List.toIterator(r, 1 :: 2 :: 3 :: Nil)) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = region r {
        let iter = List.toIterator(r, Nil: List[Int32]);
        Iterator.reduceLeft((a, b) -> a - b, iter) == None
    }

    @test
    def reduceLeft02(): Bool = region r {
        let iter = List.toIterator(r, 1 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(1)
    }

    @test
    def reduceLeft03(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-1)
    }

    @test
    def reduceLeft04(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: 3 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-4)
    }

    @test
    def reduceLeft05(): Bool = region r {
        let iter = List.toIterator(r, 1 :: 2 :: 3 :: 4 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-8)
    }


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool = region r {
        List.toIterator(r, Nil: List[Int32]) |>
            Iterator.takeWhile(i -> i > 3) == Nil
    }

    @test
    def takeWhile02(): Bool = region r {
        List.toIterator(r, 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == Nil
    }

    @test
    def takeWhile03(): Bool = region r {
        List.toIterator(r, 4 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == 4 :: Nil
    }

    @test
    def takeWhile04(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == Nil
    }

    @test
    def takeWhile05(): Bool = region r {
        List.toIterator(r, 1 :: 5 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == Nil
    }

    @test
    def takeWhile06(): Bool = region r {
        List.toIterator(r, 5 :: 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == 5 :: Nil
    }

    @test
    def takeWhile07(): Bool = region r {
        List.toIterator(r, 5 :: 8 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == 5 :: 8 :: Nil
    }

    @test
    def takeWhile08(): Bool = region r {
        List.toIterator(r, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) == 4 :: 6 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool = region r {
        List.toIterator(r, Nil) |>
            Iterator.dropWhile(i -> i > 3) == Nil
    }

    @test
    def dropWhile02(): Bool = region r {
        List.toIterator(r, 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == 1 :: Nil
    }

    @test
    def dropWhile03(): Bool = region r {
        List.toIterator(r, 4 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == Nil
    }

    @test
    def dropWhile04(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == 1 :: 2 :: Nil
    }

    @test
    def dropWhile05(): Bool = region r {
        List.toIterator(r, 1 :: 5 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == 1 :: 5 :: Nil
    }

    @test
    def dropWhile06(): Bool = region r {
        List.toIterator(r, 5 :: 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == 1 :: Nil
    }

    @test
    def dropWhile07(): Bool = region r {
        List.toIterator(r, 5 :: 8 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == Nil
    }

    @test
    def dropWhile08(): Bool = region r {
        List.toIterator(r, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) == -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool = region r {
        List.toIterator(r, Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == Nil
    }

    @test
    def flatMap02(): Bool = region r {
        List.toIterator(r, 0 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == Nil
    }

    @test
    def flatMap03(): Bool = region r {
        List.toIterator(r, 1 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == 1 :: Nil
    }

    @test
    def flatMap04(): Bool = region r {
        List.toIterator(r, 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == 2 :: 2 :: Nil
    }

    @test
    def flatMap05(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == 1 :: 2 :: 2 :: Nil
    }

    @test
    def flatMap06(): Bool = region r {
        List.toIterator(r, 2 :: 3 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == 2 :: 2 :: 3 :: 3 :: 3 :: Nil
    }

    @test
    def flatMap07(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.flatMap(i -> if (i mod 2 == 0) Iterator.repeat(r, i, i) else new Iterator(r)) |>
            Iterator.toList == 2 :: 2 :: 4 :: 4 :: 4 :: 4 :: Nil
    }

    @test
    def flatMap08(): Bool = region r1 {
        region r2 {
            List.toIterator(r1, 1 :: 2 :: 3 :: 4 :: Nil) |>
                Iterator.flatMap(i -> if (i mod 2 == 0) Iterator.repeat(r2, i, i) else new Iterator(r2)) |>
                Iterator.toList == 2 :: 2 :: 4 :: 4 :: 4 :: 4 :: Nil
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = region r {
        List.toIterator(r, Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == Nil
    }

    @test
    def intersperse02(): Bool = region r {
        List.toIterator(r, 1 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: Nil
    }

    @test
    def intersperse03(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: Nil
    }

    @test
    def intersperse04(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: 11 :: 3 :: Nil
    }

    @test
    def intersperse05(): Bool = region r {
        List.toIterator(r, 1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool = region r {
        let iterA = Nil |> List.toIterator(r);
        let iterB = Nil |> List.toIterator(r);
        Iterator.intercalate(iterA, iterB) |> Iterator.toList == Nil: List[Int32]
    }

    @test
    def intercalate02(): Bool = region r {
        let iterA  = Nil |> List.toIterator(r);
        let iterB0 = (1 :: Nil) |> List.toIterator(r);
        let iterB  = (iterB0 :: Nil) |> List.toIterator(r);
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: Nil
    }

    @test
    def intercalate03(): Bool = region r {
        let iterA = (11 :: 12 :: 13 :: Nil) |> List.toIterator(r);
        let iterB = Nil |> List.toIterator(r);
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList ==  Nil
    }

    @test
    def intercalate04(): Bool = region r {
        let iterA  = Nil |> List.toIterator(r);
        let iterB0 = (1 :: Nil) |> List.toIterator(r);
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator(r);
        let iterB  = (iterB0 :: iterB1 :: Nil) |> List.toIterator(r);
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def intercalate05(): Bool = region r {
        let iterA  = (11 :: 12 :: 13 :: Nil) |> List.toIterator(r);
        let iterB0 = (1 :: Nil) |> List.toIterator(r);
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator(r);
        let iterB  = (iterB0 :: iterB1 :: Nil) |> List.toIterator(r);
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: Nil
    }

    @test
    def intercalate06(): Bool = region r {
        let iterA  = Nil |> List.toIterator(r);
        let iterB0 = (1 :: Nil) |> List.toIterator(r);
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator(r);
        let iterB2 = (4 :: Nil) |> List.toIterator(r);
        let iterB  = (iterB0 :: iterB1 :: iterB2 :: Nil) |> List.toIterator(r);
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: 2 :: 3 :: 4 :: Nil
    }

    @test
    def intercalate07(): Bool = region r1 {
        let iterA  = (11 :: 12 :: 13 :: Nil) |> List.toIterator(r1);
        region r2 {
            let iterB0 = (1 :: Nil) |> List.toIterator(r2);
            let iterB1 = (2 :: 3 :: Nil) |> List.toIterator(r2);
            let iterB2 = (4 :: Nil) |> List.toIterator(r2);
            region r3 {
                let iterB = (iterB0 :: iterB1 :: iterB2 :: Nil) |> List.toIterator(r3);
                Iterator.intercalate(iterA, iterB) |>
                    Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: 11 :: 12 :: 13 :: 4 :: Nil
            }
        }
    }

    @test
    def intercalate08(): Bool = region r1 {
        let iterB0 = (1 :: Nil) |> List.toIterator(r1);
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator(r1);
        let iterB2 = (4 :: Nil) |> List.toIterator(r1);
        region r2 {
            let iterB = (iterB0 :: iterB1 :: iterB2 :: Nil) |> List.toIterator(r2);
            region r3 {
                let iterA = (11 :: 12 :: 13 :: Nil) |> List.toIterator(r3);
                Iterator.intercalate(iterA, iterB) |>
                    Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: 11 :: 12 :: 13 :: 4 :: Nil
            }
        }
    }

    @test
    def intercalate09(): Bool = region r1 {
        let iterB0 = (1 :: Nil) |> List.toIterator(r1);
        let iterB1 = (2 :: 3 :: Nil) |> List.toIterator(r1);
        let iterB2 = (4 :: Nil) |> List.toIterator(r1);
        region r2 {
            let iterB = (iterB0 :: iterB1 :: iterB2 :: Nil) |> List.toIterator(r2);
            region _ {
                let iterA = (11 :: 12 :: 13 :: Nil) |> List.toIterator(r1);
                Iterator.intercalate(iterA, iterB) |>
                    Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: 11 :: 12 :: 13 :: 4 :: Nil
            }
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // unfoldWithOk                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unfoldWithOk01(): Bool = region r {
        let iter = Iterator.unfoldWithOk(r, () -> Err(0));
        Iterator.toList(iter) == Nil: List[Int32]
    }

    @test
    def unfoldWithOk02(): Bool = region r {
        let Iterator(d, n) = List.toIterator(r, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        Iterator.unfoldWithOk(r, () -> if (d()) Err("Done") else Ok(n())) |>
            Iterator.toList == 1 :: 2 :: 3 :: 4 :: 5 :: Nil
        }

    @test
    def unfoldWithOk03(): Bool = region r {
        let Iterator(d, n) = (1 :: 2 :: 3 :: 4 :: 5 :: Nil) |>
            List.map(x -> if (x == 4) Err("Done") else Ok(x)) |>
            List.toIterator(r);
        Iterator.unfoldWithOk(r, () -> { d(); n() }) |>
            Iterator.toList == 1 :: 2 :: 3 :: Nil
        }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region r {
        (Nil: List[Int32]) |> List.toIterator(r) |>
            Iterator.join(",") == ""
    }

    @test
    def join02(): Bool = region r {
        (1 :: Nil) |> List.toIterator(r) |>
            Iterator.join(",") == "1"
    }

    @test
    def join03(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |>
            Iterator.join(",") == "1,2,3"
    }

    @test
    def join04(): Bool = region r {
        ("1" :: "2" :: "3" :: Nil) |> List.toIterator(r) |>
            Iterator.join(",") == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region r {
        (Nil: List[Int32]) |> List.toIterator(r) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == ""
    }

    @test
    def joinWith02(): Bool = region r {
        (1 :: Nil) |> List.toIterator(r) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == "2"
    }

    @test
    def joinWith03(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == "2,3,4"
    }

    @test
    def joinWith04(): Bool = region r {
        ("1" :: "2" :: "3" :: Nil) |> List.toIterator(r) |>
            Iterator.joinWith(x -> x + x, ",") == "11,22,33"
    }


    /////////////////////////////////////////////////////////////////////////////
    // filterMapL                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMapL01(): Bool = region r {
        (Nil: List[Int32]) |> List.toIterator(r) |>
            Iterator.filterMapL(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == Nil
    }
    @test
    def filterMapL02(): Bool = region r {
        (1 :: Nil) |> List.toIterator(r) |>
            Iterator.filterMapL(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == Nil
    }

    @test
    def filterMapL03(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |>
            Iterator.filterMapL(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == 2 :: Nil
    }

    @test
    def filterMapL04(): Bool = region r {
        (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> List.toIterator(r) |>
            Iterator.filterMapL(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == 2 :: 4 :: 6 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // dropL                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropL01(): Bool = region r {
        (Nil: List[Int32]) |> List.toIterator(r) |>
            Iterator.dropL(1) |> Iterator.toList == Nil
    }
    @test
    def dropL02(): Bool = region r {
        (1 :: Nil) |> List.toIterator(r) |>
            Iterator.dropL(1) |> Iterator.toList == Nil
    }

    @test
    def dropL03(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |>
            Iterator.dropL(2) |> Iterator.toList == 3 :: Nil
    }

    @test
    def dropL04(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |>
            Iterator.dropL(3) |> Iterator.toList == Nil
    }

    @test
    def dropL05(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |>
            Iterator.dropL(4) |> Iterator.toList == Nil
    }

    @test
    def dropL06(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |>
            Iterator.dropL(0) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // dropWhileL                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhileL01(): Bool = region r {
        (Nil: List[Int32]) |> List.toIterator(r) |>
            Iterator.dropWhileL(x -> x < 3) |> Iterator.toList == Nil
    }

    @test
    def dropWhileL02(): Bool = region r {
        (1 :: Nil) |> List.toIterator(r) |>
            Iterator.dropWhileL(x -> x < 3) |> Iterator.toList == Nil
    }

    @test
    def dropWhileL03(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.toIterator(r) |>
            Iterator.dropWhileL(x -> x < 3) |>  Iterator.toList == 3 :: Nil
    }

    @test
    def dropWhileL04(): Bool = region r {
        (1 :: 2 :: 3 :: 4 :: Nil) |> List.toIterator(r) |>
            Iterator.dropWhileL(x -> x < 3) |>  Iterator.toList == 3 :: 4 :: Nil
    }

    @test
    def dropWhileL05(): Bool = region r {
        (1 :: 2 :: 3 :: 4 :: 0 :: Nil) |> List.toIterator(r) |>
            Iterator.dropWhileL(x -> x < 3) |>  Iterator.toList == 3 :: 4 :: 0 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // toResultList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toResultList01(): Bool = region r {
        (Nil: List[Result[Int32, String]]) |> List.toIterator(r) |>
            Iterator.toResultList == Ok(Nil)
    }
    @test
    def toResultList02(): Bool = region r {
        ((Ok(1) :: Nil): List[Result[Int32, String]]) |> List.toIterator(r) |>
            Iterator.toResultList == Ok(1 :: Nil)
    }

    @test
    def toResultList03(): Bool = region r {
        ((Ok(1) :: Ok(2) :: Ok(3) :: Nil): List[Result[Int32, String]]) |> List.toIterator(r) |>
            Iterator.toResultList == Ok(1 :: 2 :: 3 :: Nil)
    }

    @test
    def toResultList04(): Bool = region r {
        (Ok(1) :: Ok(2) :: Ok(3) :: Err("four") :: Nil) |> List.toIterator(r) |>
            Iterator.toResultList == Err("four")
    }

    @test
    def toResultList05(): Bool = region r {
        (Ok(1) :: Ok(2) :: Ok(3) :: Err("four") :: Err("zero") :: Nil) |> List.toIterator(r) |>
            Iterator.toResultList == Err("four")
    }

}
