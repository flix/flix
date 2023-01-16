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
    use Iterator.Iterator

    /////////////////////////////////////////////////////////////////////////////
    // next                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def next01(): Bool = region r {
        Iterator.empty(r) |> Iterator.next == None: Option[Int32]
    }

    @test
    def next02(): Bool = region r {
        let iter = List.iterator(r, 1 :: Nil);
        Iterator.next(iter) == Some(1)
    }

    @test
    def next03(): Bool = region r {
        let iter = List.iterator(r, 1 :: Nil);
        discard Iterator.next(iter);
        Iterator.next(iter) == None
    }

    @test
    def next04(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: Nil);
        discard Iterator.next(iter);
        Iterator.next(iter) == Some(2)
    }


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
        List.iterator(r, Nil) |> Iterator.sum == 0
    }

    @test
    def sum02(): Bool = region r {
        List.iterator(r, 1 :: Nil) |> Iterator.sum == 1
    }

    @test
    def sum03(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: Nil) |> Iterator.sum == 6
    }

    @test
    def sum04(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sum == 3
    }

    @test
    def sum05(): Bool = region r {
        List.iterator(r, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sum == -10
    }

    @test
    def sum06(): Bool = region r {
        List.iterator(r, 10 :: -10 :: Nil) |> Iterator.sum == 0
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
        List.iterator(r, Nil) |> Iterator.sumWith(x -> x + 1) == 0
    }

    @test
    def sumWith02(): Bool = region r {
        List.iterator(r, 1 :: Nil) |> Iterator.sumWith(x -> x + 1) == 2
    }

    @test
    def sumWith03(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: Nil) |> Iterator.sumWith(x -> x + 1) == 9
    }

    @test
    def sumWith04(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sumWith(x -> x + 1) == 7
    }

    @test
    def sumWith05(): Bool = region r {
        List.iterator(r, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sumWith(x -> x + 1) == -6
    }

    @test
    def sumWith06(): Bool = region r {
        List.iterator(r, 10 :: -10 :: Nil) |> Iterator.sumWith(x -> x + 1) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool = region r {
        Iterator.empty(r) |> Iterator.product == 1
    }

    @test
    def product02(): Bool = region r {
        List.iterator(r, 1 :: Nil) |> Iterator.product == 1
    }

    @test
    def product03(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: Nil) |> Iterator.product == 6
    }

    @test
    def product04(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.product == -18
    }

    @test
    def product05(): Bool = region r {
        List.iterator(r, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.product == 24
    }

    @test
    def product06(): Bool = region r {
        List.iterator(r, 10 :: -10 :: Nil) |> Iterator.product == -100
    }


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool = region r {
        Iterator.empty(r) |> Iterator.productWith(x -> x + 1) == 1
    }

    @test
    def productWith02(): Bool = region r {
        List.iterator(r, 1 :: Nil) |> Iterator.productWith(x -> x + 1) == 2
    }

    @test
    def productWith03(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: Nil) |> Iterator.productWith(x -> x + 1) == 24
    }

    @test
    def productWith04(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.productWith(x -> x + 1) == -48
    }

    @test
    def productWith05(): Bool = region r {
        List.iterator(r, -2 :: -3 :: -4 :: -5 :: Nil) |> Iterator.productWith(x -> x + 1) == 24
    }

    @test
    def productWith06(): Bool = region r {
        List.iterator(r, 10 :: -10 :: Nil) |> Iterator.productWith(x -> x + 1) == -99
    }


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = region r {
        Nil: List[Int32] |> List.iterator(r) |> Iterator.toList == Nil
    }

    @test
    def toList02(): Bool = region r {
        Iterator.range(r, -10, -5) |> Iterator.toList == -10 :: -9 :: -8 :: -7 :: -6 :: Nil
    }

    @test
    def toList03(): Bool = region r {
        Iterator.repeat(r, 3, 1) |> Iterator.toList == 1 :: 1 ::  1 :: Nil
    }

    @test
    def toList04(): Bool = region r {
        Iterator.range(r, -100, 200) |> Iterator.toList == List.range(-100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool = region r {
        Nil: List[Int32] |> List.iterator(r) |> Iterator.toChain == Chain.empty()
    }

    @test
    def toChain02(): Bool = region r {
        Iterator.range(r, -10, -5) |> Iterator.toChain == List.toChain(-10 :: -9 :: -8 :: -7 :: -6 :: Nil)
    }

    @test
    def toChain03(): Bool = region r {
        Iterator.repeat(r, 3, 1) |> Iterator.toChain == List.toChain(1 :: 1 ::  1 :: Nil)
    }

    @test
    def toChain04(): Bool = region r {
        Iterator.range(r, -100, 200) |> Iterator.toChain == Chain.range(-100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region r {
        Nil: List[Int32] |> List.iterator(r) |> Iterator.toArray(r) `Array.sameElements` [] @ r
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
        Iterator.range(r, -100, 200) |> Iterator.toArray(r) `Array.sameElements` Array.range(r, -100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = region r {
        Map.empty() |> Map.iterator(r) |> Iterator.toMap == Map.empty(): Map[Int32, Int32]
    }

    @test
    def toMap02(): Bool = region r {
        Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4} |> Map.iterator(r) |> Iterator.toMap == Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4}
    }

    @test
    def toMap03(): Bool = region r {
        Map#{1 => 2, 2 => 3, 3 => 4} |> Map.iterator(r) |> Iterator.toMap == Map#{1 => 2, 2 => 3, 3 => 4}
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region r {
        let iter = Iterator.empty(r): Iterator[Int32, r, r];
        let d = new MutDeque(r): MutDeque[Int32, _];
        Iterator.toMutDeque(r, iter) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque02(): Bool = region r {
        let iter = List.iterator(r, 1 :: Nil);
        let d = new MutDeque(r);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: Nil);
        Iterator.toMutDeque(r, iter) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque03(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: Nil);
        let d = new MutDeque(r);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: Nil);
        Iterator.toMutDeque(r, iter) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque04(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: 3 :: Nil);
        let d = new MutDeque(r);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        Iterator.toMutDeque(r, iter) `MutDeque.sameElements` d
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool = region r {
        Nil |> List.iterator(r) |> Iterator.toNel == None: Option[Nel[Int32]]
    }

    @test
    def toNel02(): Bool = region r {
        (1 :: Nil) |> List.iterator(r) |> Iterator.toNel == Some(Nel.Nel(1, Nil))
    }

    @test
    def toNel03(): Bool = region r {
        Iterator.range(r, 0, 1000) |> Iterator.toNel == Some(Nel.Nel(0, List.range(1, 1000)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool = region r {
        let iter = List.iterator(r, Nil: List[Unit]);
        Iterator.toNec(iter) == None
    }

    @test
    def toNec02(): Bool = region r {
        let iter = List.iterator(r, 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.singleton(1))
    }

    @test
    def toNec03(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(1, Nec.singleton(2)))
    }

    @test
    def toNec04(): Bool = region r {
        let iter = List.iterator(r, 1 :: 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(1, Nec.singleton(1)))
    }

    @test
    def toNec05(): Bool = region r {
        let iter = List.iterator(r, 2 :: 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(2, Nec.singleton(1)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool = region r {
        Set.empty() |> Set.iterator(r) |> Iterator.toSet == Set.empty(): Set[Int32]
    }

    @test
    def toSet02(): Bool = region r {
        Set#{"a", "b", "c", "d"} |> Set.iterator(r) |> Iterator.toSet == Set#{"a", "b", "c", "d"}
    }

    @test
    def toSet03(): Bool = region r {
        Set.range(0, 10000) |> Set.iterator(r) |> Iterator.toSet == Set.range(0, 10000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = region r {
        Nil |> List.iterator(r) |> Iterator.filter(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def filter02(): Bool = region r {
        (2 :: Nil) |> List.iterator(r) |> Iterator.filter(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def filter03(): Bool = region r {
        (4 :: Nil) |> List.iterator(r) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 4 :: Nil
    }

    @test
    def filter04(): Bool = region r {
        (1 :: 3 :: Nil) |> List.iterator(r) |> Iterator.filter(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def filter05(): Bool = region r {
        (1 :: 8 :: Nil) |> List.iterator(r) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 8 :: Nil
    }

    @test
    def filter06(): Bool = region r {
        (8 :: 1 :: Nil) |> List.iterator(r) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 8 :: Nil
    }

    @test
    def filter07(): Bool = region r {
        (8 :: 9 :: Nil) |> List.iterator(r) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 8 :: 9 :: Nil
    }

    @test
    def filter08(): Bool = region r {
        (1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> List.iterator(r) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 4 :: 11 :: 17 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = region r {
        Nil |> List.iterator(r) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == Nil
    }

    @test
    def map02(): Bool = region r {
        (1 :: Nil) |> List.iterator(r) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == false :: Nil
    }

    @test
    def map03(): Bool = region r {
        (3 :: Nil) |> List.iterator(r) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == true :: Nil
    }

    @test
    def map04(): Bool = region r {
        (1 :: 2 :: Nil) |> List.iterator(r) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == false :: false :: Nil
    }

    @test
    def map05(): Bool = region r {
        (1 :: 8 :: Nil) |> List.iterator(r) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == false :: true :: Nil
    }

    @test
    def map06(): Bool = region r {
        (8 :: 1 :: Nil) |> List.iterator(r) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == true :: false :: Nil
    }

    @test
    def map07(): Bool = region r {
        (7 :: 8 :: Nil) |> List.iterator(r) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == true :: true :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region r {
        let res = ref 21 @ r;
        Nil |> List.iterator(r) |>
            Iterator.forEach(x -> res := x);
        21 == deref res
    }

    @test
    def forEach02(): Bool = region r {
        let res = ref 21 @ r;
        (1 :: 2 :: 3 :: Nil) |> List.iterator(r) |>
            Iterator.forEach(x -> res := x);
        3 == deref res
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region r {
        let res = ref 21 @ r;
        Nil |> List.iterator(r) |>
            Iterator.forEachWithIndex((i, _) -> res := i);
        21 == deref res
    }

    @test
    def forEachWithIndex02(): Bool = region r {
        let res = ref 21 @ r;
        (1 :: 2 :: 3 :: Nil) |> List.iterator(r) |>
            Iterator.forEachWithIndex((i, _) -> res := i);
        2 == deref res
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool = region r {
        List.iterator(r, Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == Nil
    }

    @test
    def replace02(): Bool = region r {
        List.iterator(r, 1 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: Nil
    }

    @test
    def replace03(): Bool = region r {
        List.iterator(r, 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: Nil
    }

    @test
    def replace04(): Bool = region r {
        List.iterator(r, 4 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: Nil
    }

    @test
    def replace05(): Bool = region r {
        List.iterator(r, 1 :: 2 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def replace06(): Bool = region r {
        List.iterator(r, 1 :: 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: 4 :: Nil
    }

    @test
    def replace07(): Bool = region r {
        List.iterator(r, 3 :: 4 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: 4 :: Nil
    }

    @test
    def replace08(): Bool = region r {
        List.iterator(r, 3 :: 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: 4 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = region r {
        let iter1 = List.iterator(r, Nil: List[Unit]);
        let iter2 = List.iterator(r, Nil: List[Unit]);
        Iterator.append(iter1, iter2) |> Iterator.toList == Nil
    }

    @test
    def append02(): Bool = region r {
        let iter1 = List.iterator(r, Nil);
        let iter2 = List.iterator(r, 1 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: Nil
    }

    @test
    def append03(): Bool = region r {
        let iter1 = List.iterator(r, Nil);
        let iter2 = List.iterator(r, 1 :: 2 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def append04(): Bool = region r {
        let iter1 = List.iterator(r, 1 :: Nil);
        let iter2 = List.iterator(r, Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: Nil
    }

    @test
    def append05(): Bool = region r {
        let iter1 = List.iterator(r, 1 :: 2 :: Nil);
        let iter2 = List.iterator(r, Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def append06(): Bool = region r {
        let iter1 = List.iterator(r, 1 :: Nil);
        let iter2 = List.iterator(r, 2 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def append07(): Bool = region r {
        let iter1 = List.iterator(r, 1 :: 2 :: Nil);
        let iter2 = List.iterator(r, 3 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def append08(): Bool = region r {
        let iter1 = List.iterator(r, 1 :: Nil);
        let iter2 = List.iterator(r, 2 :: 3 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    // TODO - multiple region tests currently disabled
    // @test
    // def append09(): Bool = region r1 {
    //     let iter1 = List.iterator(r1, 1 :: Nil);
    //     region r2 {
    //         let iter2 = List.iterator(r2, 2 :: 3 :: Nil);
    //         Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    //     }
    // }


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = region r {
        let iterA = Nil: List[Int32] |> List.iterator(r);
        let iterB = Iterator.repeat(r, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def zip02(): Bool = region r {
        let iterA = Nil: List[Int32] |> List.iterator(r);
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

    // TODO - multiple region tests currently disabled
    // @test
    // def zip07(): Bool = region r1 {
    //     let iterA = Iterator.range(r1, 0, 500);
    //     region r2 {
    //         let iterB = Iterator.repeat(r2, 1000, "b");
    //         Iterator.zip(iterA, iterB) |> Iterator.toList == List.zip(List.range(0, 500), List.repeat(500, "b"))
    //     }
    // }


    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = region r {
        let iterA = List.iterator(r, Nil);
        let iterB = List.iterator(r, Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def zipWith02(): Bool = region r {
        let iterA = List.iterator(r, 1 :: Nil);
        let iterB = List.iterator(r, Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def zipWith03(): Bool = region r {
        let iterA = List.iterator(r, Nil);
        let iterB = List.iterator(r, true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def zipWith04(): Bool = region r {
        let iterA = List.iterator(r, 1 :: Nil);
        let iterB = List.iterator(r, true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 2 :: Nil
    }

    @test
    def zipWith05(): Bool = region r {
        let iterA = List.iterator(r, 1 :: Nil);
        let iterB = List.iterator(r, false :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 1 :: Nil
    }

    @test
    def zipWith06(): Bool = region r {
        let iterA = List.iterator(r, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        let iterB = List.iterator(r, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
    }

    @test
    def zipWith07(): Bool = region r1 {
        let iterA = List.iterator(r1, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        region r2 {
            let iterB = List.iterator(r2, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
            Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithIndex01(): Bool = region r {
        Iterator.zipWithIndex(Iterator.empty(r): Iterator[Int32, r, r]) |> Iterator.toList == Nil
    }

    @test
    def zipWithIndex02(): Bool = region r {
        Iterator.zipWithIndex(Iterator.singleton(r, 2)) |> Iterator.toList == (0, 2) :: Nil
    }

    @test
    def zipWithIndex03(): Bool = region r {
        Iterator.zipWithIndex(List.iterator(r, 1 :: 8 :: Nil)) |> Iterator.toList == (0, 1) :: (1, 8) :: Nil
    }

    @test
    def zipWithIndex04(): Bool = region r {
        Iterator.zipWithIndex(List.iterator(r, 8 :: 1 :: Nil)) |> Iterator.toList == (0, 8) :: (1, 1) :: Nil
    }

    @test
    def zipWithIndex05(): Bool = region r {
        Iterator.zipWithIndex(List.iterator(r, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) |> Iterator.toList == (0, 1) :: (1, 4) :: (2, 11) :: (3, 2) :: (4, -22) :: (5, 17) :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region r {
        let iter = List.iterator(r, Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 100
    }

    @test
    def foldLeft02(): Bool = region r {
        let iter = List.iterator(r, 1 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 198
    }

    @test
    def foldLeft03(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 196
    }

    @test
    def foldLeft04(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: 3 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, iter) == 386
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region r {
        let iter = List.iterator(r, Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 100
    }

    @test
    def foldRight02(): Bool = region r {
        let iter = List.iterator(r, 1 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 198
    }

    @test
    def foldRight03(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 194
    }

    @test
    def foldRight04(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: 3 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e rem 2 + 1), 100, iter) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = region r {
        let iter = List.iterator(r, Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 100
    }

    @test
    def foldRightWithCont02(): Bool = region r {
        let iter = List.iterator(r, 1 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 198
    }

    @test
    def foldRightWithCont03(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 194
    }

    @test
    def foldRightWithCont04(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: 3 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e rem 2 + 1), 100, iter) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = region r {
        Iterator.foldMap(x -> 2 * x, List.iterator(r, Nil)) == 0
    }

    @test
    def foldMap02(): Bool = region r {
        Iterator.foldMap(x -> 2 * x, List.iterator(r, 1 :: 2 :: Nil)) == 6
    }

    @test
    def foldMap03(): Bool = region r {
        Iterator.foldMap(x -> if (x == "a") "b" else x, List.iterator(r, "a" :: Nil)) == "b"
    }

    @test
    def foldMap04(): Bool = region r {
        Iterator.foldMap(x -> if (x == "c") "b" else x, List.iterator(r, "a" :: "b" :: "c" :: Nil)) == "abb"
    }

    @test
    def foldMap05(): Bool = region r {
        Iterator.foldMap(Int32.toString, List.iterator(r, 1 :: 2 :: 3 :: Nil)) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = region r {
        let iter = List.iterator(r, Nil: List[Int32]);
        Iterator.reduceLeft((a, b) -> a - b, iter) == None
    }

    @test
    def reduceLeft02(): Bool = region r {
        let iter = List.iterator(r, 1 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(1)
    }

    @test
    def reduceLeft03(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-1)
    }

    @test
    def reduceLeft04(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: 3 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-4)
    }

    @test
    def reduceLeft05(): Bool = region r {
        let iter = List.iterator(r, 1 :: 2 :: 3 :: 4 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-8)
    }


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool = region r {
        List.iterator(r, Nil: List[Int32]) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def takeWhile02(): Bool = region r {
        List.iterator(r, 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def takeWhile03(): Bool = region r {
        List.iterator(r, 4 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 4 :: Nil
    }

    @test
    def takeWhile04(): Bool = region r {
        List.iterator(r, 1 :: 2 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def takeWhile05(): Bool = region r {
        List.iterator(r, 1 :: 5 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def takeWhile06(): Bool = region r {
        List.iterator(r, 5 :: 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 5 :: Nil
    }

    @test
    def takeWhile07(): Bool = region r {
        List.iterator(r, 5 :: 8 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 5 :: 8 :: Nil
    }

    @test
    def takeWhile08(): Bool = region r {
        List.iterator(r, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 4 :: 6 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool = region r {
        List.iterator(r, Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def dropWhile02(): Bool = region r {
        List.iterator(r, 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: Nil
    }

    @test
    def dropWhile03(): Bool = region r {
        List.iterator(r, 4 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def dropWhile04(): Bool = region r {
        List.iterator(r, 1 :: 2 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def dropWhile05(): Bool = region r {
        List.iterator(r, 1 :: 5 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: 5 :: Nil
    }

    @test
    def dropWhile06(): Bool = region r {
        List.iterator(r, 5 :: 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: Nil
    }

    @test
    def dropWhile07(): Bool = region r {
        List.iterator(r, 5 :: 8 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def dropWhile08(): Bool = region r {
        List.iterator(r, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool = region r {
        List.iterator(r, Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == Nil
    }

    @test
    def flatMap02(): Bool = region r {
        List.iterator(r, 0 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == Nil
    }

    @test
    def flatMap03(): Bool = region r {
        List.iterator(r, 1 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == 1 :: Nil
    }

    @test
    def flatMap04(): Bool = region r {
        List.iterator(r, 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == 2 :: 2 :: Nil
    }

    @test
    def flatMap05(): Bool = region r {
        List.iterator(r, 1 :: 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == 1 :: 2 :: 2 :: Nil
    }

    @test
    def flatMap06(): Bool = region r {
        List.iterator(r, 2 :: 3 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(r, i, i)) |>
            Iterator.toList == 2 :: 2 :: 3 :: 3 :: 3 :: Nil
    }

    @test
    def flatMap07(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.flatMap(i -> if (i mod 2 == 0) Iterator.repeat(r, i, i) else Iterator.empty(r)) |>
            Iterator.toList == 2 :: 2 :: 4 :: 4 :: 4 :: 4 :: Nil
    }

    // TODO - multiple region tests currently disabled
    // @test
    // def flatMap08(): Bool = region r1 {
    //     region r2 {
    //         List.iterator(r1, 1 :: 2 :: 3 :: 4 :: Nil) |>
    //             Iterator.flatMap(i -> if (i mod 2 == 0) Iterator.repeat(r2, i, i) else Iterator.empty(r2)) |>
    //             Iterator.toList == 2 :: 2 :: 4 :: 4 :: 4 :: 4 :: Nil
    //     }
    // }


    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = region r {
        List.iterator(r, Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == Nil
    }

    @test
    def intersperse02(): Bool = region r {
        List.iterator(r, 1 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: Nil
    }

    @test
    def intersperse03(): Bool = region r {
        List.iterator(r, 1 :: 2 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: Nil
    }

    @test
    def intersperse04(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: 11 :: 3 :: Nil
    }

    @test
    def intersperse05(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool = region r {
        let iterA = Nil |> List.iterator(r);
        let iterB = Nil |> List.iterator(r);
        Iterator.intercalate(iterA, iterB) |> Iterator.toList == Nil: List[Int32]
    }

    @test
    def intercalate02(): Bool = region r {
        let iterA  = Nil |> List.iterator(r);
        let iterB0 = (1 :: Nil) |> List.iterator(r);
        let iterB  = (iterB0 :: Nil) |> List.iterator(r);
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: Nil
    }

    @test
    def intercalate03(): Bool = region r {
        let iterA = (11 :: 12 :: 13 :: Nil) |> List.iterator(r);
        let iterB = Nil |> List.iterator(r);
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList ==  Nil
    }

    @test
    def intercalate04(): Bool = region r {
        let iterA  = Nil |> List.iterator(r);
        let iterB0 = (1 :: Nil) |> List.iterator(r);
        let iterB1 = (2 :: 3 :: Nil) |> List.iterator(r);
        let iterB  = (iterB0 :: iterB1 :: Nil) |> List.iterator(r);
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def intercalate05(): Bool = region r {
        let iterA  = (11 :: 12 :: 13 :: Nil) |> List.iterator(r);
        let iterB0 = (1 :: Nil) |> List.iterator(r);
        let iterB1 = (2 :: 3 :: Nil) |> List.iterator(r);
        let iterB  = (iterB0 :: iterB1 :: Nil) |> List.iterator(r);
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: Nil
    }

    @test
    def intercalate06(): Bool = region r {
        let iterA  = Nil |> List.iterator(r);
        let iterB0 = (1 :: Nil) |> List.iterator(r);
        let iterB1 = (2 :: 3 :: Nil) |> List.iterator(r);
        let iterB2 = (4 :: Nil) |> List.iterator(r);
        let iterB  = (iterB0 :: iterB1 :: iterB2 :: Nil) |> List.iterator(r);
        Iterator.intercalate(iterA, iterB) |>
            Iterator.toList == 1 :: 2 :: 3 :: 4 :: Nil
    }

    // TODO - multiple region tests currently disabled
    // @test
    // def intercalate07(): Bool = region r1 {
    //     let iterA  = (11 :: 12 :: 13 :: Nil) |> List.iterator(r1);
    //     region r2 {
    //         let iterB0 = (1 :: Nil) |> List.iterator(r2);
    //         let iterB1 = (2 :: 3 :: Nil) |> List.iterator(r2);
    //         let iterB2 = (4 :: Nil) |> List.iterator(r2);
    //         region r3 {
    //             let iterB = (iterB0 :: iterB1 :: iterB2 :: Nil) |> List.iterator(r3);
    //             Iterator.intercalate(iterA, iterB) |>
    //                 Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: 11 :: 12 :: 13 :: 4 :: Nil
    //         }
    //     }
    // }

    // TODO - multiple region tests currently disabled
    // @test
    // def intercalate08(): Bool = region r1 {
    //     let iterB0 = (1 :: Nil) |> List.iterator(r1);
    //     let iterB1 = (2 :: 3 :: Nil) |> List.iterator(r1);
    //     let iterB2 = (4 :: Nil) |> List.iterator(r1);
    //     region r2 {
    //         let iterB = (iterB0 :: iterB1 :: iterB2 :: Nil) |> List.iterator(r2);
    //         region r3 {
    //             let iterA = (11 :: 12 :: 13 :: Nil) |> List.iterator(r3);
    //             Iterator.intercalate(iterA, iterB) |>
    //                 Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: 11 :: 12 :: 13 :: 4 :: Nil
    //         }
    //     }
    // }

    // TODO - multiple region tests currently disabled
    // @test
    // def intercalate09(): Bool = region r1 {
    //     let iterB0 = (1 :: Nil) |> List.iterator(r1);
    //     let iterB1 = (2 :: 3 :: Nil) |> List.iterator(r1);
    //     let iterB2 = (4 :: Nil) |> List.iterator(r1);
    //     region r2 {
    //         let iterB = (iterB0 :: iterB1 :: iterB2 :: Nil) |> List.iterator(r2);
    //         region _ {
    //             let iterA = (11 :: 12 :: 13 :: Nil) |> List.iterator(r1);
    //             Iterator.intercalate(iterA, iterB) |>
    //                 Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: 11 :: 12 :: 13 :: 4 :: Nil
    //         }
    //     }
    // }


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
        let iter = List.iterator(r, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        Iterator.unfoldWithOk(r, () -> {Iterator.next(iter) |> Option.toOk("Done")}) |>
            Iterator.toList == 1 :: 2 :: 3 :: 4 :: 5 :: Nil
        }

    @test
    def unfoldWithOk03(): Bool = region r {
        let iter = (1 :: 2 :: 3 :: 4 :: 5 :: Nil) |>
            List.map(x -> if (x == 4) Err("Done") else Ok(x)) |>
            List.iterator(r);
        Iterator.unfoldWithOk(r, () -> {Iterator.next(iter) |> Option.getWithDefault(Err("Done"))}) |>
            Iterator.toList == 1 :: 2 :: 3 :: Nil
        }


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool = region r {
        (Nil: List[Int32]) |> List.iterator(r) |>
            Iterator.drop(1) |> Iterator.toList == Nil
    }
    @test
    def drop02(): Bool = region r {
        (1 :: Nil) |> List.iterator(r) |>
            Iterator.drop(1) |> Iterator.toList == Nil
    }

    @test
    def drop03(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(r) |>
            Iterator.drop(2) |> Iterator.toList == 3 :: Nil
    }

    @test
    def drop04(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(r) |>
            Iterator.drop(3) |> Iterator.toList == Nil
    }

    @test
    def drop05(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(r) |>
            Iterator.drop(4) |> Iterator.toList == Nil
    }

    @test
    def drop06(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(r) |>
            Iterator.drop(0) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool = region r {
        List.iterator(r, Nil: List[Unit]) |> Iterator.take(-1) |> Iterator.toList == Nil
    }

    @test
    def take02(): Bool = region r {
        List.iterator(r, Nil: List[Unit]) |> Iterator.take(0) |> Iterator.toList == Nil
    }

    @test
    def take03(): Bool = region r {
        List.iterator(r, Nil: List[Unit]) |> Iterator.take(1) |> Iterator.toList == Nil
    }

    @test
    def take04(): Bool = region r {
        List.iterator(r, 1 :: Nil) |> Iterator.take(-1) |> Iterator.toList == Nil
    }

    @test
    def take05(): Bool = region r {
        List.iterator(r, 1 :: Nil) |> Iterator.take(0) |> Iterator.toList == Nil
    }

    @test
    def take06(): Bool = region r {
        List.iterator(r, 1 :: Nil) |> Iterator.take(1) |> Iterator.toList == 1 :: Nil
    }

    @test
    def take07(): Bool = region r {
        List.iterator(r, 1 :: Nil) |> Iterator.take(2) |> Iterator.toList == 1 :: Nil
    }

    @test
    def take08(): Bool = region r {
        List.iterator(r, 1 :: 2 :: Nil) |> Iterator.take(0) |> Iterator.toList == Nil
    }

    @test
    def take09(): Bool = region r {
        List.iterator(r, 1 :: 2 :: Nil) |> Iterator.take(1) |> Iterator.toList == 1 :: Nil
    }

    @test
    def take10(): Bool = region r {
        List.iterator(r, 1 :: 2 :: Nil) |> Iterator.take(2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def take11(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> Iterator.take(2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def take12(): Bool = region r {
        List.iterator(r, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> Iterator.take(4) |> Iterator.toList == 1 :: 2 :: 3 :: 4 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region r {
        (Nil: List[Int32]) |> List.iterator(r) |>
            Iterator.join(",") == ""
    }

    @test
    def join02(): Bool = region r {
        (1 :: Nil) |> List.iterator(r) |>
            Iterator.join(",") == "1"
    }

    @test
    def join03(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(r) |>
            Iterator.join(",") == "1,2,3"
    }

    @test
    def join04(): Bool = region r {
        ("1" :: "2" :: "3" :: Nil) |> List.iterator(r) |>
            Iterator.join(",") == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region r {
        (Nil: List[Int32]) |> List.iterator(r) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == ""
    }

    @test
    def joinWith02(): Bool = region r {
        (1 :: Nil) |> List.iterator(r) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == "2"
    }

    @test
    def joinWith03(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(r) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == "2,3,4"
    }

    @test
    def joinWith04(): Bool = region r {
        ("1" :: "2" :: "3" :: Nil) |> List.iterator(r) |>
            Iterator.joinWith(x -> x + x, ",") == "11,22,33"
    }


    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool = region r {
        (Nil: List[Int32]) |> List.iterator(r) |>
            Iterator.filterMap(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == Nil
    }
    @test
    def filterMap02(): Bool = region r {
        (1 :: Nil) |> List.iterator(r) |>
            Iterator.filterMap(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == Nil
    }

    @test
    def filterMap03(): Bool = region r {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(r) |>
            Iterator.filterMap(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == 2 :: Nil
    }

    @test
    def filterMap04(): Bool = region r {
        (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> List.iterator(r) |>
            Iterator.filterMap(x -> if (x mod 2 == 0) Some(x) else None) |> Iterator.toList == 2 :: 4 :: 6 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // toResultList                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toResultList01(): Bool = region r {
        (Nil: List[Result[String, Int32]]) |> List.iterator(r) |>
            Iterator.toResultList == Ok(Nil)
    }
    @test
    def toResultList02(): Bool = region r {
        ((Ok(1) :: Nil): List[Result[String, Int32]]) |> List.iterator(r) |>
            Iterator.toResultList == Ok(1 :: Nil)
    }

    @test
    def toResultList03(): Bool = region r {
        ((Ok(1) :: Ok(2) :: Ok(3) :: Nil): List[Result[String, Int32]]) |> List.iterator(r) |>
            Iterator.toResultList == Ok(1 :: 2 :: 3 :: Nil)
    }

    @test
    def toResultList04(): Bool = region r {
        (Ok(1) :: Ok(2) :: Ok(3) :: Err("four") :: Nil) |> List.iterator(r) |>
            Iterator.toResultList == Err("four")
    }

    @test
    def toResultList05(): Bool = region r {
        (Ok(1) :: Ok(2) :: Ok(3) :: Err("four") :: Err("zero") :: Nil) |> List.iterator(r) |>
            Iterator.toResultList == Err("four")
    }

}
