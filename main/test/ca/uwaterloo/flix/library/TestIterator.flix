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

mod TestIterator {
    use Iterator.Iterator

    /////////////////////////////////////////////////////////////////////////////
    // next                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def next01(): Bool = region rc {
        Iterator.empty(rc) |> Iterator.next == (None: Option[Int32])
    }

    @test
    def next02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.next(iter) == Some(1)
    }

    @test
    def next03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        discard Iterator.next(iter);
        Iterator.next(iter) == None
    }

    @test
    def next04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        discard Iterator.next(iter);
        Iterator.next(iter) == Some(2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def range01(): Bool = region rc {
        Iterator.range(rc, 0, 0) |> Iterator.toList == Nil
    }

    @test
    def range02(): Bool = region rc {
        Iterator.range(rc, 10, 0) |> Iterator.toList == Nil
    }

    @test
    def range03(): Bool = region rc {
        Iterator.range(rc, 1, 4) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def range04(): Bool = region rc {
        Iterator.range(rc, -1, 2) |> Iterator.toList == -1 :: 0 :: 1 :: Nil
    }

    @test
    def range05(): Bool = region rc {
        Iterator.range(rc, -100, 200) |> Iterator.toList == List.range(-100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def repeat01(): Bool = region rc {
        Iterator.repeat(rc, 0, 10) |> Iterator.toList == Nil
    }

    @test
    def repeat02(): Bool = region rc {
        Iterator.repeat(rc, -100, 2) |> Iterator.toList == Nil
    }

    @test
    def repeat03(): Bool = region rc {
        Iterator.repeat(rc, 100, 2) |> Iterator.toList == List.repeat(100, 2)
    }

    @test
    def repeat04(): Bool = region rc {
        Iterator.repeat(rc, 3, 1) |> Iterator.toList == 1 :: 1 :: 1 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region rc {
        List.iterator(rc, Nil) |> Iterator.sum == 0
    }

    @test
    def sum02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.sum == 1
    }

    @test
    def sum03(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: Nil) |> Iterator.sum == 6
    }

    @test
    def sum04(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sum == 3
    }

    @test
    def sum05(): Bool = region rc {
        List.iterator(rc, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sum == -10
    }

    @test
    def sum06(): Bool = region rc {
        List.iterator(rc, 10 :: -10 :: Nil) |> Iterator.sum == 0
    }

    @test
    def sum07(): Bool = region rc {
        Iterator.range(rc, 1, 101) |> Iterator.sum == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region rc {
        List.iterator(rc, Nil) |> Iterator.sumWith(x -> x + 1) == 0
    }

    @test
    def sumWith02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.sumWith(x -> x + 1) == 2
    }

    @test
    def sumWith03(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: Nil) |> Iterator.sumWith(x -> x + 1) == 9
    }

    @test
    def sumWith04(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sumWith(x -> x + 1) == 7
    }

    @test
    def sumWith05(): Bool = region rc {
        List.iterator(rc, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sumWith(x -> x + 1) == -6
    }

    @test
    def sumWith06(): Bool = region rc {
        List.iterator(rc, 10 :: -10 :: Nil) |> Iterator.sumWith(x -> x + 1) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |> Iterator.toList == Nil
    }

    @test
    def toList02(): Bool = region rc {
        Iterator.range(rc, -10, -5) |> Iterator.toList == -10 :: -9 :: -8 :: -7 :: -6 :: Nil
    }

    @test
    def toList03(): Bool = region rc {
        Iterator.repeat(rc, 3, 1) |> Iterator.toList == 1 :: 1 ::  1 :: Nil
    }

    @test
    def toList04(): Bool = region rc {
        Iterator.range(rc, -100, 200) |> Iterator.toList == List.range(-100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toChain01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |> Iterator.toChain == Chain.empty()
    }

    @test
    def toChain02(): Bool = region rc {
        Iterator.range(rc, -10, -5) |> Iterator.toChain == List.toChain(-10 :: -9 :: -8 :: -7 :: -6 :: Nil)
    }

    @test
    def toChain03(): Bool = region rc {
        Iterator.repeat(rc, 3, 1) |> Iterator.toChain == List.toChain(1 :: 1 ::  1 :: Nil)
    }

    @test
    def toChain04(): Bool = region rc {
        Iterator.range(rc, -100, 200) |> Iterator.toChain == Chain.range(-100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |> Iterator.toArray(rc) `Array.sameElements` Array#{} @ rc
    }

    @test
    def toArray02(): Bool = region rc {
        Iterator.range(rc, -10, -5) |> Iterator.toArray(rc) `Array.sameElements` Array#{-10, -9, -8, -7, -6} @ rc
    }

    @test
    def toArray03(): Bool = region rc {
        Iterator.repeat(rc, 3, 1) |> Iterator.toArray(rc) `Array.sameElements` Array#{1, 1, 1} @ rc
    }

    @test
    def toArray04(): Bool = region rc {
        Iterator.range(rc, -100, 200) |> Iterator.toArray(rc) `Array.sameElements` Array.range(rc, -100, 200)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toVector01(): Bool = region rc {
        (Iterator.empty(rc) |> Iterator.toVector) `Vector.equals` (Vector.empty() : Vector[Int32])
    }

    @test
    def toVector02(): Bool = region rc {
        (Iterator.singleton(rc, 0) |> Iterator.toVector) `Vector.equals` Vector.singleton(0)
    }

    @test
    def toVector03(): Bool = region rc {
        (Iterator.range(rc, 1, 4) |> Iterator.toVector) `Vector.equals` Vector.range(1, 4)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool = region rc {
        Map.empty() |> Map.iterator(rc) |> Iterator.toMap == (Map.empty(): Map[Int32, Int32])
    }

    @test
    def toMap02(): Bool = region rc {
        Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4} |> Map.iterator(rc) |> Iterator.toMap == Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4}
    }

    @test
    def toMap03(): Bool = region rc {
        Map#{1 => 2, 2 => 3, 3 => 4} |> Map.iterator(rc) |> Iterator.toMap == Map#{1 => 2, 2 => 3, 3 => 4}
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let iter = (Iterator.empty(rc): Iterator[Int32, rc, rc]);
        let d = (MutDeque.empty(rc): MutDeque[Int32, _]);
        Iterator.toMutDeque(rc, iter) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: Nil);
        Iterator.toMutDeque(rc, iter) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: Nil);
        Iterator.toMutDeque(rc, iter) `MutDeque.sameElements` d
    }

    @test
    def toMutDeque04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        let d = MutDeque.empty(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        Iterator.toMutDeque(rc, iter) `MutDeque.sameElements` d
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool = region rc {
        Nil |> List.iterator(rc) |> Iterator.toNel == (None: Option[Nel[Int32]])
    }

    @test
    def toNel02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |> Iterator.toNel == Some(Nel.Nel(1, Nil))
    }

    @test
    def toNel03(): Bool = region rc {
        Iterator.range(rc, 0, 1000) |> Iterator.toNel == Some(Nel.Nel(0, List.range(1, 1000)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNec01(): Bool = region rc {
        let iter = List.iterator(rc, (Nil: List[Unit]));
        Iterator.toNec(iter) == None
    }

    @test
    def toNec02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.singleton(1))
    }

    @test
    def toNec03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(1, Nec.singleton(2)))
    }

    @test
    def toNec04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(1, Nec.singleton(1)))
    }

    @test
    def toNec05(): Bool = region rc {
        let iter = List.iterator(rc, 2 :: 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(2, Nec.singleton(1)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool = region rc {
        Set.empty() |> Set.iterator(rc) |> Iterator.toSet == (Set.empty(): Set[Int32])
    }

    @test
    def toSet02(): Bool = region rc {
        Set#{"a", "b", "c", "d"} |> Set.iterator(rc) |> Iterator.toSet == Set#{"a", "b", "c", "d"}
    }

    @test
    def toSet03(): Bool = region rc {
        Set.range(0, 10000) |> Set.iterator(rc) |> Iterator.toSet == Set.range(0, 10000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filter01(): Bool = region rc {
        Nil |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def filter02(): Bool = region rc {
        (2 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def filter03(): Bool = region rc {
        (4 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 4 :: Nil
    }

    @test
    def filter04(): Bool = region rc {
        (1 :: 3 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def filter05(): Bool = region rc {
        (1 :: 8 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 8 :: Nil
    }

    @test
    def filter06(): Bool = region rc {
        (8 :: 1 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 8 :: Nil
    }

    @test
    def filter07(): Bool = region rc {
        (8 :: 9 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 8 :: 9 :: Nil
    }

    @test
    def filter08(): Bool = region rc {
        (1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 4 :: 11 :: 17 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def map01(): Bool = region rc {
        Nil |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == Nil
    }

    @test
    def map02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == false :: Nil
    }

    @test
    def map03(): Bool = region rc {
        (3 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == true :: Nil
    }

    @test
    def map04(): Bool = region rc {
        (1 :: 2 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == false :: false :: Nil
    }

    @test
    def map05(): Bool = region rc {
        (1 :: 8 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == false :: true :: Nil
    }

    @test
    def map06(): Bool = region rc {
        (8 :: 1 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == true :: false :: Nil
    }

    @test
    def map07(): Bool = region rc {
        (7 :: 8 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == true :: true :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def mapWithIndex01(): Bool = region rc {
        Nil |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList == Nil
    }

    @test
    def mapWithIndex02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList == 0 :: Nil
    }

    @test
    def mapWithIndex03(): Bool = region rc {
        (3 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList == 0 :: Nil
    }

    @test
    def mapWithIndex04(): Bool = region rc {
        (1 :: 2 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList == 0 :: 2 :: Nil
    }

    @test
    def mapWithIndex05(): Bool = region rc {
        (1 :: 8 :: 10 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList == 0 :: 8 :: 0 :: Nil
    }

    @test
    def mapWithIndex06(): Bool = region rc {
        (10 :: 8 :: 1 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList == 0 :: 8 :: 0 :: Nil
    }

    @test
    def mapWithIndex07(): Bool = region rc {
        (1 :: 8 :: 10 :: 14 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.remainder` 2 == 0) 0 else i) |> Iterator.toList == 0 :: 8 :: 0 :: 14 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let res = Ref.fresh(rc, 21);
        Nil |> List.iterator(rc) |>
            Iterator.forEach(x -> Ref.put(x, res));
        21 == Ref.get(res)
    }

    @test
    def forEach02(): Bool = region rc {
        let res = Ref.fresh(rc, 21);
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.forEach(x -> Ref.put(x, res));
        3 == Ref.get(res)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region rc {
        let res = Ref.fresh(rc, 21);
        Nil |> List.iterator(rc) |>
            Iterator.forEachWithIndex((i, _) -> Ref.put(i, res));
        21 == Ref.get(res)
    }

    @test
    def forEachWithIndex02(): Bool = region rc {
        let res = Ref.fresh(rc, 21);
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.forEachWithIndex((i, _) -> Ref.put(i, res));
        2 == Ref.get(res)
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool = region rc {
        List.iterator(rc, Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList == Nil
    }

    @test
    def replace02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList == 1 :: Nil
    }

    @test
    def replace03(): Bool = region rc {
        List.iterator(rc, 3 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList == 4 :: Nil
    }

    @test
    def replace04(): Bool = region rc {
        List.iterator(rc, 4 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList == 4 :: Nil
    }

    @test
    def replace05(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def replace06(): Bool = region rc {
        List.iterator(rc, 1 :: 3 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList == 1 :: 4 :: Nil
    }

    @test
    def replace07(): Bool = region rc {
        List.iterator(rc, 3 :: 4 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList == 4 :: 4 :: Nil
    }

    @test
    def replace08(): Bool = region rc {
        List.iterator(rc, 3 :: 3 :: Nil) |>
            Iterator.replace(src =  3, dst =  4) |> Iterator.toList == 4 :: 4 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def append01(): Bool = region rc {
        let iter1 = List.iterator(rc, (Nil: List[Unit]));
        let iter2 = List.iterator(rc, (Nil: List[Unit]));
        Iterator.append(iter1, iter2) |> Iterator.toList == Nil
    }

    @test
    def append02(): Bool = region rc {
        let iter1 = List.iterator(rc, Nil);
        let iter2 = List.iterator(rc, 1 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: Nil
    }

    @test
    def append03(): Bool = region rc {
        let iter1 = List.iterator(rc, Nil);
        let iter2 = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def append04(): Bool = region rc {
        let iter1 = List.iterator(rc, 1 :: Nil);
        let iter2 = List.iterator(rc, Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: Nil
    }

    @test
    def append05(): Bool = region rc {
        let iter1 = List.iterator(rc, 1 :: 2 :: Nil);
        let iter2 = List.iterator(rc, Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def append06(): Bool = region rc {
        let iter1 = List.iterator(rc, 1 :: Nil);
        let iter2 = List.iterator(rc, 2 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def append07(): Bool = region rc {
        let iter1 = List.iterator(rc, 1 :: 2 :: Nil);
        let iter2 = List.iterator(rc, 3 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def append08(): Bool = region rc {
        let iter1 = List.iterator(rc, 1 :: Nil);
        let iter2 = List.iterator(rc, 2 :: 3 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    // TODO - multiple region tests currently disabled
    // @test
    // def append09(): Bool = region rc1 {
    //     let iter1 = List.iterator(rc1, 1 :: Nil);
    //     region rc2 {
    //         let iter2 = List.iterator(rc2, 2 :: 3 :: Nil);
    //         Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    //     }
    // }


    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zip01(): Bool = region rc {
        let iterA = (Nil: List[Int32]) |> List.iterator(rc);
        let iterB = Iterator.repeat(rc, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def zip02(): Bool = region rc {
        let iterA = (Nil: List[Int32]) |> List.iterator(rc);
        let iterB = Iterator.repeat(rc, 1000, "b");
        Iterator.zip(iterB, iterA) |> Iterator.toList == Nil
    }

    @test
    def zip03(): Bool = region rc {
        let iterA = Iterator.repeat(rc, 500,  "a");
        let iterB = Iterator.repeat(rc, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(500, ("a", "b"))
    }

    @test
    def zip04(): Bool = region rc {
        let iterA = Iterator.repeat(rc, 1000, "a");
        let iterB = Iterator.repeat(rc, 500,  "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(500, ("a", "b"))
    }

    @test
    def zip05(): Bool = region rc {
        let iterA = Iterator.repeat(rc, 1000, "a");
        let iterB = Iterator.repeat(rc, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(1000, ("a", "b"))
    }

    @test
    def zip06(): Bool = region rc {
        let iterA = Iterator.range(rc, 0, 500);
        let iterB = Iterator.repeat(rc, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.zip(List.range(0, 500), List.repeat(500, "b"))
    }

    // TODO - multiple region tests currently disabled
    // @test
    // def zip07(): Bool = region rc1 {
    //     let iterA = Iterator.range(rc1, 0, 500);
    //     region rc2 {
    //         let iterB = Iterator.repeat(rc2, 1000, "b");
    //         Iterator.zip(iterA, iterB) |> Iterator.toList == List.zip(List.range(0, 500), List.repeat(500, "b"))
    //     }
    // }


    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWith01(): Bool = region rc {
        let iterA = List.iterator(rc, Nil);
        let iterB = List.iterator(rc, Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def zipWith02(): Bool = region rc {
        let iterA = List.iterator(rc, 1 :: Nil);
        let iterB = List.iterator(rc, Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def zipWith03(): Bool = region rc {
        let iterA = List.iterator(rc, Nil);
        let iterB = List.iterator(rc, true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @test
    def zipWith04(): Bool = region rc {
        let iterA = List.iterator(rc, 1 :: Nil);
        let iterB = List.iterator(rc, true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 2 :: Nil
    }

    @test
    def zipWith05(): Bool = region rc {
        let iterA = List.iterator(rc, 1 :: Nil);
        let iterB = List.iterator(rc, false :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 1 :: Nil
    }

    @test
    def zipWith06(): Bool = region rc {
        let iterA = List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        let iterB = List.iterator(rc, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
    }

    @test
    def zipWith07(): Bool = region rc1 {
        let iterA = List.iterator(rc1, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        region rc2 {
            let iterB = List.iterator(rc2, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
            Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
        }
    }


    /////////////////////////////////////////////////////////////////////////////
    // zipWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def zipWithIndex01(): Bool = region rc {
        Iterator.zipWithIndex((Iterator.empty(rc): Iterator[Int32, rc, rc])) |> Iterator.toList == Nil
    }

    @test
    def zipWithIndex02(): Bool = region rc {
        Iterator.zipWithIndex(Iterator.singleton(rc, 2)) |> Iterator.toList == (0, 2) :: Nil
    }

    @test
    def zipWithIndex03(): Bool = region rc {
        Iterator.zipWithIndex(List.iterator(rc, 1 :: 8 :: Nil)) |> Iterator.toList == (0, 1) :: (1, 8) :: Nil
    }

    @test
    def zipWithIndex04(): Bool = region rc {
        Iterator.zipWithIndex(List.iterator(rc, 8 :: 1 :: Nil)) |> Iterator.toList == (0, 8) :: (1, 1) :: Nil
    }

    @test
    def zipWithIndex05(): Bool = region rc {
        Iterator.zipWithIndex(List.iterator(rc, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) |> Iterator.toList == (0, 1) :: (1, 4) :: (2, 11) :: (3, 2) :: (4, -22) :: (5, 17) :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool = region rc {
        let iter = List.iterator(rc, Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 100
    }

    @test
    def foldLeft02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 198
    }

    @test
    def foldLeft03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 196
    }

    @test
    def foldLeft04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 386
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region rc {
        let iter = List.iterator(rc, Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 100
    }

    @test
    def foldRight02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 198
    }

    @test
    def foldRight03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 194
    }

    @test
    def foldRight04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = region rc {
        let iter = List.iterator(rc, Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 100
    }

    @test
    def foldRightWithCont02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 198
    }

    @test
    def foldRightWithCont03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 194
    }

    @test
    def foldRightWithCont04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, iter) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = region rc {
        Iterator.foldMap(x -> 2 * x, List.iterator(rc, Nil)) == 0
    }

    @test
    def foldMap02(): Bool = region rc {
        Iterator.foldMap(x -> 2 * x, List.iterator(rc, 1 :: 2 :: Nil)) == 6
    }

    @test
    def foldMap03(): Bool = region rc {
        Iterator.foldMap(x -> if (x == "a") "b" else x, List.iterator(rc, "a" :: Nil)) == "b"
    }

    @test
    def foldMap04(): Bool = region rc {
        Iterator.foldMap(x -> if (x == "c") "b" else x, List.iterator(rc, "a" :: "b" :: "c" :: Nil)) == "abb"
    }

    @test
    def foldMap05(): Bool = region rc {
        Iterator.foldMap(Int32.toString, List.iterator(rc, 1 :: 2 :: 3 :: Nil)) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool = region rc {
        let iter = List.iterator(rc, (Nil: List[Int32]));
        Iterator.reduceLeft((a, b) -> a - b, iter) == None
    }

    @test
    def reduceLeft02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(1)
    }

    @test
    def reduceLeft03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-1)
    }

    @test
    def reduceLeft04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-4)
    }

    @test
    def reduceLeft05(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: 4 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-8)
    }


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool = region rc {
        List.iterator(rc, (Nil: List[Int32])) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def takeWhile02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def takeWhile03(): Bool = region rc {
        List.iterator(rc, 4 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 4 :: Nil
    }

    @test
    def takeWhile04(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def takeWhile05(): Bool = region rc {
        List.iterator(rc, 1 :: 5 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def takeWhile06(): Bool = region rc {
        List.iterator(rc, 5 :: 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 5 :: Nil
    }

    @test
    def takeWhile07(): Bool = region rc {
        List.iterator(rc, 5 :: 8 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 5 :: 8 :: Nil
    }

    @test
    def takeWhile08(): Bool = region rc {
        List.iterator(rc, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 4 :: 6 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool = region rc {
        List.iterator(rc, Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def dropWhile02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: Nil
    }

    @test
    def dropWhile03(): Bool = region rc {
        List.iterator(rc, 4 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def dropWhile04(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def dropWhile05(): Bool = region rc {
        List.iterator(rc, 1 :: 5 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: 5 :: Nil
    }

    @test
    def dropWhile06(): Bool = region rc {
        List.iterator(rc, 5 :: 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: Nil
    }

    @test
    def dropWhile07(): Bool = region rc {
        List.iterator(rc, 5 :: 8 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @test
    def dropWhile08(): Bool = region rc {
        List.iterator(rc, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatMap01(): Bool = region rc {
        (List.iterator(rc, Nil): Iterator[Int32, _, _]) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == Nil
    }

    @test
    def flatMap02(): Bool = region rc {
        List.iterator(rc, 0 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == Nil
    }

    @test
    def flatMap03(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == 1 :: Nil
    }

    @test
    def flatMap04(): Bool = region rc {
        List.iterator(rc, 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == 2 :: 2 :: Nil
    }

    @test
    def flatMap05(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == 1 :: 2 :: 2 :: Nil
    }

    @test
    def flatMap06(): Bool = region rc {
        List.iterator(rc, 2 :: 3 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == 2 :: 2 :: 3 :: 3 :: 3 :: Nil
    }

    @test
    def flatMap07(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.flatMap(i -> if (i `Int32.remainder` 2 == 0) Iterator.repeat(rc, i, i) else Iterator.empty(rc)) |>
            Iterator.toList == 2 :: 2 :: 4 :: 4 :: 4 :: 4 :: Nil
    }

    // TODO - multiple region tests currently disabled
    // @test
    // def flatMap08(): Bool = region rc1 {
    //     region rc2 {
    //         List.iterator(rc1, 1 :: 2 :: 3 :: 4 :: Nil) |>
    //             Iterator.flatMap(i -> if (i `Int32.remainder` 2 == 0) Iterator.repeat(rc2, i, i) else Iterator.empty(rc2)) |>
    //             Iterator.toList == 2 :: 2 :: 4 :: 4 :: 4 :: 4 :: Nil
    //     }
    // }


    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool = region rc {
        List.iterator(rc, Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == Nil
    }

    @test
    def intersperse02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: Nil
    }

    @test
    def intersperse03(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: Nil
    }

    @test
    def intersperse04(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: 11 :: 3 :: Nil
    }

    @test
    def intersperse05(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intercalate01(): Bool = region rc {
        let sep: List[Int32] = Nil;
        let iter = Nil |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |> Iterator.toList == (Nil: List[Int32])
    }

    @test
    def intercalate02(): Bool = region rc {
        let sep: List[Int32] = Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iter  = (iterA1 :: Nil) |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |>
            Iterator.toList == 1 :: Nil
    }

    @test
    def intercalate03(): Bool = region rc {
        let sep = 11 :: 12 :: 13 :: Nil;
        let iter = Nil |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |>
            Iterator.toList ==  Nil
    }

    @test
    def intercalate04(): Bool = region rc {
        let sep: List[Int32] = Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iterA2 = (2 :: 3 :: Nil) |> List.iterator(rc);
        let iter = (iterA1 :: iterA2 :: Nil) |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |>
            Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @test
    def intercalate05(): Bool = region rc {
        let sep  = 11 :: 12 :: 13 :: Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iterA2 = (2 :: 3 :: Nil) |> List.iterator(rc);
        let iter = (iterA1 :: iterA2 :: Nil) |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |>
            Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: Nil
    }

    @test
    def intercalate06(): Bool = region rc {
        let sep = Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iterA2 = (2 :: 3 :: Nil) |> List.iterator(rc);
        let iterA3 = (4 :: Nil) |> List.iterator(rc);
        let iter = (iterA1 :: iterA2 :: iterA3 :: Nil) |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |>
            Iterator.toList == 1 :: 2 :: 3 :: 4 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // unfoldWithOk                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unfoldWithOk01(): Bool = region rc {
        let iter = Iterator.unfoldWithOk(rc, () -> Err(0));
        Iterator.toList(iter) == (Nil: List[Int32])
    }

    @test
    def unfoldWithOk02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        Iterator.unfoldWithOk(rc, () -> {Iterator.next(iter) |> Option.toOk("Done")}) |>
            Iterator.toList == 1 :: 2 :: 3 :: 4 :: 5 :: Nil
        }

    @test
    def unfoldWithOk03(): Bool = region rc {
        let iter = (1 :: 2 :: 3 :: 4 :: 5 :: Nil) |>
            List.map(x -> if (x == 4) Err("Done") else Ok(x)) |>
            List.iterator(rc);
        Iterator.unfoldWithOk(rc, () -> {Iterator.next(iter) |> Option.getWithDefault(Err("Done"))}) |>
            Iterator.toList == 1 :: 2 :: 3 :: Nil
        }


    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def drop01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.drop(1) |> Iterator.toList == Nil
    }
    @test
    def drop02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(1) |> Iterator.toList == Nil
    }

    @test
    def drop03(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(2) |> Iterator.toList == 3 :: Nil
    }

    @test
    def drop04(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(3) |> Iterator.toList == Nil
    }

    @test
    def drop05(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(4) |> Iterator.toList == Nil
    }

    @test
    def drop06(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(0) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def take01(): Bool = region rc {
        List.iterator(rc, (Nil: List[Unit])) |> Iterator.take(-1) |> Iterator.toList == Nil
    }

    @test
    def take02(): Bool = region rc {
        List.iterator(rc, (Nil: List[Unit])) |> Iterator.take(0) |> Iterator.toList == Nil
    }

    @test
    def take03(): Bool = region rc {
        List.iterator(rc, (Nil: List[Unit])) |> Iterator.take(1) |> Iterator.toList == Nil
    }

    @test
    def take04(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.take(-1) |> Iterator.toList == Nil
    }

    @test
    def take05(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.take(0) |> Iterator.toList == Nil
    }

    @test
    def take06(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.take(1) |> Iterator.toList == 1 :: Nil
    }

    @test
    def take07(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.take(2) |> Iterator.toList == 1 :: Nil
    }

    @test
    def take08(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |> Iterator.take(0) |> Iterator.toList == Nil
    }

    @test
    def take09(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |> Iterator.take(1) |> Iterator.toList == 1 :: Nil
    }

    @test
    def take10(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |> Iterator.take(2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def take11(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> Iterator.take(2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def take12(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> Iterator.take(4) |> Iterator.toList == 1 :: 2 :: 3 :: 4 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.join(",") == ""
    }

    @test
    def join02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.join(",") == "1"
    }

    @test
    def join03(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.join(",") == "1,2,3"
    }

    @test
    def join04(): Bool = region rc {
        ("1" :: "2" :: "3" :: Nil) |> List.iterator(rc) |>
            Iterator.join(",") == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == ""
    }

    @test
    def joinWith02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == "2"
    }

    @test
    def joinWith03(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == "2,3,4"
    }

    @test
    def joinWith04(): Bool = region rc {
        ("1" :: "2" :: "3" :: Nil) |> List.iterator(rc) |>
            Iterator.joinWith(x -> x + x, ",") == "11,22,33"
    }


    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def filterMap01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.remainder` 2 == 0) Some(x) else None) |> Iterator.toList == Nil
    }
    @test
    def filterMap02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.remainder` 2 == 0) Some(x) else None) |> Iterator.toList == Nil
    }

    @test
    def filterMap03(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.remainder` 2 == 0) Some(x) else None) |> Iterator.toList == 2 :: Nil
    }

    @test
    def filterMap04(): Bool = region rc {
        (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.remainder` 2 == 0) Some(x) else None) |> Iterator.toList == 2 :: 4 :: 6 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def cons01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.cons(1) |> Iterator.toList == 1 :: Nil
    }

    @test
    def cons02(): Bool = region rc {
        (2 :: (Nil: List[Int32])) |> List.iterator(rc) |>
            Iterator.cons(1) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @test
    def cons03(): Bool = region rc {
        (3 :: (Nil: List[Int32])) |> List.iterator(rc) |>
            Iterator.cons(2) |> Iterator.cons(1) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flatten01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList == Nil
    }

    @test
    def flatten02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList == 1 :: 1 :: Nil
    }

    @test
    def flatten03(): Bool = region rc {
        (1 :: 2 :: Nil) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList == 1 :: 1 :: 2 :: 2 :: Nil
    }

    @test
    def flatten04(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList == 1 :: 1 :: 2 :: 2 :: 3 :: 3 :: Nil
    }

}
