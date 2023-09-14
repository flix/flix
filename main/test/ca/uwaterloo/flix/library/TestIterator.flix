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

    @Test
    def next01(): Bool = region rc {
        Iterator.empty(rc) |> Iterator.next == None: Option[Int32]
    }

    @Test
    def next02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.next(iter) == Some(1)
    }

    @Test
    def next03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        discard Iterator.next(iter);
        Iterator.next(iter) == None
    }

    @Test
    def next04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        discard Iterator.next(iter);
        Iterator.next(iter) == Some(2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // range                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def range01(): Bool = region rc {
        Iterator.range(rc, 0, 0) |> Iterator.toList == Nil
    }

    @Test
    def range02(): Bool = region rc {
        Iterator.range(rc, 10, 0) |> Iterator.toList == Nil
    }

    @Test
    def range03(): Bool = region rc {
        Iterator.range(rc, 1, 4) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def range04(): Bool = region rc {
        Iterator.range(rc, -1, 2) |> Iterator.toList == -1 :: 0 :: 1 :: Nil
    }

    @Test
    def range05(): Bool = region rc {
        Iterator.range(rc, -100, 200) |> Iterator.toList == List.range(-100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Bool = region rc {
        Iterator.repeat(rc, 0, 10) |> Iterator.toList == Nil
    }

    @Test
    def repeat02(): Bool = region rc {
        Iterator.repeat(rc, -100, 2) |> Iterator.toList == Nil
    }

    @Test
    def repeat03(): Bool = region rc {
        Iterator.repeat(rc, 100, 2) |> Iterator.toList == List.repeat(100, 2)
    }

    @Test
    def repeat04(): Bool = region rc {
        Iterator.repeat(rc, 3, 1) |> Iterator.toList == 1 :: 1 :: 1 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool = region rc {
        List.iterator(rc, Nil) |> Iterator.sum == 0
    }

    @Test
    def sum02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.sum == 1
    }

    @Test
    def sum03(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: Nil) |> Iterator.sum == 6
    }

    @Test
    def sum04(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sum == 3
    }

    @Test
    def sum05(): Bool = region rc {
        List.iterator(rc, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sum == -10
    }

    @Test
    def sum06(): Bool = region rc {
        List.iterator(rc, 10 :: -10 :: Nil) |> Iterator.sum == 0
    }

    @Test
    def sum07(): Bool = region rc {
        Iterator.range(rc, 1, 101) |> Iterator.sum == 5050
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool = region rc {
        List.iterator(rc, Nil) |> Iterator.sumWith(x -> x + 1) == 0
    }

    @Test
    def sumWith02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.sumWith(x -> x + 1) == 2
    }

    @Test
    def sumWith03(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: Nil) |> Iterator.sumWith(x -> x + 1) == 9
    }

    @Test
    def sumWith04(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: -3 :: Nil) |> Iterator.sumWith(x -> x + 1) == 7
    }

    @Test
    def sumWith05(): Bool = region rc {
        List.iterator(rc, -1 :: -2 :: -3 :: -4 :: Nil) |> Iterator.sumWith(x -> x + 1) == -6
    }

    @Test
    def sumWith06(): Bool = region rc {
        List.iterator(rc, 10 :: -10 :: Nil) |> Iterator.sumWith(x -> x + 1) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Bool = region rc {
        Nil: List[Int32] |> List.iterator(rc) |> Iterator.toList == Nil
    }

    @Test
    def toList02(): Bool = region rc {
        Iterator.range(rc, -10, -5) |> Iterator.toList == -10 :: -9 :: -8 :: -7 :: -6 :: Nil
    }

    @Test
    def toList03(): Bool = region rc {
        Iterator.repeat(rc, 3, 1) |> Iterator.toList == 1 :: 1 ::  1 :: Nil
    }

    @Test
    def toList04(): Bool = region rc {
        Iterator.range(rc, -100, 200) |> Iterator.toList == List.range(-100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toChain                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChain01(): Bool = region rc {
        Nil: List[Int32] |> List.iterator(rc) |> Iterator.toChain == Chain.empty()
    }

    @Test
    def toChain02(): Bool = region rc {
        Iterator.range(rc, -10, -5) |> Iterator.toChain == List.toChain(-10 :: -9 :: -8 :: -7 :: -6 :: Nil)
    }

    @Test
    def toChain03(): Bool = region rc {
        Iterator.repeat(rc, 3, 1) |> Iterator.toChain == List.toChain(1 :: 1 ::  1 :: Nil)
    }

    @Test
    def toChain04(): Bool = region rc {
        Iterator.range(rc, -100, 200) |> Iterator.toChain == Chain.range(-100, 200)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Bool = region rc {
        Nil: List[Int32] |> List.iterator(rc) |> Iterator.toArray(rc) `Array.sameElements` Array#{} @ rc
    }

    @Test
    def toArray02(): Bool = region rc {
        Iterator.range(rc, -10, -5) |> Iterator.toArray(rc) `Array.sameElements` Array#{-10, -9, -8, -7, -6} @ rc
    }

    @Test
    def toArray03(): Bool = region rc {
        Iterator.repeat(rc, 3, 1) |> Iterator.toArray(rc) `Array.sameElements` Array#{1, 1, 1} @ rc
    }

    @Test
    def toArray04(): Bool = region rc {
        Iterator.range(rc, -100, 200) |> Iterator.toArray(rc) `Array.sameElements` Array.range(rc, -100, 200)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Bool = region rc {
        (Iterator.empty(rc) |> Iterator.toVector) `Vector.equals` (Vector.empty() : Vector[Int32])
    }

    @Test
    def toVector02(): Bool = region rc {
        (Iterator.singleton(rc, 0) |> Iterator.toVector) `Vector.equals` Vector.singleton(0)
    }

    @Test
    def toVector03(): Bool = region rc {
        (Iterator.range(rc, 1, 4) |> Iterator.toVector) `Vector.equals` Vector.range(1, 4)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Bool = region rc {
        Map.empty() |> Map.iterator(rc) |> Iterator.toMap == Map.empty(): Map[Int32, Int32]
    }

    @Test
    def toMap02(): Bool = region rc {
        Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4} |> Map.iterator(rc) |> Iterator.toMap == Map#{"A" => 1, "B" => 2, "C" => 3, "D" => 4}
    }

    @Test
    def toMap03(): Bool = region rc {
        Map#{1 => 2, 2 => 3, 3 => 4} |> Map.iterator(rc) |> Iterator.toMap == Map#{1 => 2, 2 => 3, 3 => 4}
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let iter = Iterator.empty(rc): Iterator[Int32, rc, rc];
        let d = MutDeque.new(rc): MutDeque[Int32, _];
        Iterator.toMutDeque(rc, iter) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        let d = MutDeque.new(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: Nil);
        Iterator.toMutDeque(rc, iter) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        let d = MutDeque.new(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: Nil);
        Iterator.toMutDeque(rc, iter) `MutDeque.sameElements` d
    }

    @Test
    def toMutDeque04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        let d = MutDeque.new(rc);
        List.forEach(x -> MutDeque.pushBack(x, d), 1 :: 2 :: 3 :: Nil);
        Iterator.toMutDeque(rc, iter) `MutDeque.sameElements` d
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Bool = region rc {
        Nil |> List.iterator(rc) |> Iterator.toNel == None: Option[Nel[Int32]]
    }

    @Test
    def toNel02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |> Iterator.toNel == Some(Nel.Nel(1, Nil))
    }

    @Test
    def toNel03(): Bool = region rc {
        Iterator.range(rc, 0, 1000) |> Iterator.toNel == Some(Nel.Nel(0, List.range(1, 1000)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toNec                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNec01(): Bool = region rc {
        let iter = List.iterator(rc, Nil: List[Unit]);
        Iterator.toNec(iter) == None
    }

    @Test
    def toNec02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.singleton(1))
    }

    @Test
    def toNec03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(1, Nec.singleton(2)))
    }

    @Test
    def toNec04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(1, Nec.singleton(1)))
    }

    @Test
    def toNec05(): Bool = region rc {
        let iter = List.iterator(rc, 2 :: 1 :: Nil);
        Iterator.toNec(iter) == Some(Nec.cons(2, Nec.singleton(1)))
    }


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Bool = region rc {
        Set.empty() |> Set.iterator(rc) |> Iterator.toSet == Set.empty(): Set[Int32]
    }

    @Test
    def toSet02(): Bool = region rc {
        Set#{"a", "b", "c", "d"} |> Set.iterator(rc) |> Iterator.toSet == Set#{"a", "b", "c", "d"}
    }

    @Test
    def toSet03(): Bool = region rc {
        Set.range(0, 10000) |> Set.iterator(rc) |> Iterator.toSet == Set.range(0, 10000)
    }


    /////////////////////////////////////////////////////////////////////////////
    // filter                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filter01(): Bool = region rc {
        Nil |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == Nil
    }

    @Test
    def filter02(): Bool = region rc {
        (2 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == Nil
    }

    @Test
    def filter03(): Bool = region rc {
        (4 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 4 :: Nil
    }

    @Test
    def filter04(): Bool = region rc {
        (1 :: 3 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == Nil
    }

    @Test
    def filter05(): Bool = region rc {
        (1 :: 8 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 8 :: Nil
    }

    @Test
    def filter06(): Bool = region rc {
        (8 :: 1 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 8 :: Nil
    }

    @Test
    def filter07(): Bool = region rc {
        (8 :: 9 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 8 :: 9 :: Nil
    }

    @Test
    def filter08(): Bool = region rc {
        (1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil) |> List.iterator(rc) |> Iterator.filter(i -> i > 3) |> Iterator.toList == 4 :: 11 :: 17 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Bool = region rc {
        Nil |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == Nil
    }

    @Test
    def map02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == false :: Nil
    }

    @Test
    def map03(): Bool = region rc {
        (3 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == true :: Nil
    }

    @Test
    def map04(): Bool = region rc {
        (1 :: 2 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == false :: false :: Nil
    }

    @Test
    def map05(): Bool = region rc {
        (1 :: 8 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == false :: true :: Nil
    }

    @Test
    def map06(): Bool = region rc {
        (8 :: 1 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == true :: false :: Nil
    }

    @Test
    def map07(): Bool = region rc {
        (7 :: 8 :: Nil) |> List.iterator(rc) |>
            Iterator.map(i -> i > 2) |> Iterator.toList == true :: true :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Bool = region rc {
        Nil |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.rem` 2 == 0) 0 else i) |> Iterator.toList == Nil
    }

    @Test
    def mapWithIndex02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.rem` 2 == 0) 0 else i) |> Iterator.toList == 0 :: Nil
    }

    @Test
    def mapWithIndex03(): Bool = region rc {
        (3 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.rem` 2 == 0) 0 else i) |> Iterator.toList == 0 :: Nil
    }

    @Test
    def mapWithIndex04(): Bool = region rc {
        (1 :: 2 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.rem` 2 == 0) 0 else i) |> Iterator.toList == 0 :: 2 :: Nil
    }

    @Test
    def mapWithIndex05(): Bool = region rc {
        (1 :: 8 :: 10 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.rem` 2 == 0) 0 else i) |> Iterator.toList == 0 :: 8 :: 0 :: Nil
    }

    @Test
    def mapWithIndex06(): Bool = region rc {
        (10 :: 8 :: 1 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.rem` 2 == 0) 0 else i) |> Iterator.toList == 0 :: 8 :: 0 :: Nil
    }

    @Test
    def mapWithIndex07(): Bool = region rc {
        (1 :: 8 :: 10 :: 14 :: Nil) |> List.iterator(rc) |>
            Iterator.mapWithIndex((ix, i) -> if (ix `Int32.rem` 2 == 0) 0 else i) |> Iterator.toList == 0 :: 8 :: 0 :: 14 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let res = ref 21 @ rc;
        Nil |> List.iterator(rc) |>
            Iterator.forEach(x -> res := x);
        21 == deref res
    }

    @Test
    def forEach02(): Bool = region rc {
        let res = ref 21 @ rc;
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.forEach(x -> res := x);
        3 == deref res
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let res = ref 21 @ rc;
        Nil |> List.iterator(rc) |>
            Iterator.forEachWithIndex((i, _) -> res := i);
        21 == deref res
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let res = ref 21 @ rc;
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.forEachWithIndex((i, _) -> res := i);
        2 == deref res
    }

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Bool = region rc {
        List.iterator(rc, Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == Nil
    }

    @Test
    def replace02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def replace03(): Bool = region rc {
        List.iterator(rc, 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: Nil
    }

    @Test
    def replace04(): Bool = region rc {
        List.iterator(rc, 4 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: Nil
    }

    @Test
    def replace05(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @Test
    def replace06(): Bool = region rc {
        List.iterator(rc, 1 :: 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 1 :: 4 :: Nil
    }

    @Test
    def replace07(): Bool = region rc {
        List.iterator(rc, 3 :: 4 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: 4 :: Nil
    }

    @Test
    def replace08(): Bool = region rc {
        List.iterator(rc, 3 :: 3 :: Nil) |>
            Iterator.replace(from = 3, to = 4) |> Iterator.toList == 4 :: 4 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // append                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def append01(): Bool = region rc {
        let iter1 = List.iterator(rc, Nil: List[Unit]);
        let iter2 = List.iterator(rc, Nil: List[Unit]);
        Iterator.append(iter1, iter2) |> Iterator.toList == Nil
    }

    @Test
    def append02(): Bool = region rc {
        let iter1 = List.iterator(rc, Nil);
        let iter2 = List.iterator(rc, 1 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def append03(): Bool = region rc {
        let iter1 = List.iterator(rc, Nil);
        let iter2 = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @Test
    def append04(): Bool = region rc {
        let iter1 = List.iterator(rc, 1 :: Nil);
        let iter2 = List.iterator(rc, Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def append05(): Bool = region rc {
        let iter1 = List.iterator(rc, 1 :: 2 :: Nil);
        let iter2 = List.iterator(rc, Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @Test
    def append06(): Bool = region rc {
        let iter1 = List.iterator(rc, 1 :: Nil);
        let iter2 = List.iterator(rc, 2 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @Test
    def append07(): Bool = region rc {
        let iter1 = List.iterator(rc, 1 :: 2 :: Nil);
        let iter2 = List.iterator(rc, 3 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def append08(): Bool = region rc {
        let iter1 = List.iterator(rc, 1 :: Nil);
        let iter2 = List.iterator(rc, 2 :: 3 :: Nil);
        Iterator.append(iter1, iter2) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    // TODO - multiple region tests currently disabled
    // @Test
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

    @Test
    def zip01(): Bool = region rc {
        let iterA = Nil: List[Int32] |> List.iterator(rc);
        let iterB = Iterator.repeat(rc, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == Nil
    }

    @Test
    def zip02(): Bool = region rc {
        let iterA = Nil: List[Int32] |> List.iterator(rc);
        let iterB = Iterator.repeat(rc, 1000, "b");
        Iterator.zip(iterB, iterA) |> Iterator.toList == Nil
    }

    @Test
    def zip03(): Bool = region rc {
        let iterA = Iterator.repeat(rc, 500,  "a");
        let iterB = Iterator.repeat(rc, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(500, ("a", "b"))
    }

    @Test
    def zip04(): Bool = region rc {
        let iterA = Iterator.repeat(rc, 1000, "a");
        let iterB = Iterator.repeat(rc, 500,  "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(500, ("a", "b"))
    }

    @Test
    def zip05(): Bool = region rc {
        let iterA = Iterator.repeat(rc, 1000, "a");
        let iterB = Iterator.repeat(rc, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.repeat(1000, ("a", "b"))
    }

    @Test
    def zip06(): Bool = region rc {
        let iterA = Iterator.range(rc, 0, 500);
        let iterB = Iterator.repeat(rc, 1000, "b");
        Iterator.zip(iterA, iterB) |> Iterator.toList == List.zip(List.range(0, 500), List.repeat(500, "b"))
    }

    // TODO - multiple region tests currently disabled
    // @Test
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

    @Test
    def zipWith01(): Bool = region rc {
        let iterA = List.iterator(rc, Nil);
        let iterB = List.iterator(rc, Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @Test
    def zipWith02(): Bool = region rc {
        let iterA = List.iterator(rc, 1 :: Nil);
        let iterB = List.iterator(rc, Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @Test
    def zipWith03(): Bool = region rc {
        let iterA = List.iterator(rc, Nil);
        let iterB = List.iterator(rc, true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == Nil
    }

    @Test
    def zipWith04(): Bool = region rc {
        let iterA = List.iterator(rc, 1 :: Nil);
        let iterB = List.iterator(rc, true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 2 :: Nil
    }

    @Test
    def zipWith05(): Bool = region rc {
        let iterA = List.iterator(rc, 1 :: Nil);
        let iterB = List.iterator(rc, false :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def zipWith06(): Bool = region rc {
        let iterA = List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: Nil);
        let iterB = List.iterator(rc, false :: true :: true :: false :: false :: true :: true :: true :: Nil);
        Iterator.zipWith((a, b) -> if (b) a + 1 else a, iterA, iterB) |> Iterator.toList == 1 :: 3 :: 4 :: 4 :: 5 :: 7 :: 8 :: 9 :: Nil
    }

    @Test
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

    @Test
    def zipWithIndex01(): Bool = region rc {
        Iterator.zipWithIndex(Iterator.empty(rc): Iterator[Int32, rc, rc]) |> Iterator.toList == Nil
    }

    @Test
    def zipWithIndex02(): Bool = region rc {
        Iterator.zipWithIndex(Iterator.singleton(rc, 2)) |> Iterator.toList == (0, 2) :: Nil
    }

    @Test
    def zipWithIndex03(): Bool = region rc {
        Iterator.zipWithIndex(List.iterator(rc, 1 :: 8 :: Nil)) |> Iterator.toList == (0, 1) :: (1, 8) :: Nil
    }

    @Test
    def zipWithIndex04(): Bool = region rc {
        Iterator.zipWithIndex(List.iterator(rc, 8 :: 1 :: Nil)) |> Iterator.toList == (0, 8) :: (1, 1) :: Nil
    }

    @Test
    def zipWithIndex05(): Bool = region rc {
        Iterator.zipWithIndex(List.iterator(rc, 1 :: 4 :: 11 :: 2 :: -22 :: 17 :: Nil)) |> Iterator.toList == (0, 1) :: (1, 4) :: (2, 11) :: (3, 2) :: (4, -22) :: (5, 17) :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool = region rc {
        let iter = List.iterator(rc, Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, iter) == 100
    }

    @Test
    def foldLeft02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, iter) == 198
    }

    @Test
    def foldLeft03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, iter) == 196
    }

    @Test
    def foldLeft04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        Iterator.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, iter) == 386
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool = region rc {
        let iter = List.iterator(rc, Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, iter) == 100
    }

    @Test
    def foldRight02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, iter) == 198
    }

    @Test
    def foldRight03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, iter) == 194
    }

    @Test
    def foldRight04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        Iterator.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, iter) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool = region rc {
        let iter = List.iterator(rc, Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, iter) == 100
    }

    @Test
    def foldRightWithCont02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, iter) == 198
    }

    @Test
    def foldRightWithCont03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, iter) == 194
    }

    @Test
    def foldRightWithCont04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        Iterator.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, iter) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool = region rc {
        Iterator.foldMap(x -> 2 * x, List.iterator(rc, Nil)) == 0
    }

    @Test
    def foldMap02(): Bool = region rc {
        Iterator.foldMap(x -> 2 * x, List.iterator(rc, 1 :: 2 :: Nil)) == 6
    }

    @Test
    def foldMap03(): Bool = region rc {
        Iterator.foldMap(x -> if (x == "a") "b" else x, List.iterator(rc, "a" :: Nil)) == "b"
    }

    @Test
    def foldMap04(): Bool = region rc {
        Iterator.foldMap(x -> if (x == "c") "b" else x, List.iterator(rc, "a" :: "b" :: "c" :: Nil)) == "abb"
    }

    @Test
    def foldMap05(): Bool = region rc {
        Iterator.foldMap(Int32.toString, List.iterator(rc, 1 :: 2 :: 3 :: Nil)) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceLeft01(): Bool = region rc {
        let iter = List.iterator(rc, Nil: List[Int32]);
        Iterator.reduceLeft((a, b) -> a - b, iter) == None
    }

    @Test
    def reduceLeft02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(1)
    }

    @Test
    def reduceLeft03(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-1)
    }

    @Test
    def reduceLeft04(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-4)
    }

    @Test
    def reduceLeft05(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: 4 :: Nil);
        Iterator.reduceLeft((a, b) -> a - b, iter) == Some(-8)
    }


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhile01(): Bool = region rc {
        List.iterator(rc, Nil: List[Int32]) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @Test
    def takeWhile02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @Test
    def takeWhile03(): Bool = region rc {
        List.iterator(rc, 4 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 4 :: Nil
    }

    @Test
    def takeWhile04(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @Test
    def takeWhile05(): Bool = region rc {
        List.iterator(rc, 1 :: 5 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @Test
    def takeWhile06(): Bool = region rc {
        List.iterator(rc, 5 :: 1 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 5 :: Nil
    }

    @Test
    def takeWhile07(): Bool = region rc {
        List.iterator(rc, 5 :: 8 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 5 :: 8 :: Nil
    }

    @Test
    def takeWhile08(): Bool = region rc {
        List.iterator(rc, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.takeWhile(i -> i > 3) |> Iterator.toList == 4 :: 6 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhile01(): Bool = region rc {
        List.iterator(rc, Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @Test
    def dropWhile02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def dropWhile03(): Bool = region rc {
        List.iterator(rc, 4 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @Test
    def dropWhile04(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @Test
    def dropWhile05(): Bool = region rc {
        List.iterator(rc, 1 :: 5 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: 5 :: Nil
    }

    @Test
    def dropWhile06(): Bool = region rc {
        List.iterator(rc, 5 :: 1 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def dropWhile07(): Bool = region rc {
        List.iterator(rc, 5 :: 8 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == Nil
    }

    @Test
    def dropWhile08(): Bool = region rc {
        List.iterator(rc, 4 :: 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Iterator.dropWhile(i -> i > 3) |> Iterator.toList == -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // flatMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatMap01(): Bool = region rc {
        List.iterator(rc, Nil): Iterator[Int32, _, _] |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == Nil
    }

    @Test
    def flatMap02(): Bool = region rc {
        List.iterator(rc, 0 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == Nil
    }

    @Test
    def flatMap03(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == 1 :: Nil
    }

    @Test
    def flatMap04(): Bool = region rc {
        List.iterator(rc, 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == 2 :: 2 :: Nil
    }

    @Test
    def flatMap05(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == 1 :: 2 :: 2 :: Nil
    }

    @Test
    def flatMap06(): Bool = region rc {
        List.iterator(rc, 2 :: 3 :: Nil) |>
            Iterator.flatMap(i -> Iterator.repeat(rc, i, i)) |>
            Iterator.toList == 2 :: 2 :: 3 :: 3 :: 3 :: Nil
    }

    @Test
    def flatMap07(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.flatMap(i -> if (i `Int32.rem` 2 == 0) Iterator.repeat(rc, i, i) else Iterator.empty(rc)) |>
            Iterator.toList == 2 :: 2 :: 4 :: 4 :: 4 :: 4 :: Nil
    }

    // TODO - multiple region tests currently disabled
    // @Test
    // def flatMap08(): Bool = region rc1 {
    //     region rc2 {
    //         List.iterator(rc1, 1 :: 2 :: 3 :: 4 :: Nil) |>
    //             Iterator.flatMap(i -> if (i `Int32.rem` 2 == 0) Iterator.repeat(rc2, i, i) else Iterator.empty(rc2)) |>
    //             Iterator.toList == 2 :: 2 :: 4 :: 4 :: 4 :: 4 :: Nil
    //     }
    // }


    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Bool = region rc {
        List.iterator(rc, Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == Nil
    }

    @Test
    def intersperse02(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: Nil
    }

    @Test
    def intersperse03(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: Nil
    }

    @Test
    def intersperse04(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: 11 :: 3 :: Nil
    }

    @Test
    def intersperse05(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: 4 :: Nil) |>
            Iterator.intersperse(11) |>
            Iterator.toList == 1 :: 11 :: 2 :: 11 :: 3 :: 11 :: 4 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalate01(): Bool = region rc {
        let sep: List[Int32] = Nil;
        let iter = Nil |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |> Iterator.toList == Nil: List[Int32]
    }

    @Test
    def intercalate02(): Bool = region rc {
        let sep: List[Int32] = Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iter  = (iterA1 :: Nil) |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |>
            Iterator.toList == 1 :: Nil
    }

    @Test
    def intercalate03(): Bool = region rc {
        let sep = 11 :: 12 :: 13 :: Nil;
        let iter = Nil |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |>
            Iterator.toList ==  Nil
    }

    @Test
    def intercalate04(): Bool = region rc {
        let sep: List[Int32] = Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iterA2 = (2 :: 3 :: Nil) |> List.iterator(rc);
        let iter = (iterA1 :: iterA2 :: Nil) |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |>
            Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def intercalate05(): Bool = region rc {
        let sep  = 11 :: 12 :: 13 :: Nil;
        let iterA1 = (1 :: Nil) |> List.iterator(rc);
        let iterA2 = (2 :: 3 :: Nil) |> List.iterator(rc);
        let iter = (iterA1 :: iterA2 :: Nil) |> List.iterator(rc);
        Iterator.intercalate(sep, iter) |>
            Iterator.toList == 1 :: 11 :: 12 :: 13 :: 2 :: 3 :: Nil
    }

    @Test
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

    @Test
    def unfoldWithOk01(): Bool = region rc {
        let iter = Iterator.unfoldWithOk(rc, () -> Err(0));
        Iterator.toList(iter) == Nil: List[Int32]
    }

    @Test
    def unfoldWithOk02(): Bool = region rc {
        let iter = List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: Nil);
        Iterator.unfoldWithOk(rc, () -> {Iterator.next(iter) |> Option.toOk("Done")}) |>
            Iterator.toList == 1 :: 2 :: 3 :: 4 :: 5 :: Nil
        }

    @Test
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

    @Test
    def drop01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.drop(1) |> Iterator.toList == Nil
    }
    @Test
    def drop02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(1) |> Iterator.toList == Nil
    }

    @Test
    def drop03(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(2) |> Iterator.toList == 3 :: Nil
    }

    @Test
    def drop04(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(3) |> Iterator.toList == Nil
    }

    @Test
    def drop05(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(4) |> Iterator.toList == Nil
    }

    @Test
    def drop06(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.drop(0) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def take01(): Bool = region rc {
        List.iterator(rc, Nil: List[Unit]) |> Iterator.take(-1) |> Iterator.toList == Nil
    }

    @Test
    def take02(): Bool = region rc {
        List.iterator(rc, Nil: List[Unit]) |> Iterator.take(0) |> Iterator.toList == Nil
    }

    @Test
    def take03(): Bool = region rc {
        List.iterator(rc, Nil: List[Unit]) |> Iterator.take(1) |> Iterator.toList == Nil
    }

    @Test
    def take04(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.take(-1) |> Iterator.toList == Nil
    }

    @Test
    def take05(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.take(0) |> Iterator.toList == Nil
    }

    @Test
    def take06(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.take(1) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def take07(): Bool = region rc {
        List.iterator(rc, 1 :: Nil) |> Iterator.take(2) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def take08(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |> Iterator.take(0) |> Iterator.toList == Nil
    }

    @Test
    def take09(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |> Iterator.take(1) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def take10(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: Nil) |> Iterator.take(2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @Test
    def take11(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> Iterator.take(2) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @Test
    def take12(): Bool = region rc {
        List.iterator(rc, 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> Iterator.take(4) |> Iterator.toList == 1 :: 2 :: 3 :: 4 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.join(",") == ""
    }

    @Test
    def join02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.join(",") == "1"
    }

    @Test
    def join03(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.join(",") == "1,2,3"
    }

    @Test
    def join04(): Bool = region rc {
        ("1" :: "2" :: "3" :: Nil) |> List.iterator(rc) |>
            Iterator.join(",") == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == ""
    }

    @Test
    def joinWith02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == "2"
    }

    @Test
    def joinWith03(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.joinWith(x -> "${x + 1}", ",") == "2,3,4"
    }

    @Test
    def joinWith04(): Bool = region rc {
        ("1" :: "2" :: "3" :: Nil) |> List.iterator(rc) |>
            Iterator.joinWith(x -> x + x, ",") == "11,22,33"
    }


    /////////////////////////////////////////////////////////////////////////////
    // filterMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def filterMap01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.rem` 2 == 0) Some(x) else None) |> Iterator.toList == Nil
    }
    @Test
    def filterMap02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.rem` 2 == 0) Some(x) else None) |> Iterator.toList == Nil
    }

    @Test
    def filterMap03(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.rem` 2 == 0) Some(x) else None) |> Iterator.toList == 2 :: Nil
    }

    @Test
    def filterMap04(): Bool = region rc {
        (1 :: 2 :: 3 :: 4 :: 5 :: 6 :: Nil) |> List.iterator(rc) |>
            Iterator.filterMap(x -> if (x `Int32.rem` 2 == 0) Some(x) else None) |> Iterator.toList == 2 :: 4 :: 6 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // cons                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def cons01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.cons(1) |> Iterator.toList == 1 :: Nil
    }

    @Test
    def cons02(): Bool = region rc {
        (2 :: Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.cons(1) |> Iterator.toList == 1 :: 2 :: Nil
    }

    @Test
    def cons03(): Bool = region rc {
        (3 :: Nil: List[Int32]) |> List.iterator(rc) |>
            Iterator.cons(2) |> Iterator.cons(1) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Bool = region rc {
        (Nil: List[Int32]) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList == Nil
    }

    @Test
    def flatten02(): Bool = region rc {
        (1 :: Nil) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList == 1 :: 1 :: Nil
    }

    @Test
    def flatten03(): Bool = region rc {
        (1 :: 2 :: Nil) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList == 1 :: 1 :: 2 :: 2 :: Nil
    }

    @Test
    def flatten04(): Bool = region rc {
        (1 :: 2 :: 3 :: Nil) |> List.iterator(rc) |> Iterator.map(i -> Iterator.repeat(rc, 2, i)) |>
            Iterator.flatten |> Iterator.toList == 1 :: 1 :: 2 :: 2 :: 3 :: 3 :: Nil
    }

}
