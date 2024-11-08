/*
 * Copyright 2020 Esben Bjerre
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

mod TestMutSet {

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool = region rc {
        let s: MutSet[Int32, rc] = MutSet.empty(rc);
        MutSet.toString(s) == "MutSet#{}"
    }

    @test
    def toString02(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.toString(s) == "MutSet#{1}"
    }

    @test
    def toString03(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        MutSet.add(4, s);
        MutSet.add(5, s);
        MutSet.toString(s) == "MutSet#{1, 2, 3, 4, 5}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool = region rc {
        let s1 = (MutSet.empty(rc): MutSet[Int32, _]);
        let s2 = (MutSet.empty(rc): MutSet[Int32, _]);
        MutSet.sameElements(s1, s2)
    }

    @test
    def sameElements02(): Bool = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add(1, s1);

        let s2 = MutSet.empty(rc);
        MutSet.add(1, s2);

        MutSet.sameElements(s1, s2)
    }

    @test
    def sameElements03(): Bool = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add(1, s1);

        let s2 = MutSet.empty(rc);
        MutSet.add(2, s2);

        not MutSet.sameElements(s1, s2)
    }

    @test
    def sameElements04(): Bool = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add(1, s1);
        MutSet.add(2, s1);
        MutSet.add(3, s1);

        let s2 = MutSet.empty(rc);
        MutSet.add(2, s2);
        MutSet.add(3, s2);
        MutSet.add(1, s2);

        MutSet.sameElements(s1, s2)
    }

    @test
    def sameElements05(): Bool = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add("a", s1);
        MutSet.add("b", s1);
        MutSet.add("c", s1);

        let s2 = MutSet.empty(rc);
        MutSet.add("c", s2);
        MutSet.add("a", s2);
        MutSet.add("b", s2);

        MutSet.sameElements(s1, s2)
    }

    @test
    def setSameElements06(): Bool = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add("a", s1);
        MutSet.add("b", s1);
        MutSet.add("c", s1);

        let s2 = MutSet.empty(rc);
        MutSet.add("c", s2);
        MutSet.add("a", s2);

        not MutSet.sameElements(s1, s2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // addAll! and mulitple regions                                            //
    /////////////////////////////////////////////////////////////////////////////

    // This section of tests serve two purposes:
    //  1. to test `addAll`
    //  2. to test the use of mulitple regions.
    // Thus not all tests end with a simple comparison of two sets.

    @test
    def addAll01(): Bool = region rc {
        let s1 = (MutSet.empty(rc): MutSet[Int32, _]);
        let s2 = (MutSet.empty(rc): MutSet[Int32, _]);
        MutSet.addAll(MutSet.toSet(s1), s2);
        MutSet.isEmpty(s2)
    }

    @test
    def addAll02(): Bool = region rc {
        let s1 = MutSet.empty(rc);
        let s2 = MutSet.empty(rc);
        MutSet.add(1, s1);
        MutSet.add(1, s1);
        MutSet.addAll(MutSet.toSet(s1), s2);
        MutSet.sameElements(s1, s2)
    }

    @test
    def addAll03(): Bool = region rc {
        let s1 = MutSet.empty(rc);
        let s2 = MutSet.empty(rc);
        MutSet.add(1, s1);
        MutSet.add(2, s1);
        MutSet.addAll(MutSet.toSet(s1), s2);
        MutSet.foldLeft((acc, a) -> acc + a, 0, s2) == 3
    }

    @test
    def addAll04(): Bool = region rc {
        let s1 = MutSet.empty(rc);
        let s2 = MutSet.empty(rc);
        MutSet.add(1, s1);
        MutSet.add(2, s1);
        MutSet.add(3, s1);
        MutSet.addAll(MutSet.toSet(s1), s2);
        MutSet.foldLeft((acc, a) -> acc + a, 0, s2) == 6
    }

    @test
    def addAll05(): Bool = region rc1 {
        let s1 = MutSet.empty(rc1);
        MutSet.add("a", s1);
        MutSet.add("b", s1);
        MutSet.add("c", s1);

        region rc2 {
            let s2 = MutSet.empty(rc2);
            MutSet.addAll(MutSet.toSet(s1), s2);
            MutSet.toList(s2) == "a" :: "b" :: "c" :: Nil
        }
    }

    @test
    def addAll06(): Bool = region rc {
        let s1 = MutSet.empty(rc);
        MutSet.add("a", s1);
        MutSet.add("b", s1);
        MutSet.add("c", s1);

        region rc2 {
            let s2 = MutSet.empty(rc2);
            MutSet.addAll(MutSet.toSet(s1), s2);
            MutSet.isSubsetOf(s1, s2) and MutSet.isSubsetOf(s2, s1)
            }
        }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool = region rc {
        MutSet.empty(rc) |> MutSet.sum == 0
    }

    @test
    def sum02(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.sum(s) == 1
    }

    @test
    def sum03(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        MutSet.sum(s) == 6
    }

    @test
    def sum04(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        MutSet.add(-3, s);
        MutSet.sum(s) == 3
    }

    @test
    def sum05(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(-3, s);
        MutSet.add(-4, s);
        MutSet.sum(s) == -4
    }

    @test
    def sum06(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(10, s);
        MutSet.add(-10, s);
        MutSet.sum(s) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool = region rc {
        MutSet.empty(rc) |> MutSet.sumWith(x -> x + 1) == 0
    }

    @test
    def sumWith02(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.sumWith(x -> x + 1, s) == 2
    }

    @test
    def sumWith03(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        MutSet.sumWith(x -> x + 1, s) == 9
    }

    @test
    def sumWith04(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        MutSet.add(-3, s);
        MutSet.sumWith(x -> x + 1, s) == 7
    }

    @test
    def sumWith05(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(-1, s);
        MutSet.add(-2, s);
        MutSet.add(-3, s);
        MutSet.add(-4, s);
        MutSet.sumWith(x -> x + 1, s) == -6
    }

    @test
    def sumWith06(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(10, s);
        MutSet.add(-10, s);
        MutSet.sumWith(x -> x + 1, s) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool = region rc {
        let s: MutSet[Int32, _] = MutSet.empty(rc);
        MutSet.join(",", s) == ""
    }

    @test
    def join02(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.join(",", s) == "1"
    }

    @test
    def join03(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        MutSet.join(",", s) == "1,2,3"
    }

    @test
    def join04(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add("1", s);
        MutSet.add("2", s);
        MutSet.add("3", s);
        MutSet.join(",", s) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool = region rc {
        MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, MutSet.empty(rc)) == 100
    }

    @test
    def foldRight02(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, s) == 198
    }

    @test
    def foldRight03(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(2, s);
        MutSet.add(1, s);
        MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, s) == 194
    }

    @test
    def foldRight04(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(3, s);
        MutSet.add(2, s);
        MutSet.add(1, s);
        MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.remainder` 2 + 1), 100, s) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool = region rc {
        MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, MutSet.empty(rc)) == 100
    }

    @test
    def foldRightWithCont02(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, s) == 198
    }

    @test
    def foldRightWithCont03(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(2, s);
        MutSet.add(1, s);
        MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, s) == 194
    }

    @test
    def foldRightWithCont04(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(3, s);
        MutSet.add(2, s);
        MutSet.add(1, s);
        MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.remainder` 2 + 1), 100, s) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldMap01(): Bool = region rc {
        MutSet.foldMap(x -> 2 * x, (MutSet.empty(rc): MutSet[Int32, _])) == 0
    }

    @test
    def foldMap02(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.foldMap(x -> 2 * x, s) == 6
    }

    @test
    def foldMap03(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add("a", s);
        MutSet.foldMap(x -> if (x == "a") "b" else x, s) == "b"
    }

    @test
    def foldMap04(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add("a", s);
        MutSet.add("b", s);
        MutSet.add("c", s);
        MutSet.foldMap(x -> if (x == "c") "b" else x, s) == "abb"
    }

    @test
    def foldMap05(): Bool = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.add(3, s);
        MutSet.foldMap(Int32.toString, s) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMutDeque01(): Bool = region rc {
        let s: Set[Int32] = Set#{};
        let d1 = MutSet.toMutDeque(rc, Set.toMutSet(rc, s));

        let d2 = MutDeque.empty(rc);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque02(): Bool = region rc {
        let s = Set#{1};
        let d1 = MutSet.toMutDeque(rc, Set.toMutSet(rc, s));

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(1, d2);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque03(): Bool = region rc {
        let s = Set#{1, 3, 6};
        let d1 = MutSet.toMutDeque(rc, Set.toMutSet(rc, s));

        let d2 = MutDeque.empty(rc);
        MutDeque.pushBack(3, d2);
        MutDeque.pushBack(6, d2);
        MutDeque.pushFront(1, d2);

        d1 `MutDeque.sameElements` d2
    }

    @test
    def toMutDeque04(): Bool = region rc {
        let s = Set#{7, 1, 3, 6};
        let d1 = MutSet.toMutDeque(rc, Set.toMutSet(rc, s));

        let d2 = MutDeque.empty(rc);
        MutDeque.pushFront(7, d2);
        MutDeque.pushFront(6, d2);
        MutDeque.pushFront(3, d2);
        MutDeque.pushFront(1, d2);

        d1 `MutDeque.sameElements` d2
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.forEach(x -> Ref.put(x, ri), s);
        21 == Ref.get(ri)
    }

    @test
    def forEach02(): Bool = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.add(0, s);
        MutSet.forEach(x -> Ref.put(x, ri), s);
        0 == Ref.get(ri)
    }

    @test
    def forEach03(): Bool = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.add(0, s);
        MutSet.add(1, s);
        MutSet.add(2, s);
        MutSet.forEach(x -> Ref.put(x, ri), s);
        2 == Ref.get(ri)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEachWithIndex01(): Bool = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.forEachWithIndex((i, _) -> Ref.put(i, ri), s);
        21 == Ref.get(ri)
    }

    @test
    def forEachWithIndex02(): Bool = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.add(100, s);
        MutSet.forEachWithIndex((i, _) -> Ref.put(i, ri), s);
        0 == Ref.get(ri)
    }

    @test
    def forEachWithIndex03(): Bool = region rc {
        let s = MutSet.empty(rc);
        let ri = Ref.fresh(rc, 21);
        MutSet.add(100, s);
        MutSet.add(101, s);
        MutSet.add(102, s);
        MutSet.forEachWithIndex((i, _) -> Ref.put(i, ri), s);
        2 == Ref.get(ri)
    }

}
