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

    @Test
    def toString01(): Bool = region rc {
        let s: MutSet[Int32, rc] = MutSet.new(rc);
        MutSet.toString(s) == "MutSet#{}"
    }

    @Test
    def toString02(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.toString(s) == "MutSet#{1}"
    }

    @Test
    def toString03(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.add!(3, s);
        MutSet.add!(4, s);
        MutSet.add!(5, s);
        MutSet.toString(s) == "MutSet#{1, 2, 3, 4, 5}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sameElements01(): Bool = region rc {
        let s1 = MutSet.new(rc): MutSet[Int32, _];
        let s2 = MutSet.new(rc): MutSet[Int32, _];
        MutSet.sameElements(s1, s2)
    }

    @Test
    def sameElements02(): Bool = region rc {
        let s1 = MutSet.new(rc);
        MutSet.add!(1, s1);

        let s2 = MutSet.new(rc);
        MutSet.add!(1, s2);

        MutSet.sameElements(s1, s2)
    }

    @Test
    def sameElements03(): Bool = region rc {
        let s1 = MutSet.new(rc);
        MutSet.add!(1, s1);

        let s2 = MutSet.new(rc);
        MutSet.add!(2, s2);

        not MutSet.sameElements(s1, s2)
    }

    @Test
    def sameElements04(): Bool = region rc {
        let s1 = MutSet.new(rc);
        MutSet.add!(1, s1);
        MutSet.add!(2, s1);
        MutSet.add!(3, s1);

        let s2 = MutSet.new(rc);
        MutSet.add!(2, s2);
        MutSet.add!(3, s2);
        MutSet.add!(1, s2);

        MutSet.sameElements(s1, s2)
    }

    @Test
    def sameElements05(): Bool = region rc {
        let s1 = MutSet.new(rc);
        MutSet.add!("a", s1);
        MutSet.add!("b", s1);
        MutSet.add!("c", s1);

        let s2 = MutSet.new(rc);
        MutSet.add!("c", s2);
        MutSet.add!("a", s2);
        MutSet.add!("b", s2);

        MutSet.sameElements(s1, s2)
    }

    @Test
    def setSameElements06(): Bool = region rc {
        let s1 = MutSet.new(rc);
        MutSet.add!("a", s1);
        MutSet.add!("b", s1);
        MutSet.add!("c", s1);

        let s2 = MutSet.new(rc);
        MutSet.add!("c", s2);
        MutSet.add!("a", s2);

        not MutSet.sameElements(s1, s2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // addAll! and mulitple regions                                            //
    /////////////////////////////////////////////////////////////////////////////

    // This section of tests serve two purposes:
    //  1. to test `addAll`
    //  2. to test the use of mulitple regions.
    // Thus not all tests end with a simple comparison of two sets.

    @Test
    def addAll01(): Bool = region rc {
        let s1 = MutSet.new(rc): MutSet[Int32, _];
        let s2 = MutSet.new(rc): MutSet[Int32, _];
        MutSet.addAll!(MutSet.toSet(s1), s2);
        MutSet.isEmpty(s2)
    }

    @Test
    def addAll02(): Bool = region rc {
        let s1 = MutSet.new(rc);
        let s2 = MutSet.new(rc);
        MutSet.add!(1, s1);
        MutSet.add!(1, s1);
        MutSet.addAll!(MutSet.toSet(s1), s2);
        MutSet.sameElements(s1, s2)
    }

    @Test
    def addAll03(): Bool = region rc {
        let s1 = MutSet.new(rc);
        let s2 = MutSet.new(rc);
        MutSet.add!(1, s1);
        MutSet.add!(2, s1);
        MutSet.addAll!(MutSet.toSet(s1), s2);
        MutSet.foldLeft((acc, a) -> acc + a, 0, s2) == 3
    }

    @Test
    def addAll04(): Bool = region rc {
        let s1 = MutSet.new(rc);
        let s2 = MutSet.new(rc);
        MutSet.add!(1, s1);
        MutSet.add!(2, s1);
        MutSet.add!(3, s1);
        MutSet.addAll!(MutSet.toSet(s1), s2);
        MutSet.foldLeft((acc, a) -> acc + a, 0, s2) == 6
    }

    @Test
    def addAll05(): Bool = region rc1 {
        let s1 = MutSet.new(rc1);
        MutSet.add!("a", s1);
        MutSet.add!("b", s1);
        MutSet.add!("c", s1);

        region rc2 {
            let s2 = MutSet.new(rc2);
            MutSet.addAll!(MutSet.toSet(s1), s2);
            MutSet.toList(s2) == "a" :: "b" :: "c" :: Nil
        }
    }

    @Test
    def addAll06(): Bool = region rc {
        let s1 = MutSet.new(rc);
        MutSet.add!("a", s1);
        MutSet.add!("b", s1);
        MutSet.add!("c", s1);

        region rc2 {
            let s2 = MutSet.new(rc2);
            MutSet.addAll!(MutSet.toSet(s1), s2);
            MutSet.isSubsetOf(s1, s2) and MutSet.isSubsetOf(s2, s1)
            }
        }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool = region rc {
        MutSet.new(rc) |> MutSet.sum == 0
    }

    @Test
    def sum02(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.sum(s) == 1
    }

    @Test
    def sum03(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.add!(3, s);
        MutSet.sum(s) == 6
    }

    @Test
    def sum04(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.add!(3, s);
        MutSet.add!(-3, s);
        MutSet.sum(s) == 3
    }

    @Test
    def sum05(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.add!(-3, s);
        MutSet.add!(-4, s);
        MutSet.sum(s) == -4
    }

    @Test
    def sum06(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(10, s);
        MutSet.add!(-10, s);
        MutSet.sum(s) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool = region rc {
        MutSet.new(rc) |> MutSet.sumWith(x -> x + 1) == 0
    }

    @Test
    def sumWith02(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.sumWith(x -> x + 1, s) == 2
    }

    @Test
    def sumWith03(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.add!(3, s);
        MutSet.sumWith(x -> x + 1, s) == 9
    }

    @Test
    def sumWith04(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.add!(3, s);
        MutSet.add!(-3, s);
        MutSet.sumWith(x -> x + 1, s) == 7
    }

    @Test
    def sumWith05(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(-1, s);
        MutSet.add!(-2, s);
        MutSet.add!(-3, s);
        MutSet.add!(-4, s);
        MutSet.sumWith(x -> x + 1, s) == -6
    }

    @Test
    def sumWith06(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(10, s);
        MutSet.add!(-10, s);
        MutSet.sumWith(x -> x + 1, s) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool = region rc {
        let s: MutSet[Int32, _] = MutSet.new(rc);
        MutSet.join(",", s) == ""
    }

    @Test
    def join02(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.join(",", s) == "1"
    }

    @Test
    def join03(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.add!(3, s);
        MutSet.join(",", s) == "1,2,3"
    }

    @Test
    def join04(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!("1", s);
        MutSet.add!("2", s);
        MutSet.add!("3", s);
        MutSet.join(",", s) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool = region rc {
        MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, MutSet.new(rc)) == 100
    }

    @Test
    def foldRight02(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, s) == 198
    }

    @Test
    def foldRight03(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(2, s);
        MutSet.add!(1, s);
        MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, s) == 194
    }

    @Test
    def foldRight04(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(3, s);
        MutSet.add!(2, s);
        MutSet.add!(1, s);
        MutSet.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, s) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool = region rc {
        MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, MutSet.new(rc)) == 100
    }

    @Test
    def foldRightWithCont02(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, s) == 198
    }

    @Test
    def foldRightWithCont03(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(2, s);
        MutSet.add!(1, s);
        MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, s) == 194
    }

    @Test
    def foldRightWithCont04(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(3, s);
        MutSet.add!(2, s);
        MutSet.add!(1, s);
        MutSet.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, s) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool = region rc {
        MutSet.foldMap(x -> 2 * x, MutSet.new(rc): MutSet[Int32, _]) == 0
    }

    @Test
    def foldMap02(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.foldMap(x -> 2 * x, s) == 6
    }

    @Test
    def foldMap03(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!("a", s);
        MutSet.foldMap(x -> if (x == "a") "b" else x, s) == "b"
    }

    @Test
    def foldMap04(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!("a", s);
        MutSet.add!("b", s);
        MutSet.add!("c", s);
        MutSet.foldMap(x -> if (x == "c") "b" else x, s) == "abb"
    }

    @Test
    def foldMap05(): Bool = region rc {
        let s = MutSet.new(rc);
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.add!(3, s);
        MutSet.foldMap(Int32.toString, s) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // toMutDeque                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMutDeque01(): Bool = region rc {
        let s: Set[Int32] = Set#{};
        let d1 = MutSet.toMutDeque(rc, Set.toMutSet(rc, s));

        let d2 = MutDeque.new(rc);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque02(): Bool = region rc {
        let s = Set#{1};
        let d1 = MutSet.toMutDeque(rc, Set.toMutSet(rc, s));

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack(1, d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque03(): Bool = region rc {
        let s = Set#{1, 3, 6};
        let d1 = MutSet.toMutDeque(rc, Set.toMutSet(rc, s));

        let d2 = MutDeque.new(rc);
        MutDeque.pushBack(3, d2);
        MutDeque.pushBack(6, d2);
        MutDeque.pushFront(1, d2);

        d1 `MutDeque.sameElements` d2
    }

    @Test
    def toMutDeque04(): Bool = region rc {
        let s = Set#{7, 1, 3, 6};
        let d1 = MutSet.toMutDeque(rc, Set.toMutSet(rc, s));

        let d2 = MutDeque.new(rc);
        MutDeque.pushFront(7, d2);
        MutDeque.pushFront(6, d2);
        MutDeque.pushFront(3, d2);
        MutDeque.pushFront(1, d2);

        d1 `MutDeque.sameElements` d2
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let s = MutSet.new(rc);
        let ri = ref 21 @ rc;
        MutSet.forEach(x -> ri := x, s);
        21 == deref ri
    }

    @Test
    def forEach02(): Bool = region rc {
        let s = MutSet.new(rc);
        let ri = ref 21 @ rc;
        MutSet.add!(0, s);
        MutSet.forEach(x -> ri := x, s);
        0 == deref ri
    }

    @Test
    def forEach03(): Bool = region rc {
        let s = MutSet.new(rc);
        let ri = ref 21 @ rc;
        MutSet.add!(0, s);
        MutSet.add!(1, s);
        MutSet.add!(2, s);
        MutSet.forEach(x -> ri := x, s);
        2 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let s = MutSet.new(rc);
        let ri = ref 21 @ rc;
        MutSet.forEachWithIndex((i, _) -> ri := i, s);
        21 == deref ri
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let s = MutSet.new(rc);
        let ri = ref 21 @ rc;
        MutSet.add!(100, s);
        MutSet.forEachWithIndex((i, _) -> ri := i, s);
        0 == deref ri
    }

    @Test
    def forEachWithIndex03(): Bool = region rc {
        let s = MutSet.new(rc);
        let ri = ref 21 @ rc;
        MutSet.add!(100, s);
        MutSet.add!(101, s);
        MutSet.add!(102, s);
        MutSet.forEachWithIndex((i, _) -> ri := i, s);
        2 == deref ri
    }

}
