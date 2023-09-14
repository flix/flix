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

mod TestMutDeque {

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Bool = region rc {
        let d: MutDeque[Int32, rc] = MutDeque.new(rc);
        MutDeque.toString(d) == "MutDeque#{}"
    }

    @Test
    def toString02(): Bool = region rc {
        let d: MutDeque[Int32, rc] = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.toString(d) == "MutDeque#{1}"
    }

    @Test
    def toString03(): Bool = region rc {
        let d: MutDeque[Int32, rc] = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(4, d);
        MutDeque.pushBack(5, d);
        MutDeque.toString(d) == "MutDeque#{1, 2, 3, 4, 5}"
    }

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Bool = region rc {
        MutDeque.size(MutDeque.new(rc)) == 0
    }

    @Test
    def size02(): Bool = region rc {
        let d = MutDeque.new(rc);

        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);

        MutDeque.size(d) == 4
    }

    @Test
    def size03(): Bool = region rc {
        let d = MutDeque.new(rc);

        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);

        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);

        MutDeque.size(d) == 8
    }

    @Test
    def size04(): Bool = region rc {
        let d = MutDeque.new(rc);

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        MutDeque.size(d) == 11
    }

    @Test
    def size05(): Bool = region rc {
        let d = MutDeque.new(rc);

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);

        MutDeque.size(d) == 2

    }

    @Test
    def size06(): Bool = region rc {
        let d = MutDeque.new(rc);

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);

        MutDeque.size(d) == 0
    }

    @Test
    def size07(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        discard MutDeque.popBack(d);
        MutDeque.size(d) == 0
    }

    @Test
    def size08(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        discard MutDeque.popFront(d);
        MutDeque.size(d) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Bool = region rc {
        MutDeque.isEmpty(MutDeque.new(rc))
    }

    @Test
    def isEmpty02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        not (MutDeque.isEmpty(d))
    }

    @Test
    def isEmpty03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        discard MutDeque.popBack(d);
        MutDeque.isEmpty(d)
    }

    @Test
    def isEmpty04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        not (MutDeque.isEmpty(d))
    }

    @Test
    def isEmpty05(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        discard MutDeque.popFront(d);
        MutDeque.isEmpty(d)
    }


    /////////////////////////////////////////////////////////////////////////////
    // popFront                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def popFront01(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.popFront(d) == None: Option[Int32]
    }

    @Test
    def popFront02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.popFront(d) == Some(1)
    }

    @Test
    def popFront03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(3, d);
        MutDeque.popFront(d) == Some(3)     and
            MutDeque.popFront(d) == Some(2) and
            MutDeque.popFront(d) == Some(1) and
            MutDeque.popFront(d) == None
    }

    @Test
    def popFront04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.popFront(d) == Some(1)     and
            MutDeque.popFront(d) == Some(2) and
            MutDeque.popFront(d) == Some(3) and
            MutDeque.popFront(d) == None
    }

    @Test
    def popFront05(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.popFront(d) == Some(1)     and
            MutDeque.popFront(d) == Some(2) and
            MutDeque.popFront(d) == None
    }

    @Test
    def popFront06(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushFront(2, d);
        MutDeque.popFront(d) == Some(2)     and
            MutDeque.popFront(d) == Some(1) and
            MutDeque.popFront(d) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // popBack                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def popBack01(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.popBack(d) == None: Option[Int32]
    }

    @Test
    def popBack02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.popBack(d) == Some(1)
    }

    @Test
    def popBack03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.popBack(d) == Some(3)     and
            MutDeque.popBack(d) == Some(2) and
            MutDeque.popBack(d) == Some(1) and
            MutDeque.popBack(d) == None
    }

    @Test
    def popBack04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(3, d);
        MutDeque.popBack(d) == Some(1)     and
            MutDeque.popBack(d) == Some(2) and
            MutDeque.popBack(d) == Some(3) and
            MutDeque.popBack(d) == None
    }

    @Test
    def popBack05(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushFront(2, d);
        MutDeque.popBack(d) == Some(1)     and
            MutDeque.popBack(d) == Some(2) and
            MutDeque.popBack(d) == None
    }

    @Test
    def popBack06(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.popBack(d) == Some(2)     and
            MutDeque.popBack(d) == Some(1) and
            MutDeque.popBack(d) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // pushFrontPushBack (expansion)                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pushFrontPushBack01(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(2, d);
        // [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2]

        MutDeque.popBack(d) == Some(2) and
        MutDeque.popFront(d) == Some(1) and
        MutDeque.popBack(d) == Some(2) and
        MutDeque.popBack(d) == Some(2) and
        MutDeque.popBack(d) == Some(2) and
        MutDeque.popBack(d) == Some(2) and
        MutDeque.popBack(d) == Some(2) and
        MutDeque.popBack(d) == Some(2) and
        MutDeque.popBack(d) == Some(2) and
        MutDeque.popBack(d) == Some(2) and
        MutDeque.popBack(d) == Some(2) and
        MutDeque.popBack(d) == Some(1) and
        MutDeque.popBack(d) == Some(1) and
        MutDeque.popBack(d) == Some(1) and
        MutDeque.popBack(d) == Some(1) and
        MutDeque.popBack(d) == Some(1) and
        MutDeque.popBack(d) == Some(1) and
        MutDeque.popBack(d) == Some(1) and
        MutDeque.popBack(d) == Some(1) and
        MutDeque.popBack(d) == Some(1) and
        MutDeque.popBack(d) == None and
        MutDeque.popFront(d) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // peekFront                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def peekFront01(): Bool = region rc {
        let d = MutDeque.new(rc): MutDeque[Int32, rc];
        MutDeque.peekFront(d) == None
    }

    @Test
    def peekFront02(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        MutDeque.peekFront(d) == Some(1)
    }

    @Test
    def peekFront03(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        MutDeque.peekFront(d) == Some(1) and
        MutDeque.peekFront(d) == Some(1) and
        MutDeque.peekFront(d) == Some(1)
    }

    @Test
    def peekFront04(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);
        MutDeque.peekFront(d) == Some(3)
    }

    @Test
    def peekFront05(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popFront(d);
        discard MutDeque.popBack(d);
        MutDeque.peekFront(d) == Some(2)
    }

    @Test
    def peekFront06(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);
        discard MutDeque.popFront(d);
        MutDeque.peekFront(d) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // peekBack                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def peekBack01(): Bool = region rc {
        let d = MutDeque.new(rc): MutDeque[Int32, rc];
        MutDeque.peekBack(d) == None
    }

    @Test
    def peekBack02(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        MutDeque.peekBack(d) == Some(3)
    }

    @Test
    def peekBack03(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        MutDeque.peekBack(d) == Some(3) and
        MutDeque.peekBack(d) == Some(3) and
        MutDeque.peekBack(d) == Some(3)
    }

    @Test
    def peekBack04(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popBack(d);
        discard MutDeque.popBack(d);
        MutDeque.peekBack(d) == Some(1)
    }

    @Test
    def peekBack05(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popBack(d);
        discard MutDeque.popFront(d);
        MutDeque.peekBack(d) == Some(2)
    }

    @Test
    def peekBack06(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popBack(d);
        discard MutDeque.popBack(d);
        discard MutDeque.popBack(d);
        MutDeque.peekBack(d) == None
    }


    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sameElements01(): Bool = region rc {
        let d1 = MutDeque.new(rc): MutDeque[Int32, _];
        let d2 = MutDeque.new(rc): MutDeque[Int32, _];
        MutDeque.sameElements(d1, d2)
    }

    @Test
    def sameElements02(): Bool = region rc {
        let d1 = MutDeque.new(rc);
        MutDeque.pushBack(1, d1);

        let d2 = MutDeque.new(rc);
        MutDeque.pushFront(1, d2);

        MutDeque.sameElements(d1, d2)
    }

    @Test
    def sameElements03(): Bool = region rc {
        let d1 = MutDeque.new(rc);
        MutDeque.pushBack(1, d1);
        MutDeque.pushBack(2, d1);

        let d2 = MutDeque.new(rc);
        MutDeque.pushFront(1, d2);
        MutDeque.pushFront(2, d2);

        not MutDeque.sameElements(d1, d2)
    }

    @Test
    def sameElements04(): Bool = region rc {
        let d1 = MutDeque.new(rc);
        MutDeque.pushBack("a", d1);
        MutDeque.pushBack("b", d1);
        MutDeque.pushBack("c", d1);

        let d2 = MutDeque.new(rc);
        MutDeque.pushFront("b", d2);
        MutDeque.pushBack("c", d2);
        MutDeque.pushFront("a", d2);

        MutDeque.sameElements(d1, d2)
    }

    @Test
    def sameElements05(): Bool = region rc {
        let d1 = MutDeque.new(rc);
        MutDeque.pushBack("a", d1);
        MutDeque.pushBack("b", d1);
        MutDeque.pushBack("c", d1);

        let d2 = MutDeque.new(rc);
        MutDeque.pushFront("a", d2);
        MutDeque.pushFront("b", d2);
        MutDeque.pushBack("c", d2);

        not MutDeque.sameElements(d1, d2)
    }


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.sum(d) == 0
    }

    @Test
    def sum02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.sum(d) == 1
    }

    @Test
    def sum03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.sum(d) == 6
    }

    @Test
    def sum04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(-3, d);
        MutDeque.sum(d) == 3
    }

    @Test
    def sum05(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(-3, d);
        MutDeque.sum(d) == 3
    }

    @Test
    def sum06(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(-1, d);
        MutDeque.pushBack(-2, d);
        MutDeque.pushFront(-3, d);
        MutDeque.pushFront(-4, d);
        MutDeque.sum(d) == -10
    }

    @Test
    def sum07(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(10, d);
        MutDeque.pushBack(-10, d);
        MutDeque.sum(d) == 0
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.sumWith(x -> x + 1, d) == 0
    }

    @Test
    def sumWith02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.sumWith(x -> x + 1, d) == 2
    }

    @Test
    def sumWith03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.sumWith(x -> x + 1, d) == 9
    }

    @Test
    def sumWith04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(-3, d);
        MutDeque.sumWith(x -> x + 1, d) == 7
    }

    @Test
    def sumWith05(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(-3, d);
        MutDeque.sumWith(x -> x + 1, d) == 7
    }

    @Test
    def sumWith06(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(-1, d);
        MutDeque.pushBack(-2, d);
        MutDeque.pushFront(-3, d);
        MutDeque.pushFront(-4, d);
        MutDeque.sumWith(x -> x + 1, d) == -6
    }

    @Test
    def sumWith07(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(10, d);
        MutDeque.pushBack(-10, d);
        MutDeque.sumWith(x -> x + 1, d) == 2
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool = region rc {
        MutDeque.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, MutDeque.new(rc)) == 100
    }

    @Test
    def foldLeft02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, d) == 198
    }

    @Test
    def foldLeft03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, d) == 198
    }

    @Test
    def foldLeft04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, d) == 196
    }

    @Test
    def foldLeft05(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(1, d);
        MutDeque.foldLeft((i, e) -> (i - e) * (e `Int32.rem` 2 + 1), 100, d) == 386
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool = region rc {
        MutDeque.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, MutDeque.new(rc)) == 100
    }

    @Test
    def foldRight02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, d) == 198
    }

    @Test
    def foldRight03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, d) == 198
    }

    @Test
    def foldRight04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, d) == 194
    }

    @Test
    def foldRight05(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(1, d);
        MutDeque.foldRight((e, acc) -> (acc - e) * (e `Int32.rem` 2 + 1), 100, d) == 382
    }


    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool = region rc {
        MutDeque.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, MutDeque.new(rc)) == 100
    }

    @Test
    def foldRightWithCont02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, d) == 198
    }

    @Test
    def foldRightWithCont03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, d) == 198
    }

    @Test
    def foldRightWithCont04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, d) == 194
    }

    @Test
    def foldRightWithCont05(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(1, d);
        MutDeque.foldRightWithCont((e, k) -> (k() - e) * (e `Int32.rem` 2 + 1), 100, d) == 382
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldMap                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldMap01(): Bool = region rc {
        MutDeque.foldMap(x -> 2 * x, MutDeque.new(rc)) == 0
    }

    @Test
    def foldMap02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.foldMap(x -> 2 * x, d) == 2
    }

    @Test
    def foldMap03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.foldMap(x -> 2 * x, d) == 6
    }

    @Test
    def foldMap04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(1, d);
        MutDeque.foldMap(x -> 2 * x, d) == 12
    }

    @Test
    def foldMap05(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushFront("c", d);
        MutDeque.pushFront("b", d);
        MutDeque.pushFront("a", d);
        MutDeque.foldMap(x -> if (x == "c") "b" else x, d) == "abb"
    }

    @Test
    def foldMap06(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.foldMap(Int32.toString, d) == "123"
    }


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Bool = region rc {
        MutDeque.toList(MutDeque.new(rc): MutDeque[Int32, _]) == Nil: List[Int32]
    }

    @Test
    def toList02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.toList(d) == 1 :: Nil
    }

    @Test
    def toList03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.toList(d) == 1 :: 2 :: Nil
    }

    @Test
    def toList04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.toList(d) == 1 :: 2 :: 3 :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Bool = region rc {
        let d = MutDeque.new(rc): MutDeque[Int32, _];

        let a = MutDeque.toArray(rc, d);
        Array.sameElements(a, Array#{} @ rc)
    }

    @Test
    def toArray02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);

        let a = MutDeque.toArray(rc, d);
        Array.sameElements(a, Array#{1} @ rc)
    }

    @Test
    def toArray03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);

        let a = MutDeque.toArray(rc, d);
        Array.sameElements(a, Array#{1, 2} @ rc)
    }

    @Test
    def toArray04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(3, d);
        MutDeque.pushFront(4, d);

        let a = MutDeque.toArray(rc, d);
        Array.sameElements(a, Array#{4, 3, 1, 2} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Bool = region rc {
        let d = MutDeque.new(rc): MutDeque[Int32, _];

        let v = MutDeque.toVector(d);
        v == Vector#{}
    }

    @Test
    def toVector02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);

        let v = MutDeque.toVector(d);
        v == Vector#{1}
    }

    @Test
    def toVector03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);

        let v = MutDeque.toVector(d);
        v == Vector#{1, 2}
    }

    @Test
    def toVector04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(3, d);
        MutDeque.pushFront(4, d);

        let v = MutDeque.toVector(d);
        v == Vector#{4, 3, 1, 2}
    }


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def join01(): Bool = region rc {
        MutDeque.new(rc): MutDeque[Int32, _] |> MutDeque.join(",") == ""
    }

    @Test
    def join02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.join(",", d) == "1"
    }

    @Test
    def join03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.join(",", d) == "1,2,3"
    }

    @Test
    def join04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack("1", d);
        MutDeque.pushBack("2", d);
        MutDeque.pushBack("3", d);
        MutDeque.join(",", d) == "1,2,3"
    }


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def joinWith01(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.joinWith(x -> "${x + 1}", ",", d) == ""
    }

    @Test
    def joinWith02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.joinWith(x -> "${x + 1}", ",", d) == "2"
    }

    @Test
    def joinWith03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.joinWith(x -> "${x + 1}", ",", d) == "2,3,4"
    }

    @Test
    def joinWith04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack("1", d);
        MutDeque.pushBack("2", d);
        MutDeque.pushBack("3", d);
        MutDeque.joinWith(x -> x + x, ",", d) == "11,22,33"
    }


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        let d = MutDeque.new(rc): MutDeque[Int32, rc];
        let iter = MutDeque.iterator(rc, d);
        match Iterator.next(iter) {
            case None => true
            case _    => false
        }
    }

    @Test
    def iterator02(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        let iter = MutDeque.iterator(rc, d);
        match Iterator.next(iter) {
            case Some(_) => true
            case _       => false
        }
    }

    @Test
    def iterator03(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        discard MutDeque.popBack(d);
        discard MutDeque.popBack(d);
        discard MutDeque.popBack(d);
        let iter = MutDeque.iterator(rc, d);
        match Iterator.next(iter) {
            case None => true
            case _    => false
        }
    }

    @Test
    def iterator04(): Bool = region rc {
        let d = List.toMutDeque(rc, 1 :: 2 :: 3 :: Nil);
        MutDeque.iterator(rc, d) |> Iterator.toList == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def iterator05(): Bool = region rc {
        let d = List.range(0, 100) |> List.toMutDeque(rc);
        MutDeque.iterator(rc, d) |> Iterator.toList == List.range(0, 100) and
        MutDeque.toList(d) == List.range(0, 100)
    }

    @Test
    def iterator06(): Bool = region rc {
        let d = List.range(0, 100) |> List.toMutDeque(rc);
        let iter = MutDeque.iterator(rc, d);
        MutDeque.forEach(_ -> discard Iterator.next(iter), MutList.range(rc, 0, 50) |> MutList.toMutDeque(rc));
        iter |> Iterator.toList == List.range(50, 100) and
        MutDeque.toList(d) == List.range(0, 100)
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let d = MutDeque.new(rc);
        let l = ref Nil @ rc;
        let fn = x -> if (x > 0) l := "T" :: deref l else l := "F" :: deref l;
        MutDeque.forEach(fn, d);
        List.reverse(deref l) |> List.join("") == ""
    }

    @Test
    def forEach02(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(0, d);
        let l = ref Nil @ rc;
        let fn = x -> if (x > 0) l := "T" :: deref l else l := "F" :: deref l;
        MutDeque.forEach(fn, d);
        List.reverse(deref l) |> List.join("") == "F"
    }

    @Test
    def forEach03(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        let l = ref Nil @ rc;
        let fn = x -> if (x > 0) l := "T" :: deref l else l := "F" :: deref l;
        MutDeque.forEach(fn, d);
        List.reverse(deref l) |> List.join("") == "T"
    }

    @Test
    def forEach04(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(0, d);
        MutDeque.pushBack(1, d);
        let l = ref Nil @ rc;
        let fn = x -> if (x > 0) l := "T" :: deref l else l := "F" :: deref l;
        MutDeque.forEach(fn, d);
        List.reverse(deref l) |> List.join("") == "FT"
    }

    @Test
    def forEach05(): Bool = region rc {
        let d = MutDeque.new(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushFront(0, d);
        let l = ref Nil @ rc;
        let fn = x -> if (x > 0) l := "T" :: deref l else l := "F" :: deref l;
        MutDeque.forEach(fn, d);
        List.reverse(deref l) |> List.join("") == "FT"
    }

    /////////////////////////////////////////////////////////////////////////////
    // forEachWithIndex                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEachWithIndex01(): Bool = region rc {
        let d = MutDeque.new(rc);
        let ri = ref 21 @ rc;
        MutDeque.forEachWithIndex((i, _) -> ri := i, d);
        21 == deref ri
    }

    @Test
    def forEachWithIndex02(): Bool = region rc {
        let d = MutDeque.new(rc);
        let ri = ref 21 @ rc;
        MutDeque.pushBack(0, d);
        MutDeque.forEachWithIndex((i, _) -> ri := i, d);
        0 == deref ri
    }

    @Test
    def forEachWithIndex03(): Bool = region rc {
        let d = MutDeque.new(rc);
        let ri = ref 21 @ rc;
        MutDeque.pushBack(0, d);
        MutDeque.pushBack(1, d);
        MutDeque.forEachWithIndex((i, _) -> ri := i, d);
        1 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // shuffle                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def shuffle01(): Bool \ NonDet = region rc {
        let l: List[Int32] = Nil;
        let d1 = List.toMutDeque(rc, l);
        let d2 = MutDeque.shuffle(rc, Random.new(), d1);

        MutDeque.size(d2) == 0 and d2 |> MutDeque.toList |> List.toSet == Set#{}
    }

    @Test
    def shuffle02(): Bool \ NonDet = region rc {
        let l = 1 :: Nil;
        let d1 = List.toMutDeque(rc, l);
        let d2 = MutDeque.shuffle(rc, Random.new(), d1);

        MutDeque.size(d2) == 1 and d2 |> MutDeque.toList |> List.toSet == Set#{1}
    }

    @Test
    def shuffle03(): Bool \ NonDet = region rc {
        let l = 1 :: 2 :: 3 :: Nil;
        let d1 = List.toMutDeque(rc, l);
        let d2 = MutDeque.shuffle(rc, Random.new(), d1);

        MutDeque.size(d2) == 3 and d2 |> MutDeque.toList |> List.toSet == Set#{1, 2, 3}
    }

    @Test
    def shuffle04(): Bool \ NonDet = region rc {
        let l = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: Nil;
        let d1 = List.toMutDeque(rc, l);
        let d2 = MutDeque.shuffle(rc, Random.new(), d1);

        MutDeque.size(d2) == 10 and
            d2 |> MutDeque.toList |> List.toSet == Set#{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
    }

}
