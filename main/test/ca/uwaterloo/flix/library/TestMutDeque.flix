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

namespace TestMutDeque {

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def size01(): Bool & Impure =
        MutDeque.size(MutDeque.new()) == 0

    @test
    def size02(): Bool & Impure =
        let d = MutDeque.new();

        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);

        MutDeque.size(d) == 4

    @test
    def size03(): Bool & Impure =
        let d = MutDeque.new();

        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);

        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(1, d);

        MutDeque.size(d) == 8

    @test
    def size04(): Bool & Impure =
        let d = MutDeque.new();

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

    @test
    def size05(): Bool & Impure =
        let d = MutDeque.new();

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        MutDeque.popFront(d);
        MutDeque.popFront(d);

        MutDeque.size(d) == 2


    @test
    def size06(): Bool & Impure =
        let d = MutDeque.new();

        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(1, d);

        MutDeque.popFront(d);
        MutDeque.popFront(d);
        MutDeque.popFront(d);
        MutDeque.popFront(d);

        MutDeque.size(d) == 0

    @test
    def size07(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.popBack(d);
        MutDeque.size(d) == 0

    @test
    def size08(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.popFront(d);
        MutDeque.size(d) == 0


    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isEmpty01(): Bool & Impure =
        MutDeque.isEmpty(MutDeque.new())

    @test
    def isEmpty02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        not (MutDeque.isEmpty(d))

    @test
    def isEmpty03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.popBack(d);
        MutDeque.isEmpty(d)

    @test
    def isEmpty04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        not (MutDeque.isEmpty(d))

    @test
    def isEmpty05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.popFront(d);
        MutDeque.isEmpty(d)


    /////////////////////////////////////////////////////////////////////////////
    // popFront                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def popFront01(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.popFront(d) == None: Option[Int32]

    @test
    def popFront02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.popFront(d) == Some(1)

    @test
    def popFront03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(3, d);
        MutDeque.popFront(d) == Some(3)     and
            MutDeque.popFront(d) == Some(2) and
            MutDeque.popFront(d) == Some(1) and
            MutDeque.popFront(d) == None

    @test
    def popFront04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.popFront(d) == Some(1)     and
            MutDeque.popFront(d) == Some(2) and
            MutDeque.popFront(d) == Some(3) and
            MutDeque.popFront(d) == None

    @test
    def popFront05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.popFront(d) == Some(1)     and
            MutDeque.popFront(d) == Some(2) and
            MutDeque.popFront(d) == None

    @test
    def popFront06(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushFront(2, d);
        MutDeque.popFront(d) == Some(2)     and
            MutDeque.popFront(d) == Some(1) and
            MutDeque.popFront(d) == None


    /////////////////////////////////////////////////////////////////////////////
    // popBack                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def popBack01(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.popBack(d) == None: Option[Int32]

    @test
    def popBack02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.popBack(d) == Some(1)

    @test
    def popBack03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.popBack(d) == Some(3)     and
            MutDeque.popBack(d) == Some(2) and
            MutDeque.popBack(d) == Some(1) and
            MutDeque.popBack(d) == None

    @test
    def popBack04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.pushFront(2, d);
        MutDeque.pushFront(3, d);
        MutDeque.popBack(d) == Some(1)     and
            MutDeque.popBack(d) == Some(2) and
            MutDeque.popBack(d) == Some(3) and
            MutDeque.popBack(d) == None

    @test
    def popBack05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushFront(2, d);
        MutDeque.popBack(d) == Some(1)     and
            MutDeque.popBack(d) == Some(2) and
            MutDeque.popBack(d) == None

    @test
    def popBack06(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.popBack(d) == Some(2)     and
            MutDeque.popBack(d) == Some(1) and
            MutDeque.popBack(d) == None


    /////////////////////////////////////////////////////////////////////////////
    // pushFrontPushBack (expansion)                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pushFrontPushBack01(): Bool & Impure =
        let d = MutDeque.new();
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

    /////////////////////////////////////////////////////////////////////////////
    // sameElements                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sameElements01(): Bool & Impure =
        let d = MutDeque.new(): MutDeque[Int32];
        let d2 = MutDeque.new(): MutDeque[Int32];
        MutDeque.sameElements(d, d2)

    @test
    def sameElements02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);

        let d2 = MutDeque.new();
        MutDeque.pushFront(1, d2);

        MutDeque.sameElements(d, d2)

    @test
    def sameElements03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);

        let d2 = MutDeque.new();
        MutDeque.pushFront(1, d2);
        MutDeque.pushFront(2, d2);

        not MutDeque.sameElements(d, d2)

    @test
    def sameElements04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack("a", d);
        MutDeque.pushBack("b", d);
        MutDeque.pushBack("c", d);

        let d2 = MutDeque.new();
        MutDeque.pushFront("b", d2);
        MutDeque.pushBack("c", d2);
        MutDeque.pushFront("a", d2);

        MutDeque.sameElements(d, d2)

    @test
    def sameElements05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack("a", d);
        MutDeque.pushBack("b", d);
        MutDeque.pushBack("c", d);

        let d2 = MutDeque.new();
        MutDeque.pushFront("a", d2);
        MutDeque.pushFront("b", d2);
        MutDeque.pushBack("c", d2);

        not MutDeque.sameElements(d, d2)


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.sum(d) == 0

    @test
    def sum02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.sum(d) == 1

    @test
    def sum03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.sum(d) == 6

    @test
    def sum04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(-3, d);
        MutDeque.sum(d) == 3

    @test
    def sum05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(-3, d);
        MutDeque.sum(d) == 3

    @test
    def sum06(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(-1, d);
        MutDeque.pushBack(-2, d);
        MutDeque.pushFront(-3, d);
        MutDeque.pushFront(-4, d);
        MutDeque.sum(d) == -10

    @test
    def sum07(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(10, d);
        MutDeque.pushBack(-10, d);
        MutDeque.sum(d) == 0


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.sumWith(x -> x + 1, d) == 0

    @test
    def sumWith02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.sumWith(x -> x + 1, d) == 2

    @test
    def sumWith03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.sumWith(x -> x + 1, d) == 9

    @test
    def sumWith04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(-3, d);
        MutDeque.sumWith(x -> x + 1, d) == 7

    @test
    def sumWith05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(-3, d);
        MutDeque.sumWith(x -> x + 1, d) == 7

    @test
    def sumWith06(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(-1, d);
        MutDeque.pushBack(-2, d);
        MutDeque.pushFront(-3, d);
        MutDeque.pushFront(-4, d);
        MutDeque.sumWith(x -> x + 1, d) == -6

    @test
    def sumWith07(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(10, d);
        MutDeque.pushBack(-10, d);
        MutDeque.sumWith(x -> x + 1, d) == 2

}
