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

    /////////////////////////////////////////////////////////////////////////////
    // product                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def product01(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.product(d) == 0

    @test
    def product02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.product(d) == 1

    @test
    def product03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.product(d) == 6

    @test
    def product04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(-3, d);
        MutDeque.product(d) == -18

    @test
    def product05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(-3, d);
        MutDeque.product(d) == -18

    @test
    def product06(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(-1, d);
        MutDeque.pushBack(-2, d);
        MutDeque.pushFront(-3, d);
        MutDeque.pushFront(-4, d);
        MutDeque.product(d) == 24

    @test
    def product07(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(10, d);
        MutDeque.pushBack(-10, d);
        MutDeque.product(d) == -100


    /////////////////////////////////////////////////////////////////////////////
    // productWith                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def productWith01(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.productWith(x -> x + 1, d) == 0

    @test
    def productWith02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.productWith(x -> x + 1, d) == 2

    @test
    def productWith03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.productWith(x -> x + 1, d) == 24

    @test
    def productWith04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushBack(-3, d);
        MutDeque.productWith(x -> x + 1, d) == -48

    @test
    def productWith05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(-3, d);
        MutDeque.productWith(x -> x + 1, d) == -48

    @test
    def productWith06(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(-2, d);
        MutDeque.pushBack(-3, d);
        MutDeque.pushFront(-4, d);
        MutDeque.pushFront(-5, d);
        MutDeque.productWith(x -> x + 1, d) == 24

    @test
    def productWith07(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(10, d);
        MutDeque.pushBack(-10, d);
        MutDeque.productWith(x -> x + 1, d) == -99


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool & Impure =
        MutDeque.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, MutDeque.new()) == 100

    @test
    def foldLeft02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 198

    @test
    def foldLeft03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 198

    @test
    def foldLeft04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 196

    @test
    def foldLeft05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(1, d);
        MutDeque.foldLeft((i, e) -> (i - e) * (e rem 2 + 1), 100, d) == 386


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool & Impure =
        MutDeque.foldRight((e, k) -> (k() - e) * (e rem 2 + 1), 100, MutDeque.new()) == 100

    @test
    def foldRight02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.foldRight((e, k) -> (k() - e) * (e rem 2 + 1), 100, d) == 198

    @test
    def foldRight03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.foldRight((e, k) -> (k() - e) * (e rem 2 + 1), 100, d) == 198

    @test
    def foldRight04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushFront(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.foldRight((e, k) -> (k() - e) * (e rem 2 + 1), 100, d) == 194

    @test
    def foldRight05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.pushFront(1, d);
        MutDeque.foldRight((e, k) -> (k() - e) * (e rem 2 + 1), 100, d) == 382


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool & Impure =
        MutDeque.toList(MutDeque.new(): MutDeque[Int32]) == Nil: List[Int32]

    @test
    def toList02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.toList(d) == 1 :: Nil

    @test
    def toList03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.toList(d) == 1 :: 2 :: Nil

    @test
    def toList04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushBack(3, d);
        MutDeque.toList(d) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // join                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def join01(): Bool & Impure =
        (Nil: List[Int32]) |> List.toMutDeque |> MutDeque.join(",") == ""

    @test
    def join02(): Bool & Impure =
        (1 :: Nil) |> List.toMutDeque |> MutDeque.join(",") == "1"

    @test
    def join03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toMutDeque |> MutDeque.join(",") == "1,2,3"

    @test
    def join04(): Bool & Impure =
        ("1" :: "2" :: "3" :: Nil) |> List.toMutDeque |> MutDeque.join(",") == "1,2,3"


    /////////////////////////////////////////////////////////////////////////////
    // joinWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def joinWith01(): Bool & Impure =
        (Nil: List[Int32]) |> List.toMutDeque |>
            MutDeque.joinWith(x -> "${x + 1}", ",") == ""

    @test
    def joinWith02(): Bool & Impure =
        (1 :: Nil) |> List.toMutDeque |>
            MutDeque.joinWith(x -> "${x + 1}", ",") == "2"

    @test
    def joinWith03(): Bool & Impure =
        (1 :: 2 :: 3 :: Nil) |> List.toMutDeque |>
            MutDeque.joinWith(x -> "${x + 1}", ",") == "2,3,4"

    @test
    def joinWith04(): Bool & Impure =
        ("1" :: "2" :: "3" :: Nil) |> List.toMutDeque |>
            MutDeque.joinWith(x -> x + x, ",") == "11,22,33"

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def iterator01(): Bool & Impure =
        match MutDeque.new() |> Iterable.iterator {
            case Iterator(done, _) => done() == true
        }

    @test
    def iterator02(): Bool & Impure =
        let m = MutDeque.new();
        MutDeque.pushBack(1, m);
        MutDeque.pushBack(2, m);

        match m |> Iterable.iterator {
            case Iterator(done, _) => done() == false
        }

    @test
    def iterator03(): Bool & Impure =
        let m = MutDeque.new();
        MutDeque.pushBack(1, m);

        match m |> Iterable.iterator {
            case Iterator(_, next) => next() == 1
        }

    @test
    def iterator04(): Bool & Impure =
        let m = MutDeque.new();
        MutDeque.pushBack(1, m);

        match m |> Iterable.iterator {
            case Iterator(done, next) =>
                next();
                done() == true
        }

    @test
    def iterator05(): Bool & Impure =
        let m = MutDeque.new();
        MutDeque.pushFront(2, m);
        MutDeque.pushFront(1, m);

        match m |> Iterable.iterator {
            case Iterator(_, next) =>
                next();
                next() == 2
        }

    /////////////////////////////////////////////////////////////////////////////
    // foreach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foreach01(): Bool & Impure =
        let d = MutDeque.new();
        let sb = StringBuilder.new();
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutDeque.foreach(fn, d);
        StringBuilder.toString(sb) == ""

    @test
    def foreach02(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(0, d);

        let sb = StringBuilder.new();
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutDeque.foreach(fn, d);
        StringBuilder.toString(sb) == "F"

    @test
    def foreach03(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        let sb = StringBuilder.new();
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutDeque.foreach(fn, d);
        StringBuilder.toString(sb) == "T"

    @test
    def foreach04(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(0, d);
        MutDeque.pushBack(1, d);
        let sb = StringBuilder.new();
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutDeque.foreach(fn, d);
        StringBuilder.toString(sb) == "FT"

    @test
    def foreach05(): Bool & Impure =
        let d = MutDeque.new();
        MutDeque.pushBack(1, d);
        MutDeque.pushFront(0, d);
        let sb = StringBuilder.new();
        let fn = x -> if (x > 0) StringBuilder.append!('T', sb) else StringBuilder.append!('F', sb);
        MutDeque.foreach(fn, d);
        StringBuilder.toString(sb) == "FT"

}
