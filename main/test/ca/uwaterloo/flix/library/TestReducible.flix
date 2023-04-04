/*
 * Copyright 2022 Jakob Schneider Villumsen
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

mod TestReducible {

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool =
        Reducible.reduceLeft((acc, a) -> acc + a, Nel.Nel("a", Nil)) == "a"

    @test
    def reduceLeft02(): Bool =
        Reducible.reduceLeft((acc, a) -> acc + a, Nel.Nel("a", "b" :: Nil)) == "ab"

    @test
    def reduceLeft03(): Bool =
        Reducible.reduceLeft((acc, a) -> acc + a, Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"

    @test
    def reduceLeft04(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(100, Nil)) == 100

    @test
    def reduceLeft05(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(100, 1 :: Nil)) == 198

    @test
    def reduceLeft06(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(100, 1 :: 2 :: Nil)) == 196

    @test
    def reduceLeft07(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(100, 1 :: 2 :: 3 :: Nil)) == 386

    @test
    def reduceLeft08(): Bool = region r {
        let l = ref Nil @ r;
        // `f` uses 0 as a control value, since the first element of a `reduce` is located in `acc`.
        let f = (acc, a) -> {
            if (acc == 0)
                l := a :: deref l
            else
                l := a :: acc :: deref l;
            0
        };
        discard Reducible.reduceLeft(f, Nel.Nel(3, 2 :: 1 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool =
        Reducible.reduceRight((a, acc) -> a + acc, Nel.Nel("a", Nil)) == "a"

    @test
    def reduceRight02(): Bool =
        Reducible.reduceRight((a, acc) -> a + acc, Nel.Nel("a", "b" :: Nil)) == "ab"

    @test
    def reduceRight03(): Bool =
        Reducible.reduceRight((a, acc) -> a + acc, Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"

    @test
    def reduceRight04(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(100, Nil)) == 100

    @test
    def reduceRight05(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(1, 100 :: Nil)) == 198

    @test
    def reduceRight06(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(1, 2 :: 100 :: Nil)) == 194

    @test
    def reduceRight07(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(1, 2 :: 3 :: 100 :: Nil)) == 382

    @test
    def reduceRight08(): Bool = region r {
        let l = ref Nil @ r;
        // `f` uses 0 as a control value, since the first element of a `reduce` is located in `acc`.
        let f = (a, acc) -> {
            if (acc == 0)
                l := a :: deref l
            else
                l := a :: acc :: deref l;
            0
        };
        discard Reducible.reduceRight(f, Nel.Nel(1, 2 :: 3 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // reduce                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduce01(): Bool =
        Reducible.reduce(Nel.Nel("a", Nil)) == "a"

    @test
    def reduce02(): Bool =
        Reducible.reduce(Nel.Nel("a", "b" :: Nil)) == "ab"

    @test
    def reduce03(): Bool =
        Reducible.reduce(Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"


    /////////////////////////////////////////////////////////////////////////////
    // reduceMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceMap01(): Bool =
        Reducible.reduceMap(a -> Int16.toInt32(a), Nel.Nel(1i16, Nil)) == 1

    @test
    def reduceMap02(): Bool =
        Reducible.reduceMap(a -> Int16.toInt32(a), Nel.Nel(1i16, 2i16 :: Nil)) == 3

    @test
    def reduceMap03(): Bool =
        Reducible.reduceMap(a -> Int16.toInt32(a), Nel.Nel(1i16, 2i16 :: 3i16 :: Nil)) == 6


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
        Reducible.foldLeft((acc, a) -> acc + a, "", Nel.Nel("a", Nil)) == "a"

    @test
    def foldLeft02(): Bool =
        Reducible.foldLeft((acc, a) -> acc + a, "", Nel.Nel("a", "b" :: Nil)) == "ab"

    @test
    def foldLeft03(): Bool =
        Reducible.foldLeft((acc, a) -> acc + a, "", Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"

    @test
    def foldLeft04(): Bool =
        Reducible.foldLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, Nil)) == 198

    @test
    def foldLeft05(): Bool =
        Reducible.foldLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: Nil)) == 196

    @test
    def foldLeft06(): Bool =
        Reducible.foldLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: 3 :: Nil)) == 386

    @test
    def foldLeft07(): Bool = region r {
        let l = ref Nil @ r;
        discard Reducible.foldLeft((_, x) -> { l := x :: deref l; x == 1 }, false, Nel.Nel(3, 2 :: 1 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
        Reducible.foldRight((a, acc) -> a + acc, "", Nel.Nel("a", Nil)) == "a"

    @test
    def foldRight02(): Bool =
        Reducible.foldRight((a, acc) -> a + acc, "", Nel.Nel("a", "b" :: Nil)) == "ab"

    @test
    def foldRight03(): Bool =
        Reducible.foldRight((a, acc) -> a + acc, "", Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"

    @test
    def foldRight04(): Bool =
        Reducible.foldRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, Nil)) == 198

    @test
    def foldRight05(): Bool =
        Reducible.foldRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: Nil)) == 194

    @test
    def foldRight06(): Bool =
        Reducible.foldRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: 3 :: Nil)) == 382

    @test
    def foldRight07(): Bool = region r {
        let l = ref Nil @ r;
        discard Reducible.foldRight((x, _) -> { l := x :: deref l; x == 1 }, false, Nel.Nel(1, 2 :: 3 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRightWithCont01(): Bool =
        Reducible.foldRightWithCont((a, k) -> a + k(), "", Nel.Nel("a", Nil)) == "a"

    @test
    def foldRightWithCont02(): Bool =
        Reducible.foldRightWithCont((a, k) -> a + k(), "", Nel.Nel("a", "b" :: Nil)) == "ab"

    @test
    def foldRightWithCont03(): Bool =
        Reducible.foldRightWithCont((a, k) -> a + k(), "", Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"

    @test
    def foldRightWithCont04(): Bool =
        Reducible.foldRightWithCont((a, k) -> (k() - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, Nil)) == 198

    @test
    def foldRightWithCont05(): Bool =
        Reducible.foldRightWithCont((a, k) -> (k() - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: Nil)) == 194

    @test
    def foldRightWithCont06(): Bool =
        Reducible.foldRightWithCont((a, k) -> (k() - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: 3 :: Nil)) == 382

    @test
    def foldRightWithCont07(): Bool = region r {
        let l = ref Nil @ r;
        discard Reducible.foldRightWithCont((x, _) -> { l := x :: deref l; x == 1 }, false, Nel.Nel(1, 2 :: 3 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool =
        Reducible.head(Nel.Nel(1, Nil)) == 1

    @test
    def head02(): Bool =
        Reducible.head(Nel.Nel(2, 1 :: Nil)) == 2

    @test
    def head03(): Bool =
        Reducible.head(Nel.Nel(3, 2 :: 1 :: Nil)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool =
        Reducible.last(Nel.Nel(1, Nil)) == 1

    @test
    def last02(): Bool =
        Reducible.last(Nel.Nel(1, 2 :: Nil)) == 2

    @test
    def last03(): Bool =
        Reducible.last(Nel.Nel(1, 2 :: 3 :: Nil)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def init01(): Bool =
        Reducible.init(Nel.Nel(1, Nil)) == Nil

    @test
    def init02(): Bool =
        Reducible.init(Nel.Nel(2, Nil)) == Nil

    @test
    def init03(): Bool =
        Reducible.init(Nel.Nel(1, 2 :: Nil)) == 1 :: Nil

    @test
    def init04(): Bool =
        Reducible.init(Nel.Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: Nil

    @test
    def init05(): Bool =
        Reducible.init(Nel.Nel(3, 2 :: 1 :: Nil)) == 3 :: 2 :: Nil

    @test
    def init06(): Bool =
        Reducible.init(Nel.Nel(1, 2 :: 3 :: 4 :: Nil)) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def tail01(): Bool =
        Reducible.tail(Nel.Nel(1, Nil)) == Nil

    @test
    def tail02(): Bool =
        Reducible.tail(Nel.Nel(2, Nil)) == Nil

    @test
    def tail03(): Bool =
        Reducible.tail(Nel.Nel(1, 2 :: Nil)) == 2 :: Nil

    @test
    def tail04(): Bool =
        Reducible.tail(Nel.Nel(1, 2 :: 3 :: Nil)) == 2 :: 3 :: Nil

    @test
    def tail05(): Bool =
        Reducible.tail(Nel.Nel(3, 2 :: 1 :: Nil)) == 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reverse01(): Bool =
        Reducible.reverse(Nel.Nel(1, Nil)) == 1 :: Nil

    @test
    def reverse02(): Bool =
        Reducible.reverse(Nel.Nel(1, 2 :: Nil)) == 2 :: 1 :: Nil

    @test
    def reverse03(): Bool =
        Reducible.reverse(Nel.Nel(1, 2 :: 3 :: Nil)) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(1, Nil)) == 0

    @test
    def count02(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(4, Nil)) == 1

    @test
    def count03(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(1, 2 :: Nil)) == 0

    @test
    def count04(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(1, 8 :: Nil)) == 1

    @test
    def count05(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(8, 1 :: Nil)) == 1

    @test
    def count06(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(6, 7 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool =
        Reducible.length(Nel.Nel(1, Nil)) == 1

    @test
    def length02(): Bool =
        Reducible.length(Nel.Nel(1, 2 :: Nil)) == 2

    @test
    def length03(): Bool =
        Reducible.length(Nel.Nel(1, 2 :: 3 :: Nil)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sum01(): Bool =
        Reducible.sum(Nel.Nel(1, Nil)) == 1

    @test
    def sum02(): Bool =
        Reducible.sum(Nel.Nel(1, 2 :: 3 :: Nil)) == 6

    @test
    def sum03(): Bool =
        Reducible.sum(Nel.Nel(1, 2 :: 3 :: -3 :: Nil)) == 3

    @test
    def sum04(): Bool =
        Reducible.sum(Nel.Nel(-1, -2 :: -3 :: -4 :: Nil)) == -10

    @test
    def sum05(): Bool =
        Reducible.sum(Nel.Nel(10, -10 :: Nil)) == 0

    @test
    def sum06(): Bool = match List.range(1, 101) {
        case x :: xs => Reducible.sum(Nel.Nel(x, xs)) == 5050
        case _       => unreachable!()
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def sumWith01(): Bool =
        Reducible.sumWith(x -> x + 1, Nel.Nel(1, Nil)) == 2

    @test
    def sumWith02(): Bool =
        Reducible.sumWith(x -> x + 1, Nel.Nel(1, 2 :: 3 :: Nil)) == 9

    @test
    def sumWith03(): Bool =
        Reducible.sumWith(x -> x + 1, Nel.Nel(1, 2 :: 3 :: -3 :: Nil)) == 7

    @test
    def sumWith04(): Bool =
        Reducible.sumWith(x -> x + 1, Nel.Nel(-1, -2 :: -3 :: -4 :: Nil)) == -6

    @test
    def sumWith05(): Bool =
        Reducible.sumWith(x -> x + 1, Nel.Nel(10, -10 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def exists01(): Bool =
        Reducible.exists(x -> x == 1, Nel.Nel(1, Nil)) == true

    @test
    def exists02(): Bool =
        Reducible.exists(x -> x == 2, Nel.Nel(1, Nil)) == false

    @test
    def exists03(): Bool =
        Reducible.exists(x -> x == 1, Nel.Nel(1, 2 :: Nil)) == true

    @test
    def exists04(): Bool =
        Reducible.exists(x -> x == 2, Nel.Nel(1, 2 :: Nil)) == true


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forAll01(): Bool =
        Reducible.forAll(x -> x == 1, Nel.Nel(1, Nil)) == true

    @test
    def forAll02(): Bool =
        Reducible.forAll(x -> x == 2, Nel.Nel(1, Nil)) == false

    @test
    def forAll03(): Bool =
        Reducible.forAll(x -> x == 1, Nel.Nel(1, 1 :: Nil)) == true

    @test
    def forAll04(): Bool =
        Reducible.forAll(x -> x == 1, Nel.Nel(1, 2 :: Nil)) == false

    @test
    def forAll05(): Bool =
        Reducible.forAll(x -> x == 1, Nel.Nel(2, 1 :: Nil)) == false


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def forEach01(): Bool = region r {
        let ri = ref 21 @ r;
        Reducible.forEach(x -> ri := x, Nel.Nel(1, Nil));
        1 == deref ri
    }

    @test
    def forEach02(): Bool = region r {
        let ri = ref 21 @ r;
        Reducible.forEach(x -> ri := x, Nel.Nel(1, 2 :: 3 :: Nil));
        3 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findLeft01(): Bool =
        Reducible.findLeft(x -> x == 0, Nel.Nel(1, Nil)) == None

    @test
    def findLeft02(): Bool =
        Reducible.findLeft(x -> x == 0, Nel.Nel(1, 2 :: Nil)) == None

    @test
    def findLeft03(): Bool =
        Reducible.findLeft(x -> x == 0, Nel.Nel(1, 2 :: 3 :: Nil)) == None

    @test
    def findLeft04(): Bool =
        Reducible.findLeft(x -> x == 1, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @test
    def findLeft05(): Bool =
        Reducible.findLeft(x -> x == 2, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @test
    def findLeft06(): Bool =
        Reducible.findLeft(x -> x == 3, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(3)

    @test
    def findLeft07(): Bool = region r {
        let l = ref Nil @ r;
        discard Reducible.findLeft(x -> { l := x :: deref l; x == 1 }, Nel.Nel(3, 2 :: 1 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def findRight01(): Bool =
        Reducible.findRight(x -> x == 0, Nel.Nel(1, Nil)) == None

    @test
    def findRight02(): Bool =
        Reducible.findRight(x -> x == 0, Nel.Nel(1, 2 :: Nil)) == None

    @test
    def findRight03(): Bool =
        Reducible.findRight(x -> x == 0, Nel.Nel(1, 2 :: 3 :: Nil)) == None

    @test
    def findRight04(): Bool =
        Reducible.findRight(x -> x == 1, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @test
    def findRight05(): Bool =
        Reducible.findRight(x -> x == 2, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @test
    def findRight06(): Bool =
        Reducible.findRight(x -> x == 3, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(3)

    @test
    def findRight07(): Bool = region r {
        let l = ref Nil @ r;
        discard Reducible.findRight(x -> { l := x :: deref l; x == 1 }, Nel.Nel(1, 2 :: 3 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def memberOf01(): Bool =
        Reducible.memberOf(0, Nel.Nel(1, Nil)) == false

    @test
    def memberOf02(): Bool =
        Reducible.memberOf(0, Nel.Nel(1, 2 :: Nil)) == false

    @test
    def memberOf03(): Bool =
        Reducible.memberOf(0, Nel.Nel(1, 2 :: 3 :: Nil)) == false

    @test
    def memberOf04(): Bool =
        Reducible.memberOf(1, Nel.Nel(1, 2 :: 3 :: Nil))

    @test
    def memberOf05(): Bool =
        Reducible.memberOf(2, Nel.Nel(1, 2 :: 3 :: Nil))

    @test
    def memberOf06(): Bool =
        Reducible.memberOf(3, Nel.Nel(1, 2 :: 3 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimum01(): Bool =
        Reducible.minimum(Nel.Nel(1, Nil)) == 1

    @test
    def minimum02(): Bool =
        Reducible.minimum(Nel.Nel(1, 2 :: 3 :: Nil)) == 1

    @test
    def minimum03(): Bool =
        Reducible.minimum(Nel.Nel(1, 2 :: 0 :: 3 :: Nil)) == 0

    @test
    def minimum04(): Bool =
        Reducible.minimum(Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 3

    @test
    def minimum05(): Bool =
        Reducible.minimum(Nel.Nel(9, 5 :: 4 :: 2 :: 7 :: 10 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def minimumBy01(): Bool =
        Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(1, Nil)) == 1

    @test
    def minimumBy02(): Bool =
        Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 3 :: Nil)) == 1

    @test
    def minimumBy03(): Bool =
        Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 0 :: 3 :: Nil)) == 0

    @test
    def minimumBy04(): Bool =
        Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 3

    @test
    def minimumBy05(): Bool =
        Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(9, 5 :: 4 :: 2 :: 7 :: 10 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximum01(): Bool =
        Reducible.maximum(Nel.Nel(1, Nil)) == 1

    @test
    def maximum02(): Bool =
        Reducible.maximum(Nel.Nel(1, 2 :: 3 :: Nil)) == 3

    @test
    def maximum03(): Bool =
        Reducible.maximum(Nel.Nel(1, 2 :: 0 :: 3 :: Nil)) == 3

    @test
    def maximum04(): Bool =
        Reducible.maximum(Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 9

    @test
    def maximum05(): Bool =
        Reducible.maximum(Nel.Nel(3, 5 :: 4 :: 9 :: 7 :: 10 :: Nil)) == 10


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def maximumBy01(): Bool =
        Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(1, Nil)) == 1

    @test
    def maximumBy02(): Bool =
        Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 3 :: Nil)) == 3

    @test
    def maximumBy03(): Bool =
        Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 0 :: 3 :: Nil)) == 3

    @test
    def maximumBy04(): Bool =
        Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 9

    @test
    def maximumBy05(): Bool =
        Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(3, 5 :: 4 :: 9 :: 7 :: 10 :: Nil)) == 10


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def dropWhile01(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(1, Nil)) == 1 :: Nil

    @test
    def dropWhile02(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(4, Nil)) == Nil

    @test
    def dropWhile03(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(1, 2 :: Nil)) == 1 :: 2 :: Nil

    @test
    def dropWhile04(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(1, 5 :: Nil)) == 1 :: 5 :: Nil

    @test
    def dropWhile05(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(5, 1 :: Nil)) == 1 :: Nil

    @test
    def dropWhile06(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(5, 8 :: Nil)) == Nil

    @test
    def dropWhile07(): Bool =
        Nel.Nel(4, 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Reducible.dropWhile(a -> a > 3) == -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def takeWhile01(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(1, Nil)) == Nil

    @test
    def takeWhile02(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(4, Nil)) == 4 :: Nil

    @test
    def takeWhile03(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(1, 2 :: Nil)) == Nil

    @test
    def takeWhile04(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(1, 5 :: Nil)) == Nil

    @test
    def takeWhile05(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(5, 1 :: Nil)) == 5 :: Nil

    @test
    def takeWhile06(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(5, 8 :: Nil)) == 5 :: 8 :: Nil

    @test
    def takeWhile07(): Bool =
        Nel.Nel(4, 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Reducible.takeWhile(a -> a > 3) == 4 :: 6 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def intersperse01(): Bool =
        Reducible.intersperse(7, Nel.Nel(1, Nil)) == 1 :: Nil

    @test
    def intersperse02(): Bool =
        Reducible.intersperse(7, Nel.Nel(1, 2 :: Nil)) == 1 :: 7 :: 2 :: Nil

    @test
    def intersperse03(): Bool =
        Reducible.intersperse(7, Nel.Nel(1, 2 :: 3 :: Nil)) == 1 :: 7 :: 2 :: 7 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toArray01(): Bool = region r {
        Array.sameElements(Reducible.toArray(r, Nel.Nel(1, Nil)), Array#{1} @ r)
    }

    @test
    def toArray02(): Bool = region r {
        Array.sameElements(Reducible.toArray(r, Nel.Nel(1, 2 :: 3 :: Nil)), Array#{1, 2, 3} @ r)
    }

    @test
    def toArray03(): Bool = region r {
        Array.sameElements(Reducible.toArray(r, Nel.Nel(1, 1 :: 2 :: 3 :: Nil)), Array#{1, 1, 2, 3} @ r)
    }


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toList01(): Bool =
        Reducible.toList(Nel.Nel(1, Nil)) == 1 :: Nil

    @test
    def toList02(): Bool =
        Reducible.toList(Nel.Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: 3 :: Nil

    @test
    def toList03(): Bool =
        Reducible.toList(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)) == 1 :: 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toMap01(): Bool =
        Reducible.toMap(Nel.Nel((1, 1), Nil)) == Map#{1 => 1}

    @test
    def toMap02(): Bool =
        Reducible.toMap(Nel.Nel(("a", 1), ("b", 2) :: ("c", 3) :: Nil)) == Map#{"a" => 1, "b" => 2, "c" => 3}

    @test
    def toMap03(): Bool =
        Reducible.toMap(Nel.Nel((1, 2), (4, 3) :: (2, 2) :: (3, 3) :: Nil)) == Map#{1 => 2, 2 => 2, 3 => 3, 4 => 3}


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toNel01(): Bool =
        Reducible.toNel(Nel.Nel(1, Nil)) == Nel.Nel(1, Nil)

    @test
    def toNel02(): Bool =
        Reducible.toNel(Nel.Nel(1, 2 :: 3 :: Nil)) == Nel.Nel(1, 2 :: 3 :: Nil)

    @test
    def toNel03(): Bool =
        Reducible.toNel(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)) == Nel.Nel(1, 1 :: 2 :: 3 :: Nil)

    @test
    def toNel04(): Bool =
        match List.range(0, 100) {
            case x :: xs => Reducible.toNel(Nel.Nel(x, xs)) |> Nel.toList == List.range(0, 100)
            case _       => unreachable!()
        }


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toSet01(): Bool =
        Reducible.toSet(Nel.Nel(1, Nil)) == Set#{1}

    @test
    def toSet02(): Bool =
        Reducible.toSet(Nel.Nel(1, 2 :: 3 :: Nil)) == Set#{1, 2, 3}

    @test
    def toSet03(): Bool =
        Reducible.toSet(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)) == Set#{1, 2, 3}

}
