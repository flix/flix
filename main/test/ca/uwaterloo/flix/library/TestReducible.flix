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

    @Test
    def reduceLeft01(): Bool =
        Reducible.reduceLeft((acc, a) -> acc + a, Nel.Nel("a", Nil)) == "a"

    @Test
    def reduceLeft02(): Bool =
        Reducible.reduceLeft((acc, a) -> acc + a, Nel.Nel("a", "b" :: Nil)) == "ab"

    @Test
    def reduceLeft03(): Bool =
        Reducible.reduceLeft((acc, a) -> acc + a, Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"

    @Test
    def reduceLeft04(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(100, Nil)) == 100

    @Test
    def reduceLeft05(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(100, 1 :: Nil)) == 198

    @Test
    def reduceLeft06(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(100, 1 :: 2 :: Nil)) == 196

    @Test
    def reduceLeft07(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(100, 1 :: 2 :: 3 :: Nil)) == 386

    @Test
    def reduceLeft08(): Bool = region rc {
        let l = ref Nil @ rc;
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

    @Test
    def reduceRight01(): Bool =
        Reducible.reduceRight((a, acc) -> a + acc, Nel.Nel("a", Nil)) == "a"

    @Test
    def reduceRight02(): Bool =
        Reducible.reduceRight((a, acc) -> a + acc, Nel.Nel("a", "b" :: Nil)) == "ab"

    @Test
    def reduceRight03(): Bool =
        Reducible.reduceRight((a, acc) -> a + acc, Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"

    @Test
    def reduceRight04(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(100, Nil)) == 100

    @Test
    def reduceRight05(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(1, 100 :: Nil)) == 198

    @Test
    def reduceRight06(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(1, 2 :: 100 :: Nil)) == 194

    @Test
    def reduceRight07(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), Nel.Nel(1, 2 :: 3 :: 100 :: Nil)) == 382

    @Test
    def reduceRight08(): Bool = region rc {
        let l = ref Nil @ rc;
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

    @Test
    def reduce01(): Bool =
        Reducible.reduce(Nel.Nel("a", Nil)) == "a"

    @Test
    def reduce02(): Bool =
        Reducible.reduce(Nel.Nel("a", "b" :: Nil)) == "ab"

    @Test
    def reduce03(): Bool =
        Reducible.reduce(Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"


    /////////////////////////////////////////////////////////////////////////////
    // reduceMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reduceMap01(): Bool =
        Reducible.reduceMap(a -> Int16.toInt32(a), Nel.Nel(1i16, Nil)) == 1

    @Test
    def reduceMap02(): Bool =
        Reducible.reduceMap(a -> Int16.toInt32(a), Nel.Nel(1i16, 2i16 :: Nil)) == 3

    @Test
    def reduceMap03(): Bool =
        Reducible.reduceMap(a -> Int16.toInt32(a), Nel.Nel(1i16, 2i16 :: 3i16 :: Nil)) == 6


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Bool =
        Reducible.foldLeft((acc, a) -> acc + a, "", Nel.Nel("a", Nil)) == "a"

    @Test
    def foldLeft02(): Bool =
        Reducible.foldLeft((acc, a) -> acc + a, "", Nel.Nel("a", "b" :: Nil)) == "ab"

    @Test
    def foldLeft03(): Bool =
        Reducible.foldLeft((acc, a) -> acc + a, "", Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"

    @Test
    def foldLeft04(): Bool =
        Reducible.foldLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, Nil)) == 198

    @Test
    def foldLeft05(): Bool =
        Reducible.foldLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: Nil)) == 196

    @Test
    def foldLeft06(): Bool =
        Reducible.foldLeft((acc, a) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: 3 :: Nil)) == 386

    @Test
    def foldLeft07(): Bool = region rc {
        let l = ref Nil @ rc;
        discard Reducible.foldLeft((_, x) -> { l := x :: deref l; x == 1 }, false, Nel.Nel(3, 2 :: 1 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Bool =
        Reducible.foldRight((a, acc) -> a + acc, "", Nel.Nel("a", Nil)) == "a"

    @Test
    def foldRight02(): Bool =
        Reducible.foldRight((a, acc) -> a + acc, "", Nel.Nel("a", "b" :: Nil)) == "ab"

    @Test
    def foldRight03(): Bool =
        Reducible.foldRight((a, acc) -> a + acc, "", Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"

    @Test
    def foldRight04(): Bool =
        Reducible.foldRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, Nil)) == 198

    @Test
    def foldRight05(): Bool =
        Reducible.foldRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: Nil)) == 194

    @Test
    def foldRight06(): Bool =
        Reducible.foldRight((a, acc) -> (acc - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: 3 :: Nil)) == 382

    @Test
    def foldRight07(): Bool = region rc {
        let l = ref Nil @ rc;
        discard Reducible.foldRight((x, _) -> { l := x :: deref l; x == 1 }, false, Nel.Nel(1, 2 :: 3 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // foldRightWithCont                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRightWithCont01(): Bool =
        Reducible.foldRightWithCont((a, k) -> a + k(), "", Nel.Nel("a", Nil)) == "a"

    @Test
    def foldRightWithCont02(): Bool =
        Reducible.foldRightWithCont((a, k) -> a + k(), "", Nel.Nel("a", "b" :: Nil)) == "ab"

    @Test
    def foldRightWithCont03(): Bool =
        Reducible.foldRightWithCont((a, k) -> a + k(), "", Nel.Nel("a", "b" :: "c" :: Nil)) == "abc"

    @Test
    def foldRightWithCont04(): Bool =
        Reducible.foldRightWithCont((a, k) -> (k() - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, Nil)) == 198

    @Test
    def foldRightWithCont05(): Bool =
        Reducible.foldRightWithCont((a, k) -> (k() - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: Nil)) == 194

    @Test
    def foldRightWithCont06(): Bool =
        Reducible.foldRightWithCont((a, k) -> (k() - a) * ((a `Int32.rem` 2) + 1), 100, Nel.Nel(1, 2 :: 3 :: Nil)) == 382

    @Test
    def foldRightWithCont07(): Bool = region rc {
        let l = ref Nil @ rc;
        discard Reducible.foldRightWithCont((x, _) -> { l := x :: deref l; x == 1 }, false, Nel.Nel(1, 2 :: 3 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def head01(): Bool =
        Reducible.head(Nel.Nel(1, Nil)) == 1

    @Test
    def head02(): Bool =
        Reducible.head(Nel.Nel(2, 1 :: Nil)) == 2

    @Test
    def head03(): Bool =
        Reducible.head(Nel.Nel(3, 2 :: 1 :: Nil)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def last01(): Bool =
        Reducible.last(Nel.Nel(1, Nil)) == 1

    @Test
    def last02(): Bool =
        Reducible.last(Nel.Nel(1, 2 :: Nil)) == 2

    @Test
    def last03(): Bool =
        Reducible.last(Nel.Nel(1, 2 :: 3 :: Nil)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Bool =
        Reducible.init(Nel.Nel(1, Nil)) == Nil

    @Test
    def init02(): Bool =
        Reducible.init(Nel.Nel(2, Nil)) == Nil

    @Test
    def init03(): Bool =
        Reducible.init(Nel.Nel(1, 2 :: Nil)) == 1 :: Nil

    @Test
    def init04(): Bool =
        Reducible.init(Nel.Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: Nil

    @Test
    def init05(): Bool =
        Reducible.init(Nel.Nel(3, 2 :: 1 :: Nil)) == 3 :: 2 :: Nil

    @Test
    def init06(): Bool =
        Reducible.init(Nel.Nel(1, 2 :: 3 :: 4 :: Nil)) == 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // tail                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def tail01(): Bool =
        Reducible.tail(Nel.Nel(1, Nil)) == Nil

    @Test
    def tail02(): Bool =
        Reducible.tail(Nel.Nel(2, Nil)) == Nil

    @Test
    def tail03(): Bool =
        Reducible.tail(Nel.Nel(1, 2 :: Nil)) == 2 :: Nil

    @Test
    def tail04(): Bool =
        Reducible.tail(Nel.Nel(1, 2 :: 3 :: Nil)) == 2 :: 3 :: Nil

    @Test
    def tail05(): Bool =
        Reducible.tail(Nel.Nel(3, 2 :: 1 :: Nil)) == 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Bool =
        Reducible.reverse(Nel.Nel(1, Nil)) == 1 :: Nil

    @Test
    def reverse02(): Bool =
        Reducible.reverse(Nel.Nel(1, 2 :: Nil)) == 2 :: 1 :: Nil

    @Test
    def reverse03(): Bool =
        Reducible.reverse(Nel.Nel(1, 2 :: 3 :: Nil)) == 3 :: 2 :: 1 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def count01(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(1, Nil)) == 0

    @Test
    def count02(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(4, Nil)) == 1

    @Test
    def count03(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(1, 2 :: Nil)) == 0

    @Test
    def count04(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(1, 8 :: Nil)) == 1

    @Test
    def count05(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(8, 1 :: Nil)) == 1

    @Test
    def count06(): Bool =
        Reducible.count(i -> i > 3, Nel.Nel(6, 7 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Bool =
        Reducible.length(Nel.Nel(1, Nil)) == 1

    @Test
    def length02(): Bool =
        Reducible.length(Nel.Nel(1, 2 :: Nil)) == 2

    @Test
    def length03(): Bool =
        Reducible.length(Nel.Nel(1, 2 :: 3 :: Nil)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // sum                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sum01(): Bool =
        Reducible.sum(Nel.Nel(1, Nil)) == 1

    @Test
    def sum02(): Bool =
        Reducible.sum(Nel.Nel(1, 2 :: 3 :: Nil)) == 6

    @Test
    def sum03(): Bool =
        Reducible.sum(Nel.Nel(1, 2 :: 3 :: -3 :: Nil)) == 3

    @Test
    def sum04(): Bool =
        Reducible.sum(Nel.Nel(-1, -2 :: -3 :: -4 :: Nil)) == -10

    @Test
    def sum05(): Bool =
        Reducible.sum(Nel.Nel(10, -10 :: Nil)) == 0

    @Test
    def sum06(): Bool = match List.range(1, 101) {
        case x :: xs => Reducible.sum(Nel.Nel(x, xs)) == 5050
        case _       => unreachable!()
    }


    /////////////////////////////////////////////////////////////////////////////
    // sumWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sumWith01(): Bool =
        Reducible.sumWith(x -> x + 1, Nel.Nel(1, Nil)) == 2

    @Test
    def sumWith02(): Bool =
        Reducible.sumWith(x -> x + 1, Nel.Nel(1, 2 :: 3 :: Nil)) == 9

    @Test
    def sumWith03(): Bool =
        Reducible.sumWith(x -> x + 1, Nel.Nel(1, 2 :: 3 :: -3 :: Nil)) == 7

    @Test
    def sumWith04(): Bool =
        Reducible.sumWith(x -> x + 1, Nel.Nel(-1, -2 :: -3 :: -4 :: Nil)) == -6

    @Test
    def sumWith05(): Bool =
        Reducible.sumWith(x -> x + 1, Nel.Nel(10, -10 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Bool =
        Reducible.exists(x -> x == 1, Nel.Nel(1, Nil)) == true

    @Test
    def exists02(): Bool =
        Reducible.exists(x -> x == 2, Nel.Nel(1, Nil)) == false

    @Test
    def exists03(): Bool =
        Reducible.exists(x -> x == 1, Nel.Nel(1, 2 :: Nil)) == true

    @Test
    def exists04(): Bool =
        Reducible.exists(x -> x == 2, Nel.Nel(1, 2 :: Nil)) == true


    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Bool =
        Reducible.forAll(x -> x == 1, Nel.Nel(1, Nil)) == true

    @Test
    def forAll02(): Bool =
        Reducible.forAll(x -> x == 2, Nel.Nel(1, Nil)) == false

    @Test
    def forAll03(): Bool =
        Reducible.forAll(x -> x == 1, Nel.Nel(1, 1 :: Nil)) == true

    @Test
    def forAll04(): Bool =
        Reducible.forAll(x -> x == 1, Nel.Nel(1, 2 :: Nil)) == false

    @Test
    def forAll05(): Bool =
        Reducible.forAll(x -> x == 1, Nel.Nel(2, 1 :: Nil)) == false


    /////////////////////////////////////////////////////////////////////////////
    // forEach                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forEach01(): Bool = region rc {
        let ri = ref 21 @ rc;
        Reducible.forEach(x -> ri := x, Nel.Nel(1, Nil));
        1 == deref ri
    }

    @Test
    def forEach02(): Bool = region rc {
        let ri = ref 21 @ rc;
        Reducible.forEach(x -> ri := x, Nel.Nel(1, 2 :: 3 :: Nil));
        3 == deref ri
    }

    /////////////////////////////////////////////////////////////////////////////
    // findLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findLeft01(): Bool =
        Reducible.findLeft(x -> x == 0, Nel.Nel(1, Nil)) == None

    @Test
    def findLeft02(): Bool =
        Reducible.findLeft(x -> x == 0, Nel.Nel(1, 2 :: Nil)) == None

    @Test
    def findLeft03(): Bool =
        Reducible.findLeft(x -> x == 0, Nel.Nel(1, 2 :: 3 :: Nil)) == None

    @Test
    def findLeft04(): Bool =
        Reducible.findLeft(x -> x == 1, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @Test
    def findLeft05(): Bool =
        Reducible.findLeft(x -> x == 2, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @Test
    def findLeft06(): Bool =
        Reducible.findLeft(x -> x == 3, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(3)

    @Test
    def findLeft07(): Bool = region rc {
        let l = ref Nil @ rc;
        discard Reducible.findLeft(x -> { l := x :: deref l; x == 1 }, Nel.Nel(3, 2 :: 1 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // findRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findRight01(): Bool =
        Reducible.findRight(x -> x == 0, Nel.Nel(1, Nil)) == None

    @Test
    def findRight02(): Bool =
        Reducible.findRight(x -> x == 0, Nel.Nel(1, 2 :: Nil)) == None

    @Test
    def findRight03(): Bool =
        Reducible.findRight(x -> x == 0, Nel.Nel(1, 2 :: 3 :: Nil)) == None

    @Test
    def findRight04(): Bool =
        Reducible.findRight(x -> x == 1, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(1)

    @Test
    def findRight05(): Bool =
        Reducible.findRight(x -> x == 2, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(2)

    @Test
    def findRight06(): Bool =
        Reducible.findRight(x -> x == 3, Nel.Nel(1, 2 :: 3 :: Nil)) == Some(3)

    @Test
    def findRight07(): Bool = region rc {
        let l = ref Nil @ rc;
        discard Reducible.findRight(x -> { l := x :: deref l; x == 1 }, Nel.Nel(1, 2 :: 3 :: Nil));
        (deref l) == 1 :: 2 :: 3 :: Nil
    }

    /////////////////////////////////////////////////////////////////////////////
    // memberOf                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def memberOf01(): Bool =
        Reducible.memberOf(0, Nel.Nel(1, Nil)) == false

    @Test
    def memberOf02(): Bool =
        Reducible.memberOf(0, Nel.Nel(1, 2 :: Nil)) == false

    @Test
    def memberOf03(): Bool =
        Reducible.memberOf(0, Nel.Nel(1, 2 :: 3 :: Nil)) == false

    @Test
    def memberOf04(): Bool =
        Reducible.memberOf(1, Nel.Nel(1, 2 :: 3 :: Nil))

    @Test
    def memberOf05(): Bool =
        Reducible.memberOf(2, Nel.Nel(1, 2 :: 3 :: Nil))

    @Test
    def memberOf06(): Bool =
        Reducible.memberOf(3, Nel.Nel(1, 2 :: 3 :: Nil))


    /////////////////////////////////////////////////////////////////////////////
    // minimum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimum01(): Bool =
        Reducible.minimum(Nel.Nel(1, Nil)) == 1

    @Test
    def minimum02(): Bool =
        Reducible.minimum(Nel.Nel(1, 2 :: 3 :: Nil)) == 1

    @Test
    def minimum03(): Bool =
        Reducible.minimum(Nel.Nel(1, 2 :: 0 :: 3 :: Nil)) == 0

    @Test
    def minimum04(): Bool =
        Reducible.minimum(Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 3

    @Test
    def minimum05(): Bool =
        Reducible.minimum(Nel.Nel(9, 5 :: 4 :: 2 :: 7 :: 10 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // minimumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def minimumBy01(): Bool =
        Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(1, Nil)) == 1

    @Test
    def minimumBy02(): Bool =
        Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 3 :: Nil)) == 1

    @Test
    def minimumBy03(): Bool =
        Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 0 :: 3 :: Nil)) == 0

    @Test
    def minimumBy04(): Bool =
        Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 3

    @Test
    def minimumBy05(): Bool =
        Reducible.minimumBy((x, y) -> x <=> y, Nel.Nel(9, 5 :: 4 :: 2 :: 7 :: 10 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // maximum                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximum01(): Bool =
        Reducible.maximum(Nel.Nel(1, Nil)) == 1

    @Test
    def maximum02(): Bool =
        Reducible.maximum(Nel.Nel(1, 2 :: 3 :: Nil)) == 3

    @Test
    def maximum03(): Bool =
        Reducible.maximum(Nel.Nel(1, 2 :: 0 :: 3 :: Nil)) == 3

    @Test
    def maximum04(): Bool =
        Reducible.maximum(Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 9

    @Test
    def maximum05(): Bool =
        Reducible.maximum(Nel.Nel(3, 5 :: 4 :: 9 :: 7 :: 10 :: Nil)) == 10


    /////////////////////////////////////////////////////////////////////////////
    // maximumBy                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def maximumBy01(): Bool =
        Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(1, Nil)) == 1

    @Test
    def maximumBy02(): Bool =
        Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 3 :: Nil)) == 3

    @Test
    def maximumBy03(): Bool =
        Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(1, 2 :: 0 :: 3 :: Nil)) == 3

    @Test
    def maximumBy04(): Bool =
        Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(9, 5 :: 4 :: 6 :: 7 :: 3 :: Nil)) == 9

    @Test
    def maximumBy05(): Bool =
        Reducible.maximumBy((x, y) -> x <=> y, Nel.Nel(3, 5 :: 4 :: 9 :: 7 :: 10 :: Nil)) == 10


    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhile01(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(1, Nil)) == 1 :: Nil

    @Test
    def dropWhile02(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(4, Nil)) == Nil

    @Test
    def dropWhile03(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(1, 2 :: Nil)) == 1 :: 2 :: Nil

    @Test
    def dropWhile04(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(1, 5 :: Nil)) == 1 :: 5 :: Nil

    @Test
    def dropWhile05(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(5, 1 :: Nil)) == 1 :: Nil

    @Test
    def dropWhile06(): Bool =
        Reducible.dropWhile(a -> a > 3, Nel.Nel(5, 8 :: Nil)) == Nil

    @Test
    def dropWhile07(): Bool =
        Nel.Nel(4, 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Reducible.dropWhile(a -> a > 3) == -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhile01(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(1, Nil)) == Nil

    @Test
    def takeWhile02(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(4, Nil)) == 4 :: Nil

    @Test
    def takeWhile03(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(1, 2 :: Nil)) == Nil

    @Test
    def takeWhile04(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(1, 5 :: Nil)) == Nil

    @Test
    def takeWhile05(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(5, 1 :: Nil)) == 5 :: Nil

    @Test
    def takeWhile06(): Bool =
        Reducible.takeWhile(a -> a > 3, Nel.Nel(5, 8 :: Nil)) == 5 :: 8 :: Nil

    @Test
    def takeWhile07(): Bool =
        Nel.Nel(4, 6 :: -3 :: 11 :: -5 :: 1 :: 2 :: 16 :: 7 :: 1 :: 7 :: Nil) |>
            Reducible.takeWhile(a -> a > 3) == 4 :: 6 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // intersperse                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intersperse01(): Bool =
        Reducible.intersperse(7, Nel.Nel(1, Nil)) == 1 :: Nil

    @Test
    def intersperse02(): Bool =
        Reducible.intersperse(7, Nel.Nel(1, 2 :: Nil)) == 1 :: 7 :: 2 :: Nil

    @Test
    def intersperse03(): Bool =
        Reducible.intersperse(7, Nel.Nel(1, 2 :: 3 :: Nil)) == 1 :: 7 :: 2 :: 7 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Bool = region rc {
        Array.sameElements(Reducible.toArray(rc, Nel.Nel(1, Nil)), Array#{1} @ rc)
    }

    @Test
    def toArray02(): Bool = region rc {
        Array.sameElements(Reducible.toArray(rc, Nel.Nel(1, 2 :: 3 :: Nil)), Array#{1, 2, 3} @ rc)
    }

    @Test
    def toArray03(): Bool = region rc {
        Array.sameElements(Reducible.toArray(rc, Nel.Nel(1, 1 :: 2 :: 3 :: Nil)), Array#{1, 1, 2, 3} @ rc)
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Bool =
        Reducible.toVector(Nel.Nel(1, Nil)) == Vector#{1}

    @Test
    def toVector02(): Bool =
        Reducible.toVector(Nel.Nel(1, 2 :: 3 :: Nil)) == Vector#{1, 2, 3}

    @Test
    def toVector03(): Bool =
        Reducible.toVector(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)) == Vector#{1, 1, 2, 3}


    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Bool =
        Reducible.toList(Nel.Nel(1, Nil)) == 1 :: Nil

    @Test
    def toList02(): Bool =
        Reducible.toList(Nel.Nel(1, 2 :: 3 :: Nil)) == 1 :: 2 :: 3 :: Nil

    @Test
    def toList03(): Bool =
        Reducible.toList(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)) == 1 :: 1 :: 2 :: 3 :: Nil


    /////////////////////////////////////////////////////////////////////////////
    // toMap                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toMap01(): Bool =
        Reducible.toMap(Nel.Nel((1, 1), Nil)) == Map#{1 => 1}

    @Test
    def toMap02(): Bool =
        Reducible.toMap(Nel.Nel(("a", 1), ("b", 2) :: ("c", 3) :: Nil)) == Map#{"a" => 1, "b" => 2, "c" => 3}

    @Test
    def toMap03(): Bool =
        Reducible.toMap(Nel.Nel((1, 2), (4, 3) :: (2, 2) :: (3, 3) :: Nil)) == Map#{1 => 2, 2 => 2, 3 => 3, 4 => 3}


    /////////////////////////////////////////////////////////////////////////////
    // toNel                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toNel01(): Bool =
        Reducible.toNel(Nel.Nel(1, Nil)) == Nel.Nel(1, Nil)

    @Test
    def toNel02(): Bool =
        Reducible.toNel(Nel.Nel(1, 2 :: 3 :: Nil)) == Nel.Nel(1, 2 :: 3 :: Nil)

    @Test
    def toNel03(): Bool =
        Reducible.toNel(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)) == Nel.Nel(1, 1 :: 2 :: 3 :: Nil)

    @Test
    def toNel04(): Bool =
        match List.range(0, 100) {
            case x :: xs => Reducible.toNel(Nel.Nel(x, xs)) |> Nel.toList == List.range(0, 100)
            case _       => unreachable!()
        }


    /////////////////////////////////////////////////////////////////////////////
    // toSet                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSet01(): Bool =
        Reducible.toSet(Nel.Nel(1, Nil)) == Set#{1}

    @Test
    def toSet02(): Bool =
        Reducible.toSet(Nel.Nel(1, 2 :: 3 :: Nil)) == Set#{1, 2, 3}

    @Test
    def toSet03(): Bool =
        Reducible.toSet(Nel.Nel(1, 1 :: 2 :: 3 :: Nil)) == Set#{1, 2, 3}

}
