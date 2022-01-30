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

namespace TestReducible {

    /////////////////////////////////////////////////////////////////////////////
    // reduceLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceLeft01(): Bool =
        Reducible.reduceLeft((acc, a) -> acc + a, Nel("a", Nil)) == "a"

    @test
    def reduceLeft02(): Bool =
        Reducible.reduceLeft((acc, a) -> acc + a, Nel("a", "b" :: Nil)) == "ab"

    @test
    def reduceLeft03(): Bool =
        Reducible.reduceLeft((acc, a) -> acc + a, Nel("a", "b" :: "c" :: Nil)) == "abc"

    @test
    def reduceLeft04(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a rem 2) + 1), Nel(100, Nil)) == 100

    @test
    def reduceLeft05(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a rem 2) + 1), Nel(100, 1 :: Nil)) == 198

    @test
    def reduceLeft06(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a rem 2) + 1), Nel(100, 1 :: 2 :: Nil)) == 196

    @test
    def reduceLeft07(): Bool =
        Reducible.reduceLeft((acc, a) -> (acc - a) * ((a rem 2) + 1), Nel(100, 1 :: 2 :: 3 :: Nil)) == 386


    /////////////////////////////////////////////////////////////////////////////
    // reduceRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceRight01(): Bool =
        Reducible.reduceRight((a, acc) -> a + acc, Nel("a", Nil)) == "a"

    @test
    def reduceRight02(): Bool =
        Reducible.reduceRight((a, acc) -> a + acc, Nel("a", "b" :: Nil)) == "ab"

    @test
    def reduceRight03(): Bool =
        Reducible.reduceRight((a, acc) -> a + acc, Nel("a", "b" :: "c" :: Nil)) == "abc"

    @test
    def reduceRight04(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a rem 2) + 1), Nel(100, Nil)) == 100

    @test
    def reduceRight05(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a rem 2) + 1), Nel(1, 100 :: Nil)) == 198

    @test
    def reduceRight06(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a rem 2) + 1), Nel(1, 2 :: 100 :: Nil)) == 194

    @test
    def reduceRight07(): Bool =
        Reducible.reduceRight((a, acc) -> (acc - a) * ((a rem 2) + 1), Nel(1, 2 :: 3 :: 100 :: Nil)) == 382


    /////////////////////////////////////////////////////////////////////////////
    // reduce                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduce01(): Bool =
        Reducible.reduce(Nel("a", Nil)) == "a"

    @test
    def reduce02(): Bool =
        Reducible.reduce(Nel("a", "b" :: Nil)) == "ab"

    @test
    def reduce03(): Bool =
        Reducible.reduce(Nel("a", "b" :: "c" :: Nil)) == "abc"


    /////////////////////////////////////////////////////////////////////////////
    // reduceMap                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reduceMap01(): Bool =
        Reducible.reduceMap(a -> Int16.toInt32(a), Nel(1i16, Nil)) == 1

    @test
    def reduceMap02(): Bool =
        Reducible.reduceMap(a -> Int16.toInt32(a), Nel(1i16, 2i16 :: Nil)) == 3

    @test
    def reduceMap03(): Bool =
        Reducible.reduceMap(a -> Int16.toInt32(a), Nel(1i16, 2i16 :: 3i16 :: Nil)) == 6


    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldLeft01(): Bool =
        Reducible.foldLeft((acc, a) -> acc + a, "", Nel("a", Nil)) == "a"

    @test
    def foldLeft02(): Bool =
        Reducible.foldLeft((acc, a) -> acc + a, "", Nel("a", "b" :: Nil)) == "ab"

    @test
    def foldLeft03(): Bool =
        Reducible.foldLeft((acc, a) -> acc + a, "", Nel("a", "b" :: "c" :: Nil)) == "abc"

    @test
    def foldLeft04(): Bool =
        Reducible.foldLeft((acc, a) -> (acc - a) * ((a rem 2) + 1), 100, Nel(1, Nil)) == 198

    @test
    def foldLeft05(): Bool =
        Reducible.foldLeft((acc, a) -> (acc - a) * ((a rem 2) + 1), 100, Nel(1, 2 :: Nil)) == 196

    @test
    def foldLeft06(): Bool =
        Reducible.foldLeft((acc, a) -> (acc - a) * ((a rem 2) + 1), 100, Nel(1, 2 :: 3 :: Nil)) == 386


    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def foldRight01(): Bool =
        Reducible.foldRight((a, acc) -> a + acc, "", Nel("a", Nil)) == "a"

    @test
    def foldRight02(): Bool =
        Reducible.foldRight((a, acc) -> a + acc, "", Nel("a", "b" :: Nil)) == "ab"

    @test
    def foldRight03(): Bool =
        Reducible.foldRight((a, acc) -> a + acc, "", Nel("a", "b" :: "c" :: Nil)) == "abc"

    @test
    def foldRight04(): Bool =
        Reducible.foldRight((a, acc) -> (acc - a) * ((a rem 2) + 1), 100, Nel(1, Nil)) == 198

    @test
    def foldRight05(): Bool =
        Reducible.foldRight((a, acc) -> (acc - a) * ((a rem 2) + 1), 100, Nel(1, 2 :: Nil)) == 194

    @test
    def foldRight06(): Bool =
        Reducible.foldRight((a, acc) -> (acc - a) * ((a rem 2) + 1), 100, Nel(1, 2 :: 3 :: Nil)) == 382


    /////////////////////////////////////////////////////////////////////////////
    // head                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def head01(): Bool =
        Reducible.head(Nel(1, Nil)) == 1

    @test
    def head02(): Bool =
        Reducible.head(Nel(2, 1 :: Nil)) == 2

    @test
    def head03(): Bool =
        Reducible.head(Nel(3, 2 :: 1 :: Nil)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // last                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def last01(): Bool =
        Reducible.last(Nel(1, Nil)) == 1

    @test
    def last02(): Bool =
        Reducible.last(Nel(1, 2 :: Nil)) == 2

    @test
    def last03(): Bool =
        Reducible.last(Nel(1, 2 :: 3 :: Nil)) == 3


    /////////////////////////////////////////////////////////////////////////////
    // count                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def count01(): Bool =
        Reducible.count(i -> i > 3, Nel(1, Nil)) == 0

    @test
    def count02(): Bool =
        Reducible.count(i -> i > 3, Nel(4, Nil)) == 1

    @test
    def count03(): Bool =
        Reducible.count(i -> i > 3, Nel(1, 2 :: Nil)) == 0

    @test
    def count04(): Bool =
        Reducible.count(i -> i > 3, Nel(1, 8 :: Nil)) == 1

    @test
    def count05(): Bool =
        Reducible.count(i -> i > 3, Nel(8, 1 :: Nil)) == 1

    @test
    def count06(): Bool =
        Reducible.count(i -> i > 3, Nel(6, 7 :: Nil)) == 2


    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def length01(): Bool =
        Reducible.length(Nel(1, Nil)) == 1

    @test
    def length02(): Bool =
        Reducible.length(Nel(1, 2 :: Nil)) == 2

    @test
    def length03(): Bool =
        Reducible.length(Nel(1, 2 :: 3 :: Nil)) == 3

}
