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

}
