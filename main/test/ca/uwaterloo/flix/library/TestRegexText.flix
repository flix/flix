/*
 *  Copyright 2023 Stephen Tetley
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

mod TestRegexText {

    /////////////////////////////////////////////////////////////////////////////
    // split                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def split01(): Bool =
        Result.map(patt -> Regex/Text.split(pattern = patt, "A B C"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok(List#{"A", "B", "C"})

    @test
    def split02(): Bool =
        Result.map(patt -> Regex/Text.split(pattern = patt, "A  B   C"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok(List#{"A", "B", "C"})

    @test
    def split03(): Bool =
        Result.map(patt -> Regex/Text.split(pattern = patt, "ABC"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok(List#{"ABC"})

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool =
        Result.map(patt -> Regex/Text.replace(from = patt, to = "_", "A B C"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("A_B_C")

    @test
    def replace02(): Bool =
        Result.map(patt -> Regex/Text.replace(from = patt, to = "_", "A  B   C"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("A_B_C")

    @test
    def replace03(): Bool =
        Result.map(patt -> Regex/Text.replace(from = patt, to = "_", "ABC"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("ABC")
}
