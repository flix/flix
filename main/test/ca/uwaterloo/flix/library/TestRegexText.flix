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

namespace TestRegexText {


    /////////////////////////////////////////////////////////////////////////////
    // isMatch                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isMatch01(): Bool = 
        Result.map(patt -> Regex/Text.isMatch(patt, ""),
            Regex/Pattern.compile("a+")) == Ok(false)

    @test
    def isMatch02(): Bool = 
        Result.map(patt -> Regex/Text.isMatch(patt, ""),
            Regex/Pattern.compile("a*")) == Ok(true)

    @test
    def isMatch03(): Bool =
        Result.map(patt -> Regex/Text.isMatch(patt, "a"),
            Regex/Pattern.compile("a*")) == Ok(true)

    @test
    def isMatch04(): Bool = 
        Result.map(patt -> Regex/Text.isMatch(patt, "aa"),
            Regex/Pattern.compile("a*")) == Ok(true)

    @test
    def isMatch05(): Bool = 
        Result.map(patt -> Regex/Text.isMatch(patt, "aab"),
            Regex/Pattern.compile("a*")) == Ok(false)

    @test
    def isMatch06(): Bool =
        Result.map(patt -> Regex/Text.isMatch(patt, "aab"),
            Regex/Pattern.compile("a*b")) == Ok(true)

    /////////////////////////////////////////////////////////////////////////////
    // isMatch                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isSubmatch01(): Bool = 
        Result.map(patt -> Regex/Text.isSubmatch(patt, ""), 
            Regex/Pattern.compile("a+")) == Ok(false)

    @test
    def isSubmatch02(): Bool = 
        Result.map(patt -> Regex/Text.isSubmatch(patt, "a"), 
            Regex/Pattern.compile("a*")) == Ok(true)

    @test
    def isSubmatch03(): Bool = 
        Result.map(patt -> Regex/Text.isSubmatch(patt, "aa"), 
            Regex/Pattern.compile("a*")) == Ok(true)

    @test
    def isSubmatch04(): Bool = 
        Result.map(patt -> Regex/Text.isSubmatch(patt, "aab"), 
            Regex/Pattern.compile("a*")) == Ok(true)

    @test
    def isSubmatch05(): Bool = 
        Result.map(patt -> Regex/Text.isSubmatch(patt, "aab"), 
            Regex/Pattern.compile("a*b")) == Ok(true)

    @test
    def isSubmatch06(): Bool = 
        Result.map(patt -> Regex/Text.isSubmatch(patt, "aab"), 
            Regex/Pattern.compile("b+")) == Ok(true)

    @test
    def isSubmatch07(): Bool = 
        Result.map(patt -> Regex/Text.isSubmatch(patt, "bbbc"), 
            Regex/Pattern.compile("b+")) == Ok(true)

    @test
    def isSubmatch08(): Bool = 
        Result.map(patt -> Regex/Text.isSubmatch(patt, "aabbbc"), 
            Regex/Pattern.compile("b+")) == Ok(true)

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indices01(): Bool = 
        Regex/Text.indices(substr = Regex/Pattern.unmatchable(), "") == List#{}

    @test
    def indices02(): Bool = 
        Regex/Text.indices(substr = Regex/Pattern.unmatchable(), "a::b::c") == List#{}

    @test
    def indices03(): Bool = 
        Result.map(patt -> Regex/Text.indices(substr = patt, ""),
            Regex/Pattern.compile(":{2}")) == Ok(List#{})

    @test
    def indices04(): Bool =
        Result.map(patt -> Regex/Text.indices(substr = patt, "a"),
            Regex/Pattern.compile(":{2}")) == Ok(List#{})

    @test
    def indices05(): Bool = 
        Result.map(patt -> Regex/Text.indices(substr = patt, "::"),
            Regex/Pattern.compile(":{2}")) == Ok(List#{0})

    @test
    def indices06(): Bool = 
        Result.map(patt -> Regex/Text.indices(substr = patt, "ab"),
            Regex/Pattern.compile(":{2}")) == Ok(List#{})

    @test
    def indices07(): Bool = 
        Result.map(patt -> Regex/Text.indices(substr = patt, "::a"),
            Regex/Pattern.compile(":{2}")) == Ok(List#{0})

    @test
    def indices08(): Bool = 
        Result.map(patt -> Regex/Text.indices(substr = patt, "a::"),
            Regex/Pattern.compile(":{2}")) == Ok(List#{1})

    @test
    def indices09(): Bool = 
        Result.map(patt -> Regex/Text.indices(substr = patt, "a::b::"),
            Regex/Pattern.compile(":{2}")) == Ok(List#{1, 4})

    @test
    def indices10(): Bool = 
        Result.map(patt -> Regex/Text.indices(substr = patt, "a::b::c"),
            Regex/Pattern.compile(":{2}")) == Ok(List#{1, 4})

    @test
    def indices11(): Bool = 
        Result.map(patt -> Regex/Text.indices(substr = patt, "a::b::c:"),
            Regex/Pattern.compile(":{2}")) == Ok(List#{1, 4})

    @test
    def indices12(): Bool = 
        Result.map(patt -> Regex/Text.indices(substr = patt, ":a::b::c"),
            Regex/Pattern.compile(":{2}")) == Ok(List#{2, 5})

    @test
    def indices13(): Bool = 
        Result.map(patt -> Regex/Text.indices(substr = patt, "::::"),
            Regex/Pattern.compile(":{2}")) == Ok(List#{0, 2})


    /////////////////////////////////////////////////////////////////////////////
    // submatches                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def submatches01(): Bool = 
        Regex/Text.submatches(substr = Regex/Pattern.unmatchable(), "") == List#{}

    @test
    def submatches02(): Bool = 
        Regex/Text.submatches(substr = Regex/Pattern.unmatchable(), "a::b::c") == List#{}

    @test
    def submatches03(): Bool = 
        Result.map(patt -> Regex/Text.submatches(substr = patt, ""),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{})

    @test
    def submatches04(): Bool =
        Result.map(patt -> Regex/Text.submatches(substr = patt, "a"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{"a"})

    @test
    def submatches05(): Bool = 
        Result.map(patt -> Regex/Text.submatches(substr = patt, "::"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{})

    @test
    def submatches06(): Bool = 
        Result.map(patt -> Regex/Text.submatches(substr = patt, "ab"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{"ab"})

    @test
    def submatches07(): Bool = 
        Result.map(patt -> Regex/Text.submatches(substr = patt, "::a"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{"a"})

    @test
    def submatches08(): Bool = 
        Result.map(patt -> Regex/Text.submatches(substr = patt, "a::"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{"a"})

    @test
    def submatches09(): Bool = 
        Result.map(patt -> Regex/Text.submatches(substr = patt, "a::b::"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{"a", "b"})

    @test
    def submatches10(): Bool = 
        Result.map(patt -> Regex/Text.submatches(substr = patt, "a::b::c"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{"a", "b", "c"})

    @test
    def submatches11(): Bool = 
        Result.map(patt -> Regex/Text.submatches(substr = patt, "a::b::c:"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{"a", "b", "c"})

    @test
    def submatches12(): Bool = 
        Result.map(patt -> Regex/Text.submatches(substr = patt, ":a::b::c"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{"a", "b", "c"})

    @test
    def submatches13(): Bool = 
        Result.map(patt -> Regex/Text.submatches(substr = patt, "::::"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(List#{})


    /////////////////////////////////////////////////////////////////////////////
    // countSubmatches                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def countSubmatches01(): Bool = 
        Regex/Text.countSubmatches(substr = Regex/Pattern.unmatchable(), "") == 0

    @test
    def countSubmatches02(): Bool = 
        Regex/Text.countSubmatches(substr = Regex/Pattern.unmatchable(), "a::b::c") == 0

    @test
    def countSubmatches03(): Bool = 
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, ""),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(0)

    @test
    def countSubmatches04(): Bool =
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, "a"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(1)

    @test
    def countSubmatches05(): Bool = 
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, "::"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(0)

    @test
    def countSubmatches06(): Bool = 
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, "ab"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(1)

    @test
    def countSubmatches07(): Bool = 
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, "::a"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(1)

    @test
    def countSubmatches08(): Bool = 
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, "a::"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(1)

    @test
    def countSubmatches09(): Bool = 
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, "a::b::"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(2)

    @test
    def countSubmatches10(): Bool = 
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, "a::b::c"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(3)

    @test
    def countSubmatches11(): Bool = 
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, "a::b::c:"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(3)

    @test
    def countSubmatches12(): Bool = 
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, ":a::b::c"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(3)

    @test
    def countSubmatches13(): Bool = 
        Result.map(patt -> Regex/Text.countSubmatches(substr = patt, "::::"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(0)

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
        Result.map(patt -> Regex/Text.replace(from = patt, to = "_", ""),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("")

    @test
    def replace02(): Bool =
        Result.map(patt -> Regex/Text.replace(from = patt, to = "_", "A B C"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("A_B_C")

    @test
    def replace03(): Bool =
        Result.map(patt -> Regex/Text.replace(from = patt, to = "_", "A  B   C"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("A_B_C")

    @test
    def replace04(): Bool =
        Result.map(patt -> Regex/Text.replace(from = patt, to = "_", "ABC"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("ABC")

    @test
    def replace05(): Bool =
        Result.map(patt -> Regex/Text.replace(from = patt, to = "__", "A B  C"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("A__B__C")

    @test
    def replace06(): Bool =
        Result.map(patt -> Regex/Text.replace(from = patt, to = "__", " A B  C  "),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("__A__B__C__")

    /////////////////////////////////////////////////////////////////////////////
    // replaceFirst                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replaceFirst01(): Bool =
        Result.map(patt -> Regex/Text.replaceFirst(from = patt, to = "_", ""),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("")

    @test
    def replaceFirst02(): Bool =
        Result.map(patt -> Regex/Text.replaceFirst(from = patt, to = "_", "A B C"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("A_B C")

    @test
    def replaceFirst03(): Bool =
        Result.map(patt -> Regex/Text.replaceFirst(from = patt, to = "_", "A  B   C"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("A_B   C")

    @test
    def replaceFirst04(): Bool =
        Result.map(patt -> Regex/Text.replaceFirst(from = patt, to = "_", "ABC"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("ABC")

    @test
    def replaceFirst05(): Bool =
        Result.map(patt -> Regex/Text.replaceFirst(from = patt, to = "__", "A B  C"),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("A__B  C")

    @test
    def replaceFirst06(): Bool =
        Result.map(patt -> Regex/Text.replaceFirst(from = patt, to = "__", " A B  C  "),
            Regex/Pattern.compile("\\p{Blank}+")) == Ok("__A B  C  ")

    /////////////////////////////////////////////////////////////////////////////
    // startsWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def startsWith01(): Bool =
        Regex/Text.startsWith(prefix = Regex/Pattern.unmatchable(), "") == false

    @test
    def startsWith02(): Bool =
        Regex/Text.startsWith(prefix = Regex/Pattern.unmatchable(), "A B C") == false

    @test
    def startsWith03(): Bool =
        Result.map(patt -> Regex/Text.startsWith(prefix = patt, ""),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(false)

    @test
    def startsWith04(): Bool =
        Result.map(patt -> Regex/Text.startsWith(prefix = patt, "   A B C"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(false)

    @test
    def startsWith05(): Bool =
        Result.map(patt -> Regex/Text.startsWith(prefix = patt, "A B C"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(true)

    @test
    def startsWith06(): Bool =
        Result.map(patt -> Regex/Text.startsWith(prefix = patt, "AA BBB CCCC"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(true)

    /////////////////////////////////////////////////////////////////////////////
    // endsWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def endsWith01(): Bool =
        Regex/Text.endsWith(suffix = Regex/Pattern.unmatchable(), "") == false

    @test
    def endsWith02(): Bool =
        Regex/Text.endsWith(suffix = Regex/Pattern.unmatchable(), "A B C") == false

    @test
    def endsWith03(): Bool =
        Result.map(patt -> Regex/Text.endsWith(suffix = patt, ""),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(false)

    @test
    def endsWith04(): Bool =
        Result.map(patt -> Regex/Text.endsWith(suffix = patt, "A B C  "),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(false)
    @test
    def endsWith05(): Bool =
        Result.map(patt -> Regex/Text.endsWith(suffix = patt, "A B C"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(true)
    @test
    def endsWith06(): Bool =
        Result.map(patt -> Regex/Text.endsWith(suffix = patt, "AA BBB CCCC"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(true)

    /////////////////////////////////////////////////////////////////////////////
    // stripPrefix                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stripPrefix01(): Bool =
        Regex/Text.stripPrefix(substr = Regex/Pattern.unmatchable(), "") == None

    @test
    def stripPrefix02(): Bool =
        Regex/Text.stripPrefix(substr = Regex/Pattern.unmatchable(), "A B C") == None

    @test
    def stripPrefix03(): Bool =
        Result.map(patt -> Regex/Text.stripPrefix(substr = patt, ""),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(None)

    @test
    def stripPrefix04(): Bool =
        Result.map(patt -> Regex/Text.stripPrefix(substr = patt, "   A B C"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(None)

    @test
    def stripPrefix05(): Bool =
        Result.map(patt -> Regex/Text.stripPrefix(substr = patt, "A B C"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(Some(" B C"))

    @test
    def stripPrefix06(): Bool =
        Result.map(patt -> Regex/Text.stripPrefix(substr = patt, "AA BBB CCCC"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(Some(" BBB CCCC"))

    /////////////////////////////////////////////////////////////////////////////
    // stripSuffix                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stripSuffix01(): Bool =
        Regex/Text.stripSuffix(substr = Regex/Pattern.unmatchable(), "") == None

    @test
    def stripSuffix02(): Bool =
        Regex/Text.stripSuffix(substr = Regex/Pattern.unmatchable(), "A B C") == None

    @test
    def stripSuffix03(): Bool =
        Result.map(patt -> Regex/Text.stripSuffix(substr = patt, ""),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(None)

    @test
    def stripSuffix04(): Bool =
        Result.map(patt -> Regex/Text.stripSuffix(substr = patt, "A B C  "),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(None)
    @test
    def stripSuffix05(): Bool =
        Result.map(patt -> Regex/Text.stripSuffix(substr = patt, "A B C"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(Some("A B "))
    @test
    def stripSuffix06(): Bool =
        Result.map(patt -> Regex/Text.stripSuffix(substr = patt, "AA BBB CCCC"),
            Regex/Pattern.compile("\\p{Alpha}+")) == Ok(Some("AA BBB "))

}
