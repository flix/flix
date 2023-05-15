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

mod TestRegex {

    /////////////////////////////////////////////////////////////////////////////
    // Regex.toString                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def toString01(): Bool =
        ToString.toString(Regex.unmatchable()) != ""

    /////////////////////////////////////////////////////////////////////////////
    // unmatchable                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unmatchable01(): Bool =
        Regex.unmatchable() |> Regex.pattern == "^\\b$"

    /////////////////////////////////////////////////////////////////////////////
    // quote                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def quote01(): Bool =
        Regex.quote("ab") == "\\Qab\\E"

    @test
    def quote02(): Bool =
        Regex.quote("a*b") == "\\Qa*b\\E"

    /////////////////////////////////////////////////////////////////////////////
    // pattern                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def pattern01(): Bool =
        Regex.pattern(regex"a*b") == "a*b"

    @test
    def pattern02(): Bool =
        Regex.pattern(regex"\\p{Blank}+") == "\\p{Blank}+"

    /////////////////////////////////////////////////////////////////////////////
    // flags                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def flags01(): Bool =
        Result.map(Regex.flags, String.toRegex("\\p{Blank}+")) == Ok(Set#{})

    @test
    def flags02(): Bool =
        Result.map(Regex.flags, String.toRegexWithFlags(Set#{}, "\\p{Blank}+")) == Ok(Set#{})

    @test
    def flags03(): Bool =
        use Regex.Flag.{CaseInsenstive};
        Result.map(Regex.flags, String.toRegexWithFlags(Set#{CaseInsenstive}, "\\p{Blank}")) == Ok(Set#{CaseInsenstive})

    @test
    def flags04(): Bool =
        use Regex.Flag.{CaseInsenstive, Multiline};
        Result.map(Regex.flags, String.toRegexWithFlags(Set#{CaseInsenstive, Multiline}, "\\p{Blank}")) == Ok(Set#{CaseInsenstive, Multiline})

    /////////////////////////////////////////////////////////////////////////////
    // isMatch                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isMatch01(): Bool =
        Regex.isMatch(regex"a+", "") == false

    @test
    def isMatch02(): Bool =
        Regex.isMatch(regex"a*", "") == true

    @test
    def isMatch03(): Bool =
        Regex.isMatch(regex"a*", "a") == true

    @test
    def isMatch04(): Bool =
        Regex.isMatch(regex"a*", "aa") == true

    @test
    def isMatch05(): Bool =
        Regex.isMatch(regex"a*", "aab") == false

    @test
    def isMatch06(): Bool =
        Regex.isMatch(regex"a*b", "aab") == true

    /////////////////////////////////////////////////////////////////////////////
    // isSubmatch                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isSubmatch01(): Bool =
        Regex.isSubmatch(regex"a+", "") == false

    @test
    def isSubmatch02(): Bool =
        Regex.isSubmatch(regex"a*", "a") == true

    @test
    def isSubmatch03(): Bool =
        Regex.isSubmatch(regex"a*", "aa") == true

    @test
    def isSubmatch04(): Bool =
        Regex.isSubmatch(regex"a*", "aab") == true

    @test
    def isSubmatch05(): Bool =
        Regex.isSubmatch(regex"a*b", "aab") == true

    @test
    def isSubmatch06(): Bool =
        Regex.isSubmatch(regex"b+", "aab") == true

    @test
    def isSubmatch07(): Bool =
        Regex.isSubmatch(regex"b+", "bbbc") == true

    @test
    def isSubmatch08(): Bool =
        Regex.isSubmatch(regex"b+", "aabbbc") == true

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indices01(): Bool =
        Regex.indices(substr = Regex.unmatchable(), "") == List#{}

    @test
    def indices02(): Bool =
        Regex.indices(substr = Regex.unmatchable(), "a::b::c") == List#{}

    @test
    def indices03(): Bool =
        Regex.indices(substr = regex":{2}", "") == List#{}

    @test
    def indices04(): Bool =
        Regex.indices(substr = regex":{2}", "a") == List#{}

    @test
    def indices05(): Bool =
        Regex.indices(substr = regex":{2}", "::") == List#{0}

    @test
    def indices06(): Bool =
        Regex.indices(substr = regex":{2}", "ab") == List#{}

    @test
    def indices07(): Bool =
        Regex.indices(substr = regex":{2}", "::a") == List#{0}

    @test
    def indices08(): Bool =
        Regex.indices(substr = regex":{2}", "a::") == List#{1}

    @test
    def indices09(): Bool =
        Regex.indices(substr = regex":{2}", "a::b::") == List#{1, 4}

    @test
    def indices10(): Bool =
        Regex.indices(substr = regex":{2}", "a::b::c") == List#{1, 4}

    @test
    def indices11(): Bool =
        Regex.indices(substr = regex":{2}", "a::b::c:") == List#{1, 4}

    @test
    def indices12(): Bool =
        Regex.indices(substr = regex":{2}", ":a::b::c") == List#{2, 5}

    @test
    def indices13(): Bool =
        Regex.indices(substr = regex":{2}", "::::") == List#{0, 2}

    /////////////////////////////////////////////////////////////////////////////
    // submatches                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def submatches01(): Bool =
        Regex.submatches(substr = Regex.unmatchable(), "") == List#{}

    @test
    def submatches02(): Bool =
        Regex.submatches(substr = Regex.unmatchable(), "a::b::c") == List#{}

    @test
    def submatches03(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "") == List#{}

    @test
    def submatches04(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "a") == List#{"a"}

    @test
    def submatches05(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "::") == List#{}

    @test
    def submatches06(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "ab") == List#{"ab"}

    @test
    def submatches07(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "::a") == List#{"a"}

    @test
    def submatches08(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "a::") == List#{"a"}

    @test
    def submatches09(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "a::b::") == List#{"a", "b"}

    @test
    def submatches10(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "a::b::c") == List#{"a", "b", "c"}

    @test
    def submatches11(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "a::b::c:") == List#{"a", "b", "c"}

    @test
    def submatches12(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", ":a::b::c") == List#{"a", "b", "c"}

    @test
    def submatches13(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "::::") == List#{}

    /////////////////////////////////////////////////////////////////////////////
    // countSubmatches                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def countSubmatches01(): Bool =
        Regex.countSubmatches(substr = Regex.unmatchable(), "") == 0

    @test
    def countSubmatches02(): Bool =
        Regex.countSubmatches(substr = Regex.unmatchable(), "a::b::c") == 0

    @test
    def countSubmatches03(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "") == 0

    @test
    def countSubmatches04(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "a") == 1

    @test
    def countSubmatches05(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "::") == 0

    @test
    def countSubmatches06(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "ab") == 1

    @test
    def countSubmatches07(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "::a") == 1

    @test
    def countSubmatches08(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "a::") == 1

    @test
    def countSubmatches09(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "a::b::") == 2

    @test
    def countSubmatches10(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "a::b::c") == 3

    @test
    def countSubmatches11(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "a::b::c:") == 3

    @test
    def countSubmatches12(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", ":a::b::c") == 3

    @test
    def countSubmatches13(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "::::") == 0

    /////////////////////////////////////////////////////////////////////////////
    // split                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def split01(): Bool =
        Regex.split(regex = regex"\\p{Blank}+", "A B C") == List#{"A", "B", "C"}

    @test
    def split02(): Bool =
        Regex.split(regex = regex"\\p{Blank}+", "A  B   C") == List#{"A", "B", "C"}

    @test
    def split03(): Bool =
        Regex.split(regex = regex"\\p{Blank}+", "ABC") == List#{"ABC"}

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replace01(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "_", "") == ""

    @test
    def replace02(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "_", "A B C") == "A_B_C"

    @test
    def replace03(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "_", "A  B   C") == "A_B_C"

    @test
    def replace04(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "_", "ABC") == "ABC"

    @test
    def replace05(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "__", "A B  C") == "A__B__C"

    @test
    def replace06(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "__", " A B  C  ") == "__A__B__C__"

    /////////////////////////////////////////////////////////////////////////////
    // replaceFirst                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def replaceFirst01(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "_", "") == ""

    @test
    def replaceFirst02(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "_", "A B C") == "A_B C"

    @test
    def replaceFirst03(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "_", "A  B   C") == "A_B   C"

    @test
    def replaceFirst04(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "_", "ABC")  == "ABC"

    @test
    def replaceFirst05(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "__", "A B  C") == "A__B  C"

    @test
    def replaceFirst06(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "__", " A B  C  ") == "__A B  C  "

    /////////////////////////////////////////////////////////////////////////////
    // startsWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def startsWith01(): Bool =
        Regex.startsWith(prefix = Regex.unmatchable(), "") == false

    @test
    def startsWith02(): Bool =
        Regex.startsWith(prefix = Regex.unmatchable(), "A B C") == false

    @test
    def startsWith03(): Bool =
        Regex.startsWith(prefix = regex"\\p{Alpha}+", "") == false

    @test
    def startsWith04(): Bool =
        Regex.startsWith(prefix = regex"\\p{Alpha}+", "   A B C") == false

    @test
    def startsWith05(): Bool =
        Regex.startsWith(prefix = regex"\\p{Alpha}+", "A B C") == true

    @test
    def startsWith06(): Bool =
        Regex.startsWith(prefix = regex"\\p{Alpha}+", "AA BBB CCCC") == true

    /////////////////////////////////////////////////////////////////////////////
    // endsWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def endsWith01(): Bool =
        Regex.endsWith(suffix = Regex.unmatchable(), "") == false

    @test
    def endsWith02(): Bool =
        Regex.endsWith(suffix = Regex.unmatchable(), "A B C") == false

    @test
    def endsWith03(): Bool =
        Regex.endsWith(suffix = regex"\\p{Alpha}+", "") == false

    @test
    def endsWith04(): Bool =
        Regex.endsWith(suffix = regex"\\p{Alpha}+", "A B C  ") == false

    @test
    def endsWith05(): Bool =
        Regex.endsWith(suffix = regex"\\p{Alpha}+", "A B C") == true

    @test
    def endsWith06(): Bool =
        Regex.endsWith(suffix = regex"\\p{Alpha}+", "AA BBB CCCC") == true

    /////////////////////////////////////////////////////////////////////////////
    // getPrefix                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getPrefix01(): Bool =
        Regex.getPrefix(substr = Regex.unmatchable(), "") == None

    @test
    def getPrefix02(): Bool =
        Regex.getPrefix(substr = Regex.unmatchable(), "A B C") == None

    @test
    def getPrefix03(): Bool =
        Regex.getPrefix(substr = regex"\\p{Alpha}+", "") == None

    @test
    def getPrefix04(): Bool =
        Regex.getPrefix(substr = regex"\\p{Alpha}+", "   A B C") == None

    @test
    def getPrefix05(): Bool =
        Regex.getPrefix(substr = regex"\\p{Alpha}+", "A B C") == Some("A")

    @test
    def getPrefix06(): Bool =
        Regex.getPrefix(substr = regex"\\p{Alpha}+", "AA BBB CCCC") == Some("AA")

    /////////////////////////////////////////////////////////////////////////////
    // getSuffix                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getSuffix01(): Bool =
        Regex.getSuffix(substr = Regex.unmatchable(), "") == None

    @test
    def getSuffix02(): Bool =
        Regex.getSuffix(substr = Regex.unmatchable(), "A B C") == None

    @test
    def getSuffix03(): Bool =
        Regex.getSuffix(substr = regex"\\p{Alpha}+", "") == None

    @test
    def getSuffix04(): Bool =
        Regex.getSuffix(substr = regex"\\p{Alpha}+", "A B C  ") == None

    @test
    def getSuffix05(): Bool =
        Regex.getSuffix(substr = regex"\\p{Alpha}+", "A B C") == Some("C")

    @test
    def getSuffix06(): Bool =
        Regex.getSuffix(substr = regex"\\p{Alpha}+", "AA BBB CCCC") == Some("CCCC")

    /////////////////////////////////////////////////////////////////////////////
    // stripPrefix                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stripPrefix01(): Bool =
        Regex.stripPrefix(substr = Regex.unmatchable(), "") == None

    @test
    def stripPrefix02(): Bool =
        Regex.stripPrefix(substr = Regex.unmatchable(), "A B C") == None

    @test
    def stripPrefix03(): Bool =
        Regex.stripPrefix(substr = regex"\\p{Alpha}+", "") == None

    @test
    def stripPrefix04(): Bool =
        Regex.stripPrefix(substr = regex"\\p{Alpha}+", "   A B C") == None

    @test
    def stripPrefix05(): Bool =
        Regex.stripPrefix(substr = regex"\\p{Alpha}+", "A B C") == Some(" B C")

    @test
    def stripPrefix06(): Bool =
        Regex.stripPrefix(substr = regex"\\p{Alpha}+", "AA BBB CCCC") == Some(" BBB CCCC")

    /////////////////////////////////////////////////////////////////////////////
    // stripSuffix                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stripSuffix01(): Bool =
        Regex.stripSuffix(substr = Regex.unmatchable(), "") == None

    @test
    def stripSuffix02(): Bool =
        Regex.stripSuffix(substr = Regex.unmatchable(), "A B C") == None

    @test
    def stripSuffix03(): Bool =
        Regex.stripSuffix(substr = regex"\\p{Alpha}+", "") == None

    @test
    def stripSuffix04(): Bool =
        Regex.stripSuffix(substr = regex"\\p{Alpha}+", "A B C  ") == None

    @test
    def stripSuffix05(): Bool =
        Regex.stripSuffix(substr = regex"\\p{Alpha}+", "A B C") == Some("A B ")

    @test
    def stripSuffix06(): Bool =
        Regex.stripSuffix(substr = regex"\\p{Alpha}+", "AA BBB CCCC") == Some("AA BBB ")

    /////////////////////////////////////////////////////////////////////////////
    // indexOfFirst                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfFirst01(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "") == None

    @test
    def indexOfFirst02(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "_") == None

    @test
    def indexOfFirst03(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "a") == Some(0)

    @test
    def indexOfFirst04(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "__") == None

    @test
    def indexOfFirst05(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "ab") == Some(0)

    @test
    def indexOfFirst06(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "_b") == Some(1)

    @test
    def indexOfFirst07(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "ab") == Some(0)

    @test
    def indexOfFirst08(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "ab_") == Some(0)

    @test
    def indexOfFirst09(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "_bc") == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLast                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLast01(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "") == None

    @test
    def indexOfLast02(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "_") == None

    @test
    def indexOfLast03(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "a") == Some(0)

    @test
    def indexOfLast04(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "__") == None

    @test
    def indexOfLast05(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "a_") == Some(0)

    @test
    def indexOfLast06(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "_b") == Some(1)

    @test
    def indexOfLast07(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "aa") == Some(0)

    @test
    def indexOfLast08(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}{2}", "ab") == Some(0)

    @test
    def indexOfLast09(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}{2}", "abcd") == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // getFirst                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getFirst01(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "") == None

    @test
    def getFirst02(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "_") == None

    @test
    def getFirst03(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "a") == Some("a")

    @test
    def getFirst04(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "__") == None

    @test
    def getFirst05(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "ab") == Some("ab")

    @test
    def getFirst06(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "_b") == Some("b")

    @test
    def getFirst07(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "a_") == Some("a")

    @test
    def getFirst08(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "ab_") == Some("ab")

    @test
    def getFirst09(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "_bc") == Some("bc")

    /////////////////////////////////////////////////////////////////////////////
    // getLast                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getLast01(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "") == None

    @test
    def getLast02(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "_") == None

    @test
    def getLast03(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "a") == Some("a")

    @test
    def getLast04(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "__") == None

    @test
    def getLast05(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "a_") == Some("a")

    @test
    def getLast06(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "_b") == Some("b")

    @test
    def getLast07(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "aa") == Some("aa")

    @test
    def getLast08(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}{2}", "ab") == Some("ab")

    @test
    def getLast09(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}{2}", "abcd") == Some("cd")

    /////////////////////////////////////////////////////////////////////////////
    // indexOfFirstWithOffset                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfFirstWithOffset01(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "") == None

    @test
    def indexOfFirstWithOffset02(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_") == None

    @test
    def indexOfFirstWithOffset03(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "a") == Some(0)

    @test
    def indexOfFirstWithOffset04(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "a") == None

    @test
    def indexOfFirstWithOffset05(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab") == Some(0)

    @test
    def indexOfFirstWithOffset06(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "aa") == Some(1)

    @test
    def indexOfFirstWithOffset07(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_b") == Some(1)

    @test
    def indexOfFirstWithOffset08(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab ab") == Some(0)

    @test
    def indexOfFirstWithOffset09(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab ab") == Some(1)

    @test
    def indexOfFirstWithOffset10(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "ab ab") == Some(3)

    @test
    def indexOfFirstWithOffset11(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab ab") == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLastWithOffset                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indexOfLastWithOffset01(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "") == None

    @test
    def indexOfLastWithOffset02(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_") == None

    @test
    def indexOfLastWithOffset03(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "a") == Some(0)

    @test
    def indexOfLastWithOffset04(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "a") == None

    @test
    def indexOfLastWithOffset05(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab") == Some(1)

    @test
    def indexOfLastWithOffset06(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "aa") == None

    @test
    def indexOfLastWithOffset07(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab") == None

    @test
    def indexOfLastWithOffset08(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab ab") == Some(3)

    @test
    def indexOfLastWithOffset09(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset= 2, "ab ab") == Some(3)

    @test
    def indexOfLastWithOffset10(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab ab") == Some(3)

    @test
    def indexOfLastWithOffset11(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab ab") == Some(3)

    @test
    def indexOfLastWithOffset12(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = -1, "ab ab") == None

    @test
    def indexOfLastWithOffset13(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "ab __") == None

    /////////////////////////////////////////////////////////////////////////////
    // getFirstWithOffset                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getFirstWithOffset01(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "") == None

    @test
    def getFirstWithOffset02(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_") == None

    @test
    def getFirstWithOffset03(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "a") == Some("a")

    @test
    def getFirstWithOffset04(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "a") == None

    @test
    def getFirstWithOffset05(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab") == Some("ab")

    @test
    def getFirstWithOffset06(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "aa") == Some("a")

    @test
    def getFirstWithOffset07(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_b") == Some("b")

    @test
    def getFirstWithOffset08(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab cd") == Some("ab")

    @test
    def getFirstWithOffset09(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab cd") == Some("b")

    @test
    def getFirstWithOffset10(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "ab cd") == Some("cd")

    @test
    def getFirstWithOffset11(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab cd") == Some("cd")

    /////////////////////////////////////////////////////////////////////////////
    // getLastWithOffset                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getLastWithOffset01(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "") == None

    @test
    def getLastWithOffset02(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_") == None

    @test
    def getLastWithOffset03(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "a") == Some("a")

    @test
    def getLastWithOffset04(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "a") == None

    @test
    def getLastWithOffset05(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab") == Some("b")

    @test
    def getLastWithOffset06(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "aa") == None

    @test
    def getLastWithOffset07(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab") == None

    @test
    def getLastWithOffset08(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab cd") == Some("cd")

    @test
    def getLastWithOffset09(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset= 2, "ab cd") == Some("cd")

    @test
    def getLastWithOffset10(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab cd") == Some("cd")

    @test
    def getLastWithOffset11(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab cd") == Some("cd")

    @test
    def getLastWithOffset12(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = -1, "ab cd") == None

    @test
    def getLastWithOffset13(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "ab __") == None

    /////////////////////////////////////////////////////////////////////////////
    // breakOnFirst                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def breakOnFirst01(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "") == ("", "")

    @test
    def breakOnFirst02(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "aaa") == ("aaa", "")

    @test
    def breakOnFirst03(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "::") == ("", "::")

    @test
    def breakOnFirst04(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "::aaa") == ("", "::aaa")

    @test
    def breakOnFirst05(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "aaa:") == ("aaa:", "")

    @test
    def breakOnFirst06(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "aaa::") == ("aaa", "::")

    @test
    def breakOnFirst07(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "aaa::bbb") == ("aaa", "::bbb")

    @test
    def breakOnFirst08(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "aaa::bbb::ccc") == ("aaa", "::bbb::ccc")

    /////////////////////////////////////////////////////////////////////////////
    // breakAfterFirst                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def breakAfterFirst01(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "") == ("", "")

    @test
    def breakAfterFirst02(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "aaa") == ("aaa", "")

    @test
    def breakAfterFirst03(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "::") == ("::", "")

    @test
    def breakAfterFirst04(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "::aaa") == ("::", "aaa")

    @test
    def breakAfterFirst05(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "aaa:") == ("aaa:", "")

    @test
    def breakAfterFirst06(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "aaa::") == ("aaa::", "")

    @test
    def breakAfterFirst07(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "aaa::bbb") == ("aaa::", "bbb")

    @test
    def breakAfterFirst08(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "aaa::bbb::ccc") == ("aaa::", "bbb::ccc")

    /////////////////////////////////////////////////////////////////////////////
    // breakOnLast                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def breakOnLast01(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "") == ("", "")

    @test
    def breakOnLast02(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa") == ("aaa", "")

    @test
    def breakOnLast03(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "::") == ("::", "")

    @test
    def breakOnLast04(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa") == ("aaa", "")

    @test
    def breakOnLast05(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa:") == ("aaa:", "")

    @test
    def breakOnLast06(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa::") == ("aaa::", "")

    @test
    def breakOnLast07(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa::bbb") == ("aaa::", "bbb")

    @test
    def breakOnLast08(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa::bbb::ccc") == ("aaa::bbb::", "ccc")

    /////////////////////////////////////////////////////////////////////////////
    // breakBeforeLast                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def breakBeforeLast01(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "") == ("", "")

    @test
    def breakBeforeLast02(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa") == ("aaa", "")

    @test
    def breakBeforeLast03(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "::") == ("", "::")

    @test
    def breakBeforeLast04(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa") == ("aaa", "")

    @test
    def breakBeforeLast05(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa:") == ("aaa:", "")

    @test
    def breakBeforeLast06(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa::") == ("aaa", "::")

    @test
    def breakBeforeLast07(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa::bbb") == ("aaa", "::bbb")

    @test
    def breakBeforeLast08(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa::bbb::ccc") == ("aaa::bbb", "::ccc")

    /////////////////////////////////////////////////////////////////////////////
    // isMatchWithBounds                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isMatchWithBounds01(): Bool =
        Regex.isMatchWithBounds(regex"a+", start = 0, end = 1, "") == false

    @test
    def isMatchWithBounds02(): Bool =
        Regex.isMatchWithBounds(regex"a+", start = 0, end = 1, "a") == true

    @test
    def isMatchWithBounds03(): Bool =
        Regex.isMatchWithBounds(regex"a+", start = 0, end = 3, "a") == false

    @test
    def isMatchWithBounds04(): Bool =
        Regex.isMatchWithBounds(regex"a+", start = 0, end = 1, "aa") == true

    @test
    def isMatchWithBounds05(): Bool =
        Regex.isMatchWithBounds(regex"a+", start = 2, end = 3, "aab") == false

    @test
    def isMatchWithBounds06(): Bool =
        Regex.isMatchWithBounds(regex"b+", start = 2, end = 3, "aab") == true

    /////////////////////////////////////////////////////////////////////////////
    // isSubmatchWithBounds                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isSubmatchWithBounds01(): Bool =
        Regex.isSubmatchWithBounds(regex"a+", start = 0, end = 1, "") == false

    @test
    def isSubmatchWithBounds02(): Bool =
        Regex.isSubmatchWithBounds(regex"a+", start = 0, end = 1, "a") == true

    @test
    def isSubmatchWithBounds03(): Bool =
        Regex.isSubmatchWithBounds(regex"a+", start = 1, end = 2, "aa") == true

    @test
    def isSubmatchWithBounds04(): Bool =
        Regex.isSubmatchWithBounds(regex"a+", start = -1, end = 1, "aab") == false

    @test
    def isSubmatchWithBounds05(): Bool =
        Regex.isSubmatchWithBounds(regex"a+", start = 0, end = 2, "aab") == true

    @test
    def isSubmatchWithBounds06(): Bool =
        Regex.isSubmatchWithBounds(regex"b+", start = 2, end = 3, "aab") == true

    @test
    def isSubmatchWithBounds07(): Bool =
        Regex.isSubmatchWithBounds(regex"b+", start = 1, end = 2, "bbbc") == true

    @test
    def isSubmatchWithBounds08(): Bool =
        Regex.isSubmatchWithBounds(regex"b+", start = 0, end = 5, "aabbbc") == true

    /////////////////////////////////////////////////////////////////////////////
    // indicesWithBounds                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def indicesWithBounds01(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = 0, end = 1, "") == List#{}

    @test
    def indicesWithBounds02(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = -1, end = 10, "a::b::c") == List#{}

    @test
    def indicesWithBounds03(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = 0, end = 7, "a::b::c") == List#{1, 4}

    @test
    def indicesWithBounds04(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = 0, end = 3, "a::b::c") == List#{1}

    @test
    def indicesWithBounds05(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = 3, end = 7, "a::b::c") == List#{4}

    @test
    def indicesWithBounds06(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = 0, end = 3, "a:b") == List#{}

    /////////////////////////////////////////////////////////////////////////////
    // submatchesWithBounds                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def submatchesWithBounds01(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "") == List#{}

    @test
    def submatchesWithBounds02(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = -1, end = 10, "a::b::c") == List#{}

    @test
    def submatchesWithBounds03(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "aaa") == List#{"aaa"}

    @test
    def submatchesWithBounds04(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 10, "aaa") == List#{}

    @test
    def submatchesWithBounds05(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "::") == List#{}

    @test
    def submatchesWithBounds06(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "ab") == List#{"ab"}

    @test
    def submatchesWithBounds07(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "::a") == List#{"a"}

    @test
    def submatchesWithBounds08(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "a::") == List#{"a"}

    @test
    def submatchesWithBounds09(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "a::b::") == List#{"a"}

    @test
    def submatchesWithBounds10(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 2, end = 7, "a::b::c") == List#{"b", "c"}

    @test
    def submatchesWithBounds11(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c:") == List#{"a", "b", "c"}

    @test
    def submatchesWithBounds12(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 6, ":a::b::c") == List#{"a", "b"}

    @test
    def submatchesWithBounds13(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, "::::") == List#{}

    /////////////////////////////////////////////////////////////////////////////
    // countSubmatchesWithBounds                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def countSubmatchesWithBounds01(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "") == 0

    @test
    def countSubmatchesWithBounds02(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = -1, end = 10, "a::b::c") == 0

    @test
    def countSubmatchesWithBounds03(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c") == 3

    @test
    def countSubmatchesWithBounds04(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, "a::b::c") == 2

    @test
    def countSubmatchesWithBounds05(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 1, end = 7, "a::b::c") == 2

    @test
    def countSubmatchesWithBounds06(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "ab") == 1

    @test
    def countSubmatchesWithBounds07(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 1, end = 3, "::a") == 1

    @test
    def countSubmatchesWithBounds08(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "a::") == 1

    @test
    def countSubmatchesWithBounds09(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, "a::b::") == 2

    @test
    def countSubmatchesWithBounds10(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, "a::b::c") == 2

    @test
    def countSubmatchesWithBounds11(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 4, end = 8, "a::b::c:") == 1

    @test
    def countSubmatchesWithBounds12(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, ":a::b::c") == 1

    @test
    def countSubmatchesWithBounds13(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, "::::") == 0

    /////////////////////////////////////////////////////////////////////////////
    // getFirstWithBounds                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getFirstWithBounds01(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "") == None

    @test
    def getFirstWithBounds02(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = -1, end = 5, "a::b") == None

    @test
    def getFirstWithBounds03(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "a") == Some("a")

    @test
    def getFirstWithBounds04(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c") == Some("a")

    @test
    def getFirstWithBounds05(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "a::b::c") == Some("a")

    @test
    def getFirstWithBounds06(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 3, end = 7, "a::b::c") == Some("b")

    @test
    def getFirstWithBounds07(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 6, end = 7, "a::b::c") == Some("c")

    @test
    def getFirstWithBounds08(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 1, end = 6, "a::b::c") == Some("b")

    @test
    def getFirstWithBounds09(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 7, end = 9, "a::b::c::") == None

    /////////////////////////////////////////////////////////////////////////////
    // getLastWithBounds                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def getLastWithBounds01(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "") == None

    @test
    def getLastWithBounds02(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = -1, end = 5, "a::b") == None

    @test
    def getLastWithBounds03(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "a") == Some("a")

    @test
    def getLastWithBounds04(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c") == Some("c")

    @test
    def getLastWithBounds05(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 5, end = 7, "a::b::c") == Some("c")

    @test
    def getLastWithBounds06(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 6, "a::b::c") == Some("b")

    @test
    def getLastWithBounds07(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "a::b::c") == Some("a")

    @test
    def getLastWithBounds08(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 1, end = 6, "a::b::c") == Some("b")

    @test
    def getLastWithBounds09(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "::a::b::c") == None

}
