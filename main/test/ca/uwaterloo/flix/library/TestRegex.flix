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

    @Test
    def toString01(): Bool =
        ToString.toString(Regex.unmatchable()) != ""

    /////////////////////////////////////////////////////////////////////////////
    // unmatchable                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unmatchable01(): Bool =
        Regex.unmatchable() |> Regex.pattern == "^\\b$"

    /////////////////////////////////////////////////////////////////////////////
    // quote                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def quote01(): Bool =
        Regex.quote("ab") == "\\Qab\\E"

    @Test
    def quote02(): Bool =
        Regex.quote("a*b") == "\\Qa*b\\E"

    /////////////////////////////////////////////////////////////////////////////
    // pattern                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def pattern01(): Bool =
        Regex.pattern(regex"a*b") == "a*b"

    @Test
    def pattern02(): Bool =
        Regex.pattern(regex"\\p{Blank}+") == "\\p{Blank}+"

    /////////////////////////////////////////////////////////////////////////////
    // flags                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flags01(): Bool =
        Result.map(Regex.flags, String.toRegex("\\p{Blank}+")) == Ok(Set#{})

    @Test
    def flags02(): Bool =
        Result.map(Regex.flags, String.toRegexWithFlags(Set#{}, "\\p{Blank}+")) == Ok(Set#{})

    @Test
    def flags03(): Bool =
        use Regex.Flag.{CaseInsenstive};
        Result.map(Regex.flags, String.toRegexWithFlags(Set#{CaseInsenstive}, "\\p{Blank}")) == Ok(Set#{CaseInsenstive})

    @Test
    def flags04(): Bool =
        use Regex.Flag.{CaseInsenstive, Multiline};
        Result.map(Regex.flags, String.toRegexWithFlags(Set#{CaseInsenstive, Multiline}, "\\p{Blank}")) == Ok(Set#{CaseInsenstive, Multiline})

    /////////////////////////////////////////////////////////////////////////////
    // isMatch                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isMatch01(): Bool =
        Regex.isMatch(regex"a+", "") == false

    @Test
    def isMatch02(): Bool =
        Regex.isMatch(regex"a*", "") == true

    @Test
    def isMatch03(): Bool =
        Regex.isMatch(regex"a*", "a") == true

    @Test
    def isMatch04(): Bool =
        Regex.isMatch(regex"a*", "aa") == true

    @Test
    def isMatch05(): Bool =
        Regex.isMatch(regex"a*", "aab") == false

    @Test
    def isMatch06(): Bool =
        Regex.isMatch(regex"a*b", "aab") == true

    /////////////////////////////////////////////////////////////////////////////
    // isSubmatch                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSubmatch01(): Bool =
        Regex.isSubmatch(regex"a+", "") == false

    @Test
    def isSubmatch02(): Bool =
        Regex.isSubmatch(regex"a*", "a") == true

    @Test
    def isSubmatch03(): Bool =
        Regex.isSubmatch(regex"a*", "aa") == true

    @Test
    def isSubmatch04(): Bool =
        Regex.isSubmatch(regex"a*", "aab") == true

    @Test
    def isSubmatch05(): Bool =
        Regex.isSubmatch(regex"a*b", "aab") == true

    @Test
    def isSubmatch06(): Bool =
        Regex.isSubmatch(regex"b+", "aab") == true

    @Test
    def isSubmatch07(): Bool =
        Regex.isSubmatch(regex"b+", "bbbc") == true

    @Test
    def isSubmatch08(): Bool =
        Regex.isSubmatch(regex"b+", "aabbbc") == true

    /////////////////////////////////////////////////////////////////////////////
    // indices                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indices01(): Bool =
        Regex.indices(substr = Regex.unmatchable(), "") == List#{}

    @Test
    def indices02(): Bool =
        Regex.indices(substr = Regex.unmatchable(), "a::b::c") == List#{}

    @Test
    def indices03(): Bool =
        Regex.indices(substr = regex":{2}", "") == List#{}

    @Test
    def indices04(): Bool =
        Regex.indices(substr = regex":{2}", "a") == List#{}

    @Test
    def indices05(): Bool =
        Regex.indices(substr = regex":{2}", "::") == List#{0}

    @Test
    def indices06(): Bool =
        Regex.indices(substr = regex":{2}", "ab") == List#{}

    @Test
    def indices07(): Bool =
        Regex.indices(substr = regex":{2}", "::a") == List#{0}

    @Test
    def indices08(): Bool =
        Regex.indices(substr = regex":{2}", "a::") == List#{1}

    @Test
    def indices09(): Bool =
        Regex.indices(substr = regex":{2}", "a::b::") == List#{1, 4}

    @Test
    def indices10(): Bool =
        Regex.indices(substr = regex":{2}", "a::b::c") == List#{1, 4}

    @Test
    def indices11(): Bool =
        Regex.indices(substr = regex":{2}", "a::b::c:") == List#{1, 4}

    @Test
    def indices12(): Bool =
        Regex.indices(substr = regex":{2}", ":a::b::c") == List#{2, 5}

    @Test
    def indices13(): Bool =
        Regex.indices(substr = regex":{2}", "::::") == List#{0, 2}

    /////////////////////////////////////////////////////////////////////////////
    // submatches                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def submatches01(): Bool =
        Regex.submatches(substr = Regex.unmatchable(), "") == List#{}

    @Test
    def submatches02(): Bool =
        Regex.submatches(substr = Regex.unmatchable(), "a::b::c") == List#{}

    @Test
    def submatches03(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "") == List#{}

    @Test
    def submatches04(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "a") == List#{"a"}

    @Test
    def submatches05(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "::") == List#{}

    @Test
    def submatches06(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "ab") == List#{"ab"}

    @Test
    def submatches07(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "::a") == List#{"a"}

    @Test
    def submatches08(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "a::") == List#{"a"}

    @Test
    def submatches09(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "a::b::") == List#{"a", "b"}

    @Test
    def submatches10(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "a::b::c") == List#{"a", "b", "c"}

    @Test
    def submatches11(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "a::b::c:") == List#{"a", "b", "c"}

    @Test
    def submatches12(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", ":a::b::c") == List#{"a", "b", "c"}

    @Test
    def submatches13(): Bool =
        Regex.submatches(substr = regex"\\p{Alpha}+", "::::") == List#{}

    /////////////////////////////////////////////////////////////////////////////
    // countSubmatches                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def countSubmatches01(): Bool =
        Regex.countSubmatches(substr = Regex.unmatchable(), "") == 0

    @Test
    def countSubmatches02(): Bool =
        Regex.countSubmatches(substr = Regex.unmatchable(), "a::b::c") == 0

    @Test
    def countSubmatches03(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "") == 0

    @Test
    def countSubmatches04(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "a") == 1

    @Test
    def countSubmatches05(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "::") == 0

    @Test
    def countSubmatches06(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "ab") == 1

    @Test
    def countSubmatches07(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "::a") == 1

    @Test
    def countSubmatches08(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "a::") == 1

    @Test
    def countSubmatches09(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "a::b::") == 2

    @Test
    def countSubmatches10(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "a::b::c") == 3

    @Test
    def countSubmatches11(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "a::b::c:") == 3

    @Test
    def countSubmatches12(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", ":a::b::c") == 3

    @Test
    def countSubmatches13(): Bool =
        Regex.countSubmatches(substr = regex"\\p{Alpha}+", "::::") == 0

    /////////////////////////////////////////////////////////////////////////////
    // split                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def split01(): Bool =
        Regex.split(regex = regex"\\p{Blank}+", "A B C") == List#{"A", "B", "C"}

    @Test
    def split02(): Bool =
        Regex.split(regex = regex"\\p{Blank}+", "A  B   C") == List#{"A", "B", "C"}

    @Test
    def split03(): Bool =
        Regex.split(regex = regex"\\p{Blank}+", "ABC") == List#{"ABC"}

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "_", "") == ""

    @Test
    def replace02(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "_", "A B C") == "A_B_C"

    @Test
    def replace03(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "_", "A  B   C") == "A_B_C"

    @Test
    def replace04(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "_", "ABC") == "ABC"

    @Test
    def replace05(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "__", "A B  C") == "A__B__C"

    @Test
    def replace06(): Bool =
        Regex.replace(from = regex"\\p{Blank}+", to = "__", " A B  C  ") == "__A__B__C__"

    /////////////////////////////////////////////////////////////////////////////
    // replaceFirst                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replaceFirst01(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "_", "") == ""

    @Test
    def replaceFirst02(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "_", "A B C") == "A_B C"

    @Test
    def replaceFirst03(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "_", "A  B   C") == "A_B   C"

    @Test
    def replaceFirst04(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "_", "ABC")  == "ABC"

    @Test
    def replaceFirst05(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "__", "A B  C") == "A__B  C"

    @Test
    def replaceFirst06(): Bool =
        Regex.replaceFirst(from = regex"\\p{Blank}+", to = "__", " A B  C  ") == "__A B  C  "

    /////////////////////////////////////////////////////////////////////////////
    // startsWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def startsWith01(): Bool =
        Regex.startsWith(prefix = Regex.unmatchable(), "") == false

    @Test
    def startsWith02(): Bool =
        Regex.startsWith(prefix = Regex.unmatchable(), "A B C") == false

    @Test
    def startsWith03(): Bool =
        Regex.startsWith(prefix = regex"\\p{Alpha}+", "") == false

    @Test
    def startsWith04(): Bool =
        Regex.startsWith(prefix = regex"\\p{Alpha}+", "   A B C") == false

    @Test
    def startsWith05(): Bool =
        Regex.startsWith(prefix = regex"\\p{Alpha}+", "A B C") == true

    @Test
    def startsWith06(): Bool =
        Regex.startsWith(prefix = regex"\\p{Alpha}+", "AA BBB CCCC") == true

    /////////////////////////////////////////////////////////////////////////////
    // endsWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def endsWith01(): Bool =
        Regex.endsWith(suffix = Regex.unmatchable(), "") == false

    @Test
    def endsWith02(): Bool =
        Regex.endsWith(suffix = Regex.unmatchable(), "A B C") == false

    @Test
    def endsWith03(): Bool =
        Regex.endsWith(suffix = regex"\\p{Alpha}+", "") == false

    @Test
    def endsWith04(): Bool =
        Regex.endsWith(suffix = regex"\\p{Alpha}+", "A B C  ") == false

    @Test
    def endsWith05(): Bool =
        Regex.endsWith(suffix = regex"\\p{Alpha}+", "A B C") == true

    @Test
    def endsWith06(): Bool =
        Regex.endsWith(suffix = regex"\\p{Alpha}+", "AA BBB CCCC") == true

    /////////////////////////////////////////////////////////////////////////////
    // getPrefix                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getPrefix01(): Bool =
        Regex.getPrefix(substr = Regex.unmatchable(), "") == None

    @Test
    def getPrefix02(): Bool =
        Regex.getPrefix(substr = Regex.unmatchable(), "A B C") == None

    @Test
    def getPrefix03(): Bool =
        Regex.getPrefix(substr = regex"\\p{Alpha}+", "") == None

    @Test
    def getPrefix04(): Bool =
        Regex.getPrefix(substr = regex"\\p{Alpha}+", "   A B C") == None

    @Test
    def getPrefix05(): Bool =
        Regex.getPrefix(substr = regex"\\p{Alpha}+", "A B C") == Some("A")

    @Test
    def getPrefix06(): Bool =
        Regex.getPrefix(substr = regex"\\p{Alpha}+", "AA BBB CCCC") == Some("AA")

    /////////////////////////////////////////////////////////////////////////////
    // getSuffix                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getSuffix01(): Bool =
        Regex.getSuffix(substr = Regex.unmatchable(), "") == None

    @Test
    def getSuffix02(): Bool =
        Regex.getSuffix(substr = Regex.unmatchable(), "A B C") == None

    @Test
    def getSuffix03(): Bool =
        Regex.getSuffix(substr = regex"\\p{Alpha}+", "") == None

    @Test
    def getSuffix04(): Bool =
        Regex.getSuffix(substr = regex"\\p{Alpha}+", "A B C  ") == None

    @Test
    def getSuffix05(): Bool =
        Regex.getSuffix(substr = regex"\\p{Alpha}+", "A B C") == Some("C")

    @Test
    def getSuffix06(): Bool =
        Regex.getSuffix(substr = regex"\\p{Alpha}+", "AA BBB CCCC") == Some("CCCC")

    /////////////////////////////////////////////////////////////////////////////
    // stripPrefix                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stripPrefix01(): Bool =
        Regex.stripPrefix(substr = Regex.unmatchable(), "") == None

    @Test
    def stripPrefix02(): Bool =
        Regex.stripPrefix(substr = Regex.unmatchable(), "A B C") == None

    @Test
    def stripPrefix03(): Bool =
        Regex.stripPrefix(substr = regex"\\p{Alpha}+", "") == None

    @Test
    def stripPrefix04(): Bool =
        Regex.stripPrefix(substr = regex"\\p{Alpha}+", "   A B C") == None

    @Test
    def stripPrefix05(): Bool =
        Regex.stripPrefix(substr = regex"\\p{Alpha}+", "A B C") == Some(" B C")

    @Test
    def stripPrefix06(): Bool =
        Regex.stripPrefix(substr = regex"\\p{Alpha}+", "AA BBB CCCC") == Some(" BBB CCCC")

    /////////////////////////////////////////////////////////////////////////////
    // stripSuffix                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stripSuffix01(): Bool =
        Regex.stripSuffix(substr = Regex.unmatchable(), "") == None

    @Test
    def stripSuffix02(): Bool =
        Regex.stripSuffix(substr = Regex.unmatchable(), "A B C") == None

    @Test
    def stripSuffix03(): Bool =
        Regex.stripSuffix(substr = regex"\\p{Alpha}+", "") == None

    @Test
    def stripSuffix04(): Bool =
        Regex.stripSuffix(substr = regex"\\p{Alpha}+", "A B C  ") == None

    @Test
    def stripSuffix05(): Bool =
        Regex.stripSuffix(substr = regex"\\p{Alpha}+", "A B C") == Some("A B ")

    @Test
    def stripSuffix06(): Bool =
        Regex.stripSuffix(substr = regex"\\p{Alpha}+", "AA BBB CCCC") == Some("AA BBB ")

    /////////////////////////////////////////////////////////////////////////////
    // indexOfFirst                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfFirst01(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "") == None

    @Test
    def indexOfFirst02(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "_") == None

    @Test
    def indexOfFirst03(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "a") == Some(0)

    @Test
    def indexOfFirst04(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "__") == None

    @Test
    def indexOfFirst05(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "ab") == Some(0)

    @Test
    def indexOfFirst06(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "_b") == Some(1)

    @Test
    def indexOfFirst07(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "ab") == Some(0)

    @Test
    def indexOfFirst08(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "ab_") == Some(0)

    @Test
    def indexOfFirst09(): Bool =
        Regex.indexOfFirst(substr = regex"\\p{Alpha}+", "_bc") == Some(1)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLast                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLast01(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "") == None

    @Test
    def indexOfLast02(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "_") == None

    @Test
    def indexOfLast03(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "a") == Some(0)

    @Test
    def indexOfLast04(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "__") == None

    @Test
    def indexOfLast05(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "a_") == Some(0)

    @Test
    def indexOfLast06(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "_b") == Some(1)

    @Test
    def indexOfLast07(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}+", "aa") == Some(0)

    @Test
    def indexOfLast08(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}{2}", "ab") == Some(0)

    @Test
    def indexOfLast09(): Bool =
        Regex.indexOfLast(substr = regex"\\p{Alpha}{2}", "abcd") == Some(2)

    /////////////////////////////////////////////////////////////////////////////
    // getFirst                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getFirst01(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "") == None

    @Test
    def getFirst02(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "_") == None

    @Test
    def getFirst03(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "a") == Some("a")

    @Test
    def getFirst04(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "__") == None

    @Test
    def getFirst05(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "ab") == Some("ab")

    @Test
    def getFirst06(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "_b") == Some("b")

    @Test
    def getFirst07(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "a_") == Some("a")

    @Test
    def getFirst08(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "ab_") == Some("ab")

    @Test
    def getFirst09(): Bool =
        Regex.getFirst(substr = regex"\\p{Alpha}+", "_bc") == Some("bc")

    /////////////////////////////////////////////////////////////////////////////
    // getLast                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getLast01(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "") == None

    @Test
    def getLast02(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "_") == None

    @Test
    def getLast03(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "a") == Some("a")

    @Test
    def getLast04(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "__") == None

    @Test
    def getLast05(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "a_") == Some("a")

    @Test
    def getLast06(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "_b") == Some("b")

    @Test
    def getLast07(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}+", "aa") == Some("aa")

    @Test
    def getLast08(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}{2}", "ab") == Some("ab")

    @Test
    def getLast09(): Bool =
        Regex.getLast(substr = regex"\\p{Alpha}{2}", "abcd") == Some("cd")

    /////////////////////////////////////////////////////////////////////////////
    // indexOfFirstWithOffset                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfFirstWithOffset01(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "") == None

    @Test
    def indexOfFirstWithOffset02(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_") == None

    @Test
    def indexOfFirstWithOffset03(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "a") == Some(0)

    @Test
    def indexOfFirstWithOffset04(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "a") == None

    @Test
    def indexOfFirstWithOffset05(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab") == Some(0)

    @Test
    def indexOfFirstWithOffset06(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "aa") == Some(1)

    @Test
    def indexOfFirstWithOffset07(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_b") == Some(1)

    @Test
    def indexOfFirstWithOffset08(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab ab") == Some(0)

    @Test
    def indexOfFirstWithOffset09(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab ab") == Some(1)

    @Test
    def indexOfFirstWithOffset10(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "ab ab") == Some(3)

    @Test
    def indexOfFirstWithOffset11(): Bool =
        Regex.indexOfFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab ab") == Some(3)

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLastWithOffset                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLastWithOffset01(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "") == None

    @Test
    def indexOfLastWithOffset02(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_") == None

    @Test
    def indexOfLastWithOffset03(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "a") == Some(0)

    @Test
    def indexOfLastWithOffset04(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "a") == None

    @Test
    def indexOfLastWithOffset05(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab") == Some(1)

    @Test
    def indexOfLastWithOffset06(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "aa") == None

    @Test
    def indexOfLastWithOffset07(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab") == None

    @Test
    def indexOfLastWithOffset08(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab ab") == Some(3)

    @Test
    def indexOfLastWithOffset09(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset= 2, "ab ab") == Some(3)

    @Test
    def indexOfLastWithOffset10(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab ab") == Some(3)

    @Test
    def indexOfLastWithOffset11(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab ab") == Some(3)

    @Test
    def indexOfLastWithOffset12(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = -1, "ab ab") == None

    @Test
    def indexOfLastWithOffset13(): Bool =
        Regex.indexOfLastWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "ab __") == None

    /////////////////////////////////////////////////////////////////////////////
    // getFirstWithOffset                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getFirstWithOffset01(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "") == None

    @Test
    def getFirstWithOffset02(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_") == None

    @Test
    def getFirstWithOffset03(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "a") == Some("a")

    @Test
    def getFirstWithOffset04(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "a") == None

    @Test
    def getFirstWithOffset05(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab") == Some("ab")

    @Test
    def getFirstWithOffset06(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "aa") == Some("a")

    @Test
    def getFirstWithOffset07(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_b") == Some("b")

    @Test
    def getFirstWithOffset08(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab cd") == Some("ab")

    @Test
    def getFirstWithOffset09(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab cd") == Some("b")

    @Test
    def getFirstWithOffset10(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "ab cd") == Some("cd")

    @Test
    def getFirstWithOffset11(): Bool =
        Regex.getFirstWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab cd") == Some("cd")

    /////////////////////////////////////////////////////////////////////////////
    // getLastWithOffset                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getLastWithOffset01(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "") == None

    @Test
    def getLastWithOffset02(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "_") == None

    @Test
    def getLastWithOffset03(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "a") == Some("a")

    @Test
    def getLastWithOffset04(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "a") == None

    @Test
    def getLastWithOffset05(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab") == Some("b")

    @Test
    def getLastWithOffset06(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "aa") == None

    @Test
    def getLastWithOffset07(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab") == None

    @Test
    def getLastWithOffset08(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 3, "ab cd") == Some("cd")

    @Test
    def getLastWithOffset09(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset= 2, "ab cd") == Some("cd")

    @Test
    def getLastWithOffset10(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 1, "ab cd") == Some("cd")

    @Test
    def getLastWithOffset11(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 0, "ab cd") == Some("cd")

    @Test
    def getLastWithOffset12(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = -1, "ab cd") == None

    @Test
    def getLastWithOffset13(): Bool =
        Regex.getLastWithOffset(substr = regex"\\p{Alpha}+", offset = 2, "ab __") == None

    /////////////////////////////////////////////////////////////////////////////
    // breakOnFirst                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def breakOnFirst01(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "") == ("", "")

    @Test
    def breakOnFirst02(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "aaa") == ("aaa", "")

    @Test
    def breakOnFirst03(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "::") == ("", "::")

    @Test
    def breakOnFirst04(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "::aaa") == ("", "::aaa")

    @Test
    def breakOnFirst05(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "aaa:") == ("aaa:", "")

    @Test
    def breakOnFirst06(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "aaa::") == ("aaa", "::")

    @Test
    def breakOnFirst07(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "aaa::bbb") == ("aaa", "::bbb")

    @Test
    def breakOnFirst08(): Bool =
        Regex.breakOnFirst(substr = regex":{2}", "aaa::bbb::ccc") == ("aaa", "::bbb::ccc")

    /////////////////////////////////////////////////////////////////////////////
    // breakAfterFirst                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def breakAfterFirst01(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "") == ("", "")

    @Test
    def breakAfterFirst02(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "aaa") == ("aaa", "")

    @Test
    def breakAfterFirst03(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "::") == ("::", "")

    @Test
    def breakAfterFirst04(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "::aaa") == ("::", "aaa")

    @Test
    def breakAfterFirst05(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "aaa:") == ("aaa:", "")

    @Test
    def breakAfterFirst06(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "aaa::") == ("aaa::", "")

    @Test
    def breakAfterFirst07(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "aaa::bbb") == ("aaa::", "bbb")

    @Test
    def breakAfterFirst08(): Bool =
        Regex.breakAfterFirst(substr = regex":{2}", "aaa::bbb::ccc") == ("aaa::", "bbb::ccc")

    /////////////////////////////////////////////////////////////////////////////
    // breakOnLast                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def breakOnLast01(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "") == ("", "")

    @Test
    def breakOnLast02(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa") == ("aaa", "")

    @Test
    def breakOnLast03(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "::") == ("::", "")

    @Test
    def breakOnLast04(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa") == ("aaa", "")

    @Test
    def breakOnLast05(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa:") == ("aaa:", "")

    @Test
    def breakOnLast06(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa::") == ("aaa::", "")

    @Test
    def breakOnLast07(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa::bbb") == ("aaa::", "bbb")

    @Test
    def breakOnLast08(): Bool =
        Regex.breakOnLast(substr = regex":{2}", "aaa::bbb::ccc") == ("aaa::bbb::", "ccc")

    /////////////////////////////////////////////////////////////////////////////
    // breakBeforeLast                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def breakBeforeLast01(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "") == ("", "")

    @Test
    def breakBeforeLast02(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa") == ("aaa", "")

    @Test
    def breakBeforeLast03(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "::") == ("", "::")

    @Test
    def breakBeforeLast04(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa") == ("aaa", "")

    @Test
    def breakBeforeLast05(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa:") == ("aaa:", "")

    @Test
    def breakBeforeLast06(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa::") == ("aaa", "::")

    @Test
    def breakBeforeLast07(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa::bbb") == ("aaa", "::bbb")

    @Test
    def breakBeforeLast08(): Bool =
        Regex.breakBeforeLast(substr = regex":{2}", "aaa::bbb::ccc") == ("aaa::bbb", "::ccc")

    /////////////////////////////////////////////////////////////////////////////
    // isMatchWithBounds                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isMatchWithBounds01(): Bool =
        Regex.isMatchWithBounds(regex"a+", start = 0, end = 1, "") == false

    @Test
    def isMatchWithBounds02(): Bool =
        Regex.isMatchWithBounds(regex"a+", start = 0, end = 1, "a") == true

    @Test
    def isMatchWithBounds03(): Bool =
        Regex.isMatchWithBounds(regex"a+", start = 0, end = 3, "a") == false

    @Test
    def isMatchWithBounds04(): Bool =
        Regex.isMatchWithBounds(regex"a+", start = 0, end = 1, "aa") == true

    @Test
    def isMatchWithBounds05(): Bool =
        Regex.isMatchWithBounds(regex"a+", start = 2, end = 3, "aab") == false

    @Test
    def isMatchWithBounds06(): Bool =
        Regex.isMatchWithBounds(regex"b+", start = 2, end = 3, "aab") == true

    /////////////////////////////////////////////////////////////////////////////
    // isSubmatchWithBounds                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSubmatchWithBounds01(): Bool =
        Regex.isSubmatchWithBounds(regex"a+", start = 0, end = 1, "") == false

    @Test
    def isSubmatchWithBounds02(): Bool =
        Regex.isSubmatchWithBounds(regex"a+", start = 0, end = 1, "a") == true

    @Test
    def isSubmatchWithBounds03(): Bool =
        Regex.isSubmatchWithBounds(regex"a+", start = 1, end = 2, "aa") == true

    @Test
    def isSubmatchWithBounds04(): Bool =
        Regex.isSubmatchWithBounds(regex"a+", start = -1, end = 1, "aab") == false

    @Test
    def isSubmatchWithBounds05(): Bool =
        Regex.isSubmatchWithBounds(regex"a+", start = 0, end = 2, "aab") == true

    @Test
    def isSubmatchWithBounds06(): Bool =
        Regex.isSubmatchWithBounds(regex"b+", start = 2, end = 3, "aab") == true

    @Test
    def isSubmatchWithBounds07(): Bool =
        Regex.isSubmatchWithBounds(regex"b+", start = 1, end = 2, "bbbc") == true

    @Test
    def isSubmatchWithBounds08(): Bool =
        Regex.isSubmatchWithBounds(regex"b+", start = 0, end = 5, "aabbbc") == true

    /////////////////////////////////////////////////////////////////////////////
    // indicesWithBounds                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indicesWithBounds01(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = 0, end = 1, "") == List#{}

    @Test
    def indicesWithBounds02(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = -1, end = 10, "a::b::c") == List#{}

    @Test
    def indicesWithBounds03(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = 0, end = 7, "a::b::c") == List#{1, 4}

    @Test
    def indicesWithBounds04(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = 0, end = 3, "a::b::c") == List#{1}

    @Test
    def indicesWithBounds05(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = 3, end = 7, "a::b::c") == List#{4}

    @Test
    def indicesWithBounds06(): Bool =
        Regex.indicesWithBounds(substr = regex":{2}", start = 0, end = 3, "a:b") == List#{}

    /////////////////////////////////////////////////////////////////////////////
    // submatchesWithBounds                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def submatchesWithBounds01(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "") == List#{}

    @Test
    def submatchesWithBounds02(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = -1, end = 10, "a::b::c") == List#{}

    @Test
    def submatchesWithBounds03(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "aaa") == List#{"aaa"}

    @Test
    def submatchesWithBounds04(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 10, "aaa") == List#{}

    @Test
    def submatchesWithBounds05(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "::") == List#{}

    @Test
    def submatchesWithBounds06(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "ab") == List#{"ab"}

    @Test
    def submatchesWithBounds07(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "::a") == List#{"a"}

    @Test
    def submatchesWithBounds08(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "a::") == List#{"a"}

    @Test
    def submatchesWithBounds09(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "a::b::") == List#{"a"}

    @Test
    def submatchesWithBounds10(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 2, end = 7, "a::b::c") == List#{"b", "c"}

    @Test
    def submatchesWithBounds11(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c:") == List#{"a", "b", "c"}

    @Test
    def submatchesWithBounds12(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 6, ":a::b::c") == List#{"a", "b"}

    @Test
    def submatchesWithBounds13(): Bool =
        Regex.submatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, "::::") == List#{}

    /////////////////////////////////////////////////////////////////////////////
    // countSubmatchesWithBounds                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def countSubmatchesWithBounds01(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "") == 0

    @Test
    def countSubmatchesWithBounds02(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = -1, end = 10, "a::b::c") == 0

    @Test
    def countSubmatchesWithBounds03(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c") == 3

    @Test
    def countSubmatchesWithBounds04(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, "a::b::c") == 2

    @Test
    def countSubmatchesWithBounds05(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 1, end = 7, "a::b::c") == 2

    @Test
    def countSubmatchesWithBounds06(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "ab") == 1

    @Test
    def countSubmatchesWithBounds07(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 1, end = 3, "::a") == 1

    @Test
    def countSubmatchesWithBounds08(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "a::") == 1

    @Test
    def countSubmatchesWithBounds09(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, "a::b::") == 2

    @Test
    def countSubmatchesWithBounds10(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, "a::b::c") == 2

    @Test
    def countSubmatchesWithBounds11(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 4, end = 8, "a::b::c:") == 1

    @Test
    def countSubmatchesWithBounds12(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, ":a::b::c") == 1

    @Test
    def countSubmatchesWithBounds13(): Bool =
        Regex.countSubmatchesWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 4, "::::") == 0

    /////////////////////////////////////////////////////////////////////////////
    // indexOfFirstWithBounds                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfFirstWithBounds01(): Bool =
        Regex.indexOfFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "") == None

    @Test
    def indexOfFirstWithBounds02(): Bool =
        Regex.indexOfFirstWithBounds(substr = regex"\\p{Alpha}+", start = -1, end = 5, "a::b") == None

    @Test
    def indexOfFirstWithBounds03(): Bool =
        Regex.indexOfFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "a") == Some(0)

    @Test
    def indexOfFirstWithBounds04(): Bool =
        Regex.indexOfFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c") == Some(0)

    @Test
    def indexOfFirstWithBounds05(): Bool =
        Regex.indexOfFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "a::b::c") == Some(0)

    @Test
    def indexOfFirstWithBounds06(): Bool =
        Regex.indexOfFirstWithBounds(substr = regex"\\p{Alpha}+", start = 3, end = 7, "a::b::c") == Some(3)

    @Test
    def indexOfFirstWithBounds07(): Bool =
        Regex.indexOfFirstWithBounds(substr = regex"\\p{Alpha}+", start = 6, end = 7, "a::b::c") == Some(6)

    @Test
    def indexOfFirstWithBounds08(): Bool =
        Regex.indexOfFirstWithBounds(substr = regex"\\p{Alpha}+", start = 1, end = 6, "a::b::c") == Some(3)

    @Test
    def indexOfFirstWithBounds09(): Bool =
        Regex.indexOfFirstWithBounds(substr = regex"\\p{Alpha}+", start = 7, end = 9, "a::b::c::") == None

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLastWithBounds                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLastWithBounds01(): Bool =
        Regex.indexOfLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "") == None

    @Test
    def indexOfLastWithBounds02(): Bool =
        Regex.indexOfLastWithBounds(substr = regex"\\p{Alpha}+", start = -1, end = 5, "a::b") == None

    @Test
    def indexOfLastWithBounds03(): Bool =
        Regex.indexOfLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "a") == Some(0)

    @Test
    def indexOfLastWithBounds04(): Bool =
        Regex.indexOfLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c") == Some(6)

    @Test
    def indexOfLastWithBounds05(): Bool =
        Regex.indexOfLastWithBounds(substr = regex"\\p{Alpha}+", start = 5, end = 7, "a::b::c") == Some(6)

    @Test
    def indexOfLastWithBounds06(): Bool =
        Regex.indexOfLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 6, "a::b::c") == Some(3)

    @Test
    def indexOfLastWithBounds07(): Bool =
        Regex.indexOfLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "a::b::c") == Some(0)

    @Test
    def indexOfLastWithBounds08(): Bool =
        Regex.indexOfLastWithBounds(substr = regex"\\p{Alpha}+", start = 1, end = 6, "a::b::c") == Some(3)

    @Test
    def indexOfLastWithBounds09(): Bool =
        Regex.indexOfLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "::a::b::c") == None

    /////////////////////////////////////////////////////////////////////////////
    // getFirstWithBounds                                                      //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getFirstWithBounds01(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "") == None

    @Test
    def getFirstWithBounds02(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = -1, end = 5, "a::b") == None

    @Test
    def getFirstWithBounds03(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "a") == Some("a")

    @Test
    def getFirstWithBounds04(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c") == Some("a")

    @Test
    def getFirstWithBounds05(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "a::b::c") == Some("a")

    @Test
    def getFirstWithBounds06(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 3, end = 7, "a::b::c") == Some("b")

    @Test
    def getFirstWithBounds07(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 6, end = 7, "a::b::c") == Some("c")

    @Test
    def getFirstWithBounds08(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 1, end = 6, "a::b::c") == Some("b")

    @Test
    def getFirstWithBounds09(): Bool =
        Regex.getFirstWithBounds(substr = regex"\\p{Alpha}+", start = 7, end = 9, "a::b::c::") == None

    /////////////////////////////////////////////////////////////////////////////
    // getLastWithBounds                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getLastWithBounds01(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "") == None

    @Test
    def getLastWithBounds02(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = -1, end = 5, "a::b") == None

    @Test
    def getLastWithBounds03(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 1, "a") == Some("a")

    @Test
    def getLastWithBounds04(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 7, "a::b::c") == Some("c")

    @Test
    def getLastWithBounds05(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 5, end = 7, "a::b::c") == Some("c")

    @Test
    def getLastWithBounds06(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 6, "a::b::c") == Some("b")

    @Test
    def getLastWithBounds07(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 3, "a::b::c") == Some("a")

    @Test
    def getLastWithBounds08(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 1, end = 6, "a::b::c") == Some("b")

    @Test
    def getLastWithBounds09(): Bool =
        Regex.getLastWithBounds(substr = regex"\\p{Alpha}+", start = 0, end = 2, "::a::b::c") == None

}
