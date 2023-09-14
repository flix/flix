/*
 * Copyright 2019 Magnus Madsen, Stephen Tetley
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

mod TestString {

/////////////////////////////////////////////////////////////////////////////
// charAt                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def charAt01(): Bool = String.charAt(0, "Hello World!") == 'H'

@Test
def charAt02(): Bool = String.charAt(1, "Hello World!") == 'e'

@Test
def charAt03(): Bool = String.charAt(2, "Hello World!") == 'l'

@Test
def charAt04(): Bool = String.charAt(3, "Hello World!") == 'l'

@Test
def charAt05(): Bool = String.charAt(4, "Hello World!") == 'o'

@Test
def charAt06(): Bool = String.charAt(5, "Hello World!") == ' '

/////////////////////////////////////////////////////////////////////////////
// nth                                                                     //
/////////////////////////////////////////////////////////////////////////////

@Test
def nth01(): Bool = String.nth(0, "") == None

@Test
def nth02(): Bool = String.nth(-1, "Hello World!") == None

@Test
def nth03(): Bool = String.nth(15, "Hello World!") == None

@Test
def nth04(): Bool = String.nth(0, "Hello World!") == Some('H')

@Test
def nth05(): Bool = String.nth(1, "Hello World!") == Some('e')

@Test
def nth06(): Bool = String.nth(2, "Hello World!") == Some('l')

@Test
def nth07(): Bool = String.nth(3, "Hello World!") == Some('l')

@Test
def nth08(): Bool = String.nth(4, "Hello World!") == Some('o')

@Test
def nth09(): Bool = String.nth(5, "Hello World!") == Some(' ')

@Test
def nth10(): Bool = String.nth(11, "Hello World!") == Some('!')

/////////////////////////////////////////////////////////////////////////////
// codePointAt                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def codePointAt01(): Bool = String.codePointAt(0, CodePoint.toString(0x1f3dd)) == 0x1f3dd

@Test
def codePointAt02(): Bool = String.codePointAt(2, "\uD83C\uDFDD.") == 0x002E

@Test
def codePointAt03(): Bool = String.codePointAt(1, ".\uD83C\uDFDD") == 0x1f3dd

/////////////////////////////////////////////////////////////////////////////
// nthCodePoint                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def nthCodePoint01(): Bool = String.nthCodePoint(0, "") == None

@Test
def nthCodePoint02(): Bool = String.nthCodePoint(0, CodePoint.toString(0x1f3dd)) == Some(0x1f3dd)

@Test
def nthCodePoint03(): Bool = String.nthCodePoint(2, CodePoint.toString(0x1f3dd)) == None

@Test
def nthCodePoint04(): Bool = String.nthCodePoint(2, "\uD83C\uDFDD.") == Some(0x002E)

@Test
def nthCodePoint05(): Bool = String.nthCodePoint(1, ".\uD83C\uDFDD") == Some(0x1f3dd)

/////////////////////////////////////////////////////////////////////////////
// concat                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def concat01(): Bool = "Hello World!" == "Hello World!"

@Test
def concat02(): Bool = "Hello" + " World!" == "Hello World!"

@Test
def concat03(): Bool = "Hello" + " World!" == "Hello" + " World!"

@Test
def concat04(): Bool = "Hello World!" + "" == "Hello World!"

@Test
def concat05(): Bool = "Hello" + " " + "World" + "!" == "Hello World!"

@Test
def concat06(): Bool =
    let f = x -> (x + " World!");
    f("Hello") == "Hello World!"

@Test
def concat07(): Bool =
    let f = (x, y) -> (x + y);
    f("Hello", " World!") == "Hello World!"

@Test
def concat08(): Bool =
    let f = flip((x, y) -> (x + y));
    f(" World!", "Hello") == "Hello World!"

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def isEmpty01(): Bool = String.isEmpty("") == true

@Test
def isEmpty02(): Bool = String.isEmpty("Hello") == false

/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def length01(): Bool = String.length("") == 0

@Test
def length02(): Bool = String.length("0123456789") == 10

/////////////////////////////////////////////////////////////////////////////
// split                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def split01(): Bool =
   String.split({regex = "\\s+"}, "") == "" :: Nil

@Test
def split02(): Bool =
    List.length(String.split({regex = "\\s+"}, "1 3")) == 2

/////////////////////////////////////////////////////////////////////////////
// toLowerCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def toLowerCase01(): Bool = String.toLowerCase("") == ""

@Test
def toLowerCase02(): Bool = String.toLowerCase("Hello World!") == "hello world!"

@Test
def toLowerCase03(): Bool = String.toLowerCase("â†’Hello Worldâˆ€!") == "â†’hello worldâˆ€!"

/////////////////////////////////////////////////////////////////////////////
// toUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def toUpperCase01(): Bool = String.toUpperCase("") == ""

@Test
def toUpperCase02(): Bool = String.toUpperCase("Hello World!") == "HELLO WORLD!"

@Test
def toUpperCase03(): Bool = String.toUpperCase("â†’Hello Worldâˆ€!") == "Â†’HELLO WORLDÂˆ€!"

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def toList01(): Bool = String.toList("") == Nil

@Test
def toList02(): Bool = String.toList("ABC") == 'A' :: 'B' :: 'C' :: Nil

@Test
def toList03(): Bool = String.toList("1 2 3") == '1' :: ' ' :: '2' :: ' ' :: '3' :: Nil

/////////////////////////////////////////////////////////////////////////////
// trim                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def trim01(): Bool = "Hello World" == String.trim("Hello World")

@Test
def trim02(): Bool = "abc" == String.trim(" abc")

@Test
def trim03(): Bool = "abc" == String.trim("abc ")

@Test
def trim04(): Bool = "abc" == String.trim(" abc ")

@Test
def trim05(): Bool = "abc" == String.trim("  abc")

@Test
def trim06(): Bool = "" == String.trim("  ")

/////////////////////////////////////////////////////////////////////////////
// lineSeparator                                                           //
/////////////////////////////////////////////////////////////////////////////

@Test
def lineSeparator01(): Bool =
    let newline = String.lineSeparator();
    Char.isWhiteSpace(String.charAt(0, newline)) == true

@Test
def lineSeparator02(): Bool =
    let newline = String.lineSeparator();
    String.length(newline) == 1 or String.length(newline) == 2

/////////////////////////////////////////////////////////////////////////////
// toArray                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def toArray01(): Bool = region rc {
    Array.sameElements(String.toArray(rc, ""), Array#{} @ rc)
}

@Test
def toArray02(): Bool = region rc {
    Array.sameElements(String.toArray(rc, "ABC"), Array#{'A', 'B', 'C'} @ rc)
}

@Test
def toArray03(): Bool = region rc {
    Array.sameElements(String.toArray(rc, "1 2 3"), Array#{'1', ' ', '2', ' ', '3'} @ rc)
}

/////////////////////////////////////////////////////////////////////////////
// toVector                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def toVector01(): Bool =
    String.toVector("") == Vector#{}

@Test
def toVector02(): Bool =
    String.toVector("ABC") == Vector#{'A', 'B', 'C'}

@Test
def toVector03(): Bool =
    String.toVector("1 2 3") == Vector#{'1', ' ', '2', ' ', '3'}

/////////////////////////////////////////////////////////////////////////////
// toRegex                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def toRegex01(): Bool =
    String.toRegex("\\p{Alpha}+") |> Result.isOk

@Test
def toRegex02(): Bool =
    String.toRegex("\\p{Alpha+") |> Result.isErr

/////////////////////////////////////////////////////////////////////////////
// toRegexWithFlags                                                        //
/////////////////////////////////////////////////////////////////////////////

@Test
def compileWithFlags01(): Bool =
    use Regex.Flag.{CaseInsenstive};
    String.toRegexWithFlags(Set#{CaseInsenstive}, "\\p{Alpha}+") |> Result.isOk

@Test
def compileWithFlags02(): Bool =
    use Regex.Flag.{CaseInsenstive};
    String.toRegexWithFlags(Set#{CaseInsenstive}, "\\p{Alpha+") |> Result.isErr


@Test
def compileWithFlags03(): Bool =
    use Regex.Flag.{CaseInsenstive, Multiline};
    String.toRegexWithFlags(Set#{CaseInsenstive, Multiline}, "\\p{Alpha}+") |> Result.isOk

/////////////////////////////////////////////////////////////////////////////
// init                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def init01(): Bool = String.init(_ -> 'A', 0) == ""

@Test
def init02(): Bool = String.init(_ -> 'A', -1) == ""

@Test
def init03(): Bool = String.init(_ -> 'A', 1) == "A"

@Test
def init04(): Bool = String.init(_ -> 'A', 2) == "AA"

@Test
def init05(): Bool =
    let s = String.init(_ -> 'A', 50);
    String.length(s) == 50

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def flatten01(): Bool = String.flatten(Nil) == ""

@Test
def flatten02(): Bool = String.flatten("" :: Nil) == ""

@Test
def flatten03(): Bool = String.flatten("a" :: Nil) == "a"

@Test
def flatten04(): Bool = String.flatten("ab" :: Nil) == "ab"

@Test
def flatten05(): Bool = String.flatten("" :: "" :: Nil) == ""

@Test
def flatten06(): Bool = String.flatten("a" :: "" :: Nil) == "a"

@Test
def flatten07(): Bool = String.flatten("" :: "a" :: Nil) == "a"

@Test
def flatten08(): Bool = String.flatten("a" :: "b" :: Nil) == "ab"

@Test
def flatten09(): Bool = String.flatten("ab" :: "cde" :: Nil) == "abcde"

@Test
def flatten10(): Bool = String.flatten("a" :: "bc" :: "d" :: Nil) == "abcd"

/////////////////////////////////////////////////////////////////////////////
// intercalate                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def intercalate01(): Bool = String.intercalate("-", Nil) == ""

@Test
def intercalate02(): Bool = String.intercalate("-", "" :: Nil) == ""

@Test
def intercalate03(): Bool = String.intercalate("-", "a" :: Nil) == "a"

@Test
def intercalate04(): Bool = String.intercalate("-", "ab" :: Nil) == "ab"

@Test
def intercalate05(): Bool = String.intercalate("-", "" :: "" :: Nil) == "-"

@Test
def intercalate06(): Bool = String.intercalate("-", "a" :: "" :: Nil) == "a-"

@Test
def intercalate07(): Bool = String.intercalate("-", "" :: "a" :: Nil) == "-a"

@Test
def intercalate08(): Bool = String.intercalate("-", "a" :: "b" :: Nil) == "a-b"

@Test
def intercalate09(): Bool = String.intercalate("-", "ab" :: "cde" :: Nil) == "ab-cde"

@Test
def intercalate10(): Bool = String.intercalate("-", "a" :: "bc" :: "d" :: Nil) == "a-bc-d"

@Test
def intercalate11(): Bool = String.intercalate("", "a" :: "bc" :: "d" :: Nil) == "abcd"

@Test
def intercalate12(): Bool = String.intercalate("<>", "a" :: "bc" :: "d" :: Nil) == "a<>bc<>d"

/////////////////////////////////////////////////////////////////////////////
// intercalateChar                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def intercalateChar01(): Bool = String.intercalateChar('-', Nil) == ""

@Test
def intercalateChar02(): Bool = String.intercalateChar('-', "" :: Nil) == ""

@Test
def intercalateChar03(): Bool = String.intercalateChar('-', "a" :: Nil) == "a"

@Test
def intercalateChar04(): Bool = String.intercalateChar('-', "ab" :: Nil) == "ab"

@Test
def intercalateChar05(): Bool = String.intercalateChar('-', "" :: "" :: Nil) == "-"

@Test
def intercalateChar06(): Bool = String.intercalateChar('-', "a" :: "" :: Nil) == "a-"

@Test
def intercalateChar07(): Bool = String.intercalateChar('-', "" :: "a" :: Nil) == "-a"

@Test
def intercalateChar08(): Bool = String.intercalateChar('-', "a" :: "b" :: Nil) == "a-b"

@Test
def intercalateChar09(): Bool = String.intercalateChar('-', "ab" :: "cde" :: Nil) == "ab-cde"

@Test
def intercalateChar10(): Bool = String.intercalateChar('-', "a" :: "bc" :: "d" :: Nil) == "a-bc-d"

@Test
def intercalateChar11(): Bool = String.intercalateChar(' ', "a" :: "bc" :: "d" :: Nil) == "a bc d"

@Test
def intercalateChar12(): Bool = String.intercalateChar('/', "a" :: "bc" :: "d" :: Nil) == "a/bc/d"

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////

@Test
def map01(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "") == ""

@Test
def map02(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "a") == "f"

@Test
def map03(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "A") == "t"

@Test
def map04(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "ab") == "ff"

@Test
def map05(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "aB") == "ft"

@Test
def map06(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "Ab") == "tf"

@Test
def map07(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "AB") == "tt"

/////////////////////////////////////////////////////////////////////////////
// mapWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def mapWithIndex01(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "") == ""

@Test
def mapWithIndex02(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "a") == "f"

@Test
def mapWithIndex03(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "A") == "t"

@Test
def mapWithIndex04(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "aB") == "fT"

@Test
def mapWithIndex05(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "ab") == "fF"

@Test
def mapWithIndex06(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "AB") == "tT"

@Test
def mapWithIndex07(): Bool =
    let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "Ab") == "tF"

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def reverse01(): Bool =
    String.reverse("") == ""

@Test
def reverse02(): Bool =
    String.reverse("A") == "A"

@Test
def reverse03(): Bool =
    String.reverse("Hello World") == "dlroW olleH"

@Test
def reverse04(): Bool =
    String.reverse("Hello World!") == "!dlroW olleH"

@Test
def reverse05(): Bool =
    String.reverse("Hello World!") != "!dlroW OLLEH"

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def rotateLeft01(): Bool = String.rotateLeft(0, "") == ""

@Test
def rotateLeft02(): Bool = String.rotateLeft(1, "") == ""

@Test
def rotateLeft03(): Bool = String.rotateLeft(0, "a") == "a"

@Test
def rotateLeft04(): Bool = String.rotateLeft(0, "ab") == "ab"

@Test
def rotateLeft05(): Bool = String.rotateLeft(1, "ab") == "ba"

@Test
def rotateLeft06(): Bool = String.rotateLeft(2, "ab") == "ab"

@Test
def rotateLeft07(): Bool = String.rotateLeft(3, "ab") == "ba"

@Test
def rotateLeft08(): Bool = String.rotateLeft(-1, "ab") == "ba"

@Test
def rotateLeft09(): Bool = String.rotateLeft(0, "abc") == "abc"

@Test
def rotateLeft10(): Bool = String.rotateLeft(1, "abc") == "bca"

@Test
def rotateLeft11(): Bool = String.rotateLeft(2, "abc") == "cab"

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def rotateRight01(): Bool = String.rotateRight(0, "") == ""

@Test
def rotateRight02(): Bool = String.rotateRight(1, "") == ""

@Test
def rotateRight03(): Bool = String.rotateRight(0, "a") == "a"

@Test
def rotateRight04(): Bool = String.rotateRight(0, "ab") == "ab"

@Test
def rotateRight05(): Bool = String.rotateRight(1, "ab") == "ba"

@Test
def rotateRight06(): Bool = String.rotateRight(2, "ab") == "ab"

@Test
def rotateRight07(): Bool = String.rotateRight(3, "ab") == "ba"

@Test
def rotateRight08(): Bool = String.rotateRight(-1, "ab") == "ba"

@Test
def rotateRight09(): Bool = String.rotateRight(0, "abc") == "abc"

@Test
def rotateRight10(): Bool = String.rotateRight(1, "abc") == "cab"

@Test
def rotateRight11(): Bool = String.rotateRight(2, "abc") == "bca"

/////////////////////////////////////////////////////////////////////////////
// isMatch                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def isMatch01(): Bool = String.isMatch({regex = ".*"}, "") == true

@Test
def isMatch02(): Bool = String.isMatch({regex = ".+"}, "") == false

@Test
def isMatch03(): Bool = String.isMatch({regex = "[a]*"}, "") == true

@Test
def isMatch04(): Bool = String.isMatch({regex = "[a]+"}, "") == false

@Test
def isMatch05(): Bool = String.isMatch({regex = "[a]*"}, "a") == true

@Test
def isMatch06(): Bool = String.isMatch({regex = "[a]*"}, "aa") == true

@Test
def isMatch07(): Bool = String.isMatch({regex = "[a]*"}, "aab") == false

@Test
def isMatch08(): Bool = String.isMatch({regex = "[a]*b"}, "aab") == true

@Test
def isSMatch09(): Bool = String.isMatch({regex = "[b]+"}, "aab") == false

@Test
def isMatch10(): Bool = String.isMatch({regex = "[b]+"}, "bbbc") == false

@Test
def isMatch11(): Bool = String.isMatch({regex = "[b]+"}, "aabbbc") == false

/////////////////////////////////////////////////////////////////////////////
// isSubmatch                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def isSubmatch01(): Bool = String.isSubmatch({regex = ".*"}, "") == true

@Test
def isSubmatch02(): Bool = String.isSubmatch({regex = ".+"}, "") == false

@Test
def isSubmatch03(): Bool = String.isSubmatch({regex = "[a]*"}, "") == true

@Test
def isSubmatch04(): Bool = String.isSubmatch({regex = "[a]+"}, "") == false

@Test
def isSubmatch05(): Bool = String.isSubmatch({regex = "[a]*"}, "a") == true

@Test
def isSubmatch06(): Bool = String.isSubmatch({regex = "[a]*"}, "aa") == true

@Test
def isSubmatch07(): Bool = String.isSubmatch({regex = "[a]*"}, "aab") == true

@Test
def isSubmatch08(): Bool = String.isSubmatch({regex = "[a]*b"}, "aab") == true

@Test
def isSubmatch09(): Bool = String.isSubmatch({regex = "[b]+"}, "aab") == true

@Test
def isSubmatch10(): Bool = String.isSubmatch({regex = "[b]+"}, "bbbc") == true

@Test
def isSubmatch11(): Bool = String.isSubmatch({regex = "[b]+"}, "aabbbc") == true

/////////////////////////////////////////////////////////////////////////////
// slice                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def slice01(): Bool = String.slice(start = 0, end = 1, "") == ""

@Test
def slice02(): Bool = String.slice(start = 0, end = 1, "a") == "a"

@Test
def slice03(): Bool = String.slice(start = 1, end = 2, "a") == ""

@Test
def slice04(): Bool = String.slice(start = 0, end = 2, "a") == ""

@Test
def slice05(): Bool = String.slice(start = -1, end = 0, "a") == ""

@Test
def slice06(): Bool = String.slice(start = 0, end = 1, "ab") == "a"

@Test
def slice07(): Bool = String.slice(start = 1, end = 2, "ab") == "b"

@Test
def slice08(): Bool = String.slice(start = 0, end = 2, "ab") == "ab"

@Test
def slice09(): Bool = String.slice(start = 0, end = 3, "ab") == ""

@Test
def slice10(): Bool = String.slice(start = -1, end = 2, "ab") == ""

@Test
def slice11(): Bool = String.slice(start = 0, end = 1, "abc") == "a"

@Test
def slice12(): Bool = String.slice(start = 1, end = 2, "abc") == "b"

@Test
def slice13(): Bool = String.slice(start = 2, end = 3, "abc") == "c"

@Test
def slice14(): Bool = String.slice(start = 0, end = 2, "abc") == "ab"

@Test
def slice15(): Bool = String.slice(start = 1, end = 3, "abc") == "bc"

@Test
def slice16(): Bool = String.slice(start = 0, end = 3, "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// sliceLeft                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def sliceLeft01(): Bool = String.sliceLeft(end = 0, "") == ""

@Test
def sliceLeft02(): Bool = String.sliceLeft(end = 1, "") == ""

@Test
def sliceLeft03(): Bool = String.sliceLeft(end = 1, "a") == "a"

@Test
def sliceLeft04(): Bool = String.sliceLeft(end = 0, "a") == ""

@Test
def sliceLeft05(): Bool = String.sliceLeft(end = 2, "a") == ""

@Test
def sliceLeft06(): Bool = String.sliceLeft(end = 1, "ab") == "a"

@Test
def sliceLeft07(): Bool = String.sliceLeft(end = 2, "ab") == "ab"

@Test
def sliceLeft08(): Bool = String.sliceLeft(end = 0, "ab") == ""

@Test
def sliceLeft09(): Bool = String.sliceLeft(end = 3, "ab") == ""

@Test
def sliceLeft10(): Bool = String.sliceLeft(end = -1, "ab") == ""

@Test
def sliceLeft11(): Bool = String.sliceLeft(end = 1, "abc") == "a"

@Test
def sliceLeft12(): Bool = String.sliceLeft(end = 2, "abc") == "ab"

@Test
def sliceLeft13(): Bool = String.sliceLeft(end = 3, "abc") == "abc"

@Test
def sliceLeft14(): Bool = String.sliceLeft(end = 0, "abc") == ""

@Test
def sliceLeft15(): Bool = String.sliceLeft(end = 4, "abc") == ""

@Test
def sliceLeft16(): Bool = String.sliceLeft(end = -1, "abc") == ""

/////////////////////////////////////////////////////////////////////////////
// sliceRight                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def sliceRight01(): Bool = String.sliceRight(start = 0, "") == ""

@Test
def sliceRight02(): Bool = String.sliceRight(start = 1, "") == ""

@Test
def sliceRight03(): Bool = String.sliceRight(start = 1, "a") == ""

@Test
def sliceRight04(): Bool = String.sliceRight(start = 0, "a") == "a"

@Test
def sliceRight05(): Bool = String.sliceRight(start = 1, "a") == ""

@Test
def sliceRight06(): Bool = String.sliceRight(start = 0, "ab") == "ab"

@Test
def sliceRight07(): Bool = String.sliceRight(start = 1, "ab") == "b"

@Test
def sliceRight08(): Bool = String.sliceRight(start = -1, "ab") == ""

@Test
def sliceRight09(): Bool = String.sliceRight(start = 2, "ab") == ""

@Test
def sliceRight10(): Bool = String.sliceRight(start = 0, "abc") == "abc"

@Test
def sliceRight11(): Bool = String.sliceRight(start = 1, "abc") == "bc"

@Test
def sliceRight12(): Bool = String.sliceRight(start = 2, "abc") == "c"

@Test
def sliceRight13(): Bool = String.sliceRight(start = 3, "abc") == ""

@Test
def sliceRight14(): Bool = String.sliceRight(start = -1, "abc") == ""

/////////////////////////////////////////////////////////////////////////////
// findIndexOfLeft                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def findIndexOfLeft01(): Bool = String.findIndexOfLeft(Char.isUpperCase, "") == None

@Test
def findIndexOfLeft02(): Bool = String.findIndexOfLeft(Char.isUpperCase, "a") == None

@Test
def findIndexOfLeft03(): Bool = String.findIndexOfLeft(Char.isUpperCase, "A") == Some(0)

@Test
def findIndexOfLeft04(): Bool = String.findIndexOfLeft(Char.isUpperCase, "ab") == None

@Test
def findIndexOfLeft05(): Bool = String.findIndexOfLeft(Char.isUpperCase, "Ab") == Some(0)

@Test
def findIndexOfLeft06(): Bool = String.findIndexOfLeft(Char.isUpperCase, "aB") == Some(1)

@Test
def findIndexOfLeft07(): Bool = String.findIndexOfLeft(Char.isUpperCase, "AB") == Some(0)

/////////////////////////////////////////////////////////////////////////////
// findIndexOfRight                                                        //
/////////////////////////////////////////////////////////////////////////////

@Test
def findIndexOfRight01(): Bool = String.findIndexOfRight(Char.isUpperCase, "") == None

@Test
def findIndexOfRight02(): Bool = String.findIndexOfRight(Char.isUpperCase, "a") == None

@Test
def findIndexOfRight03(): Bool = String.findIndexOfRight(Char.isUpperCase, "A") == Some(0)

@Test
def findIndexOfRight04(): Bool = String.findIndexOfRight(Char.isUpperCase, "ab") == None

@Test
def findIndexOfRight05(): Bool = String.findIndexOfRight(Char.isUpperCase, "Ab") == Some(0)

@Test
def findIndexOfRight06(): Bool = String.findIndexOfRight(Char.isUpperCase, "aB") == Some(1)

@Test
def findIndexOfRight07(): Bool = String.findIndexOfRight(Char.isUpperCase, "AB") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// findIndices                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def findIndices01(): Bool = String.findIndices(Char.isDigit, "") == Nil

@Test
def findIndices02(): Bool = String.findIndices(Char.isDigit, "a") == Nil

@Test
def findIndices03(): Bool = String.findIndices(Char.isDigit, "1") == 0 :: Nil

@Test
def findIndices04(): Bool = String.findIndices(Char.isDigit, "ab") == Nil

@Test
def findIndices05(): Bool = String.findIndices(Char.isDigit, "1b") == 0 :: Nil

@Test
def findIndices06(): Bool = String.findIndices(Char.isDigit, "a2") == 1 :: Nil

@Test
def findIndices07(): Bool = String.findIndices(Char.isDigit, "12") == 0 :: 1 :: Nil

/////////////////////////////////////////////////////////////////////////////
// indices                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def indices01(): Bool = String.indices({substr = ""}, "") == Nil

@Test
def indices02(): Bool = String.indices({substr = ""}, "a::b::c") == Nil

@Test
def indices03(): Bool = String.indices({substr = "::"}, "") == Nil

@Test
def indices04(): Bool = String.indices({substr = "::"}, "a") == Nil

@Test
def indices05(): Bool = String.indices({substr = "::"}, "::") == 0 :: Nil

@Test
def indices06(): Bool = String.indices({substr = "::"}, "ab") == Nil

@Test
def indices07(): Bool = String.indices({substr = "::"}, "::a") == 0 :: Nil

@Test
def indices08(): Bool = String.indices({substr = "::"}, "a::") == 1 :: Nil

@Test
def indices09(): Bool = String.indices({substr = "::"}, "a::b::") == 1 :: 4 :: Nil

@Test
def indices10(): Bool = String.indices({substr = "::"}, "a::b::c") == 1 :: 4 :: Nil

@Test
def indices11(): Bool = String.indices({substr = "::"}, "a::b::c:") == 1 :: 4 :: Nil

@Test
def indices12(): Bool = String.indices({substr = "::"}, ":a::b::c") == 2 :: 5 :: Nil

@Test
def indices13(): Bool = String.indices({substr = "::"}, "::::") == 0 :: 2 :: Nil


/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def take01(): Bool = String.take(-1, "") == ""

@Test
def take02(): Bool = String.take(0, "") == ""

@Test
def take03(): Bool = String.take(1, "") == ""

@Test
def take04(): Bool = String.take(-1, "a") == ""

@Test
def take05(): Bool = String.take(0, "a") == ""

@Test
def take06(): Bool = String.take(1, "a") == "a"

@Test
def take07(): Bool = String.take(2, "a") == "a"

@Test
def take08(): Bool = String.take(0, "ab") == ""

@Test
def take09(): Bool = String.take(1, "ab") == "a"

@Test
def take10(): Bool = String.take(2, "ab") == "ab"

@Test
def take11(): Bool = String.take(2, "abcdef") == "ab"

@Test
def take12(): Bool = String.take(4, "abcdef") == "abcd"

@Test
def take13(): Bool = String.take(10, "abcdef") == "abcdef"

/////////////////////////////////////////////////////////////////////////////
// takeLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def takeLeft01(): Bool = String.takeLeft(-1, "") == ""

@Test
def takeLeft02(): Bool = String.takeLeft(0, "") == ""

@Test
def takeLeft03(): Bool = String.takeLeft(1, "") == ""

@Test
def takeLeft04(): Bool = String.takeLeft(-1, "a") == ""

@Test
def takeLeft05(): Bool = String.takeLeft(0, "a") == ""

@Test
def takeLeft06(): Bool = String.takeLeft(1, "a") == "a"

@Test
def takeLeft07(): Bool = String.takeLeft(2, "a") == "a"

@Test
def takeLeft08(): Bool = String.takeLeft(0, "ab") == ""

@Test
def takeLeft09(): Bool = String.takeLeft(1, "ab") == "a"

@Test
def takeLeft10(): Bool = String.takeLeft(2, "ab") == "ab"

@Test
def takeLeft11(): Bool = String.takeLeft(2, "abcdef") == "ab"

@Test
def takeLeft12(): Bool = String.takeLeft(4, "abcdef") == "abcd"

@Test
def takeLeft13(): Bool = String.takeLeft(10, "abcdef") == "abcdef"

/////////////////////////////////////////////////////////////////////////////
// takeRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def takeRight01(): Bool = String.takeRight(-1, "") == ""

@Test
def takeRight02(): Bool = String.takeRight(0, "") == ""

@Test
def takeRight03(): Bool = String.takeRight(1, "") == ""

@Test
def takeRight04(): Bool = String.takeRight(-1, "a") == ""

@Test
def takeRight05(): Bool = String.takeRight(0, "a") == ""

@Test
def takeRight06(): Bool = String.takeRight(1, "a") == "a"

@Test
def takeRight07(): Bool = String.takeRight(2, "a") == "a"

@Test
def takeRight08(): Bool = String.takeRight(0, "ab") == ""

@Test
def takeRight09(): Bool = String.takeRight(1, "ab") == "b"

@Test
def takeRight10(): Bool = String.takeRight(2, "ab") == "ab"

@Test
def takeRight11(): Bool = String.takeRight(2, "abcdef") == "ef"

@Test
def takeRight12(): Bool = String.takeRight(4, "abcdef") == "cdef"

@Test
def takeRight13(): Bool = String.takeRight(10, "abcdef") == "abcdef"

/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def drop01(): Bool = String.drop(-1, "") == ""

@Test
def drop02(): Bool = String.drop(0, "") == ""

@Test
def drop03(): Bool = String.drop(1, "") == ""

@Test
def drop04(): Bool = String.drop(-1, "a") == "a"

@Test
def drop05(): Bool = String.drop(0, "a") == "a"

@Test
def drop06(): Bool = String.drop(1, "a") == ""

@Test
def drop07(): Bool = String.drop(2, "a") == ""

@Test
def drop08(): Bool = String.drop(0, "ab") == "ab"

@Test
def drop09(): Bool = String.drop(1, "ab") == "b"

@Test
def drop10(): Bool = String.drop(2, "ab") == ""

@Test
def drop11(): Bool = String.drop(2, "abcdef") == "cdef"

@Test
def drop12(): Bool = String.drop(4, "abcdef") == "ef"

/////////////////////////////////////////////////////////////////////////////
// dropLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def dropLeft01(): Bool = String.dropLeft(-1, "") == ""

@Test
def dropLeft02(): Bool = String.dropLeft(0, "") == ""

@Test
def dropLeft03(): Bool = String.dropLeft(1, "") == ""

@Test
def dropLeft04(): Bool = String.dropLeft(-1, "a") == "a"

@Test
def dropLeft05(): Bool = String.dropLeft(0, "a") == "a"

@Test
def dropLeft06(): Bool = String.dropLeft(1, "a") == ""

@Test
def dropLeft07(): Bool = String.dropLeft(2, "a") == ""

@Test
def dropLeft08(): Bool = String.dropLeft(0, "ab") == "ab"

@Test
def dropLeft09(): Bool = String.dropLeft(1, "ab") == "b"

@Test
def dropLeft10(): Bool = String.dropLeft(2, "ab") == ""

@Test
def dropLeft11(): Bool = String.dropLeft(2, "abcdef") == "cdef"

@Test
def dropLeft12(): Bool = String.dropLeft(4, "abcdef") == "ef"

/////////////////////////////////////////////////////////////////////////////
// dropRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def dropRight01(): Bool = String.dropRight(-1, "") == ""

@Test
def dropRight02(): Bool = String.dropRight(0, "") == ""

@Test
def dropRight03(): Bool = String.dropRight(1, "") == ""

@Test
def dropRight04(): Bool = String.dropRight(-1, "a") == "a"

@Test
def dropRight05(): Bool = String.dropRight(0, "a") == "a"

@Test
def dropRight06(): Bool = String.dropRight(1, "a") == ""

@Test
def dropRight07(): Bool = String.dropRight(2, "a") == ""

@Test
def dropRight08(): Bool = String.dropRight(0, "ab") == "ab"

@Test
def dropRight09(): Bool = String.dropRight(1, "ab") == "a"

@Test
def dropRight10(): Bool = String.dropRight(2, "ab") == ""

@Test
def dropRight11(): Bool = String.dropRight(2, "abcdef") == "abcd"

@Test
def dropRight12(): Bool = String.dropRight(4, "abcdef") == "ab"

/////////////////////////////////////////////////////////////////////////////
// takeWhile                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def takeWhile01(): Bool = String.takeWhile(Char.isUpperCase, "") == ""

@Test
def takeWhile02(): Bool = String.takeWhile(Char.isUpperCase, "a") == ""

@Test
def takeWhile03(): Bool = String.takeWhile(Char.isUpperCase, "A") == "A"

@Test
def takeWhile04(): Bool = String.takeWhile(Char.isUpperCase, "ab") == ""

@Test
def takeWhile05(): Bool = String.takeWhile(Char.isUpperCase, "aB") == ""

@Test
def takeWhile06(): Bool = String.takeWhile(Char.isUpperCase, "Ab") == "A"

@Test
def takeWhile07(): Bool = String.takeWhile(Char.isUpperCase, "AB") == "AB"

@Test
def takeWhile08(): Bool = String.takeWhile(Char.isUpperCase, "ABcDefgHIjK") == "AB"

/////////////////////////////////////////////////////////////////////////////
// takeWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////

@Test
def takeWhileLeft01(): Bool = String.takeWhileLeft(Char.isUpperCase, "") == ""

@Test
def takeWhileLeft02(): Bool = String.takeWhileLeft(Char.isUpperCase, "a") == ""

@Test
def takeWhileLeft03(): Bool = String.takeWhileLeft(Char.isUpperCase, "A") == "A"

@Test
def takeWhileLeft04(): Bool = String.takeWhileLeft(Char.isUpperCase, "ab") == ""

@Test
def takeWhileLeft05(): Bool = String.takeWhileLeft(Char.isUpperCase, "aB") == ""

@Test
def takeWhileLeft06(): Bool = String.takeWhileLeft(Char.isUpperCase, "Ab") == "A"

@Test
def takeWhileLeft07(): Bool = String.takeWhileLeft(Char.isUpperCase, "AB") == "AB"

@Test
def takeWhileLeft08(): Bool = String.takeWhileLeft(Char.isUpperCase, "ABcDefgHIjK") == "AB"

/////////////////////////////////////////////////////////////////////////////
// takeWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////

@Test
def takeWhileRight01(): Bool = String.takeWhileRight(Char.isUpperCase, "") == ""

@Test
def takeWhileRight02(): Bool = String.takeWhileRight(Char.isUpperCase, "a") == ""

@Test
def takeWhileRight03(): Bool = String.takeWhileRight(Char.isUpperCase, "A") == "A"

@Test
def takeWhileRight04(): Bool = String.takeWhileRight(Char.isUpperCase, "ab") == ""

@Test
def takeWhileRight05(): Bool = String.takeWhileRight(Char.isUpperCase, "aB") == "B"

@Test
def takeWhileRight06(): Bool = String.takeWhileRight(Char.isUpperCase, "Ab") == ""

@Test
def takeWhileRight07(): Bool = String.takeWhileRight(Char.isUpperCase, "AB") == "AB"

@Test
def takeWhileRight08(): Bool = String.takeWhileRight(Char.isUpperCase, "ABcDefgHIjK") == "K"

/////////////////////////////////////////////////////////////////////////////
// dropWhile                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def dropWhile01(): Bool = String.dropWhile(Char.isUpperCase, "") == ""

@Test
def dropWhile02(): Bool = String.dropWhile(Char.isUpperCase, "a") == "a"

@Test
def dropWhile03(): Bool = String.dropWhile(Char.isUpperCase, "A") == ""

@Test
def dropWhile04(): Bool = String.dropWhile(Char.isUpperCase, "ab") == "ab"

@Test
def dropWhile05(): Bool = String.dropWhile(Char.isUpperCase, "aB") == "aB"

@Test
def dropWhile06(): Bool = String.dropWhile(Char.isUpperCase, "Ab") == "b"

@Test
def dropWhile07(): Bool = String.dropWhile(Char.isUpperCase, "AB") == ""

@Test
def dropWhile08(): Bool = String.dropWhile(Char.isUpperCase, "ABcDefgHIjK") == "cDefgHIjK"

/////////////////////////////////////////////////////////////////////////////
// dropWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////

@Test
def dropWhileLeft01(): Bool = String.dropWhileLeft(Char.isUpperCase, "") == ""

@Test
def dropWhileLeft02(): Bool = String.dropWhileLeft(Char.isUpperCase, "a") == "a"

@Test
def dropWhileLeft03(): Bool = String.dropWhileLeft(Char.isUpperCase, "A") == ""

@Test
def dropWhileLeft04(): Bool = String.dropWhileLeft(Char.isUpperCase, "ab") == "ab"

@Test
def dropWhileLeft05(): Bool = String.dropWhileLeft(Char.isUpperCase, "aB") == "aB"

@Test
def dropWhileLeft06(): Bool = String.dropWhileLeft(Char.isUpperCase, "Ab") == "b"

@Test
def dropWhileLeft07(): Bool = String.dropWhileLeft(Char.isUpperCase, "AB") == ""

@Test
def dropWhileLeft08(): Bool = String.dropWhileLeft(Char.isUpperCase, "ABcDefgHIjK") == "cDefgHIjK"

/////////////////////////////////////////////////////////////////////////////
// dropWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////

@Test
def dropWhileRight01(): Bool = String.dropWhileRight(Char.isUpperCase, "") == ""

@Test
def dropWhileRight02(): Bool = String.dropWhileRight(Char.isUpperCase, "a") == "a"

@Test
def dropWhileRight03(): Bool = String.dropWhileRight(Char.isUpperCase, "A") == ""

@Test
def dropWhileRight04(): Bool = String.dropWhileRight(Char.isUpperCase, "ab") == "ab"

@Test
def dropWhileRight05(): Bool = String.dropWhileRight(Char.isUpperCase, "aB") == "a"

@Test
def dropWhileRight06(): Bool = String.dropWhileRight(Char.isUpperCase, "Ab") == "Ab"

@Test
def dropWhileRight07(): Bool = String.dropWhileRight(Char.isUpperCase, "AB") == ""

@Test
def dropWhileRight08(): Bool = String.dropWhileRight(Char.isUpperCase, "ABcDefgHIjK") == "ABcDefgHIj"

/////////////////////////////////////////////////////////////////////////////
// dropWhileAround                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def dropWhileAround01(): Bool = String.dropWhileRight(Char.isUpperCase, "") == ""

@Test
def dropWhileAround02(): Bool = String.dropWhileAround(Char.isUpperCase, "a") == "a"

@Test
def dropWhileAround03(): Bool = String.dropWhileAround(Char.isUpperCase, "A") == ""

@Test
def dropWhileAround04(): Bool = String.dropWhileAround(Char.isUpperCase, "ab") == "ab"

@Test
def dropWhileAround05(): Bool = String.dropWhileAround(Char.isUpperCase, "aB") == "a"

@Test
def dropWhileAround06(): Bool = String.dropWhileAround(Char.isUpperCase, "Ab") == "b"

@Test
def dropWhileAround07(): Bool = String.dropWhileAround(Char.isUpperCase, "AB") == ""

@Test
def dropWhileAround08(): Bool = String.dropWhileAround(Char.isUpperCase, "ABcDefgHIjK") == "cDefgHIj"

/////////////////////////////////////////////////////////////////////////////
// splitAt                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def splitAt01(): Bool = String.splitAt(0, "") == ("", "")

@Test
def splitAt02(): Bool = String.splitAt(1, "") == ("", "")

@Test
def splitAt03(): Bool = String.splitAt(-1, "") == ("", "")

@Test
def splitAt04(): Bool = String.splitAt(0, "a") == ("", "a")

@Test
def splitAt05(): Bool = String.splitAt(1, "a") == ("a", "")

@Test
def splitAt06(): Bool = String.splitAt(-1, "a") == ("", "a")

@Test
def splitAt07(): Bool = String.splitAt(0, "ab") == ("", "ab")

@Test
def splitAt08(): Bool = String.splitAt(1, "ab") == ("a", "b")

@Test
def splitAt09(): Bool = String.splitAt(2, "ab") == ("ab", "")

@Test
def splitAt10(): Bool = String.splitAt(-1, "ab") == ("", "ab")

@Test
def splitAt11(): Bool = String.splitAt(3, "ab") == ("ab", "")

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def foldLeft01(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "", "")  == ""

@Test
def foldLeft02(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "a") == "a"

@Test
def foldLeft03(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "ab") == "ab"

@Test
def foldLeft04(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abc") == "abc"

@Test
def foldLeft05(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abcd") == "abcd"

@Test
def foldLeft06(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abcde") == "abcde"

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def foldRight01(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "", "")  == ""

@Test
def foldRight02(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "", "a") == "a"

@Test
def foldRight03(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "", "ab") == "ab"

@Test
def foldRight04(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abc") == "abc"

@Test
def foldRight05(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abcd") == "abcd"

@Test
def foldRight06(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abcde") == "abcde"

/////////////////////////////////////////////////////////////////////////////
// foldLeft2                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def foldLeft201(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "", "") == ""

@Test
def foldLeft202(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "a", "_") == "a"

@Test
def foldLeft203(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "a", "A") == "A"

@Test
def foldLeft204(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "__") == "ab"

@Test
def foldLeft205(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "A_") == "Ab"

@Test
def foldLeft206(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "AB") == "AB"

@Test
def foldLeft207(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "") == ""

@Test
def foldLeft208(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "_") == "a"

@Test
def foldLeft209(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "A") == "A"

/////////////////////////////////////////////////////////////////////////////
// foldRight2                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def foldRight201(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "", "") == ""

@Test
def foldRight202(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "a", "_") == "a"

@Test
def foldRight203(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "a", "A") == "A"

@Test
def foldRight204(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "__") == "ab"

@Test
def foldRight205(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "A_") == "Ab"

@Test
def foldRight206(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "AB") == "AB"

@Test
def foldRight207(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "") == ""

@Test
def foldRight208(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "_") == "b"

@Test
def foldRight209(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "B") == "B"

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def unfold01(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 0) == "aaaaa"

@Test
def unfold02(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 1) == "aaaa"

@Test
def unfold03(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 2) == "aaa"

@Test
def unfold04(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 3) == "aa"

@Test
def unfold05(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 4) == "a"

@Test
def unfold06(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 5) == ""

@Test
def unfold07(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 6) == ""

@Test
def unfold08(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, -1) == "aaaaaa"

/////////////////////////////////////////////////////////////////////////////
// unfoldWithIter                                                          //
/////////////////////////////////////////////////////////////////////////////

///
/// Converts Int32 in ranges [65;90] (inclusive) and [97;122] (inclusive) to a Char.
///
def toChar(step: Int32): Char = match step {
    case 65  => 'A'
    case 66  => 'B'
    case 67  => 'C'
    case 68  => 'D'
    case 69  => 'E'
    case 70  => 'F'
    case 71  => 'G'
    case 72  => 'H'
    case 73  => 'I'
    case 74  => 'J'
    case 75  => 'K'
    case 76  => 'L'
    case 77  => 'M'
    case 78  => 'N'
    case 79  => 'O'
    case 80  => 'P'
    case 81  => 'Q'
    case 82  => 'R'
    case 83  => 'S'
    case 84  => 'T'
    case 85  => 'U'
    case 86  => 'V'
    case 87  => 'W'
    case 88  => 'X'
    case 89  => 'Y'
    case 90  => 'Z'
    case 97  => 'a'
    case 98  => 'b'
    case 99  => 'c'
    case 100 => 'd'
    case 101 => 'e'
    case 102 => 'f'
    case 103 => 'g'
    case 104 => 'h'
    case 105 => 'i'
    case 106 => 'j'
    case 107 => 'k'
    case 108 => 'l'
    case 109 => 'm'
    case 110 => 'n'
    case 111 => 'o'
    case 112 => 'p'
    case 113 => 'q'
    case 114 => 'r'
    case 115 => 's'
    case 116 => 't'
    case 117 => 'u'
    case 118 => 'v'
    case 119 => 'w'
    case 120 => 'x'
    case 121 => 'y'
    case 122 => 'z'
    case _   => bug!("toChar called with bad input!")
}

@Test
def unfoldWithIter01(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (true)
            None
        else {
            let c = Some(toChar(deref x + 97));
            x := deref x + 1;
            c
        };
    String.unfoldWithIter(step) == ""
}

@Test
def unfoldWithIter02(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 0)
            None
        else {
            let c = Some(toChar(deref x + 97));
            x := deref x + 1;
            c
        };
    String.unfoldWithIter(step) == "a"
}

@Test
def unfoldWithIter03(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 1)
            None
        else {
            let c = Some(toChar(deref x + 97));
            x := deref x + 1;
            c
        };
    String.unfoldWithIter(step) == "ab"
}

@Test
def unfoldWithIter04(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(toChar(deref x + 97));
            x := deref x + 1;
            c
        };
    String.unfoldWithIter(step) == "abcdefghij"
}

@Test
def unfoldWithIter05(): Bool = region rc {
    let x = ref 5 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(toChar(deref x + 97));
            x := deref x + 1;
            c
        };
    String.unfoldWithIter(step) == "fghij"
}

@Test
def unfoldWithIter06(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c = Some(toChar(deref x + 97));
            x := deref x + 2;
            c
        };
    String.unfoldWithIter(step) == "acegi"
}

/////////////////////////////////////////////////////////////////////////////
// unfoldString                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def unfoldString01(): Bool =
    String.unfoldString(x -> if (x < 5) Some("ab.", x+1) else None, 0) == "ab.ab.ab.ab.ab."

@Test
def unfoldString02(): Bool =
    String.unfoldString(x -> if (x < 5) Some("ab.", x+1) else None, 1) == "ab.ab.ab.ab."

@Test
def unfoldString03(): Bool =
    String.unfoldString(x -> if (x < 5) Some("ab.", x+1) else None, 2) == "ab.ab.ab."

@Test
def unfoldString04(): Bool =
    String.unfoldString(x -> if (x < 5) Some("ab.", x+1) else None, 3) == "ab.ab."

@Test
def unfoldString05(): Bool =
    String.unfoldString(x -> if (x < 5) Some("ab.", x+1) else None, 4) == "ab."

@Test
def unfoldString06(): Bool =
    String.unfoldString(x -> if (x < 5) Some("", x+1) else None, 5) == ""

@Test
def unfoldString07(): Bool =
    String.unfoldString(x -> if (x < 5) Some("ab.", x+1) else None, 6) == ""

@Test
def unfoldString08(): Bool =
    String.unfoldString(x -> if (x < 5) Some("ab.", x+1) else None, -1) == "ab.ab.ab.ab.ab.ab."


/////////////////////////////////////////////////////////////////////////////
// unfoldStringWithIter                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def unfoldStringWithIter01(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (true)
            None
        else {
            let c1 = toChar(deref x + 65);
            let c2 = toChar(deref x + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            x := deref x + 1;
            Some(s)
        };
    String.unfoldStringWithIter(step) == ""
}

@Test
def unfoldStringWithIter02(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 0)
            None
        else {
            let c1 = toChar(deref x + 65);
            let c2 = toChar(deref x + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            x := deref x + 1;
            Some(s)
        };
    String.unfoldStringWithIter(step) == "Aa."
}

@Test
def unfoldStringWithIter03(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x > 1)
            None
        else {
            let c1 = toChar(deref x + 65);
            let c2 = toChar(deref x + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            x := deref x + 1;
            Some(s)
        };
    String.unfoldStringWithIter(step) == "Aa.Bb."
}

@Test
def unfoldStringWithIter04(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c1 = toChar(deref x + 65);
            let c2 = toChar(deref x + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            x := deref x + 1;
            Some(s)
        };
    String.unfoldStringWithIter(step) == "Aa.Bb.Cc.Dd.Ee.Ff.Gg.Hh.Ii.Jj."
}

@Test
def unfoldStringWithIter05(): Bool = region rc {
    let x = ref 5 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c1 = toChar(deref x + 65);
            let c2 = toChar(deref x + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            x := deref x + 1;
            Some(s)
        };
    String.unfoldStringWithIter(step) == "Ff.Gg.Hh.Ii.Jj."
}

@Test
def unfoldStringWithIter06(): Bool = region rc {
    let x = ref 0 @ rc;
    let step = () ->
        if (deref x >= 10)
            None
        else {
            let c1 = toChar(deref x + 65);
            let c2 = toChar(deref x + 97);
            let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
            x := deref x + 2;
            Some(s)
        };
    String.unfoldStringWithIter(step) == "Aa.Cc.Ee.Gg.Ii."
}

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def exists01(): Bool = String.exists(Char.isUpperCase, "") == false

@Test
def exists02(): Bool = String.exists(Char.isUpperCase, "a") == false

@Test
def exists03(): Bool = String.exists(Char.isUpperCase, "A") == true

@Test
def exists04(): Bool = String.exists(Char.isUpperCase, "ab") == false

@Test
def exists05(): Bool = String.exists(Char.isUpperCase, "aB") == true

@Test
def exists06(): Bool = String.exists(Char.isUpperCase, "Ab") == true

@Test
def exists07(): Bool = String.exists(Char.isUpperCase, "AB") == true

@Test
def exists08(): Bool = String.exists(Char.isUpperCase, "a*c") == false

@Test
def exists09(): Bool = String.exists(Char.isUpperCase, "aBc") == true

/////////////////////////////////////////////////////////////////////////////
// forAll                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def forAll01(): Bool = String.forAll(Char.isUpperCase, "") == true

@Test
def forAll02(): Bool = String.forAll(Char.isUpperCase, "a") == false

@Test
def forAll03(): Bool = String.forAll(Char.isUpperCase, "A") == true

@Test
def forAll04(): Bool = String.forAll(Char.isUpperCase, "a") == false

@Test
def forAll05(): Bool = String.forAll(Char.isUpperCase, "aB") == false

@Test
def forAll06(): Bool = String.forAll(Char.isUpperCase, "Ab") == false

@Test
def forAll07(): Bool = String.forAll(Char.isUpperCase, "AB") == true

@Test
def forAll08(): Bool = String.forAll(Char.isUpperCase, "a*b") == false

@Test
def forAll09(): Bool = String.forAll(Char.isUpperCase, "aBc") == false

@Test
def forAll10(): Bool = String.forAll(Char.isUpperCase, "ABC") == true

/////////////////////////////////////////////////////////////////////////////
// isAscii                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def isAscii01(): Bool = String.isAscii("") == true

@Test
def isAscii02(): Bool = String.isAscii("a") == true

@Test
def isAscii03(): Bool = String.isAscii("A") == true

@Test
def isAscii04(): Bool = String.isAscii("0") == true

@Test
def isAscii05(): Bool = String.isAscii("~") == true

@Test
def isAscii06(): Bool = String.isAscii("Â") == false

@Test
def isAscii07(): Bool = String.isAscii("†") == false

@Test
def isAscii08(): Bool = String.isAscii("ab") == true

@Test
def isAscii09(): Bool = String.isAscii("a†") == false

@Test
def isAscii10(): Bool = String.isAscii("†b") == false

/////////////////////////////////////////////////////////////////////////////
// isWhiteSpace                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def isWhiteSpace01(): Bool = String.isWhiteSpace("") == true

@Test
def isWhiteSpace02(): Bool = String.isWhiteSpace(" ") == true

@Test
def isWhiteSpace03(): Bool = String.isWhiteSpace("A") == false

@Test
def isWhiteSpace04(): Bool = String.isWhiteSpace("0") == false

@Test
def isWhiteSpace05(): Bool = String.isWhiteSpace("~") == false

@Test
def isWhiteSpace06(): Bool = String.isWhiteSpace(String.lineSeparator()) == true

@Test
def isWhiteSpace07(): Bool = String.isWhiteSpace("  ") == true

/////////////////////////////////////////////////////////////////////////////
// trimLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def trimLeft01(): Bool = String.trimLeft("") == ""

@Test
def trimLeft02(): Bool = String.trimLeft(" ") == ""

@Test
def trimLeft03(): Bool = String.trimLeft("a") == "a"

@Test
def trimLeft04(): Bool = String.trimLeft(" a") == "a"

@Test
def trimLeft05(): Bool = String.trimLeft("a ") == "a "

@Test
def trimLeft06(): Bool = String.trimLeft("aa") == "aa"

@Test
def trimLeft07(): Bool = String.trimLeft("  aa") == "aa"

@Test
def trimLeft08(): Bool = String.trimLeft("aa  ") == "aa  "

@Test
def trimLeft09(): Bool = String.trimLeft(" aa ") == "aa "

@Test
def trimLeft10(): Bool = String.trimLeft("  aa  ") == "aa  "

@Test
def trimLeft11(): Bool = String.trimLeft(" a a ") == "a a "

@Test
def trimLeft12(): Bool = String.trimLeft(String.lineSeparator() + "aa") == "aa"


/////////////////////////////////////////////////////////////////////////////
// trimRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def trimRight01(): Bool = String.trimRight("") == ""

@Test
def trimRight02(): Bool = String.trimRight(" ") == ""

@Test
def trimRight03(): Bool = String.trimRight("a") == "a"

@Test
def trimRight04(): Bool = String.trimRight(" a") == " a"

@Test
def trimRight05(): Bool = String.trimRight("a ") == "a"

@Test
def trimRight06(): Bool = String.trimRight("aa") == "aa"

@Test
def trimRight07(): Bool = String.trimRight("  aa") == "  aa"

@Test
def trimRight08(): Bool = String.trimRight("aa  ") == "aa"

@Test
def trimRight09(): Bool = String.trimRight(" aa ") == " aa"

@Test
def trimRight10(): Bool = String.trimRight("  aa  ") == "  aa"

@Test
def trimRight11(): Bool = String.trimRight(" a a ") == " a a"

@Test
def trimRight12(): Bool = String.trimRight("aa" + String.lineSeparator()) == "aa"

/////////////////////////////////////////////////////////////////////////////
// repeat                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def repeat01(): Bool = String.repeat(-1, "a") == ""

@Test
def repeat02(): Bool = String.repeat(0, "a") == ""

@Test
def repeat03(): Bool = String.repeat(1, "a") == "a"

@Test
def repeat04(): Bool = String.repeat(2, "a") == "aa"

@Test
def repeat05(): Bool = String.repeat(3, "a") == "aaa"

@Test
def repeat06(): Bool = String.repeat(0, "ab") == ""

@Test
def repeat07(): Bool = String.repeat(1, "ab") == "ab"

@Test
def repeat08(): Bool = String.repeat(2, "ab") == "abab"

@Test
def repeat09(): Bool = String.repeat(3, "ab") == "ababab"

/////////////////////////////////////////////////////////////////////////////
// padLeft                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def padLeft01(): Bool = String.padLeft(0, ' ', "") == ""

@Test
def padLeft02(): Bool = String.padLeft(1, ' ', "") == " "

@Test
def padLeft03(): Bool = String.padLeft(2, ' ', "") == "  "

@Test
def padLeft04(): Bool = String.padLeft(3, ' ', "") == "   "

@Test
def padLeft05(): Bool = String.padLeft(0, ' ', "a") == "a"

@Test
def padLeft06(): Bool = String.padLeft(1, ' ', "a") == "a"

@Test
def padLeft07(): Bool = String.padLeft(2, ' ', "a") == " a"

@Test
def padLeft08(): Bool = String.padLeft(3, ' ', "a") == "  a"

@Test
def padLeft09(): Bool = String.padLeft(0, ' ', "ab") == "ab"

@Test
def padLeft10(): Bool = String.padLeft(1, ' ', "ab") == "ab"

@Test
def padLeft11(): Bool = String.padLeft(2, ' ', "ab") == "ab"

@Test
def padLeft12(): Bool = String.padLeft(3, ' ', "ab") == " ab"

@Test
def padLeft13(): Bool = String.padLeft(3, '_', "a") == "__a"

@Test
def padLeft14(): Bool = String.padLeft(3, '_', "ab") == "_ab"

@Test
def padLeft15(): Bool = String.padLeft(3, '_', "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// padRight                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def padRight01(): Bool = String.padRight(0, ' ', "") == ""

@Test
def padRight02(): Bool = String.padRight(1, ' ', "") == " "

@Test
def padRight03(): Bool = String.padRight(2, ' ', "") == "  "

@Test
def padRight04(): Bool = String.padRight(3, ' ', "") == "   "

@Test
def padRight05(): Bool = String.padRight(0, ' ', "a") == "a"

@Test
def padRight06(): Bool = String.padRight(1, ' ', "a") == "a"

@Test
def padRight07(): Bool = String.padRight(2, ' ', "a") == "a "

@Test
def padRight08(): Bool = String.padRight(3, ' ', "a") == "a  "

@Test
def padRight09(): Bool = String.padRight(0, ' ', "ab") == "ab"

@Test
def padRight10(): Bool = String.padRight(1, ' ', "ab") == "ab"

@Test
def padRight11(): Bool = String.padRight(2, ' ', "ab") == "ab"

@Test
def padRight12(): Bool = String.padRight(3, ' ', "ab") == "ab "

@Test
def padRight13(): Bool = String.padRight(3, '_', "a") == "a__"

@Test
def padRight14(): Bool = String.padRight(3, '_', "ab") == "ab_"

@Test
def padRight15(): Bool = String.padRight(3, '_', "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// center                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def center01(): Bool = String.center(0, ' ', "") == ""

@Test
def center02(): Bool = String.center(1, ' ', "") == " "

@Test
def center03(): Bool = String.center(2, ' ', "") == "  "

@Test
def center04(): Bool = String.center(3, ' ', "") == "   "

@Test
def center05(): Bool = String.center(0, ' ', "a") == "a"

@Test
def center06(): Bool = String.center(1, ' ', "a") == "a"

@Test
def center07(): Bool = String.center(2, ' ', "a") == "a "

@Test
def center08(): Bool = String.center(3, ' ', "a") == " a "

@Test
def center09(): Bool = String.center(0, ' ', "ab") == "ab"

@Test
def center10(): Bool = String.center(1, ' ', "ab") == "ab"

@Test
def center11(): Bool = String.center(2, ' ', "ab") == "ab"

@Test
def center12(): Bool = String.center(3, ' ', "ab") == "ab "

@Test
def center13(): Bool = String.center(3, '_', "a") == "_a_"

@Test
def center14(): Bool = String.center(3, '_', "ab") == "ab_"

@Test
def center15(): Bool = String.center(3, '_', "abc") == "abc"

@Test
def center16(): Bool = String.center(7, '_', "abc") == "__abc__"

@Test
def center17(): Bool = String.center(8, '_', "abc") == "__abc___"

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def replace01(): Bool = String.replace({from = "c"}, {to = "d"}, "") == ""

@Test
def replace02(): Bool = String.replace({from = "c"}, {to = "d"}, "a") == "a"

@Test
def replace03(): Bool = String.replace({from = "c"}, {to = "d"}, "c") == "d"

@Test
def replace04(): Bool = String.replace({from = "c"}, {to = "d"}, "d") == "d"

@Test
def replace05(): Bool = String.replace({from = "c"}, {to = "d"}, "ab") == "ab"

@Test
def replace06(): Bool = String.replace({from = "c"}, {to = "d"}, "abc") == "abd"

@Test
def replace07(): Bool = String.replace({from = "c"}, {to = "d"}, "cd") == "dd"

@Test
def replace08(): Bool = String.replace({from = "c"}, {to = "d"}, "cc") == "dd"

/////////////////////////////////////////////////////////////////////////////
// replaceChar                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def replaceChar01(): Bool = String.replaceChar({from = 'c'}, {to = 'd'}, "") == ""

@Test
def replaceChar02(): Bool = String.replaceChar({from = 'c'}, {to = 'd'}, "a") == "a"

@Test
def replaceChar03(): Bool = String.replaceChar({from = 'c'}, {to = 'd'}, "c") == "d"

@Test
def replaceChar04(): Bool = String.replaceChar({from = 'c'}, {to = 'd'}, "d") == "d"

@Test
def replaceChar05(): Bool = String.replaceChar({from = 'c'}, {to = 'd'}, "ab") == "ab"

@Test
def replaceChar06(): Bool = String.replaceChar({from = 'c'}, {to = 'd'}, "abc") == "abd"

@Test
def replaceChar07(): Bool = String.replaceChar({from = 'c'}, {to = 'd'}, "cd") == "dd"

@Test
def replaceChar08(): Bool = String.replaceChar({from = 'c'}, {to = 'd'}, "cc") == "dd"

/////////////////////////////////////////////////////////////////////////////
// replaceMatches                                                          //
/////////////////////////////////////////////////////////////////////////////

@Test
def replaceMatches01(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "") == ""

@Test
def replaceMatches02(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "a") == "a"

@Test
def replaceMatches03(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "c") == "d"

@Test
def replaceMatches04(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "d") == "d"

@Test
def replaceMatches05(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "ab") == "ab"

@Test
def replaceMatches06(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "abc") == "abd"

@Test
def replaceMatches07(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "cd") == "dd"

@Test
def replaceMatches08(): Bool = String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "cc") == "dd"

/////////////////////////////////////////////////////////////////////////////
// replaceFirstMatch                                                       //
/////////////////////////////////////////////////////////////////////////////

@Test
def replaceFirstMatch01(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "") == ""

@Test
def replaceFirstMatch02(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "a") == "a"

@Test
def replaceFirstMatch03(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "c") == "d"

@Test
def replaceFirstMatch04(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "d") == "d"

@Test
def replaceFirstMatch05(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "ab") == "ab"

@Test
def replaceFirstMatch06(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "abc") == "abd"

@Test
def replaceFirstMatch07(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "cd") == "dd"

@Test
def replaceFirstMatch08(): Bool = String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "cc") == "dc"

/////////////////////////////////////////////////////////////////////////////
// update                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def update01(): Bool = String.update(0, 'b', "") == ""

@Test
def update02(): Bool = String.update(-1, 'b', "a") == "a"

@Test
def update03(): Bool = String.update(0, 'b', "a") == "b"

@Test
def update04(): Bool = String.update(1, 'b', "a") == "a"

@Test
def update05(): Bool = String.update(0, 'e', "ab") == "eb"

@Test
def update06(): Bool = String.update(1, 'e', "ab") == "ae"

@Test
def update07(): Bool = String.update(2, 'e', "ab") == "ab"

/////////////////////////////////////////////////////////////////////////////
// patch                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def patch01(): Bool = String.patch(0, 0, "", "") == ""

@Test
def patch02(): Bool = String.patch(0, 2, "ab", "") == ""

@Test
def patch03(): Bool = String.patch(0, 2, "", "ab") == "ab"

@Test
def patch04(): Bool = String.patch(-3, 3, "abd", "ab") == "ab"

@Test
def patch05(): Bool = String.patch(2, 3, "abd", "ab") == "ab"

@Test
def patch06(): Bool = String.patch(0, 0, "", "a") == "a"

@Test
def patch07(): Bool = String.patch(1, 0, "b", "a") == "a"

@Test
def patch08(): Bool = String.patch(0, 1, "b", "a") == "b"

@Test
def patch09(): Bool = String.patch(0, 2, "bd", "a") == "b"

@Test
def patch10(): Bool = String.patch(-1, 2, "bd", "a") == "d"

@Test
def patch11(): Bool = String.patch(-1, 2, "cd", "ab") == "db"

@Test
def patch12(): Bool = String.patch(1, 2, "cd", "ab") == "ac"

@Test
def patch13(): Bool = String.patch(-2, 2, "cd", "ab") == "ab"

@Test
def patch14(): Bool = String.patch(2, 2, "cd", "ab") == "ab"

@Test
def patch15(): Bool = String.patch(1, 1, "c", "ab") == "ac"

@Test
def patch16(): Bool = String.patch(0, 2, "cd", "ab") == "cd"

@Test
def patch17(): Bool = String.patch(0, 1, "d", "abc") == "dbc"

@Test
def patch18(): Bool = String.patch(1, 1, "d", "abc") == "adc"

@Test
def patch19(): Bool = String.patch(2, 1, "d", "abc") == "abd"

@Test
def patch20(): Bool = String.patch(0, 2, "de", "abc") == "dec"

@Test
def patch21(): Bool = String.patch(1, 2, "de", "abc") == "ade"

@Test
def patch22(): Bool = String.patch(0, 2, "def", "abc") == "dec"

@Test
def patch23(): Bool = String.patch(0, 3, "def", "abc") == "def"

@Test
def patch24(): Bool = String.patch(2, 4, "nopq", "abcdefg") == "abnopqg"

@Test
def patch25(): Bool = String.patch(-2, 4, "nopq", "abcdefg") == "pqcdefg"

@Test
def patch26(): Bool = String.patch(4, 5, "nopq", "abcdefg") == "abcdnop"

@Test
def patch27(): Bool = String.patch(4, 2, "nopq", "abcdefg") == "abcdnog"

@Test
def patch28(): Bool = String.patch(-1, 10, "ABCDEFGH", "abcdefg") == "BCDEFGH"

/////////////////////////////////////////////////////////////////////////////
// indent                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def indent01(): Bool =
    String.indent(3, "") == ""

@Test
def indent02(): Bool =
    String.indent(3, String.unlines("abc" :: Nil)) == String.unlines("   abc" :: Nil)

@Test
def indent03(): Bool =
    String.indent(3, String.unlines("abc" :: "def" :: Nil)) == String.unlines("   abc" :: "   def" :: Nil)

@Test
def indent04(): Bool =
    String.indent(3, String.unlines("ab" :: "cd" :: "ef" :: Nil)) == String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)

@Test
def indent05(): Bool =
    String.indent(1, String.unlines("abc" :: Nil)) == String.unlines(" abc" :: Nil)

@Test
def indent06(): Bool =
    String.indent(1, String.unlines("abc" :: "def" :: Nil)) == String.unlines(" abc" :: " def" :: Nil)

@Test
def indent07(): Bool =
    String.indent(1, String.unlines("ab" :: "cd" :: "ef" :: Nil)) == String.unlines(" ab" :: " cd" :: " ef" :: Nil)

@Test
def indent08(): Bool =
    String.indent(0, String.unlines("abc" :: Nil)) == String.unlines("abc" :: Nil)

@Test
def indent09(): Bool =
    String.indent(0, String.unlines("abc" :: "def" :: Nil)) == String.unlines("abc" :: "def" :: Nil)

@Test
def indent10(): Bool =
    String.indent(0, String.unlines("ab" :: "cd" :: "ef" :: Nil)) == String.unlines("ab" :: "cd" :: "ef" :: Nil)

/////////////////////////////////////////////////////////////////////////////
// stripIndent                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def stripIndent01(): Bool =
    String.stripIndent(3, "") == ""

@Test
def stripIndent02(): Bool =
    String.stripIndent(3, String.unlines("   abc" :: Nil)) == String.unlines("abc" :: Nil)

@Test
def stripIndent03(): Bool =
    String.stripIndent(3, String.unlines("   abc" :: "   def" :: Nil)) == String.unlines("abc" :: "def" :: Nil)

@Test
def stripIndent04(): Bool =
    String.stripIndent(3, String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)) == String.unlines("ab" :: "cd" :: "ef" :: Nil)

@Test
def stripIndent05(): Bool =
    String.stripIndent(1, String.unlines("   abc" :: Nil)) == String.unlines("  abc" :: Nil)

@Test
def stripIndent06(): Bool =
    String.stripIndent(1, String.unlines("   abc" :: "   def" :: Nil)) == String.unlines("  abc" :: "  def" :: Nil)

@Test
def stripIndent07(): Bool =
    String.stripIndent(1, String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)) == String.unlines("  ab" :: "  cd" :: "  ef" :: Nil)

@Test
def stripIndent08(): Bool =
    String.stripIndent(0, String.unlines("   abc" :: Nil)) == String.unlines("   abc" :: Nil)

@Test
def stripIndent09(): Bool =
    String.stripIndent(0, String.unlines("   abc" :: "   def" :: Nil)) == String.unlines("   abc" :: "   def" :: Nil)

@Test
def stripIndent10(): Bool  =
    String.stripIndent(0, String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)) == String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)

/////////////////////////////////////////////////////////////////////////////
// lines                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def lines01(): Bool = String.lines("") == Nil

@Test
def lines02(): Bool = String.lines("a")  == "a" :: Nil

@Test
def lines03(): Bool = String.lines("a" + String.lineSeparator()) == "a" :: Nil

@Test
def lines04(): Bool = String.lines(String.lineSeparator() + "a") == "" :: "a" :: Nil

@Test
def lines05(): Bool = String.lines("a" + String.lineSeparator() + "b") == "a" :: "b" :: Nil

@Test
def lines06(): Bool = String.lines("a" + Char.toString('\n') + "b") == "a" :: "b" :: Nil

@Test
def lines07(): Bool = String.lines("a" + Char.toString('\r') + Char.toString('\n') + "b") ==  "a" :: "b" :: Nil

/////////////////////////////////////////////////////////////////////////////
// unlines                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def unlines01(): Bool = String.unlines(Nil) == ""

@Test
def unlines02(): Bool =
    String.unlines("" :: Nil) == String.lineSeparator()

@Test
def unlines03(): Bool =
    String.unlines("a" :: Nil) == "a" + String.lineSeparator()

@Test
def unlines04(): Bool =
    String.unlines("a" :: "" :: Nil) == "a" + String.lineSeparator() + String.lineSeparator()

@Test
def unlines05(): Bool =
    String.unlines("" :: "b" :: Nil) == String.lineSeparator() + "b" + String.lineSeparator()

@Test
def unlines06(): Bool =
    String.unlines("a" :: "b" :: Nil) == "a" + String.lineSeparator() + "b" + String.lineSeparator()

@Test
def unlines07(): Bool =
    String.unlines("a" :: String.lineSeparator() :: Nil) == "a" + String.lineSeparator() + String.lineSeparator() + String.lineSeparator()

@Test
def unlines08(): Bool =
    String.unlines(String.lineSeparator() :: "b" :: Nil) ==  String.lineSeparator() + String.lineSeparator() + "b" + String.lineSeparator()

/////////////////////////////////////////////////////////////////////////////
// words                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def words01(): Bool = String.words("") == Nil

@Test
def words02(): Bool = String.words("a") == "a" :: Nil

@Test
def words03(): Bool = String.words("a ") == "a" :: Nil

@Test
def words04(): Bool = String.words(" a") == "a" :: Nil

@Test
def words05(): Bool = String.words("a b") == "a" :: "b" :: Nil

@Test
def words06(): Bool = String.words("a  b") == "a" :: "b" :: Nil

@Test
def words07(): Bool = String.words("a" + Char.toString('\t') + "b") == "a" :: "b" :: Nil

/////////////////////////////////////////////////////////////////////////////
// unwords                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def unwords01(): Bool = String.unwords(Nil) == ""

@Test
def unwords02(): Bool = String.unwords("" :: Nil) == ""

@Test
def unwords03(): Bool = String.unwords("a" :: Nil) == "a"

@Test
def unwords04(): Bool = String.unwords("a" :: "" :: Nil) == "a "

@Test
def unwords05(): Bool = String.unwords("" :: "b" :: Nil) == " b"

@Test
def unwords06(): Bool = String.unwords("a" :: "b" :: Nil) == "a b"

@Test
def unwords07(): Bool = String.unwords("a" :: String.lineSeparator() :: Nil) == "a " + String.lineSeparator()

@Test
def unwords08(): Bool = String.unwords(String.lineSeparator() :: "b" :: Nil) ==  String.lineSeparator() + " b"

/////////////////////////////////////////////////////////////////////////////
// indexOf                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def indexOf01(): Bool = String.indexOf({substr = "a"}, "") == None

@Test
def indexOf02(): Bool = String.indexOf({substr = "a"}, "A") == None

@Test
def indexOf03(): Bool = String.indexOf({substr = "a"}, "a") == Some(0)

@Test
def indexOf04(): Bool = String.indexOf({substr = "a"}, "AB") == None

@Test
def indexOf05(): Bool = String.indexOf({substr = "a"}, "ab") == Some(0)

@Test
def indexOf06(): Bool = String.indexOf({substr = "b"}, "ab") == Some(1)

@Test
def indexOf07(): Bool = String.indexOf({substr = "a"}, "aa") == Some(0)

@Test
def indexOf08(): Bool = String.indexOf({substr = "ab"}, "ab") == Some(0)

@Test
def indexOf09(): Bool = String.indexOf({substr = "bc"}, "abc") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// indexOfLeft                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def indexOfLeft01(): Bool = String.indexOfLeft({substr = "a"}, "") == None

@Test
def indexOfLeft02(): Bool = String.indexOfLeft({substr = "a"}, "A") == None

@Test
def indexOfLeft03(): Bool = String.indexOfLeft({substr = "a"}, "a") == Some(0)

@Test
def indexOfLeft04(): Bool = String.indexOfLeft({substr = "a"}, "AB") == None

@Test
def indexOfLeft05(): Bool = String.indexOfLeft({substr = "a"}, "ab") == Some(0)

@Test
def indexOfLeft06(): Bool = String.indexOfLeft({substr = "b"}, "ab") == Some(1)

@Test
def indexOfLeft07(): Bool = String.indexOfLeft({substr = "a"}, "aa") == Some(0)

@Test
def indexOfLeft08(): Bool = String.indexOfLeft({substr = "ab"}, "ab") == Some(0)

@Test
def indexOfLeft09(): Bool = String.indexOfLeft({substr = "bc"}, "abc") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// indexOfRight                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def indexOfRight01(): Bool = String.indexOfRight({substr = "a"}, "") == None

@Test
def indexOfRight02(): Bool = String.indexOfRight({substr = "a"}, "A") == None

@Test
def indexOfRight03(): Bool = String.indexOfRight({substr = "a"}, "a") == Some(0)

@Test
def indexOfRight04(): Bool = String.indexOfRight({substr = "a"}, "AB") == None

@Test
def indexOfRight05(): Bool = String.indexOfRight({substr = "a"}, "ab") == Some(0)

@Test
def indexOfRight06(): Bool = String.indexOfRight({substr = "b"}, "ab") == Some(1)

@Test
def indexOfRight07(): Bool = String.indexOfRight({substr = "a"}, "aa") == Some(1)

@Test
def indexOfRight08(): Bool = String.indexOfRight({substr = "ab"}, "ab") == Some(0)

@Test
def indexOfRight09(): Bool = String.indexOfRight({substr = "bc"}, "abc") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// startsWith                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def startsWith01(): Bool = String.startsWith({prefix = ""}, "") == true

@Test
def startsWith02(): Bool = String.startsWith({prefix = ""}, "a") == true

@Test
def startsWith03(): Bool = String.startsWith({prefix = ""}, "abc") == true

@Test
def startsWith04(): Bool = String.startsWith({prefix = "a"}, "") == false

@Test
def startsWith05(): Bool = String.startsWith({prefix = "a"}, "a") == true

@Test
def startsWith06(): Bool = String.startsWith({prefix = "a"}, "b") == false

@Test
def startsWith07(): Bool = String.startsWith({prefix = "a"}, "aa") == true

@Test
def startsWith08(): Bool = String.startsWith({prefix = "ab"}, "ab") == true

@Test
def startsWith09(): Bool = String.startsWith({prefix = "bs"}, "abc") == false

@Test
def startsWith10(): Bool = String.startsWith({prefix = ""}, "Hello World!") == true

@Test
def startsWith11(): Bool = String.startsWith({prefix = "Hello"}, "Hello World!") == true

@Test
def startsWith12(): Bool = String.startsWith({prefix = "HELLO"}, "Hello World!") == false

/////////////////////////////////////////////////////////////////////////////
// contains                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def contains01(): Bool = String.contains({substr = ""}, "") == true

@Test
def contains02(): Bool = String.contains({substr = ""}, "a") == true

@Test
def contains03(): Bool = String.contains({substr = ""}, "abc") == true

@Test
def contains04(): Bool = String.contains({substr = "a"}, "") == false

@Test
def contains05(): Bool = String.contains({substr = "a"}, "a") == true

@Test
def contains06(): Bool = String.contains({substr = "a"}, "b") == false

@Test
def contains07(): Bool = String.contains({substr = "a"}, "aa") == true

@Test
def contains08(): Bool = String.contains({substr = "ab"}, "ab") == true

@Test
def contains09(): Bool = String.contains({substr = "ab"}, "abc") == true

/////////////////////////////////////////////////////////////////////////////
// endsWith                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def endsWith01(): Bool = String.endsWith({suffix = ""}, "") == true

@Test
def endsWith02(): Bool = String.endsWith({suffix = ""}, "a") == true

@Test
def endsWith03(): Bool = String.endsWith({suffix = ""}, "abc") == true

@Test
def endsWith04(): Bool = String.endsWith({suffix = "a"}, "") == false

@Test
def endsWith05(): Bool = String.endsWith({suffix = "a"}, "a") == true

@Test
def endsWith06(): Bool = String.endsWith({suffix = "a"}, "b") == false

@Test
def endsWith07(): Bool = String.endsWith({suffix = "a"}, "aa") == true

@Test
def endsWith08(): Bool = String.endsWith({suffix = "ab"}, "ab") == true

@Test
def endsWith09(): Bool = String.endsWith({suffix = "bc"}, "abc") == true

@Test
def endsWith10(): Bool = String.endsWith({suffix = "ab"}, "abc") == false

@Test
def endsWith11(): Bool = String.endsWith({suffix = ""}, "Hello World!") == true

@Test
def endsWith12(): Bool = String.endsWith({suffix = "World!"}, "Hello World!") == true

@Test
def endsWith13(): Bool = String.endsWith({suffix = "WORLD!"}, "Hello World!") == false

/////////////////////////////////////////////////////////////////////////////
// commonPrefix                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def commonPrefix01(): Bool =
    String.commonPrefix("", "") == ""

@Test
def commonPrefix02(): Bool =
    String.commonPrefix("a", "") == ""

@Test
def commonPrefix03(): Bool =
    String.commonPrefix("", "a") == ""

@Test
def commonPrefix04(): Bool =
    String.commonPrefix("a", "a") == "a"

@Test
def commonPrefix05(): Bool =
    String.commonPrefix("a", "b") == ""

@Test
def commonPrefix06(): Bool =
    String.commonPrefix("ab", "bc") == ""

@Test
def commonPrefix07(): Bool =
    String.commonPrefix("ab", "ac") == "a"

@Test
def commonPrefix08(): Bool =
    String.commonPrefix("ab", "ab") == "ab"

@Test
def commonPrefix09(): Bool =
    String.commonPrefix("abc", "ABC") == ""

@Test
def commonPrefix10(): Bool =
    String.commonPrefix("abc", "abcd") == "abc"

@Test
def commonPrefix11(): Bool =
    String.commonPrefix("abcd", "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// commonSuffix                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def commonSuffix01(): Bool =
    String.commonSuffix("", "") == ""

@Test
def commonSuffix02(): Bool =
    String.commonSuffix("a", "") == ""

@Test
def commonSuffix03(): Bool =
    String.commonSuffix("", "a") == ""

@Test
def commonSuffix04(): Bool =
    String.commonSuffix("a", "a") == "a"

@Test
def commonSuffix05(): Bool =
    String.commonSuffix("a", "b") == ""

@Test
def commonSuffix06(): Bool =
    String.commonSuffix("ab", "bc") == ""

@Test
def commonSuffix07(): Bool =
    String.commonSuffix("ab", "_b") == "b"

@Test
def commonSuffix08(): Bool =
    String.commonSuffix("ab", "ab") == "ab"

@Test
def commonSuffix09(): Bool =
    String.commonSuffix("abc", "ABC") == ""

@Test
def commonSuffix10(): Bool =
    String.commonSuffix("bcd", "abcd") == "bcd"

@Test
def commonSuffix11(): Bool =
    String.commonSuffix("abcd", "bcd") == "bcd"

/////////////////////////////////////////////////////////////////////////////
// abbreviateLeft                                                          //
/////////////////////////////////////////////////////////////////////////////

@Test
def abbreviateLeft01(): Bool =
    String.abbreviateLeft(0, "") == ""

@Test
def abbreviateLeft02(): Bool =
    String.abbreviateLeft(0, "a") == ""

@Test
def abbreviateLeft03(): Bool =
    String.abbreviateLeft(0, "abcde") == ""

@Test
def abbreviateLeft04(): Bool =
    String.abbreviateLeft(1, "") == ""

@Test
def abbreviateLeft05(): Bool =
    String.abbreviateLeft(1, "a") == ""

@Test
def abbreviateLeft06(): Bool =
    String.abbreviateLeft(1, "abcde") == ""

@Test
def abbreviateLeft07(): Bool =
    String.abbreviateLeft(3, "") == ""

@Test
def abbreviateLeft08(): Bool =
    String.abbreviateLeft(3, "a") == "a"

@Test
def abbreviateLeft09(): Bool =
    String.abbreviateLeft(3, "abcde") == "..."

@Test
def abbreviateLeft10(): Bool =
    String.abbreviateLeft(4, "") == ""

@Test
def abbreviateLeft11(): Bool =
    String.abbreviateLeft(4, "a") == "a"

@Test
def abbreviateLeft12(): Bool =
    String.abbreviateLeft(4, "abcde") == "...e"

@Test
def abbreviateLeft13(): Bool =
    String.abbreviateLeft(5, "") == ""

@Test
def abbreviateLeft14(): Bool =
    String.abbreviateLeft(5, "a") == "a"

@Test
def abbreviateLeft15(): Bool =
    String.abbreviateLeft(5, "abcde") == "...de"


/////////////////////////////////////////////////////////////////////////////
// abbreviateRight                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def abbreviateRight01(): Bool =
    String.abbreviateRight(0, "") == ""

@Test
def abbreviateRight02(): Bool =
    String.abbreviateRight(0, "a") == ""

@Test
def abbreviateRight03(): Bool =
    String.abbreviateRight(0, "abcde") == ""

@Test
def abbreviateRight04(): Bool =
    String.abbreviateRight(1, "") == ""

@Test
def abbreviateRight05(): Bool =
    String.abbreviateRight(1, "a") == ""

@Test
def abbreviateRight06(): Bool =
    String.abbreviateRight(1, "abcde") == ""

@Test
def abbreviateRight07(): Bool =
    String.abbreviateRight(3, "") == ""

@Test
def abbreviateRight08(): Bool =
    String.abbreviateRight(3, "a") == "a"

@Test
def abbreviateRight09(): Bool =
    String.abbreviateRight(3, "abcde") == "..."

@Test
def abbreviateRight10(): Bool =
    String.abbreviateRight(4, "") == ""

@Test
def abbreviateRight11(): Bool =
    String.abbreviateRight(4, "a") == "a"

@Test
def abbreviateRight12(): Bool =
    String.abbreviateRight(4, "abcde") == "a..."

@Test
def abbreviateRight13(): Bool =
    String.abbreviateRight(5, "") == ""

@Test
def abbreviateRight14(): Bool =
    String.abbreviateRight(5, "a") == "a"

@Test
def abbreviateRight15(): Bool =
    String.abbreviateRight(5, "abcde") == "ab..."

/////////////////////////////////////////////////////////////////////////////
// levenshteinDistance                                                     //
/////////////////////////////////////////////////////////////////////////////

@Test
def levenshteinDistance01(): Bool =
    String.levenshteinDistance("", "") == 0

@Test
def levenshteinDistance02(): Bool =
    String.levenshteinDistance("A", "") == 1

@Test
def levenshteinDistance03(): Bool =
    String.levenshteinDistance("", "A") == 1

@Test
def levenshteinDistance04(): Bool =
    String.levenshteinDistance("A", "A") == 0

@Test
def levenshteinDistance05(): Bool =
    String.levenshteinDistance("kitten", "sitting") == 3

@Test
def levenshteinDistance06(): Bool =
    String.levenshteinDistance("Saturday", "Sunday") == 3

@Test
def levenshteinDistance07(): Bool =
    String.levenshteinDistance("Gumbo", "Gambol") == 2

/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////

@Test
def zip01(): Bool =
    String.zip("", "") == Nil: List[(Char,Char)]

@Test
def zip02(): Bool =
    String.zip("a", "") == Nil: List[(Char, Char)]

@Test
def zip03(): Bool =
    String.zip("", "1") == Nil: List[(Char, Char)]

@Test
def zip04(): Bool =
    String.zip("a", "1") == ('a', '1') :: Nil

@Test
def zip05(): Bool =
    String.zip("ab", "12") == ('a', '1') :: ('b', '2') :: Nil

@Test
def zip06(): Bool =
    String.zip("abc", "123") == ('a', '1') :: ('b', '2') :: ('c', '3') :: Nil

@Test
def zip07(): Bool =
    String.zip("abcd", "1234") == ('a', '1') :: ('b', '2') :: ('c', '3') :: ('d', '4') :: Nil

/////////////////////////////////////////////////////////////////////////////
// zipWith                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def zipWith01(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "", "");
    s == ""

@Test
def zipWith02(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "a", "");
    s == ""

@Test
def zipWith03(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "", "1");
    s == ""

@Test
def zipWith04(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_", "1");
    s == "1"

@Test
def zipWith05(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "a", "1");
    s == "a"

@Test
def zipWith06(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fgh", "12345678");
    s == "1bc45fgh"

@Test
def zipWith07(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fghijkl", "12345678");
    s == "1bc45fgh"

@Test
def zipWith08(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fgh", "1234567890");
    s == "1bc45fgh"

/////////////////////////////////////////////////////////////////////////////
// splitOn                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def splitOn01(): Bool = String.splitOn({substr = ""}, "") == "" :: Nil

@Test
def splitOn02(): Bool = String.splitOn({substr = ""}, "a::b") == "a::b" :: Nil

@Test
def splitOn03(): Bool = String.splitOn({substr = "::"}, "") == "" :: Nil

@Test
def splitOn04(): Bool = String.splitOn({substr = "::"}, "a") == "a" :: Nil

@Test
def splitOn05(): Bool = String.splitOn({substr = "::"}, "a:b") == "a:b" :: Nil

@Test
def splitOn06(): Bool = String.splitOn({substr = "::"}, "a::b") == "a" :: "b" :: Nil

@Test
def splitOn07(): Bool = String.splitOn({substr = "::"}, "::a::b") == "" :: "a" :: "b" :: Nil

@Test
def splitOn08(): Bool = String.splitOn({substr = "::"}, "a::b::") == "a" :: "b" :: "" :: Nil

@Test
def splitOn09(): Bool = String.splitOn({substr = "::"}, "::a::b::") == "" :: "a" :: "b" :: "" :: Nil

@Test
def splitOn10(): Bool = String.splitOn({substr = "::"}, "a:b::c") == "a:b" :: "c" :: Nil

@Test
def splitOn11(): Bool = String.splitOn({substr = "::"}, "a::b:c") == "a" :: "b:c" :: Nil

/////////////////////////////////////////////////////////////////////////////
// indexOfLeftWithOffset                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def indexOfLeftWithOffset01(): Bool = String.indexOfLeftWithOffset({substr = ""}, {offset = 0}, "") == None

@Test
def indexOfLeftWithOffset02(): Bool = String.indexOfLeftWithOffset({substr = ""}, {offset = 0}, "a") == None

@Test
def indexOfLeftWithOffset03(): Bool = String.indexOfLeftWithOffset({substr = "a"}, {offset = 0}, "a") == Some(0)

@Test
def indexOfLeftWithOffset04(): Bool = String.indexOfLeftWithOffset({substr = "a"}, {offset = 1}, "a") == None

@Test
def indexOfLeftWithOffset05(): Bool = String.indexOfLeftWithOffset({substr = "a"}, {offset = 0}, "ab") == Some(0)

@Test
def indexOfLeftWithOffset06(): Bool = String.indexOfLeftWithOffset({substr = "a"}, {offset = 1}, "aa") == Some(1)

@Test
def indexOfLeftWithOffset07(): Bool = String.indexOfLeftWithOffset({substr = "ab"}, {offset = 0}, "ab") == Some(0)

@Test
def indexOfLeftWithOffset08(): Bool = String.indexOfLeftWithOffset({substr = "ab"}, {offset = 0}, "abab") == Some(0)

@Test
def indexOfLeftWithOffset09(): Bool = String.indexOfLeftWithOffset({substr = "ab"}, {offset = 1}, "abab") == Some(2)

@Test
def indexOfLeftWithOffset10(): Bool = String.indexOfLeftWithOffset({substr = "ab"}, {offset = 2}, "abab") == Some(2)

@Test
def indexOfLeftWithOffset11(): Bool = String.indexOfLeftWithOffset({substr = "ab"}, {offset = 3}, "abab") == None

/////////////////////////////////////////////////////////////////////////////
// indexOfRightWithOffset                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def indexOfRightWithOffset01(): Bool = String.indexOfRightWithOffset({substr = ""}, {offset = 0}, "") == None

@Test
def indexOfRightWithOffset02(): Bool = String.indexOfRightWithOffset({substr = ""}, {offset = 0}, "a") == None

@Test
def indexOfRightWithOffset03(): Bool = String.indexOfRightWithOffset({substr = "a"}, {offset = 0}, "a") == Some(0)

@Test
def indexOfRightWithOffset04(): Bool = String.indexOfRightWithOffset({substr = "a"}, {offset = 1}, "a") == Some(0)

@Test
def indexOfRightWithOffset05(): Bool = String.indexOfRightWithOffset({substr = "a"}, {offset = 2}, "ab") == Some(0)

@Test
def indexOfRightWithOffset06(): Bool = String.indexOfRightWithOffset({substr = "a"}, {offset = 2}, "aa") == Some(1)

@Test
def indexOfRightWithOffset07(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = 2}, "ab") == Some(0)

@Test
def indexOfRightWithOffset08(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = 3}, "abab") == Some(2)

@Test
def indexOfRightWithOffset09(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = 2}, "abab") == Some(2)

@Test
def indexOfRightWithOffset10(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = 1}, "abab") == Some(0)

@Test
def indexOfRightWithOffset11(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = 0}, "abab") == Some(0)

@Test
def indexOfRightWithOffset12(): Bool = String.indexOfRightWithOffset({substr = "ab"}, {offset = -1}, "abab") == None

/////////////////////////////////////////////////////////////////////////////
// toChunks                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def toChunks01(): Bool = String.toChunks(0, "") == Nil

@Test
def toChunks02(): Bool = String.toChunks(0, "aaa") == Nil

@Test
def toChunks03(): Bool = String.toChunks(3, "") == Nil

@Test
def toChunks04(): Bool = String.toChunks(3, "a") == "a" :: Nil

@Test
def toChunks05(): Bool = String.toChunks(3, "aa") == "aa" :: Nil

@Test
def toChunks06(): Bool = String.toChunks(3, "aaa") == "aaa" :: Nil

@Test
def toChunks07(): Bool = String.toChunks(3, "aaab") == "aaa" :: "b" :: Nil

@Test
def toChunks08(): Bool = String.toChunks(3, "aaabb") == "aaa" :: "bb" :: Nil

@Test
def toChunks09(): Bool = String.toChunks(3, "aaabbb") == "aaa" :: "bbb" :: Nil

@Test
def toChunks10(): Bool = String.toChunks(3, "aaabbbc") == "aaa" :: "bbb" :: "c" :: Nil

@Test
def toChunks11(): Bool = String.toChunks(1, "aaabbbc") == "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: "c" :: Nil

@Test
def toChunks12(): Bool = String.toChunks(2, "aaabbbc") == "aa" :: "ab" :: "bb" :: "c" :: Nil

@Test
def toChunks13(): Bool = String.toChunks(4, "aaabbbc") == "aaab" :: "bbc" :: Nil

@Test
def toChunks14(): Bool = String.toChunks(10, "aaabbbc") == "aaabbbc" :: Nil

/////////////////////////////////////////////////////////////////////////////
// breakOnLeft                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def breakOnLeft01(): Bool = String.breakOnLeft({substr = ""}, "") == ("", "")

@Test
def breakOnLeft02(): Bool = String.breakOnLeft({substr = ""}, "aaa") == ("aaa", "")

@Test
def breakOnLeft03(): Bool = String.breakOnLeft({substr = "::"}, "") == ("", "")

@Test
def breakOnLeft04(): Bool = String.breakOnLeft({substr = "::"}, "aaa") == ("aaa", "")

@Test
def breakOnLeft05(): Bool = String.breakOnLeft({substr = "::"}, "aaa:") == ("aaa:", "")

@Test
def breakOnLeft06(): Bool = String.breakOnLeft({substr = "::"}, "aaa::") == ("aaa", "::")

@Test
def breakOnLeft07(): Bool = String.breakOnLeft({substr = "::"}, "aaa::bbb") == ("aaa", "::bbb")

@Test
def breakOnLeft08(): Bool = String.breakOnLeft({substr = "::"}, "aaa::bbb::ccc") == ("aaa", "::bbb::ccc")

/////////////////////////////////////////////////////////////////////////////
// breakOnRight                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def breakOnRight01(): Bool = String.breakOnRight({substr = ""}, "") == ("", "")

@Test
def breakOnRight02(): Bool = String.breakOnRight({substr = ""}, "aaa") == ("aaa", "")

@Test
def breakOnRight03(): Bool = String.breakOnRight({substr = "::"}, "") == ("", "")

@Test
def breakOnRight04(): Bool = String.breakOnRight({substr = "::"}, "aaa") == ("aaa", "")

@Test
def breakOnRight05(): Bool = String.breakOnRight({substr = "::"}, "aaa:") == ("aaa:", "")

@Test
def breakOnRight06(): Bool = String.breakOnRight({substr = "::"}, "aaa::") == ("aaa::", "")

@Test
def breakOnRight07(): Bool = String.breakOnRight({substr = "::"}, "aaa::bbb") == ("aaa::", "bbb")

@Test
def breakOnRight08(): Bool = String.breakOnRight({substr = "::"}, "aaa::bbb::ccc") == ("aaa::bbb::", "ccc")

/////////////////////////////////////////////////////////////////////////////
// countSubstring                                                          //
/////////////////////////////////////////////////////////////////////////////

@Test
def countSubstring01(): Bool = String.countSubstring({substr = ""}, "") == 0

@Test
def countSubstring02(): Bool = String.countSubstring({substr = ""}, "abc abc") == 0

@Test
def countSubstring03(): Bool = String.countSubstring({substr = "abc"}, "") == 0

@Test
def countSubstring04(): Bool = String.countSubstring({substr = "abc"}, "abc") == 1

@Test
def countSubstring05(): Bool = String.countSubstring({substr = "abc"}, "aabc") == 1

@Test
def countSubstring06(): Bool = String.countSubstring({substr = "abc"}, "abcc") == 1

@Test
def countSubstring07(): Bool = String.countSubstring({substr = "abc"}, "abc def ") == 1

@Test
def countSubstring08(): Bool = String.countSubstring({substr = "abc"}, " abc def") == 1

@Test
def countSubstring09(): Bool = String.countSubstring({substr = "abc"}, " abc abc") == 2

@Test
def countSubstring10(): Bool = String.countSubstring({substr = "abc"}, " abc ABC abc") == 2

@Test
def countSubstring11(): Bool = String.countSubstring({substr = "abc"}, "ABC abc def abc") == 2

@Test
def countSubstring12(): Bool = String.countSubstring({substr = "abc"}, "aBC::abc::AbC::abc::Abc") == 2

/////////////////////////////////////////////////////////////////////////////
// stripMargin                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def stripMargin01(): Bool = String.stripMargin("") == ""

@Test
def stripMargin02(): Bool = String.stripMargin("abc") == "abc"

@Test
def stripMargin03(): Bool = String.stripMargin("${'\t'}") == "${'\t'}"

@Test
def stripMargin04(): Bool = String.stripMargin("    |abc") == "abc"

@Test
def stripMargin05(): Bool = String.stripMargin("${'\t'} |abc") == "abc"

@Test
def stripMargin06(): Bool = String.stripMargin(" | abc") == " abc"

@Test
def stripMargin07(): Bool = String.stripMargin(" ||abc") == "|abc"

@Test
def stripMargin08(): Bool = String.stripMargin(String.intercalate("\n", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)) == String.intercalate("\n", "abc" :: "${'\t'}def" :: "ghi" :: Nil)

@Test
def stripMargin09(): Bool = String.stripMargin(String.intercalate("\u000D\u000A", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)) == String.intercalate("\u000D\u000A", "abc" :: "${'\t'}def" :: "ghi" :: Nil)

/////////////////////////////////////////////////////////////////////////////
// stripMarginWith                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def stripMarginWith01(): Bool = String.stripMarginWith({margin = "|"}, "") == ""

@Test
def stripMarginWith02(): Bool = String.stripMarginWith({margin = "|"}, "abc") == "abc"

@Test
def stripMarginWith03(): Bool = String.stripMarginWith({margin = "|"}, "${'\t'}") == "${'\t'}"

@Test
def stripMarginWith04(): Bool = String.stripMarginWith({margin = "|"}, "    |abc") == "abc"

@Test
def stripMarginWith05(): Bool = String.stripMarginWith({margin = "|"}, "${'\t'} |abc") == "abc"

@Test
def stripMarginWith06(): Bool = String.stripMarginWith({margin = "|"}, " | abc") == " abc"

@Test
def stripMarginWith07(): Bool = String.stripMarginWith({margin = "|"}, " ||abc") == "|abc"

@Test
def stripMarginWith08(): Bool = String.stripMarginWith({margin = "| "}, " | abc") == "abc"

@Test
def stripMarginWith09(): Bool = String.stripMarginWith({margin = "|"}, String.intercalate("\n", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)) == String.intercalate("\n", "abc" :: "${'\t'}def" :: "ghi" :: Nil)

@Test
def stripMarginWith10(): Bool = String.stripMarginWith({margin = "|"}, String.intercalate("\u000D\u000A", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)) == String.intercalate("\u000D\u000A", "abc" :: "${'\t'}def" :: "ghi" :: Nil)


/////////////////////////////////////////////////////////////////////////////
// stripPrefix                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def stripPrefix01(): Bool = String.stripPrefix({substr = ""}, "") == Some("")

@Test
def stripPrefix02(): Bool = String.stripPrefix({substr = ""}, "abc") == Some("abc")

@Test
def stripPrefix03(): Bool = String.stripPrefix({substr = "abc"}, "") == None

@Test
def stripPrefix04(): Bool = String.stripPrefix({substr = "abc"}, "abc") == Some("")

@Test
def stripPrefix05(): Bool = String.stripPrefix({substr = "abc"}, "abcdef") == Some("def")

@Test
def stripPrefix06(): Bool = String.stripPrefix({substr = "def"}, "abcdef") == None

@Test
def stripPrefix07(): Bool = String.stripPrefix({substr = "abc"}, "ABCDEF") == None

/////////////////////////////////////////////////////////////////////////////
// stripSuffix                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def stripSuffix01(): Bool = String.stripSuffix({substr = ""}, "") == Some("")

@Test
def stripSuffix02(): Bool = String.stripSuffix({substr = ""}, "abc") == Some("abc")

@Test
def stripSuffix03(): Bool = String.stripSuffix({substr = "abc"}, "") == None

@Test
def stripSuffix04(): Bool = String.stripSuffix({substr = "abc"}, "abc") == Some("")

@Test
def stripSuffix05(): Bool = String.stripSuffix({substr = "abc"}, "abcdef") == None

@Test
def stripSuffix06(): Bool = String.stripSuffix({substr = "def"}, "abcdef") == Some("abc")

@Test
def stripSuffix07(): Bool = String.stripPrefix({substr = "abc"}, "ABCDEF") == None

/////////////////////////////////////////////////////////////////////////////
// wrap                                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def wrap01(): Bool = String.wrap(0, "a") == ""

@Test
def wrap02(): Bool = String.wrap(1, "a") == "a"

@Test
def wrap03(): Bool = String.wrap(2, "a") == "a"

@Test
def wrap04(): Bool = String.wrap(2, "ab") == "ab"

@Test
def wrap05(): Bool = String.wrap(1, "ab") == "a" + String.lineSeparator() + "b"

@Test
def wrap06(): Bool = String.wrap(2, "a b") == "a" + String.lineSeparator() + "b"

@Test
def wrap07(): Bool = String.wrap(2, "a b c") == "a" + String.lineSeparator() + "b" + String.lineSeparator() + "c"

@Test
def wrap08(): Bool = String.wrap(2, "ab   cd") == "ab" + String.lineSeparator() + "cd"

@Test
def wrap09(): Bool = String.wrap(3, "ab   cd") == "ab" + String.lineSeparator() + "cd"

@Test
def wrap10(): Bool = String.wrap(5, "ab   cd") == "ab" + String.lineSeparator() + "cd"

@Test
def wrap11(): Bool =
    String.wrap(5, "Lorem ipsum dolor sitamet")
        == "Lorem" + String.lineSeparator()
        + "ipsum" + String.lineSeparator()
        + "dolor" + String.lineSeparator()
        + "sitam" + String.lineSeparator()
        + "et"

/////////////////////////////////////////////////////////////////////////////
// unwrap                                                                  //
/////////////////////////////////////////////////////////////////////////////

@Test
def unwrap01(): Bool = String.unwrap("") == ""

@Test
def unwrap02(): Bool = String.unwrap("a\nb") == "a b"

@Test
def unwrap03(): Bool = String.unwrap("a\nb\nc") == "a b c"

@Test
def unwrap04(): Bool = String.unwrap("ab\nc\nd") == "ab c d"

@Test
def unwrap05(): Bool = String.unwrap("a\rb") == "a b"

@Test
def unwrap06(): Bool = String.unwrap("a\rb\rc") == "a b c"

@Test
def unwrap07(): Bool = String.unwrap("ab\rc\rd") == "ab c d"

@Test
def unwrap08(): Bool = String.unwrap("a\r\nb") == "a b"

@Test
def unwrap09(): Bool = String.unwrap("a\r\nb\r\nc") == "a b c"

@Test
def unwrap10(): Bool = String.unwrap("ab\r\nc\r\nd") == "ab c d"

/////////////////////////////////////////////////////////////////////////////
// escapeInterpolation                                                     //
/////////////////////////////////////////////////////////////////////////////

@Test
def escapeInterpolation01(): Bool = "\${" == ("$" + "{")

@Test
def escapeInterpolation02(): Bool = "\${1 + {2}" == ("$" + "{1 + {2}")

@Test
def escapeInterpolation03(): Bool = "\${}" == ("$" + "{}")

@Test
def escapeInterpolation04(): String = "$"


    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Bool = region rc {
        String.iterator(rc, "") |> Iterator.toList == Nil
    }

    @Test
    def iterator02(): Bool = region rc {
        String.iterator(rc, "abc") |> Iterator.toList == 'a' :: 'b' :: 'c' :: Nil
    }


    /////////////////////////////////////////////////////////////////////////////
    // enumerator                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def enumerator01(): Bool = region rc {
        String.enumerator(rc, "") |> Iterator.toList == Nil
    }

    @Test
    def enumerator02(): Bool = region rc {
        String.enumerator(rc, "abc") |> Iterator.toList == (0, 'a') :: (1, 'b') :: (2, 'c') :: Nil
    }

}

