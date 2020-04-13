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

namespace TestString {

/////////////////////////////////////////////////////////////////////////////
// charAt                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def charAt01(): Bool = String.charAt(0, "Hello World!") == 'H'

@test
def charAt02(): Bool = String.charAt(1, "Hello World!") == 'e'

@test
def charAt03(): Bool = String.charAt(2, "Hello World!") == 'l'

@test
def charAt04(): Bool = String.charAt(3, "Hello World!") == 'l'

@test
def charAt05(): Bool = String.charAt(4, "Hello World!") == 'o'

@test
def charAt06(): Bool = String.charAt(5, "Hello World!") == ' '

/////////////////////////////////////////////////////////////////////////////
// concat                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def concat01(): Bool = "Hello World!" == "Hello World!"

@test
def concat02(): Bool = "Hello" + " World!" == "Hello World!"

@test
def concat03(): Bool = "Hello" + " World!" == "Hello" + " World!"

@test
def concat04(): Bool = "Hello World!" + "" == "Hello World!"

@test
def concat05(): Bool = "Hello" + " " + "World" + "!" == "Hello World!"

@test
def concat06(): Bool =
    let f = x -> (x + " World!");
    f("Hello") == "Hello World!"

@test
def concat07(): Bool =
    let f = (x, y) -> (x + y);
    f("Hello", " World!") == "Hello World!"

@test
def concat08(): Bool =
    let f = flip((x, y) -> (x + y));
    f(" World!", "Hello") == "Hello World!"

/////////////////////////////////////////////////////////////////////////////
// isEmpty                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def isEmpty01(): Bool = String.isEmpty("") == true

@test
def isEmpty02(): Bool = String.isEmpty("Hello") == false

/////////////////////////////////////////////////////////////////////////////
// length                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def length01(): Bool = String.length("") == 0

@test
def length02(): Bool = String.length("0123456789") == 10

/////////////////////////////////////////////////////////////////////////////
// startsWith                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def startsWith01(): Bool = String.startsWith("Hello World!", "") == true

@test
def startsWith02(): Bool = String.startsWith("Hello World!", "Hello") == true

@test
def startsWith03(): Bool = String.startsWith("Hello World!", "HELLO") == false

/////////////////////////////////////////////////////////////////////////////
// endsWith                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def endsWith01(): Bool = String.endsWith("Hello World!", "") == true

@test
def endsWith02(): Bool = String.endsWith("Hello World!", "World!") == true

@test
def endsWith03(): Bool = String.endsWith("Hello World!", "WORLD!") == false

/////////////////////////////////////////////////////////////////////////////
// split                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def split01(): Bool & Impure =
    Array.toList(String.split("", "\s+")) == "" :: Nil

@test
def split02(): Bool & Impure =
    Array.length(String.split("1 3", "\s+")) == 2

/////////////////////////////////////////////////////////////////////////////
// toLowerCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def toLowerCase01(): Bool = String.toLowerCase("") == ""

@test
def toLowerCase02(): Bool = String.toLowerCase("Hello World!") == "hello world!"

@test
def toLowerCase03(): Bool = String.toLowerCase("â†’Hello Worldâˆ€!") == "â†’hello worldâˆ€!"

/////////////////////////////////////////////////////////////////////////////
// toUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def toUpperCase01(): Bool = String.toUpperCase("") == ""

@test
def toUpperCase02(): Bool = String.toUpperCase("Hello World!") == "HELLO WORLD!"

@test
def toUpperCase03(): Bool = String.toUpperCase("â†’Hello Worldâˆ€!") == "Â†’HELLO WORLDÂˆ€!"

/////////////////////////////////////////////////////////////////////////////
// toList                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def toList01(): Bool = String.toList("") == Nil

@test
def toList02(): Bool = String.toList("ABC") == 'A' :: 'B' :: 'C' :: Nil

@test
def toList03(): Bool = String.toList("1 2 3") == '1' :: ' ' :: '2' :: ' ' :: '3' :: Nil

/////////////////////////////////////////////////////////////////////////////
// trim                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def trim01(): Bool = "Hello World" == String.trim("Hello World")

@test
def trim02(): Bool = "abc" == String.trim(" abc")

@test
def trim03(): Bool = "abc" == String.trim("abc ")

@test
def trim04(): Bool = "abc" == String.trim(" abc ")

@test
def trim05(): Bool = "abc" == String.trim("  abc")

@test
def trim06(): Bool = "" == String.trim("  ")

/////////////////////////////////////////////////////////////////////////////
// lineSeparator                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def lineSeparator01(): Bool =
    let newline = String.lineSeparator();
    Char.isWhiteSpace(String.charAt(0, newline)) == true

@test
def lineSeparator02(): Bool =
    let newline = String.lineSeparator();
    String.length(newline) == 1 || String.length(newline) == 2

/////////////////////////////////////////////////////////////////////////////
// toArray                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def toArray01(): Bool & Impure = Array.sameElements(String.toArray(""), []: Array[Char])

@test
def toArray02(): Bool & Impure = Array.sameElements(String.toArray("ABC"), ['A', 'B', 'C'])

@test
def toArray03(): Bool & Impure = Array.sameElements(String.toArray("1 2 3"), ['1', ' ', '2', ' ', '3'])

/////////////////////////////////////////////////////////////////////////////
// init                                                                    //
/////////////////////////////////////////////////////////////////////////////
@test
def init01(): Bool = String.init(_ -> 'A', 0) == ""

@test
def init02(): Bool = String.init(_ -> 'A', -1) == ""

@test
def init03(): Bool = String.init(_ -> 'A', 1) == "A"

@test
def init04(): Bool = String.init(_ -> 'A', 2) == "AA"

@test
def init05(): Bool =
    let s = String.init(_ -> 'A', 50);
    String.length(s) == 50

/////////////////////////////////////////////////////////////////////////////
// flatten                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def flatten01(): Bool = String.flatten(Nil) == ""

@test
def flatten02(): Bool = String.flatten("" :: Nil) == ""

@test
def flatten03(): Bool = String.flatten("a" :: Nil) == "a"

@test
def flatten04(): Bool = String.flatten("ab" :: Nil) == "ab"

@test
def flatten05(): Bool = String.flatten("" :: "" :: Nil) == ""

@test
def flatten06(): Bool = String.flatten("a" :: "" :: Nil) == "a"

@test
def flatten07(): Bool = String.flatten("" :: "a" :: Nil) == "a"

@test
def flatten08(): Bool = String.flatten("a" :: "b" :: Nil) == "ab"

@test
def flatten09(): Bool = String.flatten("ab" :: "cde" :: Nil) == "abcde"

@test
def flatten10(): Bool = String.flatten("a" :: "bc" :: "d" :: Nil) == "abcd"

/////////////////////////////////////////////////////////////////////////////
// flattenWithSep                                                          //
/////////////////////////////////////////////////////////////////////////////
@test
def flattenWithSep01(): Bool = String.flattenWithSep("-", Nil) == ""

@test
def flattenWithSep02(): Bool = String.flattenWithSep("-", "" :: Nil) == ""

@test
def flattenWithSep03(): Bool = String.flattenWithSep("-", "a" :: Nil) == "a"

@test
def flattenWithSep04(): Bool = String.flattenWithSep("-", "ab" :: Nil) == "ab"

@test
def flattenWithSep05(): Bool = String.flattenWithSep("-", "" :: "" :: Nil) == "-"

@test
def flattenWithSep06(): Bool = String.flattenWithSep("-", "a" :: "" :: Nil) == "a-"

@test
def flattenWithSep07(): Bool = String.flattenWithSep("-", "" :: "a" :: Nil) == "-a"

@test
def flattenWithSep08(): Bool = String.flattenWithSep("-", "a" :: "b" :: Nil) == "a-b"

@test
def flattenWithSep09(): Bool = String.flattenWithSep("-", "ab" :: "cde" :: Nil) == "ab-cde"

@test
def flattenWithSep10(): Bool = String.flattenWithSep("-", "a" :: "bc" :: "d" :: Nil) == "a-bc-d"

@test
def flattenWithSep11(): Bool = String.flattenWithSep("", "a" :: "bc" :: "d" :: Nil) == "abcd"

@test
def flattenWithSep12(): Bool = String.flattenWithSep("<>", "a" :: "bc" :: "d" :: Nil) == "a<>bc<>d"

/////////////////////////////////////////////////////////////////////////////
// flattenWithSepChar                                                      //
/////////////////////////////////////////////////////////////////////////////
@test
def flattenWithSepChar01(): Bool = String.flattenWithSepChar('-', Nil) == ""

@test
def flattenWithSepChar02(): Bool = String.flattenWithSepChar('-', "" :: Nil) == ""

@test
def flattenWithSepChar03(): Bool = String.flattenWithSepChar('-', "a" :: Nil) == "a"

@test
def flattenWithSepChar04(): Bool = String.flattenWithSepChar('-', "ab" :: Nil) == "ab"

@test
def flattenWithSepChar05(): Bool = String.flattenWithSepChar('-', "" :: "" :: Nil) == "-"

@test
def flattenWithSepChar06(): Bool = String.flattenWithSepChar('-', "a" :: "" :: Nil) == "a-"

@test
def flattenWithSepChar07(): Bool = String.flattenWithSepChar('-', "" :: "a" :: Nil) == "-a"

@test
def flattenWithSepChar08(): Bool = String.flattenWithSepChar('-', "a" :: "b" :: Nil) == "a-b"

@test
def flattenWithSepChar09(): Bool = String.flattenWithSepChar('-', "ab" :: "cde" :: Nil) == "ab-cde"

@test
def flattenWithSepChar10(): Bool = String.flattenWithSepChar('-', "a" :: "bc" :: "d" :: Nil) == "a-bc-d"

@test
def flattenWithSepChar11(): Bool = String.flattenWithSepChar(' ', "a" :: "bc" :: "d" :: Nil) == "a bc d"

@test
def flattenWithSepChar12(): Bool = String.flattenWithSepChar('/', "a" :: "bc" :: "d" :: Nil) == "a/bc/d"

/////////////////////////////////////////////////////////////////////////////
// map                                                                     //
/////////////////////////////////////////////////////////////////////////////
@test
def map01(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "") == ""

@test
def map02(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "a") == "f"

@test
def map03(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "A") == "t"

@test
def map04(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "ab") == "ff"

@test
def map05(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "aB") == "ft"

@test
def map06(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "Ab") == "tf"

@test
def map07(): Bool = String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "AB") == "tt"

/////////////////////////////////////////////////////////////////////////////
// mapWithIndex                                                            //
/////////////////////////////////////////////////////////////////////////////
@test
def mapWithIndex01(): Bool =
    let test = (c,i) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "") == ""

@test
def mapWithIndex02(): Bool =
    let test = (c,i) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "a") == "f"

@test
def mapWithIndex03(): Bool =
    let test = (c,i) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "A") == "t"

@test
def mapWithIndex04(): Bool =
    let test = (c,i) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "aB") == "fT"

@test
def mapWithIndex05(): Bool =
    let test = (c,i) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "ab") == "fF"

@test
def mapWithIndex06(): Bool =
    let test = (c,i) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "AB") == "tT"

@test
def mapWithIndex07(): Bool =
    let test = (c,i) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
    String.mapWithIndex(test, "Ab") == "tF"

/////////////////////////////////////////////////////////////////////////////
// map2                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def map201(): Bool =
    let s = String.map2((a, b) -> if (Char.isLowerCase(a)) a else b, "", "");
    s == ""

@test
def map202(): Bool =
    let s = String.map2((a, b) -> if (Char.isLowerCase(a)) a else b, "a", "");
    s == ""

@test
def map203(): Bool =
    let s = String.map2((a, b) -> if (Char.isLowerCase(a)) a else b, "", "1");
    s == ""

@test
def map204(): Bool =
    let s = String.map2((a, b) -> if (Char.isLowerCase(a)) a else b, "A", "1");
    s == "1"

@test
def map205(): Bool =
    let s = String.map2((a, b) -> if (Char.isLowerCase(a)) a else b, "a", "1");
    s == "a"

@test
def map206(): Bool =
    let s = String.map2((a, b) -> if (Char.isLowerCase(a)) a else b, "_bc__fgh", "12345678");
    s == "1bc45fgh"

/////////////////////////////////////////////////////////////////////////////
// reverse                                                                 //
/////////////////////////////////////////////////////////////////////////////
@test
def reverse01(): Bool =
    String.reverse("") == ""

@test
def reverse02(): Bool =
    String.reverse("A") == "A"

@test
def reverse03(): Bool =
    String.reverse("Hello World") == "dlroW olleH"

@test
def reverse04(): Bool =
    String.reverse("Hello World!") == "!dlroW olleH"

@test
def reverse05(): Bool =
    String.reverse("Hello World!") != "!dlroW OLLEH"

/////////////////////////////////////////////////////////////////////////////
// rotateLeft                                                              //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateLeft01(): Bool = String.rotateLeft(0, "") == ""

@test
def rotateLeft02(): Bool = String.rotateLeft(1, "") == ""

@test
def rotateLeft03(): Bool = String.rotateLeft(0, "a") == "a"

@test
def rotateLeft04(): Bool = String.rotateLeft(0, "ab") == "ab"

@test
def rotateLeft05(): Bool = String.rotateLeft(1, "ab") == "ba"

@test
def rotateLeft06(): Bool = String.rotateLeft(2, "ab") == "ab"

@test
def rotateLeft07(): Bool = String.rotateLeft(3, "ab") == "ba"

@test
def rotateLeft08(): Bool = String.rotateLeft(-1, "ab") == "ba"

@test
def rotateLeft09(): Bool = String.rotateLeft(0, "abc") == "abc"

@test
def rotateLeft10(): Bool = String.rotateLeft(1, "abc") == "bca"

@test
def rotateLeft11(): Bool = String.rotateLeft(2, "abc") == "cab"

/////////////////////////////////////////////////////////////////////////////
// rotateRight                                                             //
/////////////////////////////////////////////////////////////////////////////
@test
def rotateRight01(): Bool = String.rotateRight(0, "") == ""

@test
def rotateRight02(): Bool = String.rotateRight(1, "") == ""

@test
def rotateRight03(): Bool = String.rotateRight(0, "a") == "a"

@test
def rotateRight04(): Bool = String.rotateRight(0, "ab") == "ab"

@test
def rotateRight05(): Bool = String.rotateRight(1, "ab") == "ba"

@test
def rotateRight06(): Bool = String.rotateRight(2, "ab") == "ab"

@test
def rotateRight07(): Bool = String.rotateRight(3, "ab") == "ba"

@test
def rotateRight08(): Bool = String.rotateRight(-1, "ab") == "ba"

@test
def rotateRight09(): Bool = String.rotateRight(0, "abc") == "abc"

@test
def rotateRight10(): Bool = String.rotateRight(1, "abc") == "cab"

@test
def rotateRight11(): Bool = String.rotateRight(2, "abc") == "bca"

/////////////////////////////////////////////////////////////////////////////
// isMatch                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def isMatch01(): Bool = String.isMatch(".*", "") == true

@test
def isMatch02(): Bool = String.isMatch(".+", "") == false

@test
def isMatch03(): Bool = String.isMatch("[a]*", "") == true

@test
def isMatch04(): Bool = String.isMatch("[a]+", "") == false

@test
def isMatch05(): Bool = String.isMatch("[a]*", "a") == true

@test
def isMatch06(): Bool = String.isMatch("[a]*", "aa") == true

@test
def isMatch07(): Bool = String.isMatch("[a]*", "aab") == false

@test
def isMatch08(): Bool = String.isMatch("[a]*b", "aab") == true

@test
def isSMatch09(): Bool = String.isMatch("[b]+", "aab") == false

@test
def isMatch10(): Bool = String.isMatch("[b]+", "bbbc") == false

@test
def isMatch11(): Bool = String.isMatch("[b]+", "aabbbc") == false

/////////////////////////////////////////////////////////////////////////////
// isSubmatch                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def isSubmatch01(): Bool = String.isSubmatch(".*", "") == true

@test
def isSubmatch02(): Bool = String.isSubmatch(".+", "") == false

@test
def isSubmatch03(): Bool = String.isSubmatch("[a]*", "") == true

@test
def isSubmatch04(): Bool = String.isSubmatch("[a]+", "") == false

@test
def isSubmatch05(): Bool = String.isSubmatch("[a]*", "a") == true

@test
def isSubmatch06(): Bool = String.isSubmatch("[a]*", "aa") == true

@test
def isSubmatch07(): Bool = String.isSubmatch("[a]*", "aab") == true

@test
def isSubmatch08(): Bool = String.isSubmatch("[a]*b", "aab") == true

@test
def isSubmatch09(): Bool = String.isSubmatch("[b]+", "aab") == true

@test
def isSubmatch10(): Bool = String.isSubmatch("[b]+", "bbbc") == true

@test
def isSubmatch11(): Bool = String.isSubmatch("[b]+", "aabbbc") == true

/////////////////////////////////////////////////////////////////////////////
// hashCode                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def hashCode01(): Bool = String.hashCode("") == 0

@test
def hashCode02(): Bool = String.hashCode("a") == String.hashCode("a")

@test
def hashCode03(): Bool = String.hashCode("ab") == String.hashCode("ab")

@test
def hashCode04(): Bool = String.hashCode("a") < String.hashCode("ab")

@test
def hashCode05(): Bool = String.hashCode("abc") == String.hashCode("abc")

@test
def hashCode06(): Bool = String.hashCode("ab") < String.hashCode("abc")

/////////////////////////////////////////////////////////////////////////////
// slice                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def slice01(): Bool = String.slice("", 0, 1) == ""

@test
def slice02(): Bool = String.slice("a", 0, 1) == "a"

@test
def slice03(): Bool = String.slice("a", 1, 2) == ""

@test
def slice04(): Bool = String.slice("a", 0, 2) == ""

@test
def slice05(): Bool = String.slice("a", -1, 0) == ""

@test
def slice06(): Bool = String.slice("ab", 0, 1) == "a"

@test
def slice07(): Bool = String.slice("ab", 1, 2) == "b"

@test
def slice08(): Bool = String.slice("ab", 0, 2) == "ab"

@test
def slice09(): Bool = String.slice("ab", 0, 3) == ""

@test
def slice10(): Bool = String.slice("ab", -1, 2) == ""

@test
def slice11(): Bool = String.slice("abc", 0, 1) == "a"

@test
def slice12(): Bool = String.slice("abc", 1, 2) == "b"

@test
def slice13(): Bool = String.slice("abc", 2, 3) == "c"

@test
def slice14(): Bool = String.slice("abc", 0, 2) == "ab"

@test
def slice15(): Bool = String.slice("abc", 1, 3) == "bc"

@test
def slice16(): Bool = String.slice("abc", 0, 3) == "abc"

/////////////////////////////////////////////////////////////////////////////
// sliceLeft                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def sliceLeft01(): Bool = String.sliceLeft("", 0) == ""

@test
def sliceLeft02(): Bool = String.sliceLeft("", 1) == ""

@test
def sliceLeft03(): Bool = String.sliceLeft("a", 1) == "a"

@test
def sliceLeft04(): Bool = String.sliceLeft("a", 0) == ""

@test
def sliceLeft05(): Bool = String.sliceLeft("a", 2) == ""

@test
def sliceLeft06(): Bool = String.sliceLeft("ab", 1) == "a"

@test
def sliceLeft07(): Bool = String.sliceLeft("ab", 2) == "ab"

@test
def sliceLeft08(): Bool = String.sliceLeft("ab", 0) == ""

@test
def sliceLeft09(): Bool = String.sliceLeft("ab", 3) == ""

@test
def sliceLeft10(): Bool = String.sliceLeft("ab", -1) == ""

@test
def sliceLeft11(): Bool = String.sliceLeft("abc", 1) == "a"

@test
def sliceLeft12(): Bool = String.sliceLeft("abc", 2) == "ab"

@test
def sliceLeft13(): Bool = String.sliceLeft("abc", 3) == "abc"

@test
def sliceLeft14(): Bool = String.sliceLeft("abc", 0) == ""

@test
def sliceLeft15(): Bool = String.sliceLeft("abc", 4) == ""

@test
def sliceLeft16(): Bool = String.sliceLeft("abc", -1) == ""

/////////////////////////////////////////////////////////////////////////////
// sliceRight                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def sliceRight01(): Bool = String.sliceRight("", 0) == ""

@test
def sliceRight02(): Bool = String.sliceRight("", 1) == ""

@test
def sliceRight03(): Bool = String.sliceRight("a", 1) == ""

@test
def sliceRight04(): Bool = String.sliceRight("a", 0) == "a"

@test
def sliceRight05(): Bool = String.sliceRight("a", 1) == ""

@test
def sliceRight06(): Bool = String.sliceRight("ab", 0) == "ab"

@test
def sliceRight07(): Bool = String.sliceRight("ab", 1) == "b"

@test
def sliceRight08(): Bool = String.sliceRight("ab", -1) == ""

@test
def sliceRight09(): Bool = String.sliceRight("ab", 2) == ""

@test
def sliceRight10(): Bool = String.sliceRight("abc", 0) == "abc"

@test
def sliceRight11(): Bool = String.sliceRight("abc", 1) == "bc"

@test
def sliceRight12(): Bool = String.sliceRight("abc", 2) == "c"

@test
def sliceRight13(): Bool = String.sliceRight("abc", 3) == ""

@test
def sliceRight14(): Bool = String.sliceRight("abc", -1) == ""

/////////////////////////////////////////////////////////////////////////////
// findIndexOfLeft                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def findIndexOfLeft01(): Bool = String.findIndexOfLeft(Char.isUpperCase, "") == None

@test
def findIndexOfLeft02(): Bool = String.findIndexOfLeft(Char.isUpperCase, "a") == None

@test
def findIndexOfLeft03(): Bool = String.findIndexOfLeft(Char.isUpperCase, "A") == Some(0)

@test
def findIndexOfLeft04(): Bool = String.findIndexOfLeft(Char.isUpperCase, "ab") == None

@test
def findIndexOfLeft05(): Bool = String.findIndexOfLeft(Char.isUpperCase, "Ab") == Some(0)

@test
def findIndexOfLeft06(): Bool = String.findIndexOfLeft(Char.isUpperCase, "aB") == Some(1)

@test
def findIndexOfLeft07(): Bool = String.findIndexOfLeft(Char.isUpperCase, "AB") == Some(0)

/////////////////////////////////////////////////////////////////////////////
// findIndexOfRight                                                        //
/////////////////////////////////////////////////////////////////////////////

@test
def findIndexOfRight01(): Bool = String.findIndexOfRight(Char.isUpperCase, "") == None

@test
def findIndexOfRight02(): Bool = String.findIndexOfRight(Char.isUpperCase, "a") == None

@test
def findIndexOfRight03(): Bool = String.findIndexOfRight(Char.isUpperCase, "A") == Some(0)

@test
def findIndexOfRight04(): Bool = String.findIndexOfRight(Char.isUpperCase, "ab") == None

@test
def findIndexOfRight05(): Bool = String.findIndexOfRight(Char.isUpperCase, "Ab") == Some(0)

@test
def findIndexOfRight06(): Bool = String.findIndexOfRight(Char.isUpperCase, "aB") == Some(1)

@test
def findIndexOfRight07(): Bool = String.findIndexOfRight(Char.isUpperCase, "AB") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// findIndices                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def findIndices01(): Bool & Impure =
    let a = String.findIndices(Char.isDigit, "");
    Array.sameElements(a, []: Array[Int32])

@test
def findIndices02(): Bool & Impure =
    let a = String.findIndices(Char.isDigit, "a");
    Array.sameElements(a, []: Array[Int32])

@test
def findIndices03(): Bool & Impure =
    let a = String.findIndices(Char.isDigit, "1");
    Array.sameElements(a, [0])

@test
def findIndices04(): Bool & Impure =
    let a = String.findIndices(Char.isDigit, "ab");
    Array.sameElements(a, [] : Array[Int32])

@test
def findIndices05(): Bool & Impure =
    let a = String.findIndices(Char.isDigit, "1b");
    Array.sameElements(a, [0])

@test
def findIndices06(): Bool & Impure =
    let a = String.findIndices(Char.isDigit, "a2");
    Array.sameElements(a, [1])

@test
def findIndices07(): Bool & Impure =
    let a = String.findIndices(Char.isDigit, "12");
    Array.sameElements(a, [0,1])

/////////////////////////////////////////////////////////////////////////////
// indices                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def indices01(): Bool & Impure =
    let a = String.indices('a', "b");
    Array.sameElements(a, []: Array[Int32])

@test
def indices02(): Bool & Impure =
    let a = String.indices('a', "b");
    Array.sameElements(a, []: Array[Int32])

@test
def indices03(): Bool & Impure =
    let a = String.indices('a', "a");
    Array.sameElements(a, [0])

@test
def indices04(): Bool & Impure =
    let a = String.indices('a', "bc");
    Array.sameElements(a, []: Array[Int32])

@test
def indices05(): Bool & Impure =
    let a = String.indices('a', "ab");
    Array.sameElements(a, [0])

@test
def indices06(): Bool & Impure =
    let a = String.indices('b', "ab");
    Array.sameElements(a, [1])

@test
def indices07(): Bool & Impure =
    let a = String.indices('a', "aa");
    Array.sameElements(a, [0,1])

/////////////////////////////////////////////////////////////////////////////
// take                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def take01(): Bool = String.take(-1, "") == ""

@test
def take02(): Bool = String.take(0, "") == ""

@test
def take03(): Bool = String.take(1, "") == ""

@test
def take04(): Bool = String.take(-1, "a") == ""

@test
def take05(): Bool = String.take(0, "a") == ""

@test
def take06(): Bool = String.take(1, "a") == "a"

@test
def take07(): Bool = String.take(2, "a") == "a"

@test
def take08(): Bool = String.take(0, "ab") == ""

@test
def take09(): Bool = String.take(1, "ab") == "a"

@test
def take10(): Bool = String.take(2, "ab") == "ab"

@test
def take11(): Bool = String.take(2, "abcdef") == "ab"

@test
def take12(): Bool = String.take(4, "abcdef") == "abcd"

@test
def take13(): Bool = String.take(10, "abcdef") == "abcdef"

/////////////////////////////////////////////////////////////////////////////
// takeLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def takeLeft01(): Bool = String.takeLeft(-1, "") == ""

@test
def takeLeft02(): Bool = String.takeLeft(0, "") == ""

@test
def takeLeft03(): Bool = String.takeLeft(1, "") == ""

@test
def takeLeft04(): Bool = String.takeLeft(-1, "a") == ""

@test
def takeLeft05(): Bool = String.takeLeft(0, "a") == ""

@test
def takeLeft06(): Bool = String.takeLeft(1, "a") == "a"

@test
def takeLeft07(): Bool = String.takeLeft(2, "a") == "a"

@test
def takeLeft08(): Bool = String.takeLeft(0, "ab") == ""

@test
def takeLeft09(): Bool = String.takeLeft(1, "ab") == "a"

@test
def takeLeft10(): Bool = String.takeLeft(2, "ab") == "ab"

@test
def takeLeft11(): Bool = String.takeLeft(2, "abcdef") == "ab"

@test
def takeLeft12(): Bool = String.takeLeft(4, "abcdef") == "abcd"

@test
def takeLeft13(): Bool = String.takeLeft(10, "abcdef") == "abcdef"

/////////////////////////////////////////////////////////////////////////////
// takeRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def takeRight01(): Bool = String.takeRight(-1, "") == ""

@test
def takeRight02(): Bool = String.takeRight(0, "") == ""

@test
def takeRight03(): Bool = String.takeRight(1, "") == ""

@test
def takeRight04(): Bool = String.takeRight(-1, "a") == ""

@test
def takeRight05(): Bool = String.takeRight(0, "a") == ""

@test
def takeRight06(): Bool = String.takeRight(1, "a") == "a"

@test
def takeRight07(): Bool = String.takeRight(2, "a") == "a"

@test
def takeRight08(): Bool = String.takeRight(0, "ab") == ""

@test
def takeRight09(): Bool = String.takeRight(1, "ab") == "b"

@test
def takeRight10(): Bool = String.takeRight(2, "ab") == "ab"

@test
def takeRight11(): Bool = String.takeRight(2, "abcdef") == "ef"

@test
def takeRight12(): Bool = String.takeRight(4, "abcdef") == "cdef"

@test
def takeRight13(): Bool = String.takeRight(10, "abcdef") == "abcdef"

/////////////////////////////////////////////////////////////////////////////
// drop                                                                    //
/////////////////////////////////////////////////////////////////////////////

@test
def drop01(): Bool = String.drop(-1, "") == ""

@test
def drop02(): Bool = String.drop(0, "") == ""

@test
def drop03(): Bool = String.drop(1, "") == ""

@test
def drop04(): Bool = String.drop(-1, "a") == "a"

@test
def drop05(): Bool = String.drop(0, "a") == "a"

@test
def drop06(): Bool = String.drop(1, "a") == ""

@test
def drop07(): Bool = String.drop(2, "a") == ""

@test
def drop08(): Bool = String.drop(0, "ab") == "ab"

@test
def drop09(): Bool = String.drop(1, "ab") == "b"

@test
def drop10(): Bool = String.drop(2, "ab") == ""

@test
def drop11(): Bool = String.drop(2, "abcdef") == "cdef"

@test
def drop12(): Bool = String.drop(4, "abcdef") == "ef"

/////////////////////////////////////////////////////////////////////////////
// dropLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def dropLeft01(): Bool = String.dropLeft(-1, "") == ""

@test
def dropLeft02(): Bool = String.dropLeft(0, "") == ""

@test
def dropLeft03(): Bool = String.dropLeft(1, "") == ""

@test
def dropLeft04(): Bool = String.dropLeft(-1, "a") == "a"

@test
def dropLeft05(): Bool = String.dropLeft(0, "a") == "a"

@test
def dropLeft06(): Bool = String.dropLeft(1, "a") == ""

@test
def dropLeft07(): Bool = String.dropLeft(2, "a") == ""

@test
def dropLeft08(): Bool = String.dropLeft(0, "ab") == "ab"

@test
def dropLeft09(): Bool = String.dropLeft(1, "ab") == "b"

@test
def dropLeft10(): Bool = String.dropLeft(2, "ab") == ""

@test
def dropLeft11(): Bool = String.dropLeft(2, "abcdef") == "cdef"

@test
def dropLeft12(): Bool = String.dropLeft(4, "abcdef") == "ef"

/////////////////////////////////////////////////////////////////////////////
// dropRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def dropRight01(): Bool = String.dropRight(-1, "") == ""

@test
def dropRight02(): Bool = String.dropRight(0, "") == ""

@test
def dropRight03(): Bool = String.dropRight(1, "") == ""

@test
def dropRight04(): Bool = String.dropRight(-1, "a") == "a"

@test
def dropRight05(): Bool = String.dropRight(0, "a") == "a"

@test
def dropRight06(): Bool = String.dropRight(1, "a") == ""

@test
def dropRight07(): Bool = String.dropRight(2, "a") == ""

@test
def dropRight08(): Bool = String.dropRight(0, "ab") == "ab"

@test
def dropRight09(): Bool = String.dropRight(1, "ab") == "a"

@test
def dropRight10(): Bool = String.dropRight(2, "ab") == ""

@test
def dropRight11(): Bool = String.dropRight(2, "abcdef") == "abcd"

@test
def dropRight12(): Bool = String.dropRight(4, "abcdef") == "ab"

/////////////////////////////////////////////////////////////////////////////
// takeWhile                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def takeWhile01(): Bool = String.takeWhile(Char.isUpperCase, "") == ""

@test
def takeWhile02(): Bool = String.takeWhile(Char.isUpperCase, "a") == ""

@test
def takeWhile03(): Bool = String.takeWhile(Char.isUpperCase, "A") == "A"

@test
def takeWhile04(): Bool = String.takeWhile(Char.isUpperCase, "ab") == ""

@test
def takeWhile05(): Bool = String.takeWhile(Char.isUpperCase, "aB") == ""

@test
def takeWhile06(): Bool = String.takeWhile(Char.isUpperCase, "Ab") == "A"

@test
def takeWhile07(): Bool = String.takeWhile(Char.isUpperCase, "AB") == "AB"

@test
def takeWhile08(): Bool = String.takeWhile(Char.isUpperCase, "ABcDefgHIjK") == "AB"

/////////////////////////////////////////////////////////////////////////////
// takeWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def takeWhileLeft01(): Bool = String.takeWhileLeft(Char.isUpperCase, "") == ""

@test
def takeWhileLeft02(): Bool = String.takeWhileLeft(Char.isUpperCase, "a") == ""

@test
def takeWhileLeft03(): Bool = String.takeWhileLeft(Char.isUpperCase, "A") == "A"

@test
def takeWhileLeft04(): Bool = String.takeWhileLeft(Char.isUpperCase, "ab") == ""

@test
def takeWhileLeft05(): Bool = String.takeWhileLeft(Char.isUpperCase, "aB") == ""

@test
def takeWhileLeft06(): Bool = String.takeWhileLeft(Char.isUpperCase, "Ab") == "A"

@test
def takeWhileLeft07(): Bool = String.takeWhileLeft(Char.isUpperCase, "AB") == "AB"

@test
def takeWhileLeft08(): Bool = String.takeWhileLeft(Char.isUpperCase, "ABcDefgHIjK") == "AB"

/////////////////////////////////////////////////////////////////////////////
// takeWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def takeWhileRight01(): Bool = String.takeWhileRight(Char.isUpperCase, "") == ""

@test
def takeWhileRight02(): Bool = String.takeWhileRight(Char.isUpperCase, "a") == ""

@test
def takeWhileRight03(): Bool = String.takeWhileRight(Char.isUpperCase, "A") == "A"

@test
def takeWhileRight04(): Bool = String.takeWhileRight(Char.isUpperCase, "ab") == ""

@test
def takeWhileRight05(): Bool = String.takeWhileRight(Char.isUpperCase, "aB") == "B"

@test
def takeWhileRight06(): Bool = String.takeWhileRight(Char.isUpperCase, "Ab") == ""

@test
def takeWhileRight07(): Bool = String.takeWhileRight(Char.isUpperCase, "AB") == "AB"

@test
def takeWhileRight08(): Bool = String.takeWhileRight(Char.isUpperCase, "ABcDefgHIjK") == "K"

/////////////////////////////////////////////////////////////////////////////
// dropWhile                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhile01(): Bool = String.dropWhile(Char.isUpperCase, "") == ""

@test
def dropWhile02(): Bool = String.dropWhile(Char.isUpperCase, "a") == "a"

@test
def dropWhile03(): Bool = String.dropWhile(Char.isUpperCase, "A") == ""

@test
def dropWhile04(): Bool = String.dropWhile(Char.isUpperCase, "ab") == "ab"

@test
def dropWhile05(): Bool = String.dropWhile(Char.isUpperCase, "aB") == "aB"

@test
def dropWhile06(): Bool = String.dropWhile(Char.isUpperCase, "Ab") == "b"

@test
def dropWhile07(): Bool = String.dropWhile(Char.isUpperCase, "AB") == ""

@test
def dropWhile08(): Bool = String.dropWhile(Char.isUpperCase, "ABcDefgHIjK") == "cDefgHIjK"

/////////////////////////////////////////////////////////////////////////////
// dropWhileLeft                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhileLeft01(): Bool = String.dropWhileLeft(Char.isUpperCase, "") == ""

@test
def dropWhileLeft02(): Bool = String.dropWhileLeft(Char.isUpperCase, "a") == "a"

@test
def dropWhileLeft03(): Bool = String.dropWhileLeft(Char.isUpperCase, "A") == ""

@test
def dropWhileLeft04(): Bool = String.dropWhileLeft(Char.isUpperCase, "ab") == "ab"

@test
def dropWhileLeft05(): Bool = String.dropWhileLeft(Char.isUpperCase, "aB") == "aB"

@test
def dropWhileLeft06(): Bool = String.dropWhileLeft(Char.isUpperCase, "Ab") == "b"

@test
def dropWhileLeft07(): Bool = String.dropWhileLeft(Char.isUpperCase, "AB") == ""

@test
def dropWhileLeft08(): Bool = String.dropWhileLeft(Char.isUpperCase, "ABcDefgHIjK") == "cDefgHIjK"

/////////////////////////////////////////////////////////////////////////////
// dropWhileRight                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def dropWhileRight01(): Bool = String.dropWhileRight(Char.isUpperCase, "") == ""

@test
def dropWhileRight02(): Bool = String.dropWhileRight(Char.isUpperCase, "a") == "a"

@test
def dropWhileRight03(): Bool = String.dropWhileRight(Char.isUpperCase, "A") == ""

@test
def dropWhileRight04(): Bool = String.dropWhileRight(Char.isUpperCase, "ab") == "ab"

@test
def dropWhileRight05(): Bool = String.dropWhileRight(Char.isUpperCase, "aB") == "a"

@test
def dropWhileRight06(): Bool = String.dropWhileRight(Char.isUpperCase, "Ab") == "Ab"

@test
def dropWhileRight07(): Bool = String.dropWhileRight(Char.isUpperCase, "AB") == ""

@test
def dropWhileRight08(): Bool = String.dropWhileRight(Char.isUpperCase, "ABcDefgHIjK") == "ABcDefgHIj"

/////////////////////////////////////////////////////////////////////////////
// splitAt                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def splitAt01(): Bool = String.splitAt(0, "") == ("", "")

@test
def splitAt02(): Bool = String.splitAt(1, "") == ("", "")

@test
def splitAt03(): Bool = String.splitAt(-1, "") == ("", "")

@test
def splitAt04(): Bool = String.splitAt(0, "a") == ("", "a")

@test
def splitAt05(): Bool = String.splitAt(1, "a") == ("a", "")

@test
def splitAt06(): Bool = String.splitAt(-1, "a") == ("", "a")

@test
def splitAt07(): Bool = String.splitAt(0, "ab") == ("", "ab")

@test
def splitAt08(): Bool = String.splitAt(1, "ab") == ("a", "b")

@test
def splitAt09(): Bool = String.splitAt(2, "ab") == ("ab", "")

@test
def splitAt10(): Bool = String.splitAt(-1, "ab") == ("", "ab")

@test
def splitAt11(): Bool = String.splitAt(3, "ab") == ("ab", "")

/////////////////////////////////////////////////////////////////////////////
// foldLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def foldLeft01(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "", "")  == ""

@test
def foldLeft02(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "a") == "a"

@test
def foldLeft03(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "ab") == "ab"

@test
def foldLeft04(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abc") == "abc"

@test
def foldLeft05(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abcd") == "abcd"

@test
def foldLeft06(): Bool = String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abcde") == "abcde"

/////////////////////////////////////////////////////////////////////////////
// foldRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def foldRight01(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "", "")  == ""

@test
def foldRight02(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "", "a") == "a"

@test
def foldRight03(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "", "ab") == "ab"

@test
def foldRight04(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abc") == "abc"

@test
def foldRight05(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abcd") == "abcd"

@test
def foldRight06(): Bool = String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abcde") == "abcde"

/////////////////////////////////////////////////////////////////////////////
// foldLeft2                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def foldLeft201(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "", "") == ""

@test
def foldLeft202(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "a", "_") == "a"

@test
def foldLeft203(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "a", "A") == "A"

@test
def foldLeft204(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "__") == "ab"

@test
def foldLeft205(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "A_") == "Ab"

@test
def foldLeft206(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "AB") == "AB"

@test
def foldLeft207(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "") == ""

@test
def foldLeft208(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "_") == "a"

@test
def foldLeft209(): Bool =
    String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "A") == "A"

/////////////////////////////////////////////////////////////////////////////
// foldRight2                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def foldRight201(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "", "") == ""

@test
def foldRight202(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "a", "_") == "a"

@test
def foldRight203(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "a", "A") == "A"

@test
def foldRight204(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "__") == "ab"

@test
def foldRight205(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "A_") == "Ab"

@test
def foldRight206(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "AB") == "AB"

@test
def foldRight207(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "") == ""

@test
def foldRight208(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "_") == "b"

@test
def foldRight209(): Bool =
    String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "B") == "B"

/////////////////////////////////////////////////////////////////////////////
// unfold                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def unfold01(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 0) == "aaaaa"

@test
def unfold02(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 1) == "aaaa"

@test
def unfold03(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 2) == "aaa"

@test
def unfold04(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 3) == "aa"

@test
def unfold05(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 4) == "a"

@test
def unfold06(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 5) == ""

@test
def unfold07(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, 6) == ""

@test
def unfold08(): Bool =
    String.unfold(x -> if (x < 5) Some('a', x+1) else None, -1) == "aaaaaa"

/////////////////////////////////////////////////////////////////////////////
// exists                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def exists01(): Bool = String.exists(Char.isUpperCase, "") == false

@test
def exists02(): Bool = String.exists(Char.isUpperCase, "a") == false

@test
def exists03(): Bool = String.exists(Char.isUpperCase, "A") == true

@test
def exists04(): Bool = String.exists(Char.isUpperCase, "ab") == false

@test
def exists05(): Bool = String.exists(Char.isUpperCase, "aB") == true

@test
def exists06(): Bool = String.exists(Char.isUpperCase, "Ab") == true

@test
def exists07(): Bool = String.exists(Char.isUpperCase, "AB") == true

@test
def exists08(): Bool = String.exists(Char.isUpperCase, "a*c") == false

@test
def exists09(): Bool = String.exists(Char.isUpperCase, "aBc") == true

/////////////////////////////////////////////////////////////////////////////
// forall                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def forall01(): Bool = String.forall(Char.isUpperCase, "") == true

@test
def forall02(): Bool = String.forall(Char.isUpperCase, "a") == false

@test
def forall03(): Bool = String.forall(Char.isUpperCase, "A") == true

@test
def forall04(): Bool = String.forall(Char.isUpperCase, "a") == false

@test
def forall05(): Bool = String.forall(Char.isUpperCase, "aB") == false

@test
def forall06(): Bool = String.forall(Char.isUpperCase, "Ab") == false

@test
def forall07(): Bool = String.forall(Char.isUpperCase, "AB") == true

@test
def forall08(): Bool = String.forall(Char.isUpperCase, "a*b") == false

@test
def forall09(): Bool = String.forall(Char.isUpperCase, "aBc") == false

@test
def forall10(): Bool = String.forall(Char.isUpperCase, "ABC") == true

/////////////////////////////////////////////////////////////////////////////
// isAscii                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def isAscii01(): Bool = String.isAscii("") == true

@test
def isAscii02(): Bool = String.isAscii("a") == true

@test
def isAscii03(): Bool = String.isAscii("A") == true

@test
def isAscii04(): Bool = String.isAscii("0") == true

@test
def isAscii05(): Bool = String.isAscii("~") == true

@test
def isAscii06(): Bool = String.isAscii("Â") == false

@test
def isAscii07(): Bool = String.isAscii("†") == false

@test
def isAscii08(): Bool = String.isAscii("ab") == true

@test
def isAscii09(): Bool = String.isAscii("a†") == false

@test
def isAscii10(): Bool = String.isAscii("†b") == false

/////////////////////////////////////////////////////////////////////////////
// isWhiteSpace                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def isWhiteSpace01(): Bool = String.isWhiteSpace("") == true

@test
def isWhiteSpace02(): Bool = String.isWhiteSpace(" ") == true

@test
def isWhiteSpace03(): Bool = String.isWhiteSpace("A") == false

@test
def isWhiteSpace04(): Bool = String.isWhiteSpace("0") == false

@test
def isWhiteSpace05(): Bool = String.isWhiteSpace("~") == false

@test
def isWhiteSpace06(): Bool = String.isWhiteSpace(String.lineSeparator()) == true

@test
def isWhiteSpace07(): Bool = String.isWhiteSpace("  ") == true

/////////////////////////////////////////////////////////////////////////////
// trimLeft                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def trimLeft01(): Bool = String.trimLeft("") == ""

@test
def trimLeft02(): Bool = String.trimLeft(" ") == ""

@test
def trimLeft03(): Bool = String.trimLeft("a") == "a"

@test
def trimLeft04(): Bool = String.trimLeft(" a") == "a"

@test
def trimLeft05(): Bool = String.trimLeft("a ") == "a "

@test
def trimLeft06(): Bool = String.trimLeft("aa") == "aa"

@test
def trimLeft07(): Bool = String.trimLeft("  aa") == "aa"

@test
def trimLeft08(): Bool = String.trimLeft("aa  ") == "aa  "

@test
def trimLeft09(): Bool = String.trimLeft(" aa ") == "aa "

@test
def trimLeft10(): Bool = String.trimLeft("  aa  ") == "aa  "

@test
def trimLeft11(): Bool = String.trimLeft(" a a ") == "a a "

@test
def trimLeft12(): Bool = String.trimLeft(String.lineSeparator() + "aa") == "aa"


/////////////////////////////////////////////////////////////////////////////
// trimRight                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def trimRight01(): Bool = String.trimRight("") == ""

@test
def trimRight02(): Bool = String.trimRight(" ") == ""

@test
def trimRight03(): Bool = String.trimRight("a") == "a"

@test
def trimRight04(): Bool = String.trimRight(" a") == " a"

@test
def trimRight05(): Bool = String.trimRight("a ") == "a"

@test
def trimRight06(): Bool = String.trimRight("aa") == "aa"

@test
def trimRight07(): Bool = String.trimRight("  aa") == "  aa"

@test
def trimRight08(): Bool = String.trimRight("aa  ") == "aa"

@test
def trimRight09(): Bool = String.trimRight(" aa ") == " aa"

@test
def trimRight10(): Bool = String.trimRight("  aa  ") == "  aa"

@test
def trimRight11(): Bool = String.trimRight(" a a ") == " a a"

@test
def trimRight12(): Bool = String.trimRight("aa" + String.lineSeparator()) == "aa"

/////////////////////////////////////////////////////////////////////////////
// repeat                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def repeat01(): Bool = String.repeat("a", -1) == ""

@test
def repeat02(): Bool = String.repeat("a", 0) == ""

@test
def repeat03(): Bool = String.repeat("a", 1) == "a"

@test
def repeat04(): Bool = String.repeat("a", 2) == "aa"

@test
def repeat05(): Bool = String.repeat("a", 3) == "aaa"

@test
def repeat06(): Bool = String.repeat("ab", 0) == ""

@test
def repeat07(): Bool = String.repeat("ab", 1) == "ab"

@test
def repeat08(): Bool = String.repeat("ab", 2) == "abab"

@test
def repeat09(): Bool = String.repeat("ab", 3) == "ababab"

/////////////////////////////////////////////////////////////////////////////
// padLeft                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def padLeft01(): Bool = String.padLeft(0, ' ', "") == ""

@test
def padLeft02(): Bool = String.padLeft(1, ' ', "") == " "

@test
def padLeft03(): Bool = String.padLeft(2, ' ', "") == "  "

@test
def padLeft04(): Bool = String.padLeft(3, ' ', "") == "   "

@test
def padLeft05(): Bool = String.padLeft(0, ' ', "a") == "a"

@test
def padLeft06(): Bool = String.padLeft(1, ' ', "a") == "a"

@test
def padLeft07(): Bool = String.padLeft(2, ' ', "a") == " a"

@test
def padLeft08(): Bool = String.padLeft(3, ' ', "a") == "  a"

@test
def padLeft09(): Bool = String.padLeft(0, ' ', "ab") == "ab"

@test
def padLeft10(): Bool = String.padLeft(1, ' ', "ab") == "ab"

@test
def padLeft11(): Bool = String.padLeft(2, ' ', "ab") == "ab"

@test
def padLeft12(): Bool = String.padLeft(3, ' ', "ab") == " ab"

@test
def padLeft13(): Bool = String.padLeft(3, '_', "a") == "__a"

@test
def padLeft14(): Bool = String.padLeft(3, '_', "ab") == "_ab"

@test
def padLeft15(): Bool = String.padLeft(3, '_', "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// padRight                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def padRight01(): Bool = String.padRight(0, ' ', "") == ""

@test
def padRight02(): Bool = String.padRight(1, ' ', "") == " "

@test
def padRight03(): Bool = String.padRight(2, ' ', "") == "  "

@test
def padRight04(): Bool = String.padRight(3, ' ', "") == "   "

@test
def padRight05(): Bool = String.padRight(0, ' ', "a") == "a"

@test
def padRight06(): Bool = String.padRight(1, ' ', "a") == "a"

@test
def padRight07(): Bool = String.padRight(2, ' ', "a") == "a "

@test
def padRight08(): Bool = String.padRight(3, ' ', "a") == "a  "

@test
def padRight09(): Bool = String.padRight(0, ' ', "ab") == "ab"

@test
def padRight10(): Bool = String.padRight(1, ' ', "ab") == "ab"

@test
def padRight11(): Bool = String.padRight(2, ' ', "ab") == "ab"

@test
def padRight12(): Bool = String.padRight(3, ' ', "ab") == "ab "

@test
def padRight13(): Bool = String.padRight(3, '_', "a") == "a__"

@test
def padRight14(): Bool = String.padRight(3, '_', "ab") == "ab_"

@test
def padRight15(): Bool = String.padRight(3, '_', "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// replace                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def replace01(): Bool = String.replace("c", "d", "") == ""

@test
def replace02(): Bool = String.replace("c", "d", "a") == "a"

@test
def replace03(): Bool = String.replace("c", "d", "c") == "d"

@test
def replace04(): Bool = String.replace("c", "d", "d") == "d"

@test
def replace05(): Bool = String.replace("c", "d", "ab") == "ab"

@test
def replace06(): Bool = String.replace("c", "d", "abc") == "abd"

@test
def replace07(): Bool = String.replace("c", "d", "cd") == "dd"

@test
def replace08(): Bool = String.replace("c", "d", "cc") == "dd"

/////////////////////////////////////////////////////////////////////////////
// replaceChar                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def replaceChar01(): Bool = String.replaceChar('c', 'd', "") == ""

@test
def replaceChar02(): Bool = String.replaceChar('c', 'd', "a") == "a"

@test
def replaceChar03(): Bool = String.replaceChar('c', 'd', "c") == "d"

@test
def replaceChar04(): Bool = String.replaceChar('c', 'd', "d") == "d"

@test
def replaceChar05(): Bool = String.replaceChar('c', 'd', "ab") == "ab"

@test
def replaceChar06(): Bool = String.replaceChar('c', 'd', "abc") == "abd"

@test
def replaceChar07(): Bool = String.replaceChar('c', 'd', "cd") == "dd"

@test
def replaceChar08(): Bool = String.replaceChar('c', 'd', "cc") == "dd"

/////////////////////////////////////////////////////////////////////////////
// replaceMatches                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def replaceMatches01(): Bool = String.replaceMatches("[c]{1}", "d", "") == ""

@test
def replaceMatches02(): Bool = String.replaceMatches("[c]{1}", "d", "a") == "a"

@test
def replaceMatches03(): Bool = String.replaceMatches("[c]{1}", "d", "c") == "d"

@test
def replaceMatches04(): Bool = String.replaceMatches("[c]{1}", "d", "d") == "d"

@test
def replaceMatches05(): Bool = String.replaceMatches("[c]{1}", "d", "ab") == "ab"

@test
def replaceMatches06(): Bool = String.replaceMatches("[c]{1}", "d", "abc") == "abd"

@test
def replaceMatches07(): Bool = String.replaceMatches("[c]{1}", "d", "cd") == "dd"

@test
def replaceMatches08(): Bool = String.replaceMatches("[c]{1}", "d", "cc") == "dd"

/////////////////////////////////////////////////////////////////////////////
// replaceFirstMatch                                                       //
/////////////////////////////////////////////////////////////////////////////

@test
def replaceFirstMatch01(): Bool = String.replaceFirstMatch("[c]{1}", "d", "") == ""

@test
def replaceFirstMatch02(): Bool = String.replaceFirstMatch("[c]{1}", "d", "a") == "a"

@test
def replaceFirstMatch03(): Bool = String.replaceFirstMatch("[c]{1}", "d", "c") == "d"

@test
def replaceFirstMatch04(): Bool = String.replaceFirstMatch("[c]{1}", "d", "d") == "d"

@test
def replaceFirstMatch05(): Bool = String.replaceFirstMatch("[c]{1}", "d", "ab") == "ab"

@test
def replaceFirstMatch06(): Bool = String.replaceFirstMatch("[c]{1}", "d", "abc") == "abd"

@test
def replaceFirstMatch07(): Bool = String.replaceFirstMatch("[c]{1}", "d", "cd") == "dd"

@test
def replaceFirstMatch08(): Bool = String.replaceFirstMatch("[c]{1}", "d", "cc") == "dc"

/////////////////////////////////////////////////////////////////////////////
// update                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def update01(): Bool = String.update(0, 'b', "") == ""

@test
def update02(): Bool = String.update(-1, 'b', "a") == "a"

@test
def update03(): Bool = String.update(0, 'b', "a") == "b"

@test
def update04(): Bool = String.update(1, 'b', "a") == "a"

@test
def update05(): Bool = String.update(0, 'e', "ab") == "eb"

@test
def update06(): Bool = String.update(1, 'e', "ab") == "ae"

@test
def update07(): Bool = String.update(2, 'e', "ab") == "ab"

/////////////////////////////////////////////////////////////////////////////
// patch                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def patch01(): Bool = String.patch(0, 0, "", "") == ""

@test
def patch02(): Bool = String.patch(0, 2, "ab", "") == ""

@test
def patch03(): Bool = String.patch(0, 2, "", "ab") == "ab"

@test
def patch04(): Bool = String.patch(-3, 3, "abd", "ab") == "ab"

@test
def patch05(): Bool = String.patch(2, 3, "abd", "ab") == "ab"

@test
def patch06(): Bool = String.patch(0, 0, "", "a") == "a"

@test
def patch07(): Bool = String.patch(1, 0, "b", "a") == "a"

@test
def patch08(): Bool = String.patch(0, 1, "b", "a") == "b"

@test
def patch09(): Bool = String.patch(0, 2, "bd", "a") == "b"

@test
def patch10(): Bool = String.patch(-1, 2, "bd", "a") == "d"

@test
def patch11(): Bool = String.patch(-1, 2, "cd", "ab") == "db"

@test
def patch12(): Bool = String.patch(1, 2, "cd", "ab") == "ac"

@test
def patch13(): Bool = String.patch(-2, 2, "cd", "ab") == "ab"

@test
def patch14(): Bool = String.patch(2, 2, "cd", "ab") == "ab"

@test
def patch15(): Bool = String.patch(1, 1, "c", "ab") == "ac"

@test
def patch16(): Bool = String.patch(0, 2, "cd", "ab") == "cd"

@test
def patch17(): Bool = String.patch(0, 1, "d", "abc") == "dbc"

@test
def patch18(): Bool = String.patch(1, 1, "d", "abc") == "adc"

@test
def patch19(): Bool = String.patch(2, 1, "d", "abc") == "abd"

@test
def patch20(): Bool = String.patch(0, 2, "de", "abc") == "dec"

@test
def patch21(): Bool = String.patch(1, 2, "de", "abc") == "ade"

@test
def patch22(): Bool = String.patch(0, 2, "def", "abc") == "dec"

@test
def patch23(): Bool = String.patch(0, 3, "def", "abc") == "def"

@test
def patch24(): Bool = String.patch(2, 4, "nopq", "abcdefg") == "abnopqg"

@test
def patch25(): Bool = String.patch(-2, 4, "nopq", "abcdefg") == "pqcdefg"

@test
def patch26(): Bool = String.patch(4, 5, "nopq", "abcdefg") == "abcdnop"

@test
def patch27(): Bool = String.patch(4, 2, "nopq", "abcdefg") == "abcdnog"

@test
def patch28(): Bool = String.patch(-1, 10, "ABCDEFGH", "abcdefg") == "BCDEFGH"

/////////////////////////////////////////////////////////////////////////////
// indent                                                                  //
/////////////////////////////////////////////////////////////////////////////

@test
def indent01(): Bool =
    String.indent(3, "") == ""

@test
def indent02(): Bool & Impure =
    String.indent(3, String.unlines(["abc"])) == String.unlines(["   abc"])

@test
def indent03(): Bool & Impure =
    String.indent(3, String.unlines(["abc", "def"])) == String.unlines(["   abc", "   def"])

@test
def indent04(): Bool & Impure =
    String.indent(3, String.unlines(["ab", "cd", "ef"])) == String.unlines(["   ab", "   cd", "   ef"])

@test
def indent05(): Bool & Impure =
    String.indent(1, String.unlines(["abc"])) == String.unlines([" abc"])

@test
def indent06(): Bool & Impure =
    String.indent(1, String.unlines(["abc", "def"])) == String.unlines([" abc", " def"])

@test
def indent07(): Bool & Impure =
    String.indent(1, String.unlines(["ab", "cd", "ef"])) == String.unlines([" ab", " cd", " ef"])

@test
def indent08(): Bool & Impure =
    String.indent(0, String.unlines(["abc"])) == String.unlines(["abc"])

@test
def indent09(): Bool & Impure =
    String.indent(0, String.unlines(["abc", "def"])) == String.unlines(["abc", "def"])

@test
def indent10(): Bool & Impure =
    String.indent(0, String.unlines(["ab", "cd", "ef"])) == String.unlines(["ab", "cd", "ef"])

/////////////////////////////////////////////////////////////////////////////
// stripIndent                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def stripIndent01(): Bool =
    String.stripIndent(3, "") == ""

@test
def stripIndent02(): Bool & Impure =
    String.stripIndent(3, String.unlines(["   abc"])) == String.unlines(["abc"])

@test
def stripIndent03(): Bool & Impure =
    String.stripIndent(3, String.unlines(["   abc", "   def"])) == String.unlines(["abc", "def"])

@test
def stripIndent04(): Bool & Impure =
    String.stripIndent(3, String.unlines(["   ab", "   cd", "   ef"])) == String.unlines(["ab", "cd", "ef"])

@test
def stripIndent05(): Bool & Impure =
    String.stripIndent(1, String.unlines(["   abc"])) == String.unlines(["  abc"])

@test
def stripIndent06(): Bool & Impure =
    String.stripIndent(1, String.unlines(["   abc", "   def"])) == String.unlines(["  abc", "  def"])

@test
def stripIndent07(): Bool & Impure =
    String.stripIndent(1, String.unlines(["   ab", "   cd", "   ef"])) == String.unlines(["  ab", "  cd", "  ef"])

@test
def stripIndent08(): Bool & Impure =
    String.stripIndent(0, String.unlines(["   abc"])) == String.unlines(["   abc"])

@test
def stripIndent09(): Bool & Impure =
    String.stripIndent(0, String.unlines(["   abc", "   def"])) == String.unlines(["   abc", "   def"])

@test
def stripIndent10(): Bool & Impure =
    String.stripIndent(0, String.unlines(["   ab", "   cd", "   ef"])) == String.unlines(["   ab", "   cd", "   ef"])

/////////////////////////////////////////////////////////////////////////////
// lines                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def lines01(): Bool & Impure =
    let a = String.lines("");
    Array.sameElements(a, []: Array[String])

@test
def lines02(): Bool & Impure =
    let a = String.lines("a");
    Array.sameElements(a, ["a"])

@test
def lines03(): Bool & Impure =
    let a = String.lines("a" + String.lineSeparator());
    Array.sameElements(a, ["a"])

@test
def lines04(): Bool & Impure =
    let a = String.lines(String.lineSeparator() + "a");
    Array.sameElements(a, ["", "a"])

@test
def lines05(): Bool & Impure =
    let a = String.lines("a" + String.lineSeparator() + "b");
    Array.sameElements(a, ["a", "b"])

@test
def lines06(): Bool & Impure =
    let a = String.lines("a" + Char.toString('\n') + "b");
    Array.sameElements(a, ["a", "b"])

@test
def lines07(): Bool & Impure =
    let a = String.lines("a" + Char.toString('\r') + Char.toString('\n') + "b");
    Array.sameElements(a, ["a", "b"])

/////////////////////////////////////////////////////////////////////////////
// unlines                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def unlines01(): Bool & Impure = String.unlines([]) == ""

@test
def unlines02(): Bool & Impure =
    String.unlines([""]) == String.lineSeparator()

@test
def unlines03(): Bool & Impure =
    String.unlines(["a"]) == "a" + String.lineSeparator()

@test
def unlines04(): Bool & Impure =
    String.unlines(["a", ""]) == "a" + String.lineSeparator() + String.lineSeparator()

@test
def unlines05(): Bool & Impure =
    String.unlines(["", "b"]) == String.lineSeparator() + "b" + String.lineSeparator()

@test
def unlines06(): Bool & Impure =
    String.unlines(["a", "b"]) == "a" + String.lineSeparator() + "b" + String.lineSeparator()

@test
def unlines07(): Bool & Impure =
    String.unlines(["a", String.lineSeparator()]) == "a" + String.lineSeparator() + String.lineSeparator() + String.lineSeparator()

@test
def unlines08(): Bool & Impure =
    String.unlines([String.lineSeparator(), "b"]) ==  String.lineSeparator() + String.lineSeparator() + "b" + String.lineSeparator()

/////////////////////////////////////////////////////////////////////////////
// words                                                                   //
/////////////////////////////////////////////////////////////////////////////

@test
def words01(): Bool & Impure =
    let a = String.words("");
    Array.sameElements(a, []: Array[String])

@test
def words02(): Bool & Impure =
    let a = String.words("a");
    Array.sameElements(a, ["a"])

@test
def words03(): Bool & Impure =
    let a = String.words("a ");
    Array.sameElements(a, ["a"])

@test
def words04(): Bool & Impure =
    let a = String.words(" a");
    Array.sameElements(a, ["a"])

@test
def words05(): Bool & Impure =
    let a = String.words("a b");
    Array.sameElements(a, ["a", "b"])

@test
def words06(): Bool & Impure =
    let a = String.words("a  b");
    Array.sameElements(a, ["a", "b"])

@test
def words07(): Bool & Impure =
    let a = String.words("a" + Char.toString('\t') + "b");
    Array.sameElements(a, ["a", "b"])

/////////////////////////////////////////////////////////////////////////////
// unwords                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def unwords01(): Bool & Impure = String.unwords([]) == ""

@test
def unwords02(): Bool & Impure = String.unwords([""]) == ""

@test
def unwords03(): Bool & Impure = String.unwords(["a"]) == "a"

@test
def unwords04(): Bool & Impure = String.unwords(["a", ""]) == "a "

@test
def unwords05(): Bool & Impure = String.unwords(["", "b"]) == " b"

@test
def unwords06(): Bool & Impure = String.unwords(["a", "b"]) == "a b"

@test
def unwords07(): Bool & Impure = String.unwords(["a", String.lineSeparator()]) == "a " + String.lineSeparator()

@test
def unwords08(): Bool & Impure = String.unwords([String.lineSeparator(), "b"]) ==  String.lineSeparator() + " b"

/////////////////////////////////////////////////////////////////////////////
// indexOf                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOf01(): Bool = String.indexOf("a", "") == None

@test
def indexOf02(): Bool = String.indexOf("a", "A") == None

@test
def indexOf03(): Bool = String.indexOf("a", "a") == Some(0)

@test
def indexOf04(): Bool = String.indexOf("a", "AB") == None

@test
def indexOf05(): Bool = String.indexOf("a", "ab") == Some(0)

@test
def indexOf06(): Bool = String.indexOf("b", "ab") == Some(1)

@test
def indexOf07(): Bool = String.indexOf("a", "aa") == Some(0)

@test
def indexOf08(): Bool = String.indexOf("ab", "ab") == Some(0)

@test
def indexOf09(): Bool = String.indexOf("bc", "abc") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// indexOfLeft                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOfLeft01(): Bool = String.indexOfLeft("a", "") == None

@test
def indexOfLeft02(): Bool = String.indexOfLeft("a", "A") == None

@test
def indexOfLeft03(): Bool = String.indexOfLeft("a", "a") == Some(0)

@test
def indexOfLeft04(): Bool = String.indexOfLeft("a", "AB") == None

@test
def indexOfLeft05(): Bool = String.indexOfLeft("a", "ab") == Some(0)

@test
def indexOfLeft06(): Bool = String.indexOfLeft("b", "ab") == Some(1)

@test
def indexOfLeft07(): Bool = String.indexOfLeft("a", "aa") == Some(0)

@test
def indexOfLeft08(): Bool = String.indexOfLeft("ab", "ab") == Some(0)

@test
def indexOfLeft09(): Bool = String.indexOfLeft("bc", "abc") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// indexOfRight                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def indexOfRight01(): Bool = String.indexOfRight("a", "") == None

@test
def indexOfRight02(): Bool = String.indexOfRight("a", "A") == None

@test
def indexOfRight03(): Bool = String.indexOfRight("a", "a") == Some(0)

@test
def indexOfRight04(): Bool = String.indexOfRight("a", "AB") == None

@test
def indexOfRight05(): Bool = String.indexOfRight("a", "ab") == Some(0)

@test
def indexOfRight06(): Bool = String.indexOfRight("b", "ab") == Some(1)

@test
def indexOfRight07(): Bool = String.indexOfRight("a", "aa") == Some(1)

@test
def indexOfRight08(): Bool = String.indexOfRight("ab", "ab") == Some(0)

@test
def indexOfRight09(): Bool = String.indexOfRight("bc", "abc") == Some(1)

/////////////////////////////////////////////////////////////////////////////
// isSubstringOf                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def isSubstringOf01(): Bool = String.isSubstringOf("", "") == true

@test
def isSubstringOf02(): Bool = String.isSubstringOf("", "a") == true

@test
def isSubstringOf03(): Bool = String.isSubstringOf("", "abc") == true

@test
def isSubstringOf04(): Bool = String.isSubstringOf("a", "") == false

@test
def isSubstringOf05(): Bool = String.isSubstringOf("a", "a") == true

@test
def isSubstringOf06(): Bool = String.isSubstringOf("a", "b") == false

@test
def isSubstringOf07(): Bool = String.isSubstringOf("a", "aa") == true

@test
def isSubstringOf08(): Bool = String.isSubstringOf("ab", "ab") == true

@test
def isSubstringOf09(): Bool = String.isSubstringOf("bc", "abc") == true

/////////////////////////////////////////////////////////////////////////////
// commonPrefix                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def commonPrefix01(): Bool =
    String.commonPrefix("", "") == ""

@test
def commonPrefix02(): Bool =
    String.commonPrefix("a", "") == ""

@test
def commonPrefix03(): Bool =
    String.commonPrefix("", "a") == ""

@test
def commonPrefix04(): Bool =
    String.commonPrefix("a", "a") == "a"

@test
def commonPrefix05(): Bool =
    String.commonPrefix("a", "b") == ""

@test
def commonPrefix06(): Bool =
    String.commonPrefix("ab", "bc") == ""

@test
def commonPrefix07(): Bool =
    String.commonPrefix("ab", "ac") == "a"

@test
def commonPrefix08(): Bool =
    String.commonPrefix("ab", "ab") == "ab"

@test
def commonPrefix09(): Bool =
    String.commonPrefix("abc", "ABC") == ""

@test
def commonPrefix10(): Bool =
    String.commonPrefix("abc", "abcd") == "abc"

@test
def commonPrefix11(): Bool =
    String.commonPrefix("abcd", "abc") == "abc"

/////////////////////////////////////////////////////////////////////////////
// commonSuffix                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def commonSuffix01(): Bool =
    String.commonSuffix("", "") == ""

@test
def commonSuffix02(): Bool =
    String.commonSuffix("a", "") == ""

@test
def commonSuffix03(): Bool =
    String.commonSuffix("", "a") == ""

@test
def commonSuffix04(): Bool =
    String.commonSuffix("a", "a") == "a"

@test
def commonSuffix05(): Bool =
    String.commonSuffix("a", "b") == ""

@test
def commonSuffix06(): Bool =
    String.commonSuffix("ab", "bc") == ""

@test
def commonSuffix07(): Bool =
    String.commonSuffix("ab", "_b") == "b"

@test
def commonSuffix08(): Bool =
    String.commonSuffix("ab", "ab") == "ab"

@test
def commonSuffix09(): Bool =
    String.commonSuffix("abc", "ABC") == ""

@test
def commonSuffix10(): Bool =
    String.commonSuffix("bcd", "abcd") == "bcd"

@test
def commonSuffix11(): Bool =
    String.commonSuffix("abcd", "bcd") == "bcd"

/////////////////////////////////////////////////////////////////////////////
// abbreviateLeft                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def abbreviateLeft01(): Bool =
    String.abbreviateLeft(0, "") == ""

@test
def abbreviateLeft02(): Bool =
    String.abbreviateLeft(0, "a") == ""

@test
def abbreviateLeft03(): Bool =
    String.abbreviateLeft(0, "abcde") == ""

@test
def abbreviateLeft04(): Bool =
    String.abbreviateLeft(1, "") == ""

@test
def abbreviateLeft05(): Bool =
    String.abbreviateLeft(1, "a") == ""

@test
def abbreviateLeft06(): Bool =
    String.abbreviateLeft(1, "abcde") == ""

@test
def abbreviateLeft07(): Bool =
    String.abbreviateLeft(3, "") == ""

@test
def abbreviateLeft08(): Bool =
    String.abbreviateLeft(3, "a") == "a"

@test
def abbreviateLeft09(): Bool =
    String.abbreviateLeft(3, "abcde") == "..."

@test
def abbreviateLeft10(): Bool =
    String.abbreviateLeft(4, "") == ""

@test
def abbreviateLeft11(): Bool =
    String.abbreviateLeft(4, "a") == "a"

@test
def abbreviateLeft12(): Bool =
    String.abbreviateLeft(4, "abcde") == "...e"

@test
def abbreviateLeft13(): Bool =
    String.abbreviateLeft(5, "") == ""

@test
def abbreviateLeft14(): Bool =
    String.abbreviateLeft(5, "a") == "a"

@test
def abbreviateLeft15(): Bool =
    String.abbreviateLeft(5, "abcde") == "...de"


/////////////////////////////////////////////////////////////////////////////
// abbreviateRight                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def abbreviateRight01(): Bool =
    String.abbreviateRight(0, "") == ""

@test
def abbreviateRight02(): Bool =
    String.abbreviateRight(0, "a") == ""

@test
def abbreviateRight03(): Bool =
    String.abbreviateRight(0, "abcde") == ""

@test
def abbreviateRight04(): Bool =
    String.abbreviateRight(1, "") == ""

@test
def abbreviateRight05(): Bool =
    String.abbreviateRight(1, "a") == ""

@test
def abbreviateRight06(): Bool =
    String.abbreviateRight(1, "abcde") == ""

@test
def abbreviateRight07(): Bool =
    String.abbreviateRight(3, "") == ""

@test
def abbreviateRight08(): Bool =
    String.abbreviateRight(3, "a") == "a"

@test
def abbreviateRight09(): Bool =
    String.abbreviateRight(3, "abcde") == "..."

@test
def abbreviateRight10(): Bool =
    String.abbreviateRight(4, "") == ""

@test
def abbreviateRight11(): Bool =
    String.abbreviateRight(4, "a") == "a"

@test
def abbreviateRight12(): Bool =
    String.abbreviateRight(4, "abcde") == "a..."

@test
def abbreviateRight13(): Bool =
    String.abbreviateRight(5, "") == ""

@test
def abbreviateRight14(): Bool =
    String.abbreviateRight(5, "a") == "a"

@test
def abbreviateRight15(): Bool =
    String.abbreviateRight(5, "abcde") == "ab..."

/////////////////////////////////////////////////////////////////////////////
// levenshteinDistance                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def levenshteinDistance01(): Bool =
    String.levenshteinDistance("", "") == 0

@test
def levenshteinDistance02(): Bool =
    String.levenshteinDistance("A", "") == 1

@test
def levenshteinDistance03(): Bool =
    String.levenshteinDistance("", "A") == 1

@test
def levenshteinDistance04(): Bool =
    String.levenshteinDistance("A", "A") == 0

@test
def levenshteinDistance05(): Bool =
    String.levenshteinDistance("kitten", "sitting") == 3

@test
def levenshteinDistance06(): Bool =
    String.levenshteinDistance("Saturday", "Sunday") == 3

@test
def levenshteinDistance07(): Bool =
    String.levenshteinDistance("Gumbo", "Gambol") == 2


/////////////////////////////////////////////////////////////////////////////
// equalsCI                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def equalsCI01(): Bool =
    String.equalsCI("", "") == true

@test
def equalsCI02(): Bool =
    String.equalsCI("Hello World", "") == false

@test
def equalsCI03(): Bool =
    String.equalsCI("", "Hello World") == false

@test
def equalsCI04(): Bool =
    String.equalsCI("Hello World", "Hello World") == true

@test
def equalsCI05(): Bool =
    String.equalsCI("Hello World", "HELLO WORLD") == true

@test
def equalsCI06(): Bool =
    String.equalsCI("Hello World", "hello world") == true

@test
def equalsCI07(): Bool =
    String.equalsCI("Hello World", "hELLO wORLD") == true

@test
def equalsCI08(): Bool =
    String.equalsCI("hello world", "HELLO WORLD") == true

@test
def equalsCI09(): Bool =
    String.equalsCI("hello world", "HELLO WORLD!") == false

/////////////////////////////////////////////////////////////////////////////
// zip                                                                     //
/////////////////////////////////////////////////////////////////////////////

@test
def zip01(): Bool & Impure =
    let a = String.zip("", "");
    Array.sameElements(a, []: Array[(Char,Char)])

@test
def zip02(): Bool & Impure =
    let a = String.zip("a", "");
    Array.sameElements(a, []: Array[(Char, Char)])

@test
def zip03(): Bool & Impure =
    let a = String.zip("", "1");
    Array.sameElements(a, []: Array[(Char, Char)])

@test
def zip04(): Bool & Impure =
    let a = String.zip("a", "1");
    Array.sameElements(a, [('a', '1')])

@test
def zip05(): Bool & Impure =
    let a = String.zip("ab", "12");
    Array.sameElements(a, [('a', '1'), ('b', '2')])

@test
def zip06(): Bool & Impure =
    let a = String.zip("abc", "123");
    Array.sameElements(a, [('a', '1'), ('b', '2'), ('c', '3')])

@test
def zip07(): Bool & Impure =
    let a = String.zip("abcd", "1234");
    Array.sameElements(a, [('a', '1'), ('b', '2'), ('c', '3'), ('d', '4')])

/////////////////////////////////////////////////////////////////////////////
// zipWith                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def zipWith01(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "", "");
    s == ""

@test
def zipWith02(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "a", "");
    s == ""

@test
def zipWith03(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "", "1");
    s == ""

@test
def zipWith04(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_", "1");
    s == "1"

@test
def zipWith05(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "a", "1");
    s == "a"

@test
def zipWith06(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fgh", "12345678");
    s == "1bc45fgh"

@test
def zipWith07(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fghijkl", "12345678");
    s == "1bc45fgh"

@test
def zipWith08(): Bool =
    let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fgh", "1234567890");
    s == "1bc45fgh"

}