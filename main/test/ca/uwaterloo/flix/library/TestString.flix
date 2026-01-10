mod TestString {

    use Assert.{assertEq, assertTrue, assertFalse, assertSome, assertNone};

    /////////////////////////////////////////////////////////////////////////////
    // charAt                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def charAt01(): Unit \ Assert =
        assertEq(expected = 'H', String.charAt(0, "Hello World!"))

    @Test
    def charAt02(): Unit \ Assert =
        assertEq(expected = 'e', String.charAt(1, "Hello World!"))

    @Test
    def charAt03(): Unit \ Assert =
        assertEq(expected = 'l', String.charAt(2, "Hello World!"))

    @Test
    def charAt04(): Unit \ Assert =
        assertEq(expected = 'l', String.charAt(3, "Hello World!"))

    @Test
    def charAt05(): Unit \ Assert =
        assertEq(expected = 'o', String.charAt(4, "Hello World!"))

    @Test
    def charAt06(): Unit \ Assert =
        assertEq(expected = ' ', String.charAt(5, "Hello World!"))

    /////////////////////////////////////////////////////////////////////////////
    // nth                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nth01(): Unit \ Assert =
        assertNone(String.nth(0, ""))

    @Test
    def nth02(): Unit \ Assert =
        assertNone(String.nth(-1, "Hello World!"))

    @Test
    def nth03(): Unit \ Assert =
        assertNone(String.nth(15, "Hello World!"))

    @Test
    def nth04(): Unit \ Assert =
        assertEq(expected = Some('H'), String.nth(0, "Hello World!"))

    @Test
    def nth05(): Unit \ Assert =
        assertEq(expected = Some('e'), String.nth(1, "Hello World!"))

    @Test
    def nth06(): Unit \ Assert =
        assertEq(expected = Some('l'), String.nth(2, "Hello World!"))

    @Test
    def nth07(): Unit \ Assert =
        assertEq(expected = Some('l'), String.nth(3, "Hello World!"))

    @Test
    def nth08(): Unit \ Assert =
        assertEq(expected = Some('o'), String.nth(4, "Hello World!"))

    @Test
    def nth09(): Unit \ Assert =
        assertEq(expected = Some(' '), String.nth(5, "Hello World!"))

    @Test
    def nth10(): Unit \ Assert =
        assertEq(expected = Some('!'), String.nth(11, "Hello World!"))

    /////////////////////////////////////////////////////////////////////////////
    // codePointAt                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def codePointAt01(): Unit \ Assert =
        assertEq(expected = 0x1f3dd, String.codePointAt(0, CodePoint.toString(0x1f3dd)))

    @Test
    def codePointAt02(): Unit \ Assert =
        assertEq(expected = 0x002E, String.codePointAt(2, "\uD83C\uDFDD."))

    @Test
    def codePointAt03(): Unit \ Assert =
        assertEq(expected = 0x1f3dd, String.codePointAt(1, ".\uD83C\uDFDD"))

    /////////////////////////////////////////////////////////////////////////////
    // nthCodePoint                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nthCodePoint01(): Unit \ Assert =
        assertNone(String.nthCodePoint(0, ""))

    @Test
    def nthCodePoint02(): Unit \ Assert =
        assertEq(expected = Some(0x1f3dd), String.nthCodePoint(0, CodePoint.toString(0x1f3dd)))

    @Test
    def nthCodePoint03(): Unit \ Assert =
        assertNone(String.nthCodePoint(2, CodePoint.toString(0x1f3dd)))

    @Test
    def nthCodePoint04(): Unit \ Assert =
        assertEq(expected = Some(0x002E), String.nthCodePoint(2, "\uD83C\uDFDD."))

    @Test
    def nthCodePoint05(): Unit \ Assert =
        assertEq(expected = Some(0x1f3dd), String.nthCodePoint(1, ".\uD83C\uDFDD"))

    /////////////////////////////////////////////////////////////////////////////
    // concat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def concat01(): Unit \ Assert =
        assertEq(expected = "Hello World!", "Hello World!")

    @Test
    def concat02(): Unit \ Assert =
        assertEq(expected = "Hello World!", "Hello" + " World!")

    @Test
    def concat03(): Unit \ Assert =
        assertEq(expected = "Hello" + " World!", "Hello" + " World!")

    @Test
    def concat04(): Unit \ Assert =
        assertEq(expected = "Hello World!", "Hello World!" + "")

    @Test
    def concat05(): Unit \ Assert =
        assertEq(expected = "Hello World!", "Hello" + " " + "World" + "!")

    @Test
    def concat06(): Unit \ Assert =
        let f = x -> (x + " World!");
        assertEq(expected = "Hello World!", f("Hello"))

    @Test
    def concat07(): Unit \ Assert =
        let f = (x, y) -> (x + y);
        assertEq(expected = "Hello World!", f("Hello", " World!"))

    @Test
    def concat08(): Unit \ Assert =
        let f = flip((x, y) -> (x + y));
        assertEq(expected = "Hello World!", f(" World!", "Hello"))

    /////////////////////////////////////////////////////////////////////////////
    // isEmpty                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isEmpty01(): Unit \ Assert =
        assertTrue(String.isEmpty(""))

    @Test
    def isEmpty02(): Unit \ Assert =
        assertFalse(String.isEmpty("Hello"))

    /////////////////////////////////////////////////////////////////////////////
    // nonEmpty                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def nonEmpty01(): Unit \ Assert =
        assertFalse(String.nonEmpty(""))

    @Test
    def nonEmpty02(): Unit \ Assert =
        assertTrue(String.nonEmpty("Hello"))

    /////////////////////////////////////////////////////////////////////////////
    // length                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def length01(): Unit \ Assert =
        assertEq(expected = 0, String.length(""))

    @Test
    def length02(): Unit \ Assert =
        assertEq(expected = 10, String.length("0123456789"))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def size01(): Unit \ Assert =
        assertEq(expected = 0, String.size(""))

    @Test
    def size02(): Unit \ Assert =
        assertEq(expected = 10, String.size("0123456789"))

    /////////////////////////////////////////////////////////////////////////////
    // split                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def split01(): Unit \ Assert =
       assertEq(expected = "" :: Nil, String.split({regex = "\\s+"}, ""))

    @Test
    def split02(): Unit \ Assert =
        assertEq(expected = 2, List.length(String.split({regex = "\\s+"}, "1 3")))

    /////////////////////////////////////////////////////////////////////////////
    // toLowerCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toLowerCase01(): Unit \ Assert =
        assertEq(expected = "", String.toLowerCase(""))

    @Test
    def toLowerCase02(): Unit \ Assert =
        assertEq(expected = "hello world!", String.toLowerCase("Hello World!"))

    @Test
    def toLowerCase03(): Unit \ Assert =
        assertEq(expected = "â†'hello worldâˆ€!", String.toLowerCase("â†'Hello Worldâˆ€!"))

    /////////////////////////////////////////////////////////////////////////////
    // toUpperCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toUpperCase01(): Unit \ Assert =
        assertEq(expected = "", String.toUpperCase(""))

    @Test
    def toUpperCase02(): Unit \ Assert =
        assertEq(expected = "HELLO WORLD!", String.toUpperCase("Hello World!"))

    @Test
    def toUpperCase03(): Unit \ Assert =
        assertEq(expected = "Â†'HELLO WORLDÂˆ€!", String.toUpperCase("â†'Hello Worldâˆ€!"))

    /////////////////////////////////////////////////////////////////////////////
    // toList                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toList01(): Unit \ Assert =
        assertEq(expected = Nil, String.toList(""))

    @Test
    def toList02(): Unit \ Assert =
        assertEq(expected = 'A' :: 'B' :: 'C' :: Nil, String.toList("ABC"))

    @Test
    def toList03(): Unit \ Assert =
        assertEq(expected = '1' :: ' ' :: '2' :: ' ' :: '3' :: Nil, String.toList("1 2 3"))

    /////////////////////////////////////////////////////////////////////////////
    // trim                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def trim01(): Unit \ Assert =
        assertEq(expected = "Hello World", String.trim("Hello World"))

    @Test
    def trim02(): Unit \ Assert =
        assertEq(expected = "abc", String.trim(" abc"))

    @Test
    def trim03(): Unit \ Assert =
        assertEq(expected = "abc", String.trim("abc "))

    @Test
    def trim04(): Unit \ Assert =
        assertEq(expected = "abc", String.trim(" abc "))

    @Test
    def trim05(): Unit \ Assert =
        assertEq(expected = "abc", String.trim("  abc"))

    @Test
    def trim06(): Unit \ Assert =
        assertEq(expected = "", String.trim("  "))

    /////////////////////////////////////////////////////////////////////////////
    // lineSeparator                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def lineSeparator01(): Unit \ Assert =
        let newline = String.lineSeparator();
        assertTrue(Char.isWhitespace(String.charAt(0, newline)))

    @Test
    def lineSeparator02(): Unit \ Assert =
        let newline = String.lineSeparator();
        assertTrue(String.length(newline) == 1 or String.length(newline) == 2)

    /////////////////////////////////////////////////////////////////////////////
    // toArray                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toArray01(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(String.toArray(rc, ""), Array#{} @ rc))
    }

    @Test
    def toArray02(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(String.toArray(rc, "ABC"), Array#{'A', 'B', 'C'} @ rc))
    }

    @Test
    def toArray03(): Unit \ Assert = region rc {
        assertTrue(Array.sameElements(String.toArray(rc, "1 2 3"), Array#{'1', ' ', '2', ' ', '3'} @ rc))
    }

    /////////////////////////////////////////////////////////////////////////////
    // toVector                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toVector01(): Unit \ Assert =
        assertEq(expected = Vector#{}, String.toVector(""))

    @Test
    def toVector02(): Unit \ Assert =
        assertEq(expected = Vector#{'A', 'B', 'C'}, String.toVector("ABC"))

    @Test
    def toVector03(): Unit \ Assert =
        assertEq(expected = Vector#{'1', ' ', '2', ' ', '3'}, String.toVector("1 2 3"))

    /////////////////////////////////////////////////////////////////////////////
    // toRegex                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toRegex01(): Unit \ Assert =
        assertTrue(String.toRegex("\\p{Alpha}+") |> Result.isOk)

    @Test
    def toRegex02(): Unit \ Assert =
        assertTrue(String.toRegex("\\p{Alpha+") |> Result.isErr)

    /////////////////////////////////////////////////////////////////////////////
    // toRegexWithFlags                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def compileWithFlags01(): Unit \ Assert =
        use Regex.Flag.{CaseInsenstive};
        assertTrue(String.toRegexWithFlags(Set#{CaseInsenstive}, "\\p{Alpha}+") |> Result.isOk)

    @Test
    def compileWithFlags02(): Unit \ Assert =
        use Regex.Flag.{CaseInsenstive};
        assertTrue(String.toRegexWithFlags(Set#{CaseInsenstive}, "\\p{Alpha+") |> Result.isErr)


    @Test
    def compileWithFlags03(): Unit \ Assert =
        use Regex.Flag.{CaseInsenstive, Multiline};
        assertTrue(String.toRegexWithFlags(Set#{CaseInsenstive, Multiline}, "\\p{Alpha}+") |> Result.isOk)

    /////////////////////////////////////////////////////////////////////////////
    // init                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def init01(): Unit \ Assert =
        assertEq(expected = "", String.init(_ -> 'A', 0))

    @Test
    def init02(): Unit \ Assert =
        assertEq(expected = "", String.init(_ -> 'A', -1))

    @Test
    def init03(): Unit \ Assert =
        assertEq(expected = "A", String.init(_ -> 'A', 1))

    @Test
    def init04(): Unit \ Assert =
        assertEq(expected = "AA", String.init(_ -> 'A', 2))

    @Test
    def init05(): Unit \ Assert =
        let s = String.init(_ -> 'A', 50);
        assertEq(expected = 50, String.length(s))

    /////////////////////////////////////////////////////////////////////////////
    // flatten                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def flatten01(): Unit \ Assert =
        assertEq(expected = "", String.flatten(Nil))

    @Test
    def flatten02(): Unit \ Assert =
        assertEq(expected = "", String.flatten("" :: Nil))

    @Test
    def flatten03(): Unit \ Assert =
        assertEq(expected = "a", String.flatten("a" :: Nil))

    @Test
    def flatten04(): Unit \ Assert =
        assertEq(expected = "ab", String.flatten("ab" :: Nil))

    @Test
    def flatten05(): Unit \ Assert =
        assertEq(expected = "", String.flatten("" :: "" :: Nil))

    @Test
    def flatten06(): Unit \ Assert =
        assertEq(expected = "a", String.flatten("a" :: "" :: Nil))

    @Test
    def flatten07(): Unit \ Assert =
        assertEq(expected = "a", String.flatten("" :: "a" :: Nil))

    @Test
    def flatten08(): Unit \ Assert =
        assertEq(expected = "ab", String.flatten("a" :: "b" :: Nil))

    @Test
    def flatten09(): Unit \ Assert =
        assertEq(expected = "abcde", String.flatten("ab" :: "cde" :: Nil))

    @Test
    def flatten10(): Unit \ Assert =
        assertEq(expected = "abcd", String.flatten("a" :: "bc" :: "d" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // intercalate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalate01(): Unit \ Assert =
        assertEq(expected = "", String.intercalate("-", Nil))

    @Test
    def intercalate02(): Unit \ Assert =
        assertEq(expected = "", String.intercalate("-", "" :: Nil))

    @Test
    def intercalate03(): Unit \ Assert =
        assertEq(expected = "a", String.intercalate("-", "a" :: Nil))

    @Test
    def intercalate04(): Unit \ Assert =
        assertEq(expected = "ab", String.intercalate("-", "ab" :: Nil))

    @Test
    def intercalate05(): Unit \ Assert =
        assertEq(expected = "-", String.intercalate("-", "" :: "" :: Nil))

    @Test
    def intercalate06(): Unit \ Assert =
        assertEq(expected = "a-", String.intercalate("-", "a" :: "" :: Nil))

    @Test
    def intercalate07(): Unit \ Assert =
        assertEq(expected = "-a", String.intercalate("-", "" :: "a" :: Nil))

    @Test
    def intercalate08(): Unit \ Assert =
        assertEq(expected = "a-b", String.intercalate("-", "a" :: "b" :: Nil))

    @Test
    def intercalate09(): Unit \ Assert =
        assertEq(expected = "ab-cde", String.intercalate("-", "ab" :: "cde" :: Nil))

    @Test
    def intercalate10(): Unit \ Assert =
        assertEq(expected = "a-bc-d", String.intercalate("-", "a" :: "bc" :: "d" :: Nil))

    @Test
    def intercalate11(): Unit \ Assert =
        assertEq(expected = "abcd", String.intercalate("", "a" :: "bc" :: "d" :: Nil))

    @Test
    def intercalate12(): Unit \ Assert =
        assertEq(expected = "a<>bc<>d", String.intercalate("<>", "a" :: "bc" :: "d" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // intercalateChar                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def intercalateChar01(): Unit \ Assert =
        assertEq(expected = "", String.intercalateChar('-', Nil))

    @Test
    def intercalateChar02(): Unit \ Assert =
        assertEq(expected = "", String.intercalateChar('-', "" :: Nil))

    @Test
    def intercalateChar03(): Unit \ Assert =
        assertEq(expected = "a", String.intercalateChar('-', "a" :: Nil))

    @Test
    def intercalateChar04(): Unit \ Assert =
        assertEq(expected = "ab", String.intercalateChar('-', "ab" :: Nil))

    @Test
    def intercalateChar05(): Unit \ Assert =
        assertEq(expected = "-", String.intercalateChar('-', "" :: "" :: Nil))

    @Test
    def intercalateChar06(): Unit \ Assert =
        assertEq(expected = "a-", String.intercalateChar('-', "a" :: "" :: Nil))

    @Test
    def intercalateChar07(): Unit \ Assert =
        assertEq(expected = "-a", String.intercalateChar('-', "" :: "a" :: Nil))

    @Test
    def intercalateChar08(): Unit \ Assert =
        assertEq(expected = "a-b", String.intercalateChar('-', "a" :: "b" :: Nil))

    @Test
    def intercalateChar09(): Unit \ Assert =
        assertEq(expected = "ab-cde", String.intercalateChar('-', "ab" :: "cde" :: Nil))

    @Test
    def intercalateChar10(): Unit \ Assert =
        assertEq(expected = "a-bc-d", String.intercalateChar('-', "a" :: "bc" :: "d" :: Nil))

    @Test
    def intercalateChar11(): Unit \ Assert =
        assertEq(expected = "a bc d", String.intercalateChar(' ', "a" :: "bc" :: "d" :: Nil))

    @Test
    def intercalateChar12(): Unit \ Assert =
        assertEq(expected = "a/bc/d", String.intercalateChar('/', "a" :: "bc" :: "d" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // map                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def map01(): Unit \ Assert =
        assertEq(expected = "", String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', ""))

    @Test
    def map02(): Unit \ Assert =
        assertEq(expected = "f", String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "a"))

    @Test
    def map03(): Unit \ Assert =
        assertEq(expected = "t", String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "A"))

    @Test
    def map04(): Unit \ Assert =
        assertEq(expected = "ff", String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "ab"))

    @Test
    def map05(): Unit \ Assert =
        assertEq(expected = "ft", String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "aB"))

    @Test
    def map06(): Unit \ Assert =
        assertEq(expected = "tf", String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "Ab"))

    @Test
    def map07(): Unit \ Assert =
        assertEq(expected = "tt", String.map(c -> if (Char.isUpperCase(c)) 't' else 'f', "AB"))

    /////////////////////////////////////////////////////////////////////////////
    // mapWithIndex                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def mapWithIndex01(): Unit \ Assert =
        let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
        assertEq(expected = "", String.mapWithIndex(test, ""))

    @Test
    def mapWithIndex02(): Unit \ Assert =
        let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
        assertEq(expected = "f", String.mapWithIndex(test, "a"))

    @Test
    def mapWithIndex03(): Unit \ Assert =
        let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
        assertEq(expected = "t", String.mapWithIndex(test, "A"))

    @Test
    def mapWithIndex04(): Unit \ Assert =
        let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
        assertEq(expected = "fT", String.mapWithIndex(test, "aB"))

    @Test
    def mapWithIndex05(): Unit \ Assert =
        let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
        assertEq(expected = "fF", String.mapWithIndex(test, "ab"))

    @Test
    def mapWithIndex06(): Unit \ Assert =
        let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
        assertEq(expected = "tT", String.mapWithIndex(test, "AB"))

    @Test
    def mapWithIndex07(): Unit \ Assert =
        let test = (i, c) -> if (i < 1) { if (Char.isUpperCase(c)) 't' else 'f' } else { if (Char.isUpperCase(c)) 'T' else 'F' };
        assertEq(expected = "tF", String.mapWithIndex(test, "Ab"))

    /////////////////////////////////////////////////////////////////////////////
    // reverse                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def reverse01(): Unit \ Assert =
        assertEq(expected = "", String.reverse(""))

    @Test
    def reverse02(): Unit \ Assert =
        assertEq(expected = "A", String.reverse("A"))

    @Test
    def reverse03(): Unit \ Assert =
        assertEq(expected = "dlroW olleH", String.reverse("Hello World"))

    @Test
    def reverse04(): Unit \ Assert =
        assertEq(expected = "!dlroW olleH", String.reverse("Hello World!"))

    @Test
    def reverse05(): Unit \ Assert =
        assertTrue(String.reverse("Hello World!") != "!dlroW OLLEH")

    /////////////////////////////////////////////////////////////////////////////
    // rotateLeft                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateLeft01(): Unit \ Assert =
        assertEq(expected = "", String.rotateLeft(0, ""))

    @Test
    def rotateLeft02(): Unit \ Assert =
        assertEq(expected = "", String.rotateLeft(1, ""))

    @Test
    def rotateLeft03(): Unit \ Assert =
        assertEq(expected = "a", String.rotateLeft(0, "a"))

    @Test
    def rotateLeft04(): Unit \ Assert =
        assertEq(expected = "ab", String.rotateLeft(0, "ab"))

    @Test
    def rotateLeft05(): Unit \ Assert =
        assertEq(expected = "ba", String.rotateLeft(1, "ab"))

    @Test
    def rotateLeft06(): Unit \ Assert =
        assertEq(expected = "ab", String.rotateLeft(2, "ab"))

    @Test
    def rotateLeft07(): Unit \ Assert =
        assertEq(expected = "ba", String.rotateLeft(3, "ab"))

    @Test
    def rotateLeft08(): Unit \ Assert = assertEq(expected = "ba", String.rotateLeft(-1, "ab"))

    @Test
    def rotateLeft09(): Unit \ Assert = assertEq(expected = "abc", String.rotateLeft(0, "abc"))

    @Test
    def rotateLeft10(): Unit \ Assert = assertEq(expected = "bca", String.rotateLeft(1, "abc"))

    @Test
    def rotateLeft11(): Unit \ Assert = assertEq(expected = "cab", String.rotateLeft(2, "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // rotateRight                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def rotateRight01(): Unit \ Assert = assertEq(expected = "", String.rotateRight(0, ""))

    @Test
    def rotateRight02(): Unit \ Assert = assertEq(expected = "", String.rotateRight(1, ""))

    @Test
    def rotateRight03(): Unit \ Assert = assertEq(expected = "a", String.rotateRight(0, "a"))

    @Test
    def rotateRight04(): Unit \ Assert = assertEq(expected = "ab", String.rotateRight(0, "ab"))

    @Test
    def rotateRight05(): Unit \ Assert = assertEq(expected = "ba", String.rotateRight(1, "ab"))

    @Test
    def rotateRight06(): Unit \ Assert = assertEq(expected = "ab", String.rotateRight(2, "ab"))

    @Test
    def rotateRight07(): Unit \ Assert = assertEq(expected = "ba", String.rotateRight(3, "ab"))

    @Test
    def rotateRight08(): Unit \ Assert = assertEq(expected = "ba", String.rotateRight(-1, "ab"))

    @Test
    def rotateRight09(): Unit \ Assert = assertEq(expected = "abc", String.rotateRight(0, "abc"))

    @Test
    def rotateRight10(): Unit \ Assert = assertEq(expected = "cab", String.rotateRight(1, "abc"))

    @Test
    def rotateRight11(): Unit \ Assert = assertEq(expected = "bca", String.rotateRight(2, "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // isMatch                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isMatch01(): Unit \ Assert = assertTrue(String.isMatch({regex = ".*"}, ""))

    @Test
    def isMatch02(): Unit \ Assert = assertFalse(String.isMatch({regex = ".+"}, ""))

    @Test
    def isMatch03(): Unit \ Assert = assertTrue(String.isMatch({regex = "[a]*"}, ""))

    @Test
    def isMatch04(): Unit \ Assert = assertFalse(String.isMatch({regex = "[a]+"}, ""))

    @Test
    def isMatch05(): Unit \ Assert = assertTrue(String.isMatch({regex = "[a]*"}, "a"))

    @Test
    def isMatch06(): Unit \ Assert = assertTrue(String.isMatch({regex = "[a]*"}, "aa"))

    @Test
    def isMatch07(): Unit \ Assert = assertFalse(String.isMatch({regex = "[a]*"}, "aab"))

    @Test
    def isMatch08(): Unit \ Assert = assertTrue(String.isMatch({regex = "[a]*b"}, "aab"))

    @Test
    def isSMatch09(): Unit \ Assert = assertFalse(String.isMatch({regex = "[b]+"}, "aab"))

    @Test
    def isMatch10(): Unit \ Assert = assertFalse(String.isMatch({regex = "[b]+"}, "bbbc"))

    @Test
    def isMatch11(): Unit \ Assert = assertFalse(String.isMatch({regex = "[b]+"}, "aabbbc"))

    /////////////////////////////////////////////////////////////////////////////
    // isSubmatch                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSubmatch01(): Unit \ Assert = assertTrue(String.isSubmatch({regex = ".*"}, ""))

    @Test
    def isSubmatch02(): Unit \ Assert = assertFalse(String.isSubmatch({regex = ".+"}, ""))

    @Test
    def isSubmatch03(): Unit \ Assert = assertTrue(String.isSubmatch({regex = "[a]*"}, ""))

    @Test
    def isSubmatch04(): Unit \ Assert = assertFalse(String.isSubmatch({regex = "[a]+"}, ""))

    @Test
    def isSubmatch05(): Unit \ Assert = assertTrue(String.isSubmatch({regex = "[a]*"}, "a"))

    @Test
    def isSubmatch06(): Unit \ Assert = assertTrue(String.isSubmatch({regex = "[a]*"}, "aa"))

    @Test
    def isSubmatch07(): Unit \ Assert = assertTrue(String.isSubmatch({regex = "[a]*"}, "aab"))

    @Test
    def isSubmatch08(): Unit \ Assert = assertTrue(String.isSubmatch({regex = "[a]*b"}, "aab"))

    @Test
    def isSubmatch09(): Unit \ Assert = assertTrue(String.isSubmatch({regex = "[b]+"}, "aab"))

    @Test
    def isSubmatch10(): Unit \ Assert = assertTrue(String.isSubmatch({regex = "[b]+"}, "bbbc"))

    @Test
    def isSubmatch11(): Unit \ Assert = assertTrue(String.isSubmatch({regex = "[b]+"}, "aabbbc"))

    /////////////////////////////////////////////////////////////////////////////
    // slice                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def slice01(): Unit \ Assert = assertEq(expected = "", String.slice(start = 0, end = 1, ""))

    @Test
    def slice02(): Unit \ Assert = assertEq(expected = "a", String.slice(start = 0, end = 1, "a"))

    @Test
    def slice03(): Unit \ Assert = assertEq(expected = "", String.slice(start = 1, end = 2, "a"))

    @Test
    def slice04(): Unit \ Assert = assertEq(expected = "", String.slice(start = 0, end = 2, "a"))

    @Test
    def slice05(): Unit \ Assert = assertEq(expected = "", String.slice(start = -1, end = 0, "a"))

    @Test
    def slice06(): Unit \ Assert = assertEq(expected = "a", String.slice(start = 0, end = 1, "ab"))

    @Test
    def slice07(): Unit \ Assert = assertEq(expected = "b", String.slice(start = 1, end = 2, "ab"))

    @Test
    def slice08(): Unit \ Assert = assertEq(expected = "ab", String.slice(start = 0, end = 2, "ab"))

    @Test
    def slice09(): Unit \ Assert = assertEq(expected = "", String.slice(start = 0, end = 3, "ab"))

    @Test
    def slice10(): Unit \ Assert = assertEq(expected = "", String.slice(start = -1, end = 2, "ab"))

    @Test
    def slice11(): Unit \ Assert = assertEq(expected = "a", String.slice(start = 0, end = 1, "abc"))

    @Test
    def slice12(): Unit \ Assert = assertEq(expected = "b", String.slice(start = 1, end = 2, "abc"))

    @Test
    def slice13(): Unit \ Assert = assertEq(expected = "c", String.slice(start = 2, end = 3, "abc"))

    @Test
    def slice14(): Unit \ Assert = assertEq(expected = "ab", String.slice(start = 0, end = 2, "abc"))

    @Test
    def slice15(): Unit \ Assert = assertEq(expected = "bc", String.slice(start = 1, end = 3, "abc"))

    @Test
    def slice16(): Unit \ Assert = assertEq(expected = "abc", String.slice(start = 0, end = 3, "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // sliceLeft                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sliceLeft01(): Unit \ Assert = assertEq(expected = "", String.sliceLeft(end = 0, ""))

    @Test
    def sliceLeft02(): Unit \ Assert = assertEq(expected = "", String.sliceLeft(end = 1, ""))

    @Test
    def sliceLeft03(): Unit \ Assert = assertEq(expected = "a", String.sliceLeft(end = 1, "a"))

    @Test
    def sliceLeft04(): Unit \ Assert = assertEq(expected = "", String.sliceLeft(end = 0, "a"))

    @Test
    def sliceLeft05(): Unit \ Assert = assertEq(expected = "", String.sliceLeft(end = 2, "a"))

    @Test
    def sliceLeft06(): Unit \ Assert = assertEq(expected = "a", String.sliceLeft(end = 1, "ab"))

    @Test
    def sliceLeft07(): Unit \ Assert = assertEq(expected = "ab", String.sliceLeft(end = 2, "ab"))

    @Test
    def sliceLeft08(): Unit \ Assert = assertEq(expected = "", String.sliceLeft(end = 0, "ab"))

    @Test
    def sliceLeft09(): Unit \ Assert = assertEq(expected = "", String.sliceLeft(end = 3, "ab"))

    @Test
    def sliceLeft10(): Unit \ Assert = assertEq(expected = "", String.sliceLeft(end = -1, "ab"))

    @Test
    def sliceLeft11(): Unit \ Assert = assertEq(expected = "a", String.sliceLeft(end = 1, "abc"))

    @Test
    def sliceLeft12(): Unit \ Assert = assertEq(expected = "ab", String.sliceLeft(end = 2, "abc"))

    @Test
    def sliceLeft13(): Unit \ Assert = assertEq(expected = "abc", String.sliceLeft(end = 3, "abc"))

    @Test
    def sliceLeft14(): Unit \ Assert = assertEq(expected = "", String.sliceLeft(end = 0, "abc"))

    @Test
    def sliceLeft15(): Unit \ Assert = assertEq(expected = "", String.sliceLeft(end = 4, "abc"))

    @Test
    def sliceLeft16(): Unit \ Assert = assertEq(expected = "", String.sliceLeft(end = -1, "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // sliceRight                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def sliceRight01(): Unit \ Assert = assertEq(expected = "", String.sliceRight(start = 0, ""))

    @Test
    def sliceRight02(): Unit \ Assert = assertEq(expected = "", String.sliceRight(start = 1, ""))

    @Test
    def sliceRight03(): Unit \ Assert = assertEq(expected = "", String.sliceRight(start = 1, "a"))

    @Test
    def sliceRight04(): Unit \ Assert = assertEq(expected = "a", String.sliceRight(start = 0, "a"))

    @Test
    def sliceRight05(): Unit \ Assert = assertEq(expected = "", String.sliceRight(start = 1, "a"))

    @Test
    def sliceRight06(): Unit \ Assert = assertEq(expected = "ab", String.sliceRight(start = 0, "ab"))

    @Test
    def sliceRight07(): Unit \ Assert = assertEq(expected = "b", String.sliceRight(start = 1, "ab"))

    @Test
    def sliceRight08(): Unit \ Assert = assertEq(expected = "", String.sliceRight(start = -1, "ab"))

    @Test
    def sliceRight09(): Unit \ Assert = assertEq(expected = "", String.sliceRight(start = 2, "ab"))

    @Test
    def sliceRight10(): Unit \ Assert = assertEq(expected = "abc", String.sliceRight(start = 0, "abc"))

    @Test
    def sliceRight11(): Unit \ Assert = assertEq(expected = "bc", String.sliceRight(start = 1, "abc"))

    @Test
    def sliceRight12(): Unit \ Assert = assertEq(expected = "c", String.sliceRight(start = 2, "abc"))

    @Test
    def sliceRight13(): Unit \ Assert = assertEq(expected = "", String.sliceRight(start = 3, "abc"))

    @Test
    def sliceRight14(): Unit \ Assert = assertEq(expected = "", String.sliceRight(start = -1, "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfLeft                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOfLeft01(): Unit \ Assert = assertNone(String.findIndexOfLeft(Char.isUpperCase, ""))

    @Test
    def findIndexOfLeft02(): Unit \ Assert = assertNone(String.findIndexOfLeft(Char.isUpperCase, "a"))

    @Test
    def findIndexOfLeft03(): Unit \ Assert = assertEq(expected = Some(0), String.findIndexOfLeft(Char.isUpperCase, "A"))

    @Test
    def findIndexOfLeft04(): Unit \ Assert = assertNone(String.findIndexOfLeft(Char.isUpperCase, "ab"))

    @Test
    def findIndexOfLeft05(): Unit \ Assert = assertEq(expected = Some(0), String.findIndexOfLeft(Char.isUpperCase, "Ab"))

    @Test
    def findIndexOfLeft06(): Unit \ Assert = assertEq(expected = Some(1), String.findIndexOfLeft(Char.isUpperCase, "aB"))

    @Test
    def findIndexOfLeft07(): Unit \ Assert = assertEq(expected = Some(0), String.findIndexOfLeft(Char.isUpperCase, "AB"))

    /////////////////////////////////////////////////////////////////////////////
    // findIndexOfRight                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndexOfRight01(): Unit \ Assert = assertNone(String.findIndexOfRight(Char.isUpperCase, ""))

    @Test
    def findIndexOfRight02(): Unit \ Assert = assertNone(String.findIndexOfRight(Char.isUpperCase, "a"))

    @Test
    def findIndexOfRight03(): Unit \ Assert = assertEq(expected = Some(0), String.findIndexOfRight(Char.isUpperCase, "A"))

    @Test
    def findIndexOfRight04(): Unit \ Assert = assertNone(String.findIndexOfRight(Char.isUpperCase, "ab"))

    @Test
    def findIndexOfRight05(): Unit \ Assert = assertEq(expected = Some(0), String.findIndexOfRight(Char.isUpperCase, "Ab"))

    @Test
    def findIndexOfRight06(): Unit \ Assert = assertEq(expected = Some(1), String.findIndexOfRight(Char.isUpperCase, "aB"))

    @Test
    def findIndexOfRight07(): Unit \ Assert = assertEq(expected = Some(1), String.findIndexOfRight(Char.isUpperCase, "AB"))

    /////////////////////////////////////////////////////////////////////////////
    // findIndices                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def findIndices01(): Unit \ Assert = assertEq(expected = Vector#{}, String.findIndices(Char.isDigit, ""))

    @Test
    def findIndices02(): Unit \ Assert = assertEq(expected = Vector#{}, String.findIndices(Char.isDigit, "a"))

    @Test
    def findIndices03(): Unit \ Assert = assertEq(expected = Vector#{0}, String.findIndices(Char.isDigit, "1"))

    @Test
    def findIndices04(): Unit \ Assert = assertEq(expected = Vector#{}, String.findIndices(Char.isDigit, "ab"))

    @Test
    def findIndices05(): Unit \ Assert = assertEq(expected = Vector#{0}, String.findIndices(Char.isDigit, "1b"))

    @Test
    def findIndices06(): Unit \ Assert = assertEq(expected = Vector#{1}, String.findIndices(Char.isDigit, "a2"))

    @Test
    def findIndices07(): Unit \ Assert = assertEq(expected = Vector#{0, 1}, String.findIndices(Char.isDigit, "12"))

    /////////////////////////////////////////////////////////////////////////////
    // indicesOf                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indicesOf01(): Unit \ Assert = assertEq(expected = Vector#{}, String.indicesOf({substr = ""}, ""))

    @Test
    def indicesOf02(): Unit \ Assert = assertEq(expected = Vector#{}, String.indicesOf({substr = ""}, "a::b::c"))

    @Test
    def indicesOf03(): Unit \ Assert = assertEq(expected = Vector#{}, String.indicesOf({substr = "::"}, ""))

    @Test
    def indicesOf04(): Unit \ Assert = assertEq(expected = Vector#{}, String.indicesOf({substr = "::"}, "a"))

    @Test
    def indicesOf05(): Unit \ Assert = assertEq(expected = Vector#{0}, String.indicesOf({substr = "::"}, "::"))

    @Test
    def indicesOf06(): Unit \ Assert = assertEq(expected = Vector#{}, String.indicesOf({substr = "::"}, "ab"))

    @Test
    def indicesOf07(): Unit \ Assert = assertEq(expected = Vector#{0}, String.indicesOf({substr = "::"}, "::a"))

    @Test
    def indicesOf08(): Unit \ Assert = assertEq(expected = Vector#{1}, String.indicesOf({substr = "::"}, "a::"))

    @Test
    def indicesOf09(): Unit \ Assert = assertEq(expected = Vector#{1, 4}, String.indicesOf({substr = "::"}, "a::b::"))

    @Test
    def indicesOf10(): Unit \ Assert = assertEq(expected = Vector#{1, 4}, String.indicesOf({substr = "::"}, "a::b::c"))

    @Test
    def indicesOf11(): Unit \ Assert = assertEq(expected = Vector#{1, 4}, String.indicesOf({substr = "::"}, "a::b::c:"))

    @Test
    def indicesOf12(): Unit \ Assert = assertEq(expected = Vector#{2, 5}, String.indicesOf({substr = "::"}, ":a::b::c"))

    @Test
    def indicesOf13(): Unit \ Assert = assertEq(expected = Vector#{0, 2}, String.indicesOf({substr = "::"}, "::::"))


    /////////////////////////////////////////////////////////////////////////////
    // take                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def take01(): Unit \ Assert = assertEq(expected = "", String.take(-1, ""))

    @Test
    def take02(): Unit \ Assert = assertEq(expected = "", String.take(0, ""))

    @Test
    def take03(): Unit \ Assert = assertEq(expected = "", String.take(1, ""))

    @Test
    def take04(): Unit \ Assert = assertEq(expected = "", String.take(-1, "a"))

    @Test
    def take05(): Unit \ Assert = assertEq(expected = "", String.take(0, "a"))

    @Test
    def take06(): Unit \ Assert = assertEq(expected = "a", String.take(1, "a"))

    @Test
    def take07(): Unit \ Assert = assertEq(expected = "a", String.take(2, "a"))

    @Test
    def take08(): Unit \ Assert = assertEq(expected = "", String.take(0, "ab"))

    @Test
    def take09(): Unit \ Assert = assertEq(expected = "a", String.take(1, "ab"))

    @Test
    def take10(): Unit \ Assert = assertEq(expected = "ab", String.take(2, "ab"))

    @Test
    def take11(): Unit \ Assert = assertEq(expected = "ab", String.take(2, "abcdef"))

    @Test
    def take12(): Unit \ Assert = assertEq(expected = "abcd", String.take(4, "abcdef"))

    @Test
    def take13(): Unit \ Assert = assertEq(expected = "abcdef", String.take(10, "abcdef"))

    /////////////////////////////////////////////////////////////////////////////
    // takeLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeLeft01(): Unit \ Assert = assertEq(expected = "", String.takeLeft(-1, ""))

    @Test
    def takeLeft02(): Unit \ Assert = assertEq(expected = "", String.takeLeft(0, ""))

    @Test
    def takeLeft03(): Unit \ Assert = assertEq(expected = "", String.takeLeft(1, ""))

    @Test
    def takeLeft04(): Unit \ Assert = assertEq(expected = "", String.takeLeft(-1, "a"))

    @Test
    def takeLeft05(): Unit \ Assert = assertEq(expected = "", String.takeLeft(0, "a"))

    @Test
    def takeLeft06(): Unit \ Assert = assertEq(expected = "a", String.takeLeft(1, "a"))

    @Test
    def takeLeft07(): Unit \ Assert = assertEq(expected = "a", String.takeLeft(2, "a"))

    @Test
    def takeLeft08(): Unit \ Assert = assertEq(expected = "", String.takeLeft(0, "ab"))

    @Test
    def takeLeft09(): Unit \ Assert = assertEq(expected = "a", String.takeLeft(1, "ab"))

    @Test
    def takeLeft10(): Unit \ Assert = assertEq(expected = "ab", String.takeLeft(2, "ab"))

    @Test
    def takeLeft11(): Unit \ Assert = assertEq(expected = "ab", String.takeLeft(2, "abcdef"))

    @Test
    def takeLeft12(): Unit \ Assert = assertEq(expected = "abcd", String.takeLeft(4, "abcdef"))

    @Test
    def takeLeft13(): Unit \ Assert = assertEq(expected = "abcdef", String.takeLeft(10, "abcdef"))

    /////////////////////////////////////////////////////////////////////////////
    // takeRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeRight01(): Unit \ Assert = assertEq(expected = "", String.takeRight(-1, ""))

    @Test
    def takeRight02(): Unit \ Assert = assertEq(expected = "", String.takeRight(0, ""))

    @Test
    def takeRight03(): Unit \ Assert = assertEq(expected = "", String.takeRight(1, ""))

    @Test
    def takeRight04(): Unit \ Assert = assertEq(expected = "", String.takeRight(-1, "a"))

    @Test
    def takeRight05(): Unit \ Assert = assertEq(expected = "", String.takeRight(0, "a"))

    @Test
    def takeRight06(): Unit \ Assert = assertEq(expected = "a", String.takeRight(1, "a"))

    @Test
    def takeRight07(): Unit \ Assert = assertEq(expected = "a", String.takeRight(2, "a"))

    @Test
    def takeRight08(): Unit \ Assert = assertEq(expected = "", String.takeRight(0, "ab"))

    @Test
    def takeRight09(): Unit \ Assert = assertEq(expected = "b", String.takeRight(1, "ab"))

    @Test
    def takeRight10(): Unit \ Assert = assertEq(expected = "ab", String.takeRight(2, "ab"))

    @Test
    def takeRight11(): Unit \ Assert = assertEq(expected = "ef", String.takeRight(2, "abcdef"))

    @Test
    def takeRight12(): Unit \ Assert = assertEq(expected = "cdef", String.takeRight(4, "abcdef"))

    @Test
    def takeRight13(): Unit \ Assert = assertEq(expected = "abcdef", String.takeRight(10, "abcdef"))

    /////////////////////////////////////////////////////////////////////////////
    // drop                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def drop01(): Unit \ Assert = assertEq(expected = "", String.drop(-1, ""))

    @Test
    def drop02(): Unit \ Assert = assertEq(expected = "", String.drop(0, ""))

    @Test
    def drop03(): Unit \ Assert = assertEq(expected = "", String.drop(1, ""))

    @Test
    def drop04(): Unit \ Assert = assertEq(expected = "a", String.drop(-1, "a"))

    @Test
    def drop05(): Unit \ Assert = assertEq(expected = "a", String.drop(0, "a"))

    @Test
    def drop06(): Unit \ Assert = assertEq(expected = "", String.drop(1, "a"))

    @Test
    def drop07(): Unit \ Assert = assertEq(expected = "", String.drop(2, "a"))

    @Test
    def drop08(): Unit \ Assert = assertEq(expected = "ab", String.drop(0, "ab"))

    @Test
    def drop09(): Unit \ Assert = assertEq(expected = "b", String.drop(1, "ab"))

    @Test
    def drop10(): Unit \ Assert = assertEq(expected = "", String.drop(2, "ab"))

    @Test
    def drop11(): Unit \ Assert = assertEq(expected = "cdef", String.drop(2, "abcdef"))

    @Test
    def drop12(): Unit \ Assert = assertEq(expected = "ef", String.drop(4, "abcdef"))

    /////////////////////////////////////////////////////////////////////////////
    // dropLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropLeft01(): Unit \ Assert = assertEq(expected = "", String.dropLeft(-1, ""))

    @Test
    def dropLeft02(): Unit \ Assert = assertEq(expected = "", String.dropLeft(0, ""))

    @Test
    def dropLeft03(): Unit \ Assert = assertEq(expected = "", String.dropLeft(1, ""))

    @Test
    def dropLeft04(): Unit \ Assert = assertEq(expected = "a", String.dropLeft(-1, "a"))

    @Test
    def dropLeft05(): Unit \ Assert = assertEq(expected = "a", String.dropLeft(0, "a"))

    @Test
    def dropLeft06(): Unit \ Assert = assertEq(expected = "", String.dropLeft(1, "a"))

    @Test
    def dropLeft07(): Unit \ Assert = assertEq(expected = "", String.dropLeft(2, "a"))

    @Test
    def dropLeft08(): Unit \ Assert = assertEq(expected = "ab", String.dropLeft(0, "ab"))

    @Test
    def dropLeft09(): Unit \ Assert = assertEq(expected = "b", String.dropLeft(1, "ab"))

    @Test
    def dropLeft10(): Unit \ Assert = assertEq(expected = "", String.dropLeft(2, "ab"))

    @Test
    def dropLeft11(): Unit \ Assert = assertEq(expected = "cdef", String.dropLeft(2, "abcdef"))

    @Test
    def dropLeft12(): Unit \ Assert = assertEq(expected = "ef", String.dropLeft(4, "abcdef"))

    /////////////////////////////////////////////////////////////////////////////
    // dropRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropRight01(): Unit \ Assert = assertEq(expected = "", String.dropRight(-1, ""))

    @Test
    def dropRight02(): Unit \ Assert = assertEq(expected = "", String.dropRight(0, ""))

    @Test
    def dropRight03(): Unit \ Assert = assertEq(expected = "", String.dropRight(1, ""))

    @Test
    def dropRight04(): Unit \ Assert = assertEq(expected = "a", String.dropRight(-1, "a"))

    @Test
    def dropRight05(): Unit \ Assert = assertEq(expected = "a", String.dropRight(0, "a"))

    @Test
    def dropRight06(): Unit \ Assert = assertEq(expected = "", String.dropRight(1, "a"))

    @Test
    def dropRight07(): Unit \ Assert = assertEq(expected = "", String.dropRight(2, "a"))

    @Test
    def dropRight08(): Unit \ Assert = assertEq(expected = "ab", String.dropRight(0, "ab"))

    @Test
    def dropRight09(): Unit \ Assert = assertEq(expected = "a", String.dropRight(1, "ab"))

    @Test
    def dropRight10(): Unit \ Assert = assertEq(expected = "", String.dropRight(2, "ab"))

    @Test
    def dropRight11(): Unit \ Assert = assertEq(expected = "abcd", String.dropRight(2, "abcdef"))

    @Test
    def dropRight12(): Unit \ Assert = assertEq(expected = "ab", String.dropRight(4, "abcdef"))

    /////////////////////////////////////////////////////////////////////////////
    // takeWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhile01(): Unit \ Assert = assertEq(expected = "", String.takeWhile(Char.isUpperCase, ""))

    @Test
    def takeWhile02(): Unit \ Assert = assertEq(expected = "", String.takeWhile(Char.isUpperCase, "a"))

    @Test
    def takeWhile03(): Unit \ Assert = assertEq(expected = "A", String.takeWhile(Char.isUpperCase, "A"))

    @Test
    def takeWhile04(): Unit \ Assert = assertEq(expected = "", String.takeWhile(Char.isUpperCase, "ab"))

    @Test
    def takeWhile05(): Unit \ Assert = assertEq(expected = "", String.takeWhile(Char.isUpperCase, "aB"))

    @Test
    def takeWhile06(): Unit \ Assert = assertEq(expected = "A", String.takeWhile(Char.isUpperCase, "Ab"))

    @Test
    def takeWhile07(): Unit \ Assert = assertEq(expected = "AB", String.takeWhile(Char.isUpperCase, "AB"))

    @Test
    def takeWhile08(): Unit \ Assert = assertEq(expected = "AB", String.takeWhile(Char.isUpperCase, "ABcDefgHIjK"))

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileLeft01(): Unit \ Assert = assertEq(expected = "", String.takeWhileLeft(Char.isUpperCase, ""))

    @Test
    def takeWhileLeft02(): Unit \ Assert = assertEq(expected = "", String.takeWhileLeft(Char.isUpperCase, "a"))

    @Test
    def takeWhileLeft03(): Unit \ Assert = assertEq(expected = "A", String.takeWhileLeft(Char.isUpperCase, "A"))

    @Test
    def takeWhileLeft04(): Unit \ Assert = assertEq(expected = "", String.takeWhileLeft(Char.isUpperCase, "ab"))

    @Test
    def takeWhileLeft05(): Unit \ Assert = assertEq(expected = "", String.takeWhileLeft(Char.isUpperCase, "aB"))

    @Test
    def takeWhileLeft06(): Unit \ Assert = assertEq(expected = "A", String.takeWhileLeft(Char.isUpperCase, "Ab"))

    @Test
    def takeWhileLeft07(): Unit \ Assert = assertEq(expected = "AB", String.takeWhileLeft(Char.isUpperCase, "AB"))

    @Test
    def takeWhileLeft08(): Unit \ Assert = assertEq(expected = "AB", String.takeWhileLeft(Char.isUpperCase, "ABcDefgHIjK"))

    /////////////////////////////////////////////////////////////////////////////
    // takeWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def takeWhileRight01(): Unit \ Assert = assertEq(expected = "", String.takeWhileRight(Char.isUpperCase, ""))

    @Test
    def takeWhileRight02(): Unit \ Assert = assertEq(expected = "", String.takeWhileRight(Char.isUpperCase, "a"))

    @Test
    def takeWhileRight03(): Unit \ Assert = assertEq(expected = "A", String.takeWhileRight(Char.isUpperCase, "A"))

    @Test
    def takeWhileRight04(): Unit \ Assert = assertEq(expected = "", String.takeWhileRight(Char.isUpperCase, "ab"))

    @Test
    def takeWhileRight05(): Unit \ Assert = assertEq(expected = "B", String.takeWhileRight(Char.isUpperCase, "aB"))

    @Test
    def takeWhileRight06(): Unit \ Assert = assertEq(expected = "", String.takeWhileRight(Char.isUpperCase, "Ab"))

    @Test
    def takeWhileRight07(): Unit \ Assert = assertEq(expected = "AB", String.takeWhileRight(Char.isUpperCase, "AB"))

    @Test
    def takeWhileRight08(): Unit \ Assert = assertEq(expected = "K", String.takeWhileRight(Char.isUpperCase, "ABcDefgHIjK"))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhile01(): Unit \ Assert = assertEq(expected = "", String.dropWhile(Char.isUpperCase, ""))

    @Test
    def dropWhile02(): Unit \ Assert = assertEq(expected = "a", String.dropWhile(Char.isUpperCase, "a"))

    @Test
    def dropWhile03(): Unit \ Assert = assertEq(expected = "", String.dropWhile(Char.isUpperCase, "A"))

    @Test
    def dropWhile04(): Unit \ Assert = assertEq(expected = "ab", String.dropWhile(Char.isUpperCase, "ab"))

    @Test
    def dropWhile05(): Unit \ Assert = assertEq(expected = "aB", String.dropWhile(Char.isUpperCase, "aB"))

    @Test
    def dropWhile06(): Unit \ Assert = assertEq(expected = "b", String.dropWhile(Char.isUpperCase, "Ab"))

    @Test
    def dropWhile07(): Unit \ Assert = assertEq(expected = "", String.dropWhile(Char.isUpperCase, "AB"))

    @Test
    def dropWhile08(): Unit \ Assert = assertEq(expected = "cDefgHIjK", String.dropWhile(Char.isUpperCase, "ABcDefgHIjK"))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileLeft                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileLeft01(): Unit \ Assert = assertEq(expected = "", String.dropWhileLeft(Char.isUpperCase, ""))

    @Test
    def dropWhileLeft02(): Unit \ Assert = assertEq(expected = "a", String.dropWhileLeft(Char.isUpperCase, "a"))

    @Test
    def dropWhileLeft03(): Unit \ Assert = assertEq(expected = "", String.dropWhileLeft(Char.isUpperCase, "A"))

    @Test
    def dropWhileLeft04(): Unit \ Assert = assertEq(expected = "ab", String.dropWhileLeft(Char.isUpperCase, "ab"))

    @Test
    def dropWhileLeft05(): Unit \ Assert = assertEq(expected = "aB", String.dropWhileLeft(Char.isUpperCase, "aB"))

    @Test
    def dropWhileLeft06(): Unit \ Assert = assertEq(expected = "b", String.dropWhileLeft(Char.isUpperCase, "Ab"))

    @Test
    def dropWhileLeft07(): Unit \ Assert = assertEq(expected = "", String.dropWhileLeft(Char.isUpperCase, "AB"))

    @Test
    def dropWhileLeft08(): Unit \ Assert = assertEq(expected = "cDefgHIjK", String.dropWhileLeft(Char.isUpperCase, "ABcDefgHIjK"))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileRight                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileRight01(): Unit \ Assert = assertEq(expected = "", String.dropWhileRight(Char.isUpperCase, ""))

    @Test
    def dropWhileRight02(): Unit \ Assert = assertEq(expected = "a", String.dropWhileRight(Char.isUpperCase, "a"))

    @Test
    def dropWhileRight03(): Unit \ Assert = assertEq(expected = "", String.dropWhileRight(Char.isUpperCase, "A"))

    @Test
    def dropWhileRight04(): Unit \ Assert = assertEq(expected = "ab", String.dropWhileRight(Char.isUpperCase, "ab"))

    @Test
    def dropWhileRight05(): Unit \ Assert = assertEq(expected = "a", String.dropWhileRight(Char.isUpperCase, "aB"))

    @Test
    def dropWhileRight06(): Unit \ Assert = assertEq(expected = "Ab", String.dropWhileRight(Char.isUpperCase, "Ab"))

    @Test
    def dropWhileRight07(): Unit \ Assert = assertEq(expected = "", String.dropWhileRight(Char.isUpperCase, "AB"))

    @Test
    def dropWhileRight08(): Unit \ Assert = assertEq(expected = "ABcDefgHIj", String.dropWhileRight(Char.isUpperCase, "ABcDefgHIjK"))

    /////////////////////////////////////////////////////////////////////////////
    // dropWhileAround                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def dropWhileAround01(): Unit \ Assert = assertEq(expected = "", String.dropWhileRight(Char.isUpperCase, ""))

    @Test
    def dropWhileAround02(): Unit \ Assert = assertEq(expected = "a", String.dropWhileAround(Char.isUpperCase, "a"))

    @Test
    def dropWhileAround03(): Unit \ Assert = assertEq(expected = "", String.dropWhileAround(Char.isUpperCase, "A"))

    @Test
    def dropWhileAround04(): Unit \ Assert = assertEq(expected = "ab", String.dropWhileAround(Char.isUpperCase, "ab"))

    @Test
    def dropWhileAround05(): Unit \ Assert = assertEq(expected = "a", String.dropWhileAround(Char.isUpperCase, "aB"))

    @Test
    def dropWhileAround06(): Unit \ Assert = assertEq(expected = "b", String.dropWhileAround(Char.isUpperCase, "Ab"))

    @Test
    def dropWhileAround07(): Unit \ Assert = assertEq(expected = "", String.dropWhileAround(Char.isUpperCase, "AB"))

    @Test
    def dropWhileAround08(): Unit \ Assert = assertEq(expected = "cDefgHIj", String.dropWhileAround(Char.isUpperCase, "ABcDefgHIjK"))

    /////////////////////////////////////////////////////////////////////////////
    // splitAt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def splitAt01(): Unit \ Assert = assertEq(expected = ("", ""), String.splitAt(0, ""))

    @Test
    def splitAt02(): Unit \ Assert = assertEq(expected = ("", ""), String.splitAt(1, ""))

    @Test
    def splitAt03(): Unit \ Assert = assertEq(expected = ("", ""), String.splitAt(-1, ""))

    @Test
    def splitAt04(): Unit \ Assert = assertEq(expected = ("", "a"), String.splitAt(0, "a"))

    @Test
    def splitAt05(): Unit \ Assert = assertEq(expected = ("a", ""), String.splitAt(1, "a"))

    @Test
    def splitAt06(): Unit \ Assert = assertEq(expected = ("", "a"), String.splitAt(-1, "a"))

    @Test
    def splitAt07(): Unit \ Assert = assertEq(expected = ("", "ab"), String.splitAt(0, "ab"))

    @Test
    def splitAt08(): Unit \ Assert = assertEq(expected = ("a", "b"), String.splitAt(1, "ab"))

    @Test
    def splitAt09(): Unit \ Assert = assertEq(expected = ("ab", ""), String.splitAt(2, "ab"))

    @Test
    def splitAt10(): Unit \ Assert = assertEq(expected = ("", "ab"), String.splitAt(-1, "ab"))

    @Test
    def splitAt11(): Unit \ Assert = assertEq(expected = ("ab", ""), String.splitAt(3, "ab"))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft01(): Unit \ Assert = assertEq(expected = "", String.foldLeft((ac, c) -> ac + Char.toString(c), "", ""))

    @Test
    def foldLeft02(): Unit \ Assert = assertEq(expected = "a", String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "a"))

    @Test
    def foldLeft03(): Unit \ Assert = assertEq(expected = "ab", String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "ab"))

    @Test
    def foldLeft04(): Unit \ Assert = assertEq(expected = "abc", String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abc"))

    @Test
    def foldLeft05(): Unit \ Assert = assertEq(expected = "abcd", String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abcd"))

    @Test
    def foldLeft06(): Unit \ Assert = assertEq(expected = "abcde", String.foldLeft((ac, c) -> ac + Char.toString(c), "",  "abcde"))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight01(): Unit \ Assert = assertEq(expected = "", String.foldRight((c, ac) -> Char.toString(c) + ac, "", ""))

    @Test
    def foldRight02(): Unit \ Assert = assertEq(expected = "a", String.foldRight((c, ac) -> Char.toString(c) + ac, "", "a"))

    @Test
    def foldRight03(): Unit \ Assert = assertEq(expected = "ab", String.foldRight((c, ac) -> Char.toString(c) + ac, "", "ab"))

    @Test
    def foldRight04(): Unit \ Assert = assertEq(expected = "abc", String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abc"))

    @Test
    def foldRight05(): Unit \ Assert = assertEq(expected = "abcd", String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abcd"))

    @Test
    def foldRight06(): Unit \ Assert = assertEq(expected = "abcde", String.foldRight((c, ac) -> Char.toString(c) + ac, "",  "abcde"))

    /////////////////////////////////////////////////////////////////////////////
    // foldLeft2                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldLeft201(): Unit \ Assert =
        assertEq(expected = "", String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "", ""))

    @Test
    def foldLeft202(): Unit \ Assert =
        assertEq(expected = "a", String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "a", "_"))

    @Test
    def foldLeft203(): Unit \ Assert =
        assertEq(expected = "A", String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "a", "A"))

    @Test
    def foldLeft204(): Unit \ Assert =
        assertEq(expected = "ab", String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "__"))

    @Test
    def foldLeft205(): Unit \ Assert =
        assertEq(expected = "Ab", String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "A_"))

    @Test
    def foldLeft206(): Unit \ Assert =
        assertEq(expected = "AB", String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "AB"))

    @Test
    def foldLeft207(): Unit \ Assert =
        assertEq(expected = "", String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", ""))

    @Test
    def foldLeft208(): Unit \ Assert =
        assertEq(expected = "a", String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "_"))

    @Test
    def foldLeft209(): Unit \ Assert =
        assertEq(expected = "A", String.foldLeft2((ac, a, b) -> if (Char.isUpperCase(b)) ac + Char.toString(b) else ac + Char.toString(a), "", "ab", "A"))

    /////////////////////////////////////////////////////////////////////////////
    // foldRight2                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def foldRight201(): Unit \ Assert =
        assertEq(expected = "", String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "", ""))

    @Test
    def foldRight202(): Unit \ Assert =
        assertEq(expected = "a", String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "a", "_"))

    @Test
    def foldRight203(): Unit \ Assert =
        assertEq(expected = "A", String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "a", "A"))

    @Test
    def foldRight204(): Unit \ Assert =
        assertEq(expected = "ab", String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "__"))

    @Test
    def foldRight205(): Unit \ Assert =
        assertEq(expected = "Ab", String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "A_"))

    @Test
    def foldRight206(): Unit \ Assert =
        assertEq(expected = "AB", String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "AB"))

    @Test
    def foldRight207(): Unit \ Assert =
        assertEq(expected = "", String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", ""))

    @Test
    def foldRight208(): Unit \ Assert =
        assertEq(expected = "b", String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "_"))

    @Test
    def foldRight209(): Unit \ Assert =
        assertEq(expected = "B", String.foldRight2((a, b, ac) -> if (Char.isUpperCase(b)) Char.toString(b) + ac else Char.toString(a) + ac, "", "ab", "B"))

    /////////////////////////////////////////////////////////////////////////////
    // unfold                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfold01(): Unit \ Assert =
        assertEq(expected = "aaaaa", String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 0))

    @Test
    def unfold02(): Unit \ Assert =
        assertEq(expected = "aaaa", String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 1))

    @Test
    def unfold03(): Unit \ Assert =
        assertEq(expected = "aaa", String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 2))

    @Test
    def unfold04(): Unit \ Assert =
        assertEq(expected = "aa", String.unfold(x -> if (x < 5) Some((('a', x+1))) else None, 3))

    @Test
    def unfold05(): Unit \ Assert =
        assertEq(expected = "a", String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 4))

    @Test
    def unfold06(): Unit \ Assert =
        assertEq(expected = "", String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 5))

    @Test
    def unfold07(): Unit \ Assert =
        assertEq(expected = "", String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, 6))

    @Test
    def unfold08(): Unit \ Assert =
        assertEq(expected = "aaaaaa", String.unfold(x -> if (x < 5) Some(('a', x+1)) else None, -1))

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
    def unfoldWithIter01(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (true)
                None
            else {
                let c = Some(toChar(Ref.get(x) + 97));
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = "", String.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter02(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 0)
                None
            else {
                let c = Some(toChar(Ref.get(x) + 97));
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = "a", String.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter03(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 1)
                None
            else {
                let c = Some(toChar(Ref.get(x) + 97));
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = "ab", String.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter04(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(toChar(Ref.get(x) + 97));
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = "abcdefghij", String.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter05(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 5);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(toChar(Ref.get(x) + 97));
                Ref.put(Ref.get(x) + 1, x);
                c
            };
        assertEq(expected = "fghij", String.unfoldWithIter(step))
    }

    @Test
    def unfoldWithIter06(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c = Some(toChar(Ref.get(x) + 97));
                Ref.put(Ref.get(x) + 2, x);
                c
            };
        assertEq(expected = "acegi", String.unfoldWithIter(step))
    }

    /////////////////////////////////////////////////////////////////////////////
    // unfoldString                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfoldString01(): Unit \ Assert =
        assertEq(expected = "ab.ab.ab.ab.ab.", String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 0))

    @Test
    def unfoldString02(): Unit \ Assert =
        assertEq(expected = "ab.ab.ab.ab.", String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 1))

    @Test
    def unfoldString03(): Unit \ Assert =
        assertEq(expected = "ab.ab.ab.", String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 2))

    @Test
    def unfoldString04(): Unit \ Assert =
        assertEq(expected = "ab.ab.", String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 3))

    @Test
    def unfoldString05(): Unit \ Assert =
        assertEq(expected = "ab.", String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 4))

    @Test
    def unfoldString06(): Unit \ Assert =
        assertEq(expected = "", String.unfoldString(x -> if (x < 5) Some(("", x+1)) else None, 5))

    @Test
    def unfoldString07(): Unit \ Assert =
        assertEq(expected = "", String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, 6))

    @Test
    def unfoldString08(): Unit \ Assert =
        assertEq(expected = "ab.ab.ab.ab.ab.ab.", String.unfoldString(x -> if (x < 5) Some(("ab.", x+1)) else None, -1))


    /////////////////////////////////////////////////////////////////////////////
    // unfoldStringWithIter                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unfoldStringWithIter01(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (true)
                None
            else {
                let c1 = toChar(Ref.get(x) + 65);
                let c2 = toChar(Ref.get(x) + 97);
                let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
                Ref.put(Ref.get(x) + 1, x);
                Some(s)
            };
        assertEq(expected = "", String.unfoldStringWithIter(step))
    }

    @Test
    def unfoldStringWithIter02(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 0)
                None
            else {
                let c1 = toChar(Ref.get(x) + 65);
                let c2 = toChar(Ref.get(x) + 97);
                let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
                Ref.put(Ref.get(x) + 1, x);
                Some(s)
            };
        assertEq(expected = "Aa.", String.unfoldStringWithIter(step))
    }

    @Test
    def unfoldStringWithIter03(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) > 1)
                None
            else {
                let c1 = toChar(Ref.get(x) + 65);
                let c2 = toChar(Ref.get(x) + 97);
                let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
                Ref.put(Ref.get(x) + 1, x);
                Some(s)
            };
        assertEq(expected = "Aa.Bb.", String.unfoldStringWithIter(step))
    }

    @Test
    def unfoldStringWithIter04(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c1 = toChar(Ref.get(x) + 65);
                let c2 = toChar(Ref.get(x) + 97);
                let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
                Ref.put(Ref.get(x) + 1, x);
                Some(s)
            };
        assertEq(expected = "Aa.Bb.Cc.Dd.Ee.Ff.Gg.Hh.Ii.Jj.", String.unfoldStringWithIter(step))
    }

    @Test
    def unfoldStringWithIter05(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 5);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c1 = toChar(Ref.get(x) + 65);
                let c2 = toChar(Ref.get(x) + 97);
                let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
                Ref.put(Ref.get(x) + 1, x);
                Some(s)
            };
        assertEq(expected = "Ff.Gg.Hh.Ii.Jj.", String.unfoldStringWithIter(step))
    }

    @Test
    def unfoldStringWithIter06(): Unit \ Assert = region rc {
        let x = Ref.fresh(rc, 0);
        let step = () ->
            if (Ref.get(x) >= 10)
                None
            else {
                let c1 = toChar(Ref.get(x) + 65);
                let c2 = toChar(Ref.get(x) + 97);
                let s = List.joinWith(Char.toString, "", c1 :: c2 :: '.' :: Nil);
                Ref.put(Ref.get(x) + 2, x);
                Some(s)
            };
        assertEq(expected = "Aa.Cc.Ee.Gg.Ii.", String.unfoldStringWithIter(step))
    }

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def exists01(): Unit \ Assert = assertFalse(String.exists(Char.isUpperCase, ""))

    @Test
    def exists02(): Unit \ Assert = assertFalse(String.exists(Char.isUpperCase, "a"))

    @Test
    def exists03(): Unit \ Assert = assertTrue(String.exists(Char.isUpperCase, "A"))

    @Test
    def exists04(): Unit \ Assert = assertFalse(String.exists(Char.isUpperCase, "ab"))

    @Test
    def exists05(): Unit \ Assert = assertTrue(String.exists(Char.isUpperCase, "aB"))

    @Test
    def exists06(): Unit \ Assert = assertTrue(String.exists(Char.isUpperCase, "Ab"))

    @Test
    def exists07(): Unit \ Assert = assertTrue(String.exists(Char.isUpperCase, "AB"))

    @Test
    def exists08(): Unit \ Assert = assertFalse(String.exists(Char.isUpperCase, "a*c"))

    @Test
    def exists09(): Unit \ Assert = assertTrue(String.exists(Char.isUpperCase, "aBc"))

    /////////////////////////////////////////////////////////////////////////////
    // forAll                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forAll01(): Unit \ Assert = assertTrue(String.forAll(Char.isUpperCase, ""))

    @Test
    def forAll02(): Unit \ Assert = assertFalse(String.forAll(Char.isUpperCase, "a"))

    @Test
    def forAll03(): Unit \ Assert = assertTrue(String.forAll(Char.isUpperCase, "A"))

    @Test
    def forAll04(): Unit \ Assert = assertFalse(String.forAll(Char.isUpperCase, "a"))

    @Test
    def forAll05(): Unit \ Assert = assertFalse(String.forAll(Char.isUpperCase, "aB"))

    @Test
    def forAll06(): Unit \ Assert = assertFalse(String.forAll(Char.isUpperCase, "Ab"))

    @Test
    def forAll07(): Unit \ Assert = assertTrue(String.forAll(Char.isUpperCase, "AB"))

    @Test
    def forAll08(): Unit \ Assert = assertFalse(String.forAll(Char.isUpperCase, "a*b"))

    @Test
    def forAll09(): Unit \ Assert = assertFalse(String.forAll(Char.isUpperCase, "aBc"))

    @Test
    def forAll10(): Unit \ Assert = assertTrue(String.forAll(Char.isUpperCase, "ABC"))

    /////////////////////////////////////////////////////////////////////////////
    // isAscii                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isAscii01(): Unit \ Assert = assertTrue(String.isAscii(""))

    @Test
    def isAscii02(): Unit \ Assert = assertTrue(String.isAscii("a"))

    @Test
    def isAscii03(): Unit \ Assert = assertTrue(String.isAscii("A"))

    @Test
    def isAscii04(): Unit \ Assert = assertTrue(String.isAscii("0"))

    @Test
    def isAscii05(): Unit \ Assert = assertTrue(String.isAscii("~"))

    @Test
    def isAscii06(): Unit \ Assert = assertFalse(String.isAscii("Â"))

    @Test
    def isAscii07(): Unit \ Assert = assertFalse(String.isAscii("†"))

    @Test
    def isAscii08(): Unit \ Assert = assertTrue(String.isAscii("ab"))

    @Test
    def isAscii09(): Unit \ Assert = assertFalse(String.isAscii("a†"))

    @Test
    def isAscii10(): Unit \ Assert = assertFalse(String.isAscii("†b"))

    /////////////////////////////////////////////////////////////////////////////
    // isWhitespace                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isWhitespace01(): Unit \ Assert = assertTrue(String.isWhitespace(""))

    @Test
    def isWhitespace02(): Unit \ Assert = assertTrue(String.isWhitespace(" "))

    @Test
    def isWhitespace03(): Unit \ Assert = assertFalse(String.isWhitespace("A"))

    @Test
    def isWhitespace04(): Unit \ Assert = assertFalse(String.isWhitespace("0"))

    @Test
    def isWhitespace05(): Unit \ Assert = assertFalse(String.isWhitespace("~"))

    @Test
    def isWhitespace06(): Unit \ Assert = assertTrue(String.isWhitespace(String.lineSeparator()))

    @Test
    def isWhitespace07(): Unit \ Assert = assertTrue(String.isWhitespace("  "))

    /////////////////////////////////////////////////////////////////////////////
    // trimLeft                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def trimLeft01(): Unit \ Assert = assertEq(expected = "", String.trimLeft(""))

    @Test
    def trimLeft02(): Unit \ Assert = assertEq(expected = "", String.trimLeft(" "))

    @Test
    def trimLeft03(): Unit \ Assert = assertEq(expected = "a", String.trimLeft("a"))

    @Test
    def trimLeft04(): Unit \ Assert = assertEq(expected = "a", String.trimLeft(" a"))

    @Test
    def trimLeft05(): Unit \ Assert = assertEq(expected = "a ", String.trimLeft("a "))

    @Test
    def trimLeft06(): Unit \ Assert = assertEq(expected = "aa", String.trimLeft("aa"))

    @Test
    def trimLeft07(): Unit \ Assert = assertEq(expected = "aa", String.trimLeft("  aa"))

    @Test
    def trimLeft08(): Unit \ Assert = assertEq(expected = "aa  ", String.trimLeft("aa  "))

    @Test
    def trimLeft09(): Unit \ Assert = assertEq(expected = "aa ", String.trimLeft(" aa "))

    @Test
    def trimLeft10(): Unit \ Assert = assertEq(expected = "aa  ", String.trimLeft("  aa  "))

    @Test
    def trimLeft11(): Unit \ Assert = assertEq(expected = "a a ", String.trimLeft(" a a "))

    @Test
    def trimLeft12(): Unit \ Assert = assertEq(expected = "aa", String.trimLeft(String.lineSeparator() + "aa"))


    /////////////////////////////////////////////////////////////////////////////
    // trimRight                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def trimRight01(): Unit \ Assert = assertEq(expected = "", String.trimRight(""))

    @Test
    def trimRight02(): Unit \ Assert = assertEq(expected = "", String.trimRight(" "))

    @Test
    def trimRight03(): Unit \ Assert = assertEq(expected = "a", String.trimRight("a"))

    @Test
    def trimRight04(): Unit \ Assert = assertEq(expected = " a", String.trimRight(" a"))

    @Test
    def trimRight05(): Unit \ Assert = assertEq(expected = "a", String.trimRight("a "))

    @Test
    def trimRight06(): Unit \ Assert = assertEq(expected = "aa", String.trimRight("aa"))

    @Test
    def trimRight07(): Unit \ Assert = assertEq(expected = "  aa", String.trimRight("  aa"))

    @Test
    def trimRight08(): Unit \ Assert = assertEq(expected = "aa", String.trimRight("aa  "))

    @Test
    def trimRight09(): Unit \ Assert = assertEq(expected = " aa", String.trimRight(" aa "))

    @Test
    def trimRight10(): Unit \ Assert = assertEq(expected = "  aa", String.trimRight("  aa  "))

    @Test
    def trimRight11(): Unit \ Assert = assertEq(expected = " a a", String.trimRight(" a a "))

    @Test
    def trimRight12(): Unit \ Assert = assertEq(expected = "aa", String.trimRight("aa" + String.lineSeparator()))

    /////////////////////////////////////////////////////////////////////////////
    // repeat                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def repeat01(): Unit \ Assert = assertEq(expected = "", String.repeat(-1, "a"))

    @Test
    def repeat02(): Unit \ Assert = assertEq(expected = "", String.repeat(0, "a"))

    @Test
    def repeat03(): Unit \ Assert = assertEq(expected = "a", String.repeat(1, "a"))

    @Test
    def repeat04(): Unit \ Assert = assertEq(expected = "aa", String.repeat(2, "a"))

    @Test
    def repeat05(): Unit \ Assert = assertEq(expected = "aaa", String.repeat(3, "a"))

    @Test
    def repeat06(): Unit \ Assert = assertEq(expected = "", String.repeat(0, "ab"))

    @Test
    def repeat07(): Unit \ Assert = assertEq(expected = "ab", String.repeat(1, "ab"))

    @Test
    def repeat08(): Unit \ Assert = assertEq(expected = "abab", String.repeat(2, "ab"))

    @Test
    def repeat09(): Unit \ Assert = assertEq(expected = "ababab", String.repeat(3, "ab"))

    /////////////////////////////////////////////////////////////////////////////
    // padLeft                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def padLeft01(): Unit \ Assert = assertEq(expected = "", String.padLeft(0, ' ', ""))

    @Test
    def padLeft02(): Unit \ Assert = assertEq(expected = " ", String.padLeft(1, ' ', ""))

    @Test
    def padLeft03(): Unit \ Assert = assertEq(expected = "  ", String.padLeft(2, ' ', ""))

    @Test
    def padLeft04(): Unit \ Assert = assertEq(expected = "   ", String.padLeft(3, ' ', ""))

    @Test
    def padLeft05(): Unit \ Assert = assertEq(expected = "a", String.padLeft(0, ' ', "a"))

    @Test
    def padLeft06(): Unit \ Assert = assertEq(expected = "a", String.padLeft(1, ' ', "a"))

    @Test
    def padLeft07(): Unit \ Assert = assertEq(expected = " a", String.padLeft(2, ' ', "a"))

    @Test
    def padLeft08(): Unit \ Assert = assertEq(expected = "  a", String.padLeft(3, ' ', "a"))

    @Test
    def padLeft09(): Unit \ Assert = assertEq(expected = "ab", String.padLeft(0, ' ', "ab"))

    @Test
    def padLeft10(): Unit \ Assert = assertEq(expected = "ab", String.padLeft(1, ' ', "ab"))

    @Test
    def padLeft11(): Unit \ Assert = assertEq(expected = "ab", String.padLeft(2, ' ', "ab"))

    @Test
    def padLeft12(): Unit \ Assert = assertEq(expected = " ab", String.padLeft(3, ' ', "ab"))

    @Test
    def padLeft13(): Unit \ Assert = assertEq(expected = "__a", String.padLeft(3, '_', "a"))

    @Test
    def padLeft14(): Unit \ Assert = assertEq(expected = "_ab", String.padLeft(3, '_', "ab"))

    @Test
    def padLeft15(): Unit \ Assert = assertEq(expected = "abc", String.padLeft(3, '_', "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // padRight                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def padRight01(): Unit \ Assert = assertEq(expected = "", String.padRight(0, ' ', ""))

    @Test
    def padRight02(): Unit \ Assert = assertEq(expected = " ", String.padRight(1, ' ', ""))

    @Test
    def padRight03(): Unit \ Assert = assertEq(expected = "  ", String.padRight(2, ' ', ""))

    @Test
    def padRight04(): Unit \ Assert = assertEq(expected = "   ", String.padRight(3, ' ', ""))

    @Test
    def padRight05(): Unit \ Assert = assertEq(expected = "a", String.padRight(0, ' ', "a"))

    @Test
    def padRight06(): Unit \ Assert = assertEq(expected = "a", String.padRight(1, ' ', "a"))

    @Test
    def padRight07(): Unit \ Assert = assertEq(expected = "a ", String.padRight(2, ' ', "a"))

    @Test
    def padRight08(): Unit \ Assert = assertEq(expected = "a  ", String.padRight(3, ' ', "a"))

    @Test
    def padRight09(): Unit \ Assert = assertEq(expected = "ab", String.padRight(0, ' ', "ab"))

    @Test
    def padRight10(): Unit \ Assert = assertEq(expected = "ab", String.padRight(1, ' ', "ab"))

    @Test
    def padRight11(): Unit \ Assert = assertEq(expected = "ab", String.padRight(2, ' ', "ab"))

    @Test
    def padRight12(): Unit \ Assert = assertEq(expected = "ab ", String.padRight(3, ' ', "ab"))

    @Test
    def padRight13(): Unit \ Assert = assertEq(expected = "a__", String.padRight(3, '_', "a"))

    @Test
    def padRight14(): Unit \ Assert = assertEq(expected = "ab_", String.padRight(3, '_', "ab"))

    @Test
    def padRight15(): Unit \ Assert = assertEq(expected = "abc", String.padRight(3, '_', "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // center                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def center01(): Unit \ Assert = assertEq(expected = "", String.center(0, ' ', ""))

    @Test
    def center02(): Unit \ Assert = assertEq(expected = " ", String.center(1, ' ', ""))

    @Test
    def center03(): Unit \ Assert = assertEq(expected = "  ", String.center(2, ' ', ""))

    @Test
    def center04(): Unit \ Assert = assertEq(expected = "   ", String.center(3, ' ', ""))

    @Test
    def center05(): Unit \ Assert = assertEq(expected = "a", String.center(0, ' ', "a"))

    @Test
    def center06(): Unit \ Assert = assertEq(expected = "a", String.center(1, ' ', "a"))

    @Test
    def center07(): Unit \ Assert = assertEq(expected = "a ", String.center(2, ' ', "a"))

    @Test
    def center08(): Unit \ Assert = assertEq(expected = " a ", String.center(3, ' ', "a"))

    @Test
    def center09(): Unit \ Assert = assertEq(expected = "ab", String.center(0, ' ', "ab"))

    @Test
    def center10(): Unit \ Assert = assertEq(expected = "ab", String.center(1, ' ', "ab"))

    @Test
    def center11(): Unit \ Assert = assertEq(expected = "ab", String.center(2, ' ', "ab"))

    @Test
    def center12(): Unit \ Assert = assertEq(expected = "ab ", String.center(3, ' ', "ab"))

    @Test
    def center13(): Unit \ Assert = assertEq(expected = "_a_", String.center(3, '_', "a"))

    @Test
    def center14(): Unit \ Assert = assertEq(expected = "ab_", String.center(3, '_', "ab"))

    @Test
    def center15(): Unit \ Assert = assertEq(expected = "abc", String.center(3, '_', "abc"))

    @Test
    def center16(): Unit \ Assert = assertEq(expected = "__abc__", String.center(7, '_', "abc"))

    @Test
    def center17(): Unit \ Assert = assertEq(expected = "__abc___", String.center(8, '_', "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // replace                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replace01(): Unit \ Assert = assertEq(expected = "", String.replace({src = "c"}, {dst = "d"}, ""))

    @Test
    def replace02(): Unit \ Assert = assertEq(expected = "a", String.replace({src = "c"}, {dst = "d"}, "a"))

    @Test
    def replace03(): Unit \ Assert = assertEq(expected = "d", String.replace({src = "c"}, {dst = "d"}, "c"))

    @Test
    def replace04(): Unit \ Assert = assertEq(expected = "d", String.replace({src = "c"}, {dst = "d"}, "d"))

    @Test
    def replace05(): Unit \ Assert = assertEq(expected = "ab", String.replace({src = "c"}, {dst = "d"}, "ab"))

    @Test
    def replace06(): Unit \ Assert = assertEq(expected = "abd", String.replace({src = "c"}, {dst = "d"}, "abc"))

    @Test
    def replace07(): Unit \ Assert = assertEq(expected = "dd", String.replace({src = "c"}, {dst = "d"}, "cd"))

    @Test
    def replace08(): Unit \ Assert = assertEq(expected = "dd", String.replace({src = "c"}, {dst = "d"}, "cc"))

    /////////////////////////////////////////////////////////////////////////////
    // replaceChar                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replaceChar01(): Unit \ Assert = assertEq(expected = "", String.replaceChar({src = 'c'}, {dst = 'd'}, ""))

    @Test
    def replaceChar02(): Unit \ Assert = assertEq(expected = "a", String.replaceChar({src = 'c'}, {dst = 'd'}, "a"))

    @Test
    def replaceChar03(): Unit \ Assert = assertEq(expected = "d", String.replaceChar({src = 'c'}, {dst = 'd'}, "c"))

    @Test
    def replaceChar04(): Unit \ Assert = assertEq(expected = "d", String.replaceChar({src = 'c'}, {dst = 'd'}, "d"))

    @Test
    def replaceChar05(): Unit \ Assert = assertEq(expected = "ab", String.replaceChar({src = 'c'}, {dst = 'd'}, "ab"))

    @Test
    def replaceChar06(): Unit \ Assert = assertEq(expected = "abd", String.replaceChar({src = 'c'}, {dst = 'd'}, "abc"))

    @Test
    def replaceChar07(): Unit \ Assert = assertEq(expected = "dd", String.replaceChar({src = 'c'}, {dst = 'd'}, "cd"))

    @Test
    def replaceChar08(): Unit \ Assert = assertEq(expected = "dd", String.replaceChar({src = 'c'}, {dst = 'd'}, "cc"))

    /////////////////////////////////////////////////////////////////////////////
    // replaceMatches                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replaceMatches01(): Unit \ Assert = assertEq(expected = "", String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, ""))

    @Test
    def replaceMatches02(): Unit \ Assert = assertEq(expected = "a", String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "a"))

    @Test
    def replaceMatches03(): Unit \ Assert = assertEq(expected = "d", String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "c"))

    @Test
    def replaceMatches04(): Unit \ Assert = assertEq(expected = "d", String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "d"))

    @Test
    def replaceMatches05(): Unit \ Assert = assertEq(expected = "ab", String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "ab"))

    @Test
    def replaceMatches06(): Unit \ Assert = assertEq(expected = "abd", String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "abc"))

    @Test
    def replaceMatches07(): Unit \ Assert = assertEq(expected = "dd", String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "cd"))

    @Test
    def replaceMatches08(): Unit \ Assert = assertEq(expected = "dd", String.replaceMatches({regex = "[c]{1}"}, {to = "d"}, "cc"))

    /////////////////////////////////////////////////////////////////////////////
    // replaceFirstMatch                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def replaceFirstMatch01(): Unit \ Assert = assertEq(expected = "", String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, ""))

    @Test
    def replaceFirstMatch02(): Unit \ Assert = assertEq(expected = "a", String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "a"))

    @Test
    def replaceFirstMatch03(): Unit \ Assert = assertEq(expected = "d", String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "c"))

    @Test
    def replaceFirstMatch04(): Unit \ Assert = assertEq(expected = "d", String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "d"))

    @Test
    def replaceFirstMatch05(): Unit \ Assert = assertEq(expected = "ab", String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "ab"))

    @Test
    def replaceFirstMatch06(): Unit \ Assert = assertEq(expected = "abd", String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "abc"))

    @Test
    def replaceFirstMatch07(): Unit \ Assert = assertEq(expected = "dd", String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "cd"))

    @Test
    def replaceFirstMatch08(): Unit \ Assert = assertEq(expected = "dc", String.replaceFirstMatch({regex = "[c]{1}"}, {to = "d"}, "cc"))

    /////////////////////////////////////////////////////////////////////////////
    // update                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def update01(): Unit \ Assert = assertEq(expected = "", String.update(0, 'b', ""))

    @Test
    def update02(): Unit \ Assert = assertEq(expected = "a", String.update(-1, 'b', "a"))

    @Test
    def update03(): Unit \ Assert = assertEq(expected = "b", String.update(0, 'b', "a"))

    @Test
    def update04(): Unit \ Assert = assertEq(expected = "a", String.update(1, 'b', "a"))

    @Test
    def update05(): Unit \ Assert = assertEq(expected = "eb", String.update(0, 'e', "ab"))

    @Test
    def update06(): Unit \ Assert = assertEq(expected = "ae", String.update(1, 'e', "ab"))

    @Test
    def update07(): Unit \ Assert = assertEq(expected = "ab", String.update(2, 'e', "ab"))

    /////////////////////////////////////////////////////////////////////////////
    // patch                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def patch01(): Unit \ Assert = assertEq(expected = "", String.patch(0, 0, "", ""))

    @Test
    def patch02(): Unit \ Assert = assertEq(expected = "", String.patch(0, 2, "ab", ""))

    @Test
    def patch03(): Unit \ Assert = assertEq(expected = "ab", String.patch(0, 2, "", "ab"))

    @Test
    def patch04(): Unit \ Assert = assertEq(expected = "ab", String.patch(-3, 3, "abd", "ab"))

    @Test
    def patch05(): Unit \ Assert = assertEq(expected = "ab", String.patch(2, 3, "abd", "ab"))

    @Test
    def patch06(): Unit \ Assert = assertEq(expected = "a", String.patch(0, 0, "", "a"))

    @Test
    def patch07(): Unit \ Assert = assertEq(expected = "a", String.patch(1, 0, "b", "a"))

    @Test
    def patch08(): Unit \ Assert = assertEq(expected = "b", String.patch(0, 1, "b", "a"))

    @Test
    def patch09(): Unit \ Assert = assertEq(expected = "b", String.patch(0, 2, "bd", "a"))

    @Test
    def patch10(): Unit \ Assert = assertEq(expected = "d", String.patch(-1, 2, "bd", "a"))

    @Test
    def patch11(): Unit \ Assert = assertEq(expected = "db", String.patch(-1, 2, "cd", "ab"))

    @Test
    def patch12(): Unit \ Assert = assertEq(expected = "ac", String.patch(1, 2, "cd", "ab"))

    @Test
    def patch13(): Unit \ Assert = assertEq(expected = "ab", String.patch(-2, 2, "cd", "ab"))

    @Test
    def patch14(): Unit \ Assert = assertEq(expected = "ab", String.patch(2, 2, "cd", "ab"))

    @Test
    def patch15(): Unit \ Assert = assertEq(expected = "ac", String.patch(1, 1, "c", "ab"))

    @Test
    def patch16(): Unit \ Assert = assertEq(expected = "cd", String.patch(0, 2, "cd", "ab"))

    @Test
    def patch17(): Unit \ Assert = assertEq(expected = "dbc", String.patch(0, 1, "d", "abc"))

    @Test
    def patch18(): Unit \ Assert = assertEq(expected = "adc", String.patch(1, 1, "d", "abc"))

    @Test
    def patch19(): Unit \ Assert = assertEq(expected = "abd", String.patch(2, 1, "d", "abc"))

    @Test
    def patch20(): Unit \ Assert = assertEq(expected = "dec", String.patch(0, 2, "de", "abc"))

    @Test
    def patch21(): Unit \ Assert = assertEq(expected = "ade", String.patch(1, 2, "de", "abc"))

    @Test
    def patch22(): Unit \ Assert = assertEq(expected = "dec", String.patch(0, 2, "def", "abc"))

    @Test
    def patch23(): Unit \ Assert = assertEq(expected = "def", String.patch(0, 3, "def", "abc"))

    @Test
    def patch24(): Unit \ Assert = assertEq(expected = "abnopqg", String.patch(2, 4, "nopq", "abcdefg"))

    @Test
    def patch25(): Unit \ Assert = assertEq(expected = "pqcdefg", String.patch(-2, 4, "nopq", "abcdefg"))

    @Test
    def patch26(): Unit \ Assert = assertEq(expected = "abcdnop", String.patch(4, 5, "nopq", "abcdefg"))

    @Test
    def patch27(): Unit \ Assert = assertEq(expected = "abcdnog", String.patch(4, 2, "nopq", "abcdefg"))

    @Test
    def patch28(): Unit \ Assert = assertEq(expected = "BCDEFGH", String.patch(-1, 10, "ABCDEFGH", "abcdefg"))

    /////////////////////////////////////////////////////////////////////////////
    // indent                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indent01(): Unit \ Assert =
        assertEq(expected = "", String.indent(3, ""))

    @Test
    def indent02(): Unit \ Assert =
        assertEq(expected = String.unlines("   abc" :: Nil), String.indent(3, String.unlines("abc" :: Nil)))

    @Test
    def indent03(): Unit \ Assert =
        assertEq(expected = String.unlines("   abc" :: "   def" :: Nil), String.indent(3, String.unlines("abc" :: "def" :: Nil)))

    @Test
    def indent04(): Unit \ Assert =
        assertEq(expected = String.unlines("   ab" :: "   cd" :: "   ef" :: Nil), String.indent(3, String.unlines("ab" :: "cd" :: "ef" :: Nil)))

    @Test
    def indent05(): Unit \ Assert =
        assertEq(expected = String.unlines(" abc" :: Nil), String.indent(1, String.unlines("abc" :: Nil)))

    @Test
    def indent06(): Unit \ Assert =
        assertEq(expected = String.unlines(" abc" :: " def" :: Nil), String.indent(1, String.unlines("abc" :: "def" :: Nil)))

    @Test
    def indent07(): Unit \ Assert =
        assertEq(expected = String.unlines(" ab" :: " cd" :: " ef" :: Nil), String.indent(1, String.unlines("ab" :: "cd" :: "ef" :: Nil)))

    @Test
    def indent08(): Unit \ Assert =
        assertEq(expected = String.unlines("abc" :: Nil), String.indent(0, String.unlines("abc" :: Nil)))

    @Test
    def indent09(): Unit \ Assert =
        assertEq(expected = String.unlines("abc" :: "def" :: Nil), String.indent(0, String.unlines("abc" :: "def" :: Nil)))

    @Test
    def indent10(): Unit \ Assert =
        assertEq(expected = String.unlines("ab" :: "cd" :: "ef" :: Nil), String.indent(0, String.unlines("ab" :: "cd" :: "ef" :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // stripIndent                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stripIndent01(): Unit \ Assert =
        assertEq(expected = "", String.stripIndent(3, ""))

    @Test
    def stripIndent02(): Unit \ Assert =
        assertEq(expected = String.unlines("abc" :: Nil), String.stripIndent(3, String.unlines("   abc" :: Nil)))

    @Test
    def stripIndent03(): Unit \ Assert =
        assertEq(expected = String.unlines("abc" :: "def" :: Nil), String.stripIndent(3, String.unlines("   abc" :: "   def" :: Nil)))

    @Test
    def stripIndent04(): Unit \ Assert =
        assertEq(expected = String.unlines("ab" :: "cd" :: "ef" :: Nil), String.stripIndent(3, String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)))

    @Test
    def stripIndent05(): Unit \ Assert =
        assertEq(expected = String.unlines("  abc" :: Nil), String.stripIndent(1, String.unlines("   abc" :: Nil)))

    @Test
    def stripIndent06(): Unit \ Assert =
        assertEq(expected = String.unlines("  abc" :: "  def" :: Nil), String.stripIndent(1, String.unlines("   abc" :: "   def" :: Nil)))

    @Test
    def stripIndent07(): Unit \ Assert =
        assertEq(expected = String.unlines("  ab" :: "  cd" :: "  ef" :: Nil), String.stripIndent(1, String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)))

    @Test
    def stripIndent08(): Unit \ Assert =
        assertEq(expected = String.unlines("   abc" :: Nil), String.stripIndent(0, String.unlines("   abc" :: Nil)))

    @Test
    def stripIndent09(): Unit \ Assert =
        assertEq(expected = String.unlines("   abc" :: "   def" :: Nil), String.stripIndent(0, String.unlines("   abc" :: "   def" :: Nil)))

    @Test
    def stripIndent10(): Unit \ Assert  =
        assertEq(expected = String.unlines("   ab" :: "   cd" :: "   ef" :: Nil), String.stripIndent(0, String.unlines("   ab" :: "   cd" :: "   ef" :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // lines                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def lines01(): Unit \ Assert = assertEq(expected = Nil, String.lines(""))

    @Test
    def lines02(): Unit \ Assert = assertEq(expected = "a" :: Nil, String.lines("a"))

    @Test
    def lines03(): Unit \ Assert = assertEq(expected = "a" :: Nil, String.lines("a" + String.lineSeparator()))

    @Test
    def lines04(): Unit \ Assert = assertEq(expected = "" :: "a" :: Nil, String.lines(String.lineSeparator() + "a"))

    @Test
    def lines05(): Unit \ Assert = assertEq(expected = "a" :: "b" :: Nil, String.lines("a" + String.lineSeparator() + "b"))

    @Test
    def lines06(): Unit \ Assert = assertEq(expected = "a" :: "b" :: Nil, String.lines("a" + Char.toString('\n') + "b"))

    @Test
    def lines07(): Unit \ Assert = assertEq(expected = "a" :: "b" :: Nil, String.lines("a" + Char.toString('\r') + Char.toString('\n') + "b"))

    /////////////////////////////////////////////////////////////////////////////
    // unlines                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unlines01(): Unit \ Assert = assertEq(expected = "", String.unlines(Nil))

    @Test
    def unlines02(): Unit \ Assert =
        assertEq(expected = String.lineSeparator(), String.unlines("" :: Nil))

    @Test
    def unlines03(): Unit \ Assert =
        assertEq(expected = "a" + String.lineSeparator(), String.unlines("a" :: Nil))

    @Test
    def unlines04(): Unit \ Assert =
        assertEq(expected = "a" + String.lineSeparator() + String.lineSeparator(), String.unlines("a" :: "" :: Nil))

    @Test
    def unlines05(): Unit \ Assert =
        assertEq(expected = String.lineSeparator() + "b" + String.lineSeparator(), String.unlines("" :: "b" :: Nil))

    @Test
    def unlines06(): Unit \ Assert =
        assertEq(expected = "a" + String.lineSeparator() + "b" + String.lineSeparator(), String.unlines("a" :: "b" :: Nil))

    @Test
    def unlines07(): Unit \ Assert =
        assertEq(expected = "a" + String.lineSeparator() + String.lineSeparator() + String.lineSeparator(), String.unlines("a" :: String.lineSeparator() :: Nil))

    @Test
    def unlines08(): Unit \ Assert =
        assertEq(expected = String.lineSeparator() + String.lineSeparator() + "b" + String.lineSeparator(), String.unlines(String.lineSeparator() :: "b" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // words                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def words01(): Unit \ Assert = assertEq(expected = Nil, String.words(""))

    @Test
    def words02(): Unit \ Assert = assertEq(expected = "a" :: Nil, String.words("a"))

    @Test
    def words03(): Unit \ Assert = assertEq(expected = "a" :: Nil, String.words("a "))

    @Test
    def words04(): Unit \ Assert = assertEq(expected = "a" :: Nil, String.words(" a"))

    @Test
    def words05(): Unit \ Assert = assertEq(expected = "a" :: "b" :: Nil, String.words("a b"))

    @Test
    def words06(): Unit \ Assert = assertEq(expected = "a" :: "b" :: Nil, String.words("a  b"))

    @Test
    def words07(): Unit \ Assert = assertEq(expected = "a" :: "b" :: Nil, String.words("a" + Char.toString('\t') + "b"))

    /////////////////////////////////////////////////////////////////////////////
    // unwords                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unwords01(): Unit \ Assert = assertEq(expected = "", String.unwords(Nil))

    @Test
    def unwords02(): Unit \ Assert = assertEq(expected = "", String.unwords("" :: Nil))

    @Test
    def unwords03(): Unit \ Assert = assertEq(expected = "a", String.unwords("a" :: Nil))

    @Test
    def unwords04(): Unit \ Assert = assertEq(expected = "a ", String.unwords("a" :: "" :: Nil))

    @Test
    def unwords05(): Unit \ Assert = assertEq(expected = " b", String.unwords("" :: "b" :: Nil))

    @Test
    def unwords06(): Unit \ Assert = assertEq(expected = "a b", String.unwords("a" :: "b" :: Nil))

    @Test
    def unwords07(): Unit \ Assert = assertEq(expected = "a " + String.lineSeparator(), String.unwords("a" :: String.lineSeparator() :: Nil))

    @Test
    def unwords08(): Unit \ Assert = assertEq(expected = String.lineSeparator() + " b", String.unwords(String.lineSeparator() :: "b" :: Nil))

    /////////////////////////////////////////////////////////////////////////////
    // indexOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOf01(): Unit \ Assert = assertNone(String.indexOf({substr = "a"}, ""))

    @Test
    def indexOf02(): Unit \ Assert = assertNone(String.indexOf({substr = "a"}, "A"))

    @Test
    def indexOf03(): Unit \ Assert = assertEq(expected = Some(0), String.indexOf({substr = "a"}, "a"))

    @Test
    def indexOf04(): Unit \ Assert = assertNone(String.indexOf({substr = "a"}, "AB"))

    @Test
    def indexOf05(): Unit \ Assert = assertEq(expected = Some(0), String.indexOf({substr = "a"}, "ab"))

    @Test
    def indexOf06(): Unit \ Assert = assertEq(expected = Some(1), String.indexOf({substr = "b"}, "ab"))

    @Test
    def indexOf07(): Unit \ Assert = assertEq(expected = Some(0), String.indexOf({substr = "a"}, "aa"))

    @Test
    def indexOf08(): Unit \ Assert = assertEq(expected = Some(0), String.indexOf({substr = "ab"}, "ab"))

    @Test
    def indexOf09(): Unit \ Assert = assertEq(expected = Some(1), String.indexOf({substr = "bc"}, "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLeft01(): Unit \ Assert = assertNone(String.indexOfLeft({substr = "a"}, ""))

    @Test
    def indexOfLeft02(): Unit \ Assert = assertNone(String.indexOfLeft({substr = "a"}, "A"))

    @Test
    def indexOfLeft03(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfLeft({substr = "a"}, "a"))

    @Test
    def indexOfLeft04(): Unit \ Assert = assertNone(String.indexOfLeft({substr = "a"}, "AB"))

    @Test
    def indexOfLeft05(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfLeft({substr = "a"}, "ab"))

    @Test
    def indexOfLeft06(): Unit \ Assert = assertEq(expected = Some(1), String.indexOfLeft({substr = "b"}, "ab"))

    @Test
    def indexOfLeft07(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfLeft({substr = "a"}, "aa"))

    @Test
    def indexOfLeft08(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfLeft({substr = "ab"}, "ab"))

    @Test
    def indexOfLeft09(): Unit \ Assert = assertEq(expected = Some(1), String.indexOfLeft({substr = "bc"}, "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfRight01(): Unit \ Assert = assertNone(String.indexOfRight({substr = "a"}, ""))

    @Test
    def indexOfRight02(): Unit \ Assert = assertNone(String.indexOfRight({substr = "a"}, "A"))

    @Test
    def indexOfRight03(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfRight({substr = "a"}, "a"))

    @Test
    def indexOfRight04(): Unit \ Assert = assertNone(String.indexOfRight({substr = "a"}, "AB"))

    @Test
    def indexOfRight05(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfRight({substr = "a"}, "ab"))

    @Test
    def indexOfRight06(): Unit \ Assert = assertEq(expected = Some(1), String.indexOfRight({substr = "b"}, "ab"))

    @Test
    def indexOfRight07(): Unit \ Assert = assertEq(expected = Some(1), String.indexOfRight({substr = "a"}, "aa"))

    @Test
    def indexOfRight08(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfRight({substr = "ab"}, "ab"))

    @Test
    def indexOfRight09(): Unit \ Assert = assertEq(expected = Some(1), String.indexOfRight({substr = "bc"}, "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // startsWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def startsWith01(): Unit \ Assert = assertTrue(String.startsWith({prefix = ""}, ""))

    @Test
    def startsWith02(): Unit \ Assert = assertTrue(String.startsWith({prefix = ""}, "a"))

    @Test
    def startsWith03(): Unit \ Assert = assertTrue(String.startsWith({prefix = ""}, "abc"))

    @Test
    def startsWith04(): Unit \ Assert = assertFalse(String.startsWith({prefix = "a"}, ""))

    @Test
    def startsWith05(): Unit \ Assert = assertTrue(String.startsWith({prefix = "a"}, "a"))

    @Test
    def startsWith06(): Unit \ Assert = assertFalse(String.startsWith({prefix = "a"}, "b"))

    @Test
    def startsWith07(): Unit \ Assert = assertTrue(String.startsWith({prefix = "a"}, "aa"))

    @Test
    def startsWith08(): Unit \ Assert = assertTrue(String.startsWith({prefix = "ab"}, "ab"))

    @Test
    def startsWith09(): Unit \ Assert = assertFalse(String.startsWith({prefix = "bs"}, "abc"))

    @Test
    def startsWith10(): Unit \ Assert = assertTrue(String.startsWith({prefix = ""}, "Hello World!"))

    @Test
    def startsWith11(): Unit \ Assert = assertTrue(String.startsWith({prefix = "Hello"}, "Hello World!"))

    @Test
    def startsWith12(): Unit \ Assert = assertFalse(String.startsWith({prefix = "HELLO"}, "Hello World!"))

    /////////////////////////////////////////////////////////////////////////////
    // contains                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def contains01(): Unit \ Assert = assertTrue(String.contains({substr = ""}, ""))

    @Test
    def contains02(): Unit \ Assert = assertTrue(String.contains({substr = ""}, "a"))

    @Test
    def contains03(): Unit \ Assert = assertTrue(String.contains({substr = ""}, "abc"))

    @Test
    def contains04(): Unit \ Assert = assertFalse(String.contains({substr = "a"}, ""))

    @Test
    def contains05(): Unit \ Assert = assertTrue(String.contains({substr = "a"}, "a"))

    @Test
    def contains06(): Unit \ Assert = assertFalse(String.contains({substr = "a"}, "b"))

    @Test
    def contains07(): Unit \ Assert = assertTrue(String.contains({substr = "a"}, "aa"))

    @Test
    def contains08(): Unit \ Assert = assertTrue(String.contains({substr = "ab"}, "ab"))

    @Test
    def contains09(): Unit \ Assert = assertTrue(String.contains({substr = "ab"}, "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // endsWith                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def endsWith01(): Unit \ Assert = assertTrue(String.endsWith({suffix = ""}, ""))

    @Test
    def endsWith02(): Unit \ Assert = assertTrue(String.endsWith({suffix = ""}, "a"))

    @Test
    def endsWith03(): Unit \ Assert = assertTrue(String.endsWith({suffix = ""}, "abc"))

    @Test
    def endsWith04(): Unit \ Assert = assertFalse(String.endsWith({suffix = "a"}, ""))

    @Test
    def endsWith05(): Unit \ Assert = assertTrue(String.endsWith({suffix = "a"}, "a"))

    @Test
    def endsWith06(): Unit \ Assert = assertFalse(String.endsWith({suffix = "a"}, "b"))

    @Test
    def endsWith07(): Unit \ Assert = assertTrue(String.endsWith({suffix = "a"}, "aa"))

    @Test
    def endsWith08(): Unit \ Assert = assertTrue(String.endsWith({suffix = "ab"}, "ab"))

    @Test
    def endsWith09(): Unit \ Assert = assertTrue(String.endsWith({suffix = "bc"}, "abc"))

    @Test
    def endsWith10(): Unit \ Assert = assertFalse(String.endsWith({suffix = "ab"}, "abc"))

    @Test
    def endsWith11(): Unit \ Assert = assertTrue(String.endsWith({suffix = ""}, "Hello World!"))

    @Test
    def endsWith12(): Unit \ Assert = assertTrue(String.endsWith({suffix = "World!"}, "Hello World!"))

    @Test
    def endsWith13(): Unit \ Assert = assertFalse(String.endsWith({suffix = "WORLD!"}, "Hello World!"))

    /////////////////////////////////////////////////////////////////////////////
    // commonPrefix                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def commonPrefix01(): Unit \ Assert =
        assertEq(expected = "", String.commonPrefix("", ""))

    @Test
    def commonPrefix02(): Unit \ Assert =
        assertEq(expected = "", String.commonPrefix("a", ""))

    @Test
    def commonPrefix03(): Unit \ Assert =
        assertEq(expected = "", String.commonPrefix("", "a"))

    @Test
    def commonPrefix04(): Unit \ Assert =
        assertEq(expected = "a", String.commonPrefix("a", "a"))

    @Test
    def commonPrefix05(): Unit \ Assert =
        assertEq(expected = "", String.commonPrefix("a", "b"))

    @Test
    def commonPrefix06(): Unit \ Assert =
        assertEq(expected = "", String.commonPrefix("ab", "bc"))

    @Test
    def commonPrefix07(): Unit \ Assert =
        assertEq(expected = "a", String.commonPrefix("ab", "ac"))

    @Test
    def commonPrefix08(): Unit \ Assert =
        assertEq(expected = "ab", String.commonPrefix("ab", "ab"))

    @Test
    def commonPrefix09(): Unit \ Assert =
        assertEq(expected = "", String.commonPrefix("abc", "ABC"))

    @Test
    def commonPrefix10(): Unit \ Assert =
        assertEq(expected = "abc", String.commonPrefix("abc", "abcd"))

    @Test
    def commonPrefix11(): Unit \ Assert =
        assertEq(expected = "abc", String.commonPrefix("abcd", "abc"))

    /////////////////////////////////////////////////////////////////////////////
    // commonSuffix                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def commonSuffix01(): Unit \ Assert =
        assertEq(expected = "", String.commonSuffix("", ""))

    @Test
    def commonSuffix02(): Unit \ Assert =
        assertEq(expected = "", String.commonSuffix("a", ""))

    @Test
    def commonSuffix03(): Unit \ Assert =
        assertEq(expected = "", String.commonSuffix("", "a"))

    @Test
    def commonSuffix04(): Unit \ Assert =
        assertEq(expected = "a", String.commonSuffix("a", "a"))

    @Test
    def commonSuffix05(): Unit \ Assert =
        assertEq(expected = "", String.commonSuffix("a", "b"))

    @Test
    def commonSuffix06(): Unit \ Assert =
        assertEq(expected = "", String.commonSuffix("ab", "bc"))

    @Test
    def commonSuffix07(): Unit \ Assert =
        assertEq(expected = "b", String.commonSuffix("ab", "_b"))

    @Test
    def commonSuffix08(): Unit \ Assert =
        assertEq(expected = "ab", String.commonSuffix("ab", "ab"))

    @Test
    def commonSuffix09(): Unit \ Assert =
        assertEq(expected = "", String.commonSuffix("abc", "ABC"))

    @Test
    def commonSuffix10(): Unit \ Assert =
        assertEq(expected = "bcd", String.commonSuffix("bcd", "abcd"))

    @Test
    def commonSuffix11(): Unit \ Assert =
        assertEq(expected = "bcd", String.commonSuffix("abcd", "bcd"))

    /////////////////////////////////////////////////////////////////////////////
    // abbreviateLeft                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def abbreviateLeft01(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateLeft(0, ""))

    @Test
    def abbreviateLeft02(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateLeft(0, "a"))

    @Test
    def abbreviateLeft03(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateLeft(0, "abcde"))

    @Test
    def abbreviateLeft04(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateLeft(1, ""))

    @Test
    def abbreviateLeft05(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateLeft(1, "a"))

    @Test
    def abbreviateLeft06(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateLeft(1, "abcde"))

    @Test
    def abbreviateLeft07(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateLeft(3, ""))

    @Test
    def abbreviateLeft08(): Unit \ Assert =
        assertEq(expected = "a", String.abbreviateLeft(3, "a"))

    @Test
    def abbreviateLeft09(): Unit \ Assert =
        assertEq(expected = "...", String.abbreviateLeft(3, "abcde"))

    @Test
    def abbreviateLeft10(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateLeft(4, ""))

    @Test
    def abbreviateLeft11(): Unit \ Assert =
        assertEq(expected = "a", String.abbreviateLeft(4, "a"))

    @Test
    def abbreviateLeft12(): Unit \ Assert =
        assertEq(expected = "...e", String.abbreviateLeft(4, "abcde"))

    @Test
    def abbreviateLeft13(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateLeft(5, ""))

    @Test
    def abbreviateLeft14(): Unit \ Assert =
        assertEq(expected = "a", String.abbreviateLeft(5, "a"))

    @Test
    def abbreviateLeft15(): Unit \ Assert =
        assertEq(expected = "...de", String.abbreviateLeft(5, "abcde"))


    /////////////////////////////////////////////////////////////////////////////
    // abbreviateRight                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def abbreviateRight01(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateRight(0, ""))

    @Test
    def abbreviateRight02(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateRight(0, "a"))

    @Test
    def abbreviateRight03(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateRight(0, "abcde"))

    @Test
    def abbreviateRight04(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateRight(1, ""))

    @Test
    def abbreviateRight05(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateRight(1, "a"))

    @Test
    def abbreviateRight06(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateRight(1, "abcde"))

    @Test
    def abbreviateRight07(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateRight(3, ""))

    @Test
    def abbreviateRight08(): Unit \ Assert =
        assertEq(expected = "a", String.abbreviateRight(3, "a"))

    @Test
    def abbreviateRight09(): Unit \ Assert =
        assertEq(expected = "...", String.abbreviateRight(3, "abcde"))

    @Test
    def abbreviateRight10(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateRight(4, ""))

    @Test
    def abbreviateRight11(): Unit \ Assert =
        assertEq(expected = "a", String.abbreviateRight(4, "a"))

    @Test
    def abbreviateRight12(): Unit \ Assert =
        assertEq(expected = "a...", String.abbreviateRight(4, "abcde"))

    @Test
    def abbreviateRight13(): Unit \ Assert =
        assertEq(expected = "", String.abbreviateRight(5, ""))

    @Test
    def abbreviateRight14(): Unit \ Assert =
        assertEq(expected = "a", String.abbreviateRight(5, "a"))

    @Test
    def abbreviateRight15(): Unit \ Assert =
        assertEq(expected = "ab...", String.abbreviateRight(5, "abcde"))

    /////////////////////////////////////////////////////////////////////////////
    // levenshteinDistance                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def levenshteinDistance01(): Unit \ Assert =
        assertEq(expected = 0, String.levenshteinDistance("", ""))

    @Test
    def levenshteinDistance02(): Unit \ Assert =
        assertEq(expected = 1, String.levenshteinDistance("A", ""))

    @Test
    def levenshteinDistance03(): Unit \ Assert =
        assertEq(expected = 1, String.levenshteinDistance("", "A"))

    @Test
    def levenshteinDistance04(): Unit \ Assert =
        assertEq(expected = 0, String.levenshteinDistance("A", "A"))

    @Test
    def levenshteinDistance05(): Unit \ Assert =
        assertEq(expected = 3, String.levenshteinDistance("kitten", "sitting"))

    @Test
    def levenshteinDistance06(): Unit \ Assert =
        assertEq(expected = 3, String.levenshteinDistance("Saturday", "Sunday"))

    @Test
    def levenshteinDistance07(): Unit \ Assert =
        assertEq(expected = 2, String.levenshteinDistance("Gumbo", "Gambol"))

    /////////////////////////////////////////////////////////////////////////////
    // zip                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zip01(): Unit \ Assert =
        assertEq(expected = (Nil: List[(Char,Char)]), String.zip("", ""))

    @Test
    def zip02(): Unit \ Assert =
        assertEq(expected = (Nil: List[(Char, Char)]), String.zip("a", ""))

    @Test
    def zip03(): Unit \ Assert =
        assertEq(expected = (Nil: List[(Char, Char)]), String.zip("", "1"))

    @Test
    def zip04(): Unit \ Assert =
        assertEq(expected = ('a', '1') :: Nil, String.zip("a", "1"))

    @Test
    def zip05(): Unit \ Assert =
        assertEq(expected = ('a', '1') :: ('b', '2') :: Nil, String.zip("ab", "12"))

    @Test
    def zip06(): Unit \ Assert =
        assertEq(expected = ('a', '1') :: ('b', '2') :: ('c', '3') :: Nil, String.zip("abc", "123"))

    @Test
    def zip07(): Unit \ Assert =
        assertEq(expected = ('a', '1') :: ('b', '2') :: ('c', '3') :: ('d', '4') :: Nil, String.zip("abcd", "1234"))

    /////////////////////////////////////////////////////////////////////////////
    // zipWith                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def zipWith01(): Unit \ Assert =
        let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "", "");
        assertEq(expected = "", s)

    @Test
    def zipWith02(): Unit \ Assert =
        let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "a", "");
        assertEq(expected = "", s)

    @Test
    def zipWith03(): Unit \ Assert =
        let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "", "1");
        assertEq(expected = "", s)

    @Test
    def zipWith04(): Unit \ Assert =
        let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_", "1");
        assertEq(expected = "1", s)

    @Test
    def zipWith05(): Unit \ Assert =
        let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "a", "1");
        assertEq(expected = "a", s)

    @Test
    def zipWith06(): Unit \ Assert =
        let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fgh", "12345678");
        assertEq(expected = "1bc45fgh", s)

    @Test
    def zipWith07(): Unit \ Assert =
        let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fghijkl", "12345678");
        assertEq(expected = "1bc45fgh", s)

    @Test
    def zipWith08(): Unit \ Assert =
        let s = String.zipWith((a, b) -> if (Char.isLetter(a)) a else b, "_bc__fgh", "1234567890");
        assertEq(expected = "1bc45fgh", s)

    /////////////////////////////////////////////////////////////////////////////
    // splitOn                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def splitOn01(): Unit \ Assert = assertEq(expected = "" :: Nil, String.splitOn({substr = ""}, ""))

    @Test
    def splitOn02(): Unit \ Assert = assertEq(expected = "a::b" :: Nil, String.splitOn({substr = ""}, "a::b"))

    @Test
    def splitOn03(): Unit \ Assert = assertEq(expected = "" :: Nil, String.splitOn({substr = "::"}, ""))

    @Test
    def splitOn04(): Unit \ Assert = assertEq(expected = "a" :: Nil, String.splitOn({substr = "::"}, "a"))

    @Test
    def splitOn05(): Unit \ Assert = assertEq(expected = "a:b" :: Nil, String.splitOn({substr = "::"}, "a:b"))

    @Test
    def splitOn06(): Unit \ Assert = assertEq(expected = "a" :: "b" :: Nil, String.splitOn({substr = "::"}, "a::b"))

    @Test
    def splitOn07(): Unit \ Assert = assertEq(expected = "" :: "a" :: "b" :: Nil, String.splitOn({substr = "::"}, "::a::b"))

    @Test
    def splitOn08(): Unit \ Assert = assertEq(expected = "a" :: "b" :: "" :: Nil, String.splitOn({substr = "::"}, "a::b::"))

    @Test
    def splitOn09(): Unit \ Assert = assertEq(expected = "" :: "a" :: "b" :: "" :: Nil, String.splitOn({substr = "::"}, "::a::b::"))

    @Test
    def splitOn10(): Unit \ Assert = assertEq(expected = "a:b" :: "c" :: Nil, String.splitOn({substr = "::"}, "a:b::c"))

    @Test
    def splitOn11(): Unit \ Assert = assertEq(expected = "a" :: "b:c" :: Nil, String.splitOn({substr = "::"}, "a::b:c"))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfLeftWithOffset                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfLeftWithOffset01(): Unit \ Assert = assertNone(String.indexOfLeftWithOffset({substr = ""}, {offset = 0}, ""))

    @Test
    def indexOfLeftWithOffset02(): Unit \ Assert = assertNone(String.indexOfLeftWithOffset({substr = ""}, {offset = 0}, "a"))

    @Test
    def indexOfLeftWithOffset03(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfLeftWithOffset({substr = "a"}, {offset = 0}, "a"))

    @Test
    def indexOfLeftWithOffset04(): Unit \ Assert = assertNone(String.indexOfLeftWithOffset({substr = "a"}, {offset = 1}, "a"))

    @Test
    def indexOfLeftWithOffset05(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfLeftWithOffset({substr = "a"}, {offset = 0}, "ab"))

    @Test
    def indexOfLeftWithOffset06(): Unit \ Assert = assertEq(expected = Some(1), String.indexOfLeftWithOffset({substr = "a"}, {offset = 1}, "aa"))

    @Test
    def indexOfLeftWithOffset07(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfLeftWithOffset({substr = "ab"}, {offset = 0}, "ab"))

    @Test
    def indexOfLeftWithOffset08(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfLeftWithOffset({substr = "ab"}, {offset = 0}, "abab"))

    @Test
    def indexOfLeftWithOffset09(): Unit \ Assert = assertEq(expected = Some(2), String.indexOfLeftWithOffset({substr = "ab"}, {offset = 1}, "abab"))

    @Test
    def indexOfLeftWithOffset10(): Unit \ Assert = assertEq(expected = Some(2), String.indexOfLeftWithOffset({substr = "ab"}, {offset = 2}, "abab"))

    @Test
    def indexOfLeftWithOffset11(): Unit \ Assert = assertNone(String.indexOfLeftWithOffset({substr = "ab"}, {offset = 3}, "abab"))

    /////////////////////////////////////////////////////////////////////////////
    // indexOfRightWithOffset                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def indexOfRightWithOffset01(): Unit \ Assert = assertNone(String.indexOfRightWithOffset({substr = ""}, {offset = 0}, ""))

    @Test
    def indexOfRightWithOffset02(): Unit \ Assert = assertNone(String.indexOfRightWithOffset({substr = ""}, {offset = 0}, "a"))

    @Test
    def indexOfRightWithOffset03(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfRightWithOffset({substr = "a"}, {offset = 0}, "a"))

    @Test
    def indexOfRightWithOffset04(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfRightWithOffset({substr = "a"}, {offset = 1}, "a"))

    @Test
    def indexOfRightWithOffset05(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfRightWithOffset({substr = "a"}, {offset = 2}, "ab"))

    @Test
    def indexOfRightWithOffset06(): Unit \ Assert = assertEq(expected = Some(1), String.indexOfRightWithOffset({substr = "a"}, {offset = 2}, "aa"))

    @Test
    def indexOfRightWithOffset07(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfRightWithOffset({substr = "ab"}, {offset = 2}, "ab"))

    @Test
    def indexOfRightWithOffset08(): Unit \ Assert = assertEq(expected = Some(2), String.indexOfRightWithOffset({substr = "ab"}, {offset = 3}, "abab"))

    @Test
    def indexOfRightWithOffset09(): Unit \ Assert = assertEq(expected = Some(2), String.indexOfRightWithOffset({substr = "ab"}, {offset = 2}, "abab"))

    @Test
    def indexOfRightWithOffset10(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfRightWithOffset({substr = "ab"}, {offset = 1}, "abab"))

    @Test
    def indexOfRightWithOffset11(): Unit \ Assert = assertEq(expected = Some(0), String.indexOfRightWithOffset({substr = "ab"}, {offset = 0}, "abab"))

    @Test
    def indexOfRightWithOffset12(): Unit \ Assert = assertNone(String.indexOfRightWithOffset({substr = "ab"}, {offset = -1}, "abab"))

    /////////////////////////////////////////////////////////////////////////////
    // toChunks                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChunks01(): Unit \ Assert = assertEq(expected = Nil, String.toChunks(0, ""))

    @Test
    def toChunks02(): Unit \ Assert = assertEq(expected = Nil, String.toChunks(0, "aaa"))

    @Test
    def toChunks03(): Unit \ Assert = assertEq(expected = Nil, String.toChunks(3, ""))

    @Test
    def toChunks04(): Unit \ Assert = assertEq(expected = "a" :: Nil, String.toChunks(3, "a"))

    @Test
    def toChunks05(): Unit \ Assert = assertEq(expected = "aa" :: Nil, String.toChunks(3, "aa"))

    @Test
    def toChunks06(): Unit \ Assert = assertEq(expected = "aaa" :: Nil, String.toChunks(3, "aaa"))

    @Test
    def toChunks07(): Unit \ Assert = assertEq(expected = "aaa" :: "b" :: Nil, String.toChunks(3, "aaab"))

    @Test
    def toChunks08(): Unit \ Assert = assertEq(expected = "aaa" :: "bb" :: Nil, String.toChunks(3, "aaabb"))

    @Test
    def toChunks09(): Unit \ Assert = assertEq(expected = "aaa" :: "bbb" :: Nil, String.toChunks(3, "aaabbb"))

    @Test
    def toChunks10(): Unit \ Assert = assertEq(expected = "aaa" :: "bbb" :: "c" :: Nil, String.toChunks(3, "aaabbbc"))

    @Test
    def toChunks11(): Unit \ Assert = assertEq(expected = "a" :: "a" :: "a" :: "b" :: "b" :: "b" :: "c" :: Nil, String.toChunks(1, "aaabbbc"))

    @Test
    def toChunks12(): Unit \ Assert = assertEq(expected = "aa" :: "ab" :: "bb" :: "c" :: Nil, String.toChunks(2, "aaabbbc"))

    @Test
    def toChunks13(): Unit \ Assert = assertEq(expected = "aaab" :: "bbc" :: Nil, String.toChunks(4, "aaabbbc"))

    @Test
    def toChunks14(): Unit \ Assert = assertEq(expected = "aaabbbc" :: Nil, String.toChunks(10, "aaabbbc"))

    /////////////////////////////////////////////////////////////////////////////
    // breakOnLeft                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def breakOnLeft01(): Unit \ Assert = assertEq(expected = ("", ""), String.breakOnLeft({substr = ""}, ""))

    @Test
    def breakOnLeft02(): Unit \ Assert = assertEq(expected = ("aaa", ""), String.breakOnLeft({substr = ""}, "aaa"))

    @Test
    def breakOnLeft03(): Unit \ Assert = assertEq(expected = ("", ""), String.breakOnLeft({substr = "::"}, ""))

    @Test
    def breakOnLeft04(): Unit \ Assert = assertEq(expected = ("aaa", ""), String.breakOnLeft({substr = "::"}, "aaa"))

    @Test
    def breakOnLeft05(): Unit \ Assert = assertEq(expected = ("aaa:", ""), String.breakOnLeft({substr = "::"}, "aaa:"))

    @Test
    def breakOnLeft06(): Unit \ Assert = assertEq(expected = ("aaa", "::"), String.breakOnLeft({substr = "::"}, "aaa::"))

    @Test
    def breakOnLeft07(): Unit \ Assert = assertEq(expected = ("aaa", "::bbb"), String.breakOnLeft({substr = "::"}, "aaa::bbb"))

    @Test
    def breakOnLeft08(): Unit \ Assert = assertEq(expected = ("aaa", "::bbb::ccc"), String.breakOnLeft({substr = "::"}, "aaa::bbb::ccc"))

    /////////////////////////////////////////////////////////////////////////////
    // breakOnRight                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def breakOnRight01(): Unit \ Assert = assertEq(expected = ("", ""), String.breakOnRight({substr = ""}, ""))

    @Test
    def breakOnRight02(): Unit \ Assert = assertEq(expected = ("aaa", ""), String.breakOnRight({substr = ""}, "aaa"))

    @Test
    def breakOnRight03(): Unit \ Assert = assertEq(expected = ("", ""), String.breakOnRight({substr = "::"}, ""))

    @Test
    def breakOnRight04(): Unit \ Assert = assertEq(expected = ("aaa", ""), String.breakOnRight({substr = "::"}, "aaa"))

    @Test
    def breakOnRight05(): Unit \ Assert = assertEq(expected = ("aaa:", ""), String.breakOnRight({substr = "::"}, "aaa:"))

    @Test
    def breakOnRight06(): Unit \ Assert = assertEq(expected = ("aaa::", ""), String.breakOnRight({substr = "::"}, "aaa::"))

    @Test
    def breakOnRight07(): Unit \ Assert = assertEq(expected = ("aaa::", "bbb"), String.breakOnRight({substr = "::"}, "aaa::bbb"))

    @Test
    def breakOnRight08(): Unit \ Assert = assertEq(expected = ("aaa::bbb::", "ccc"), String.breakOnRight({substr = "::"}, "aaa::bbb::ccc"))

    /////////////////////////////////////////////////////////////////////////////
    // countSubstring                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def countSubstring01(): Unit \ Assert = assertEq(expected = 0, String.countSubstring({substr = ""}, ""))

    @Test
    def countSubstring02(): Unit \ Assert = assertEq(expected = 0, String.countSubstring({substr = ""}, "abc abc"))

    @Test
    def countSubstring03(): Unit \ Assert = assertEq(expected = 0, String.countSubstring({substr = "abc"}, ""))

    @Test
    def countSubstring04(): Unit \ Assert = assertEq(expected = 1, String.countSubstring({substr = "abc"}, "abc"))

    @Test
    def countSubstring05(): Unit \ Assert = assertEq(expected = 1, String.countSubstring({substr = "abc"}, "aabc"))

    @Test
    def countSubstring06(): Unit \ Assert = assertEq(expected = 1, String.countSubstring({substr = "abc"}, "abcc"))

    @Test
    def countSubstring07(): Unit \ Assert = assertEq(expected = 1, String.countSubstring({substr = "abc"}, "abc def "))

    @Test
    def countSubstring08(): Unit \ Assert = assertEq(expected = 1, String.countSubstring({substr = "abc"}, " abc def"))

    @Test
    def countSubstring09(): Unit \ Assert = assertEq(expected = 2, String.countSubstring({substr = "abc"}, " abc abc"))

    @Test
    def countSubstring10(): Unit \ Assert = assertEq(expected = 2, String.countSubstring({substr = "abc"}, " abc ABC abc"))

    @Test
    def countSubstring11(): Unit \ Assert = assertEq(expected = 2, String.countSubstring({substr = "abc"}, "ABC abc def abc"))

    @Test
    def countSubstring12(): Unit \ Assert = assertEq(expected = 2, String.countSubstring({substr = "abc"}, "aBC::abc::AbC::abc::Abc"))

    /////////////////////////////////////////////////////////////////////////////
    // stripMargin                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stripMargin01(): Unit \ Assert = assertEq(expected = "", String.stripMargin(""))

    @Test
    def stripMargin02(): Unit \ Assert = assertEq(expected = "abc", String.stripMargin("abc"))

    @Test
    def stripMargin03(): Unit \ Assert = assertEq(expected = "${'\t'}", String.stripMargin("${'\t'}"))

    @Test
    def stripMargin04(): Unit \ Assert = assertEq(expected = "abc", String.stripMargin("    |abc"))

    @Test
    def stripMargin05(): Unit \ Assert = assertEq(expected = "abc", String.stripMargin("${'\t'} |abc"))

    @Test
    def stripMargin06(): Unit \ Assert = assertEq(expected = " abc", String.stripMargin(" | abc"))

    @Test
    def stripMargin07(): Unit \ Assert = assertEq(expected = "|abc", String.stripMargin(" ||abc"))

    @Test
    def stripMargin08(): Unit \ Assert = assertEq(expected = String.intercalate("\n", "abc" :: "${'\t'}def" :: "ghi" :: Nil), String.stripMargin(String.intercalate("\n", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)))

    @Test
    def stripMargin09(): Unit \ Assert = assertEq(expected = String.intercalate("\u000D\u000A", "abc" :: "${'\t'}def" :: "ghi" :: Nil), String.stripMargin(String.intercalate("\u000D\u000A", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)))

    /////////////////////////////////////////////////////////////////////////////
    // stripMarginWith                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stripMarginWith01(): Unit \ Assert = assertEq(expected = "", String.stripMarginWith({margin = "|"}, ""))

    @Test
    def stripMarginWith02(): Unit \ Assert = assertEq(expected = "abc", String.stripMarginWith({margin = "|"}, "abc"))

    @Test
    def stripMarginWith03(): Unit \ Assert = assertEq(expected = "${'\t'}", String.stripMarginWith({margin = "|"}, "${'\t'}"))

    @Test
    def stripMarginWith04(): Unit \ Assert = assertEq(expected = "abc", String.stripMarginWith({margin = "|"}, "    |abc"))

    @Test
    def stripMarginWith05(): Unit \ Assert = assertEq(expected = "abc", String.stripMarginWith({margin = "|"}, "${'\t'} |abc"))

    @Test
    def stripMarginWith06(): Unit \ Assert = assertEq(expected = " abc", String.stripMarginWith({margin = "|"}, " | abc"))

    @Test
    def stripMarginWith07(): Unit \ Assert = assertEq(expected = "|abc", String.stripMarginWith({margin = "|"}, " ||abc"))

    @Test
    def stripMarginWith08(): Unit \ Assert = assertEq(expected = "abc", String.stripMarginWith({margin = "| "}, " | abc"))

    @Test
    def stripMarginWith09(): Unit \ Assert = assertEq(expected = String.intercalate("\n", "abc" :: "${'\t'}def" :: "ghi" :: Nil), String.stripMarginWith({margin = "|"}, String.intercalate("\n", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)))

    @Test
    def stripMarginWith10(): Unit \ Assert = assertEq(expected = String.intercalate("\u000D\u000A", "abc" :: "${'\t'}def" :: "ghi" :: Nil), String.stripMarginWith({margin = "|"}, String.intercalate("\u000D\u000A", " |abc" :: "${'\t'}def" :: "${'\t'}|ghi" :: Nil)))


    /////////////////////////////////////////////////////////////////////////////
    // stripPrefix                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stripPrefix01(): Unit \ Assert = assertEq(expected = Some(""), String.stripPrefix({substr = ""}, ""))

    @Test
    def stripPrefix02(): Unit \ Assert = assertEq(expected = Some("abc"), String.stripPrefix({substr = ""}, "abc"))

    @Test
    def stripPrefix03(): Unit \ Assert = assertNone(String.stripPrefix({substr = "abc"}, ""))

    @Test
    def stripPrefix04(): Unit \ Assert = assertEq(expected = Some(""), String.stripPrefix({substr = "abc"}, "abc"))

    @Test
    def stripPrefix05(): Unit \ Assert = assertEq(expected = Some("def"), String.stripPrefix({substr = "abc"}, "abcdef"))

    @Test
    def stripPrefix06(): Unit \ Assert = assertNone(String.stripPrefix({substr = "def"}, "abcdef"))

    @Test
    def stripPrefix07(): Unit \ Assert = assertNone(String.stripPrefix({substr = "abc"}, "ABCDEF"))

    /////////////////////////////////////////////////////////////////////////////
    // stripSuffix                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def stripSuffix01(): Unit \ Assert = assertEq(expected = Some(""), String.stripSuffix({substr = ""}, ""))

    @Test
    def stripSuffix02(): Unit \ Assert = assertEq(expected = Some("abc"), String.stripSuffix({substr = ""}, "abc"))

    @Test
    def stripSuffix03(): Unit \ Assert = assertNone(String.stripSuffix({substr = "abc"}, ""))

    @Test
    def stripSuffix04(): Unit \ Assert = assertEq(expected = Some(""), String.stripSuffix({substr = "abc"}, "abc"))

    @Test
    def stripSuffix05(): Unit \ Assert = assertNone(String.stripSuffix({substr = "abc"}, "abcdef"))

    @Test
    def stripSuffix06(): Unit \ Assert = assertEq(expected = Some("abc"), String.stripSuffix({substr = "def"}, "abcdef"))

    @Test
    def stripSuffix07(): Unit \ Assert = assertNone(String.stripPrefix({substr = "abc"}, "ABCDEF"))

    /////////////////////////////////////////////////////////////////////////////
    // wrap                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def wrap01(): Unit \ Assert = assertEq(expected = "", String.wrap(0, "a"))

    @Test
    def wrap02(): Unit \ Assert = assertEq(expected = "a", String.wrap(1, "a"))

    @Test
    def wrap03(): Unit \ Assert = assertEq(expected = "a", String.wrap(2, "a"))

    @Test
    def wrap04(): Unit \ Assert = assertEq(expected = "ab", String.wrap(2, "ab"))

    @Test
    def wrap05(): Unit \ Assert = assertEq(expected = "a" + String.lineSeparator() + "b", String.wrap(1, "ab"))

    @Test
    def wrap06(): Unit \ Assert = assertEq(expected = "a" + String.lineSeparator() + "b", String.wrap(2, "a b"))

    @Test
    def wrap07(): Unit \ Assert = assertEq(expected = "a" + String.lineSeparator() + "b" + String.lineSeparator() + "c", String.wrap(2, "a b c"))

    @Test
    def wrap08(): Unit \ Assert = assertEq(expected = "ab" + String.lineSeparator() + "cd", String.wrap(2, "ab   cd"))

    @Test
    def wrap09(): Unit \ Assert = assertEq(expected = "ab" + String.lineSeparator() + "cd", String.wrap(3, "ab   cd"))

    @Test
    def wrap10(): Unit \ Assert = assertEq(expected = "ab" + String.lineSeparator() + "cd", String.wrap(5, "ab   cd"))

    @Test
    def wrap11(): Unit \ Assert =
        assertEq(expected = "Lorem" + String.lineSeparator()
            + "ipsum" + String.lineSeparator()
            + "dolor" + String.lineSeparator()
            + "sitam" + String.lineSeparator()
            + "et", String.wrap(5, "Lorem ipsum dolor sitamet"))

    /////////////////////////////////////////////////////////////////////////////
    // unwrap                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def unwrap01(): Unit \ Assert = assertEq(expected = "", String.unwrap(""))

    @Test
    def unwrap02(): Unit \ Assert = assertEq(expected = "a b", String.unwrap("a\nb"))

    @Test
    def unwrap03(): Unit \ Assert = assertEq(expected = "a b c", String.unwrap("a\nb\nc"))

    @Test
    def unwrap04(): Unit \ Assert = assertEq(expected = "ab c d", String.unwrap("ab\nc\nd"))

    @Test
    def unwrap05(): Unit \ Assert = assertEq(expected = "a b", String.unwrap("a\rb"))

    @Test
    def unwrap06(): Unit \ Assert = assertEq(expected = "a b c", String.unwrap("a\rb\rc"))

    @Test
    def unwrap07(): Unit \ Assert = assertEq(expected = "ab c d", String.unwrap("ab\rc\rd"))

    @Test
    def unwrap08(): Unit \ Assert = assertEq(expected = "a b", String.unwrap("a\r\nb"))

    @Test
    def unwrap09(): Unit \ Assert = assertEq(expected = "a b c", String.unwrap("a\r\nb\r\nc"))

    @Test
    def unwrap10(): Unit \ Assert = assertEq(expected = "ab c d", String.unwrap("ab\r\nc\r\nd"))

    /////////////////////////////////////////////////////////////////////////////
    // escapeInterpolation                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def escapeInterpolation01(): Unit \ Assert = assertEq(expected = ("$" + "{"), "\${")

    @Test
    def escapeInterpolation02(): Unit \ Assert = assertEq(expected = ("$" + "{1 + {2}"), "\${1 + {2}")

    @Test
    def escapeInterpolation03(): Unit \ Assert = assertEq(expected = ("$" + "{}"), "\${}")

    @Test
    def escapeInterpolation04(): Unit \ Assert = assertEq(expected = "$", "$")

    /////////////////////////////////////////////////////////////////////////////
    // iterator                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def iterator01(): Unit \ Assert = region rc {
        assertEq(expected = Nil, String.iterator(rc, "") |> Iterator.toList)
    }

    @Test
    def iterator02(): Unit \ Assert = region rc {
        assertEq(expected = 'a' :: 'b' :: 'c' :: Nil, String.iterator(rc, "abc") |> Iterator.toList)
    }

}

