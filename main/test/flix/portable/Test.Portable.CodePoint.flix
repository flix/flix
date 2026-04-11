mod Test.Portable.CodePoint {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def predicates01(): Unit = {
        let _ = assertTrue(CodePoint.isAlphabetic(0x0061), "predicates01 failed: alphabetic a.");
        let _ = assertTrue(not CodePoint.isAlphabetic(0x0031), "predicates01 failed: alphabetic 1.");
        let _ = assertTrue(CodePoint.isIdeographic(0x4E00), "predicates01 failed: ideographic U+4E00.");
        assertTrue(not CodePoint.isIdeographic(0x0041), "predicates01 failed: ideographic A.")
    }

    @Test
    def caseMapping01(): Unit = {
        let _ = assertTrue(CodePoint.toLowerCase(0x0041) == 0x0061, "caseMapping01 failed: A -> a.");
        let _ = assertTrue(CodePoint.toUpperCase(0x0061) == 0x0041, "caseMapping01 failed: a -> A.");
        let _ = assertTrue(CodePoint.toTitleCase(0x01F3) == 0x01F2, "caseMapping01 failed: dz -> Dz.");
        assertTrue(CodePoint.toUpperCase(0x1F3DD) == 0x1F3DD, "caseMapping01 failed: pictogram unchanged.")
    }

    @Test
    def numeric01(): Unit = {
        let _ = assertTrue(CodePoint.getNumericValue(0x0031) == Some(1i32), "numeric01 failed: '1'.");
        let _ = assertTrue(CodePoint.getNumericValue(0x0041) == Some(10i32), "numeric01 failed: 'A'.");
        assertTrue(CodePoint.getNumericValue(0x1F3DD) == None, "numeric01 failed: pictogram.")
    }

    @Test
    def charsAndStrings01(): Unit = {
        let cp = 0x1F3DD;
        let _ = assertTrue(CodePoint.charCount(cp) == 2i32, "charsAndStrings01 failed: charCount.");
        let _ = assertTrue(CodePoint.toChars(cp) == Some(Vector#{'\uD83C', '\uDFDD'}), "charsAndStrings01 failed: toChars.");
        let _ = assertTrue(CodePoint.toString(cp) == "\uD83C\uDFDD", "charsAndStrings01 failed: toString.");
        let _ = assertTrue(CodePoint.highSurrogate(cp) == Some('\uD83C'), "charsAndStrings01 failed: high surrogate.");
        assertTrue(CodePoint.lowSurrogate(cp) == Some('\uDFDD'), "charsAndStrings01 failed: low surrogate.")
    }

    @Test
    def namesAndValidity01(): Unit = {
        let _ = assertTrue(CodePoint.getName(0x0061) != None, "namesAndValidity01 failed: name for 'a'.");
        let _ = assertTrue(CodePoint.getName(0x4E00) != None, "namesAndValidity01 failed: fallback name for U+4E00.");
        let _ = assertTrue(CodePoint.isValidCodePoint(0x10FFFF), "namesAndValidity01 failed: max valid.");
        assertTrue(CodePoint.getName(0x110000) == None, "namesAndValidity01 failed: invalid name.")
    }

    pub def runAll(): Unit = {
        let _ = predicates01();
        let _ = caseMapping01();
        let _ = numeric01();
        let _ = charsAndStrings01();
        let _ = namesAndValidity01();
        ()
    }

}
