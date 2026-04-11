mod Test.Portable.CodePoint {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def predicates01(): Unit = {
        let _ = assertTrue(CodePoint.isLetter(0x0061), "predicates01 failed: letter a.");
        let _ = assertTrue(CodePoint.isDigit(0x0031), "predicates01 failed: digit 1.");
        let _ = assertTrue(CodePoint.isLetterOrDigit(0x01F2), "predicates01 failed: Dz.");
        let _ = assertTrue(CodePoint.isAlphabetic(0x01F3), "predicates01 failed: alphabetic dz.");
        let _ = assertTrue(CodePoint.isIdeographic(0x4E00), "predicates01 failed: ideographic U+4E00.");
        let _ = assertTrue(CodePoint.isDefined(0x1F3DD), "predicates01 failed: defined pictogram.");
        let _ = assertTrue(CodePoint.isSupplementaryCodePoint(0x1F3DD), "predicates01 failed: supplementary.");
        assertTrue(not CodePoint.isValidCodePoint(0x110000), "predicates01 failed: invalid max+1.")
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
        let _ = assertTrue(CodePoint.getNumericValue(0x0066) == Some(15i32), "numeric01 failed: 'f'.");
        assertTrue(CodePoint.getNumericValue(0x1F3DD) == None, "numeric01 failed: pictogram.")
    }

    @Test
    def charsAndStrings01(): Unit = {
        let cp = 0x1F3DD;
        let _ = assertTrue(CodePoint.charCount(0x0041) == 1i32, "charsAndStrings01 failed: charCount A.");
        let _ = assertTrue(CodePoint.charCount(cp) == 2i32, "charsAndStrings01 failed: charCount pictogram.");
        let _ = assertTrue(CodePoint.toChars(cp) == Some(Vector#{'\uD83C', '\uDFDD'}), "charsAndStrings01 failed: toChars.");
        let _ = assertTrue(CodePoint.toBmpChar(0x0041) == Some('A'), "charsAndStrings01 failed: toBmpChar.");
        let _ = assertTrue(CodePoint.toSupplementaryChars(cp) == Some(('\uD83C', '\uDFDD')), "charsAndStrings01 failed: toSupplementaryChars.");
        assertTrue(CodePoint.toString(cp) == "\uD83C\uDFDD", "charsAndStrings01 failed: toString.")
    }

    @Test
    def getName01(): Unit = {
        let _ = assertTrue(CodePoint.getName(0x0061) == Some("LATIN SMALL LETTER A"), "getName01 failed: U+0061.");
        let _ = assertTrue(CodePoint.getName(0x0041) == Some("LATIN CAPITAL LETTER A"), "getName01 failed: U+0041.");
        let _ = assertTrue(CodePoint.getName(0x1F3DD) == Some("DESERT ISLAND"), "getName01 failed: U+1F3DD.");
        let _ = assertTrue(CodePoint.getName(0x4E00) == Some("CJK UNIFIED IDEOGRAPHS 4E00"), "getName01 failed: fallback U+4E00.");
        assertTrue(CodePoint.getName(0x110000) == None, "getName01 failed: invalid.")
    }

    pub def runAll(): Unit = {
        let _ = predicates01();
        let _ = caseMapping01();
        let _ = numeric01();
        let _ = charsAndStrings01();
        let _ = getName01();
        ()
    }

}
