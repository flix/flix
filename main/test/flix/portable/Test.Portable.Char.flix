mod Test.Portable.Char {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def isLetterAndDigit01(): Unit = {
        let _ = assertTrue(Char.isLetter('A'), "isLetterAndDigit01 failed: A should be a letter.");
        let _ = assertTrue(not Char.isLetter('\u2167'), "isLetterAndDigit01 failed: roman numeral eight is not a letter.");
        let _ = assertTrue(Char.isDigit('\u0661'), "isLetterAndDigit01 failed: arabic-indic digit one should be a digit.");
        let _ = assertTrue(Char.isLetterOrDigit('\u0661'), "isLetterAndDigit01 failed: digit one should be letter-or-digit.");
        let _ = assertTrue(not Char.isLetterOrDigit('!'), "isLetterAndDigit01 failed: ! should not be letter-or-digit.");
        ()
    }

    @Test
    def whitespace01(): Unit = {
        let _ = assertTrue(Char.isWhitespace(' '), "whitespace01 failed: space should be whitespace.");
        let _ = assertTrue(Char.isWhitespace('\u001C'), "whitespace01 failed: U+001C should be whitespace.");
        let _ = assertTrue(not Char.isWhitespace('\u00A0'), "whitespace01 failed: NBSP should not be whitespace.");
        let _ = assertTrue(not Char.isWhitespace('\u0085'), "whitespace01 failed: NEL should not be whitespace.");
        ()
    }

    @Test
    def casingAndTitleCase01(): Unit = {
        let _ = assertTrue(Char.isLowerCase('a'), "casingAndTitleCase01 failed: a should be lowercase.");
        let _ = assertTrue(Char.isUpperCase('A'), "casingAndTitleCase01 failed: A should be uppercase.");
        let _ = assertTrue(Char.isLowerCase('\u02B0'), "casingAndTitleCase01 failed: U+02B0 should be lowercase (Other_Lowercase).");
        let _ = assertTrue(Char.isUpperCase('\u212A'), "casingAndTitleCase01 failed: U+212A (Kelvin sign) should be uppercase.");

        let _ = assertTrue(Char.isTitleCase('\u01C5'), "casingAndTitleCase01 failed: U+01C5 should be titlecase.");

        let _ = assertTrue(Char.toTitleCase('\u01C6') == '\u01C5', "casingAndTitleCase01 failed: U+01C6 -> U+01C5.");
        ()
    }

    @Test
    def numericAndDigit01(): Unit = {
        let _ = assertTrue(Char.getNumericValue('\u2167') == Some(8), "numericAndDigit01 failed: roman numeral eight numeric value.");
        let _ = assertTrue(Char.digit({radix = 16}, 'A') == Some(10), "numericAndDigit01 failed: digit(A,16) == 10.");
        let _ = assertTrue(Char.digit({radix = 10}, '\u0661') == Some(1), "numericAndDigit01 failed: digit(arabic-indic one,10) == 1.");
        let _ = assertTrue(Char.digit({radix = 10}, '\u2167') == None, "numericAndDigit01 failed: digit(roman numeral eight,10) == None.");
        let _ = assertTrue(Char.forDigit({radix = 16}, 10) == Some('a'), "numericAndDigit01 failed: forDigit(16,10) == a.");
        ()
    }

    @Test
    def surrogate01(): Unit = {
        let high = '\uD83D';
        let low = '\uDE00';
        let _ = assertTrue(Char.isSurrogate(high), "surrogate01 failed: high surrogate should be surrogate.");
        let _ = assertTrue(Char.isSurrogatePair({high = high}, {low = low}), "surrogate01 failed: pair should be surrogate pair.");
        let _ = assertTrue(Char.toSupplementaryCodePoint({high = high}, {low = low}) == 0x1F600, "surrogate01 failed: toSupplementaryCodePoint should be U+1F600.");
        ()
    }

    @Test
    def misc01(): Unit = {
        let _ = assertTrue(Char.isDefined('A'), "misc01 failed: A should be defined.");
        let _ = assertTrue(not Char.isDefined('\u0378'), "misc01 failed: U+0378 should not be defined.");
        let _ = assertTrue(Char.isISOControl('\u001F'), "misc01 failed: U+001F should be ISO control.");
        let _ = assertTrue(not Char.isISOControl('A'), "misc01 failed: A should not be ISO control.");
        let _ = assertTrue(Char.isMirrored('('), "misc01 failed: '(' should be mirrored.");
        let _ = assertTrue(not Char.isMirrored('A'), "misc01 failed: A should not be mirrored.");
        ()
    }

    pub def runAll(): Unit = {
        let _ = isLetterAndDigit01();
        let _ = whitespace01();
        let _ = casingAndTitleCase01();
        let _ = numericAndDigit01();
        let _ = surrogate01();
        let _ = misc01();
        ()
    }

}

