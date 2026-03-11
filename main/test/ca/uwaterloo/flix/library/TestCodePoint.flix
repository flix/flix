mod TestCodePoint {

    use Assert.{assertEq, assertTrue, assertFalse, assertSome, assertNone};

    /////////////////////////////////////////////////////////////////////////////
    // isAscii                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isAscii01(): Unit \ Assert = assertTrue(CodePoint.isAscii(0x0061))       // 'a'

    @Test
    def isAscii02(): Unit \ Assert = assertTrue(CodePoint.isAscii(0x0041))       // 'A'

    @Test
    def isAscii03(): Unit \ Assert = assertTrue(CodePoint.isAscii(0x0020))       // ' '

    @Test
    def isAscii04(): Unit \ Assert = assertFalse(CodePoint.isAscii(0x00b1))      // plus-minus sign

    @Test
    def isAscii05(): Unit \ Assert = assertFalse(CodePoint.isAscii(0x01F2))      // Dz (single char, upper case)

    @Test
    def isAscii06(): Unit \ Assert = assertFalse(CodePoint.isAscii(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isLetter                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isLetter01(): Unit \ Assert = assertTrue(CodePoint.isLetter(0x0061))     // 'a'

    @Test
    def isLetter02(): Unit \ Assert = assertTrue(CodePoint.isLetter(0x0041))     // 'A'

    @Test
    def isLetter03(): Unit \ Assert = assertFalse(CodePoint.isLetter(0x0020))    // ' '

    @Test
    def isLetter04(): Unit \ Assert = assertFalse(CodePoint.isLetter(0x00b1))    // plus-minus sign

    @Test
    def isLetter05(): Unit \ Assert = assertTrue(CodePoint.isLetter(0x01F2))      // Dz (single char, upper case)

    @Test
    def isLetter06(): Unit \ Assert = assertFalse(CodePoint.isLetter(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isDigit                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isDigit01(): Unit \ Assert = assertTrue(CodePoint.isDigit(0x0031))       // '1'

    @Test
    def isDigit02(): Unit \ Assert = assertFalse(CodePoint.isDigit(0x0061))      // 'a'

    @Test
    def isDigit03(): Unit \ Assert = assertFalse(CodePoint.isDigit(0x0041))      // 'A'

    @Test
    def isDigit04(): Unit \ Assert = assertFalse(CodePoint.isDigit(0x0020))      // ' '

    @Test
    def isDigit05(): Unit \ Assert = assertFalse(CodePoint.isDigit(0x00b1))      // plus-minus sign

    @Test
    def isDigit06(): Unit \ Assert = assertFalse(CodePoint.isDigit(0x01F2))      // Dz (single char, upper case)

    @Test
    def isDigit07(): Unit \ Assert = assertFalse(CodePoint.isDigit(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isLetterOrDigit                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isLetterOrDigit01(): Unit \ Assert = assertTrue(CodePoint.isLetterOrDigit(0x0031))       // '1'

    @Test
    def isLetterOrDigit02(): Unit \ Assert = assertTrue(CodePoint.isLetterOrDigit(0x0061))      // 'a'

    @Test
    def isLetterOrDigit03(): Unit \ Assert = assertTrue(CodePoint.isLetterOrDigit(0x0041))      // 'A'

    @Test
    def isLetterOrDigit04(): Unit \ Assert = assertFalse(CodePoint.isLetterOrDigit(0x0020))      // ' '

    @Test
    def isLetterOrDigit05(): Unit \ Assert = assertFalse(CodePoint.isLetterOrDigit(0x00b1))      // plus-minus sign

    @Test
    def isLetterOrDigit06(): Unit \ Assert = assertTrue(CodePoint.isLetterOrDigit(0x01F2))      // Dz (single char, upper case)

    @Test
    def isLetterOrDigit07(): Unit \ Assert = assertFalse(CodePoint.isLetterOrDigit(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isAsciiDigit                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isAsciiDigit01(): Unit \ Assert = assertTrue(CodePoint.isAsciiDigit(0x0031))       // '1'

    @Test
    def isAsciiDigit02(): Unit \ Assert = assertFalse(CodePoint.isAsciiDigit(0x0061))     // 'a'

    @Test
    def isAsciiDigit03(): Unit \ Assert = assertFalse(CodePoint.isAsciiDigit(0x0041))     // 'A'

    @Test
    def isAsciiDigit04(): Unit \ Assert = assertFalse(CodePoint.isAsciiDigit(0x0020))      // ' '

    @Test
    def isAsciiDigit05(): Unit \ Assert = assertFalse(CodePoint.isAsciiDigit(0x00b1))      // plus-minus sign

    @Test
    def isAsciiDigit06(): Unit \ Assert = assertFalse(CodePoint.isAsciiDigit(0x01F2))      // Dz (single char, upper case)

    @Test
    def isAsciiDigit07(): Unit \ Assert = assertFalse(CodePoint.isAsciiDigit(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isOctDigit                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isOctDigit01(): Unit \ Assert = assertTrue(CodePoint.isOctDigit(0x0031))     // '1'

    @Test
    def isOctDigit02(): Unit \ Assert = assertFalse(CodePoint.isOctDigit(0x0061))    // 'a'

    @Test
    def isOctDigit03(): Unit \ Assert = assertFalse(CodePoint.isOctDigit(0x0041))    // 'A'

    @Test
    def isOctDigit04(): Unit \ Assert = assertFalse(CodePoint.isOctDigit(0x0020))    // ' '

    @Test
    def isOctDigit05(): Unit \ Assert = assertFalse(CodePoint.isOctDigit(0x00b1))    // plus-minus sign

    @Test
    def isOctDigit06(): Unit \ Assert = assertFalse(CodePoint.isOctDigit(0x01F2))    // Dz (single char, upper case)

    @Test
    def isOctDigit07(): Unit \ Assert = assertFalse(CodePoint.isOctDigit(0x1f3dd))   // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isHexDigit                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isHexDigit01(): Unit \ Assert = assertTrue(CodePoint.isHexDigit(0x0031))     // '1'

    @Test
    def isHexDigit02(): Unit \ Assert = assertTrue(CodePoint.isHexDigit(0x0061))     // 'a'

    @Test
    def isHexDigit03(): Unit \ Assert = assertTrue(CodePoint.isHexDigit(0x0041))     // 'A'

    @Test
    def isHexDigit04(): Unit \ Assert = assertFalse(CodePoint.isHexDigit(0x0020))    // ' '

    @Test
    def isHexDigit05(): Unit \ Assert = assertFalse(CodePoint.isHexDigit(0x00b1))    // plus-minus sign

    @Test
    def isHexDigit06(): Unit \ Assert = assertFalse(CodePoint.isHexDigit(0x01F2))    // Dz (single char, upper case)

    @Test
    def isHexDigit07(): Unit \ Assert = assertFalse(CodePoint.isHexDigit(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isLowerCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isLowerCase01(): Unit \ Assert = assertFalse(CodePoint.isLowerCase(0x0031))    // '1'

    @Test
    def isLowerCase02(): Unit \ Assert = assertTrue(CodePoint.isLowerCase(0x0061))     // 'a'

    @Test
    def isLowerCase03(): Unit \ Assert = assertFalse(CodePoint.isLowerCase(0x0041))    // 'A'

    @Test
    def isLowerCase04(): Unit \ Assert = assertFalse(CodePoint.isLowerCase(0x0020))    // ' '

    @Test
    def isLowerCase05(): Unit \ Assert = assertFalse(CodePoint.isLowerCase(0x00b1))    // plus-minus sign

    @Test
    def isLowerCase06(): Unit \ Assert = assertFalse(CodePoint.isLowerCase(0x01F2))    // Dz (single char, upper case)

    @Test
    def isLowerCase07(): Unit \ Assert = assertTrue(CodePoint.isLowerCase(0x01F3))     // dz (single char)

    @Test
    def isLowerCase08(): Unit \ Assert = assertFalse(CodePoint.isLowerCase(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isUpperCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isUpperCase01(): Unit \ Assert = assertFalse(CodePoint.isUpperCase(0x0031))    // '1'

    @Test
    def isUpperCase02(): Unit \ Assert = assertFalse(CodePoint.isUpperCase(0x0061))   // 'a'

    @Test
    def isUpperCase03(): Unit \ Assert = assertTrue(CodePoint.isUpperCase(0x0041))    // 'A'

    @Test
    def isUpperCase04(): Unit \ Assert = assertFalse(CodePoint.isUpperCase(0x0020))    // ' '

    @Test
    def isUpperCase05(): Unit \ Assert = assertFalse(CodePoint.isUpperCase(0x00b1))    // plus-minus sign

    @Test
    def isUpperCase06(): Unit \ Assert = assertFalse(CodePoint.isUpperCase(0x01F2))    // Dz (single char, title case)

    @Test
    def isUpperCase07(): Unit \ Assert = assertFalse(CodePoint.isUpperCase(0x01F3))     // dz (single char)

    @Test
    def isUpperCase08(): Unit \ Assert = assertFalse(CodePoint.isUpperCase(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isTitleCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isTitleCase01(): Unit \ Assert = assertFalse(CodePoint.isTitleCase(0x0031))      // '1'

    @Test
    def isTitleCase02(): Unit \ Assert = assertFalse(CodePoint.isTitleCase(0x0061))      // 'a'

    @Test
    def isTitleCase03(): Unit \ Assert = assertFalse(CodePoint.isTitleCase(0x0041))      // 'A'

    @Test
    def isTitleCase04(): Unit \ Assert = assertFalse(CodePoint.isTitleCase(0x0020))      // ' '

    @Test
    def isTitleCase05(): Unit \ Assert = assertFalse(CodePoint.isTitleCase(0x00b1))      // plus-minus sign

    @Test
    def isTitleCase06(): Unit \ Assert = assertTrue(CodePoint.isTitleCase(0x01F2))       // Dz (single char, title case)

    @Test
    def isTitleCase07(): Unit \ Assert = assertFalse(CodePoint.isTitleCase(0x01F3))     // dz (single char)

    @Test
    def isTitleCase08(): Unit \ Assert = assertFalse(CodePoint.isTitleCase(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isWhitespace                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isWhitespace01(): Unit \ Assert = assertFalse(CodePoint.isWhitespace(0x0031))      // '1'

    @Test
    def isWhitespace02(): Unit \ Assert = assertFalse(CodePoint.isWhitespace(0x0061))      // 'a'

    @Test
    def isWhitespace03(): Unit \ Assert = assertFalse(CodePoint.isWhitespace(0x0041))      // 'A'

    @Test
    def isWhitespace04(): Unit \ Assert = assertTrue(CodePoint.isWhitespace(0x0020))      // ' '

    @Test
    def isWhitespace05(): Unit \ Assert = assertFalse(CodePoint.isWhitespace(0x00b1))      // plus-minus sign

    @Test
    def isWhitespace06(): Unit \ Assert = assertFalse(CodePoint.isWhitespace(0x01F2))      // Dz (single char, title case)

    @Test
    def isWhitespace07(): Unit \ Assert = assertFalse(CodePoint.isWhitespace(0x01F3))      // dz (single char)

    @Test
    def isWhitespace08(): Unit \ Assert = assertFalse(CodePoint.isWhitespace(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isAlphabetic                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isAlphabetic01(): Unit \ Assert = assertFalse(CodePoint.isAlphabetic(0x0031))      // '1'

    @Test
    def isAlphabetic02(): Unit \ Assert = assertTrue(CodePoint.isAlphabetic(0x0061))       // 'a'

    @Test
    def isAlphabetic03(): Unit \ Assert = assertTrue(CodePoint.isAlphabetic(0x0041))       // 'A'

    @Test
    def isAlphabetic04(): Unit \ Assert = assertFalse(CodePoint.isAlphabetic(0x0020))      // ' '

    @Test
    def isAlphabetic05(): Unit \ Assert = assertFalse(CodePoint.isAlphabetic(0x00b1))      // plus-minus sign

    @Test
    def isAlphabetic06(): Unit \ Assert = assertTrue(CodePoint.isAlphabetic(0x01F2))       // Dz (single char, title case)

    @Test
    def isAlphabetic07(): Unit \ Assert = assertTrue(CodePoint.isAlphabetic(0x01F3))       // dz (single char)

    @Test
    def isAlphabetic08(): Unit \ Assert = assertFalse(CodePoint.isAlphabetic(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isBmpCodePoint                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isBmpCodePoint01(): Unit \ Assert = assertTrue(CodePoint.isBmpCodePoint(0x0031))       // '1'

    @Test
    def isBmpCodePoint02(): Unit \ Assert = assertTrue(CodePoint.isBmpCodePoint(0x0061))       // 'a'

    @Test
    def isBmpCodePoint03(): Unit \ Assert = assertTrue(CodePoint.isBmpCodePoint(0x0041))       // 'A'

    @Test
    def isBmpCodePoint04(): Unit \ Assert = assertTrue(CodePoint.isBmpCodePoint(0x0020))       // ' '

    @Test
    def isBmpCodePoint05(): Unit \ Assert = assertTrue(CodePoint.isBmpCodePoint(0x00b1))       // plus-minus sign

    @Test
    def isBmpCodePoint06(): Unit \ Assert = assertTrue(CodePoint.isBmpCodePoint(0x01F2))       // Dz (single char, title case)

    @Test
    def isBmpCodePoint07(): Unit \ Assert = assertTrue(CodePoint.isBmpCodePoint(0x01F3))       // dz (single char)

    @Test
    def isBmpCodePoint08(): Unit \ Assert = assertFalse(CodePoint.isBmpCodePoint(0x1f3dd))     // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isDefined                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isDefined01(): Unit \ Assert = assertTrue(CodePoint.isDefined(0x0031))       // '1'

    @Test
    def isDefined02(): Unit \ Assert = assertTrue(CodePoint.isDefined(0x0061))       // 'a'

    @Test
    def isDefined03(): Unit \ Assert = assertTrue(CodePoint.isDefined(0x0041))       // 'A'

    @Test
    def isDefined04(): Unit \ Assert = assertTrue(CodePoint.isDefined(0x0020))       // ' '

    @Test
    def isDefined05(): Unit \ Assert = assertTrue(CodePoint.isDefined(0x00b1))       // plus-minus sign

    @Test
    def isDefined06(): Unit \ Assert = assertTrue(CodePoint.isDefined(0x01F2))       // Dz (single char, title case)

    @Test
    def isDefined07(): Unit \ Assert = assertTrue(CodePoint.isDefined(0x01F3))       // dz (single char)

    @Test
    def isDefined08(): Unit \ Assert = assertTrue(CodePoint.isDefined(0x1f3dd))      // Desert Island pictogram

    @Test
    def isDefined09(): Unit \ Assert = assertFalse(CodePoint.isDefined(CodePoint.minValue()-1))

    @Test
    def isDefined10(): Unit \ Assert = assertFalse(CodePoint.isDefined(CodePoint.maxValue()+1))

    /////////////////////////////////////////////////////////////////////////////
    // isIdeographic                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isIdeographic01(): Unit \ Assert = assertFalse(CodePoint.isIdeographic(0x0031))      // '1'

    @Test
    def isIdeographic02(): Unit \ Assert = assertFalse(CodePoint.isIdeographic(0x0061))      // 'a'

    @Test
    def isIdeographic03(): Unit \ Assert = assertFalse(CodePoint.isIdeographic(0x0041))      // 'A'

    @Test
    def isIdeographic04(): Unit \ Assert = assertFalse(CodePoint.isIdeographic(0x0020))      // ' '

    @Test
    def isIdeographic05(): Unit \ Assert = assertFalse(CodePoint.isIdeographic(0x00b1))      // plus-minus sign

    @Test
    def isIdeographic06(): Unit \ Assert = assertFalse(CodePoint.isIdeographic(0x01F2))      // Dz (single char, title case)

    @Test
    def isIdeographic07(): Unit \ Assert = assertFalse(CodePoint.isIdeographic(0x01F3))      // dz (single char)

    @Test
    def isIdeographic08(): Unit \ Assert = assertFalse(CodePoint.isIdeographic(0x1f3dd))     // Desert Island pictogram

    @Test
    def isIdeographic09(): Unit \ Assert = assertTrue(CodePoint.isIdeographic(0x4E00))       // CJK Unified Ideograph-4E00

    /////////////////////////////////////////////////////////////////////////////
    // isISOControl                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isISOControl01(): Unit \ Assert = assertFalse(CodePoint.isISOControl(0x0031))      // '1'

    @Test
    def isISOControl02(): Unit \ Assert = assertFalse(CodePoint.isISOControl(0x0061))      // 'a'

    @Test
    def isISOControl03(): Unit \ Assert = assertFalse(CodePoint.isISOControl(0x0041))      // 'A'

    @Test
    def isISOControl04(): Unit \ Assert = assertFalse(CodePoint.isISOControl(0x1f3dd))     // Desert Island pictogram

    @Test
    def isISOControl05(): Unit \ Assert = assertTrue(CodePoint.isISOControl(0x0000))

    @Test
    def isISOControl06(): Unit \ Assert = assertTrue(CodePoint.isISOControl(0x0015))        // NAK

    /////////////////////////////////////////////////////////////////////////////
    // isMirrored                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isMirrored01(): Unit \ Assert = assertFalse(CodePoint.isMirrored(0x0061))        // 'a'

    @Test
    def isMirrored02(): Unit \ Assert = assertFalse(CodePoint.isMirrored(0x0041))        // 'A'

    @Test
    def isMirrored03(): Unit \ Assert = assertTrue(CodePoint.isMirrored(0x005B))         // '['

    @Test
    def isMirrored04(): Unit \ Assert = assertTrue(CodePoint.isMirrored(0x005B))         // ']'

    @Test
    def isMirrored05(): Unit \ Assert = assertFalse(CodePoint.isMirrored(0x1f3dd))       // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isSupplementaryCodePoint                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSupplementaryCodePoint01(): Unit \ Assert = assertFalse(CodePoint.isSupplementaryCodePoint(0x0031))      // '1'

    @Test
    def isSupplementaryCodePoint02(): Unit \ Assert = assertFalse(CodePoint.isSupplementaryCodePoint(0x0061))      // 'a'

    @Test
    def isSupplementaryCodePoint03(): Unit \ Assert = assertTrue(CodePoint.isSupplementaryCodePoint(0x1f3dd))       // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // isValidCodePoint                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isValidCodePoint01(): Unit \ Assert = assertTrue(CodePoint.isValidCodePoint(0x0031))       // '1'

    @Test
    def isValidCodePoint02(): Unit \ Assert = assertTrue(CodePoint.isValidCodePoint(0x0061))       // 'a'

    @Test
    def isValidCodePoint03(): Unit \ Assert = assertTrue(CodePoint.isValidCodePoint(0x1f3dd))       // Desert Island pictogram

    @Test
    def isValidCodePoint04(): Unit \ Assert = assertFalse(CodePoint.isValidCodePoint(0x110000))      // out-of-range

    /////////////////////////////////////////////////////////////////////////////
    // toLowerCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toLowerCase01(): Unit \ Assert = assertEq(expected = 0x0061, CodePoint.toLowerCase(0x0061))         // 'a'

    @Test
    def toLowerCase02(): Unit \ Assert = assertEq(expected = 0x0061, CodePoint.toLowerCase(0x0041))         // 'A' => 'a'

    @Test
    def toLowerCase03(): Unit \ Assert = assertEq(expected = 0x0020, CodePoint.toLowerCase(0x0020))         // ' ' (space)

    @Test
    def toLowerCase04(): Unit \ Assert = assertEq(expected = 0x01F3, CodePoint.toLowerCase(0x01F2))         // Dz (single char, title case) => dz

    @Test
    def toLowerCase05(): Unit \ Assert = assertEq(expected = 0x01F3, CodePoint.toLowerCase(0x01F3))         // dz (single char)

    @Test
    def toLowerCase06(): Unit \ Assert = assertEq(expected = 0x1f3dd, CodePoint.toLowerCase(0x1f3dd))       // Desert Island pictogram


    /////////////////////////////////////////////////////////////////////////////
    // toUpperCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toUpperCase01(): Unit \ Assert = assertEq(expected = 0x0041, CodePoint.toUpperCase(0x0061))        // 'a' => 'A'

    @Test
    def toUpperCase02(): Unit \ Assert = assertEq(expected = 0x0041, CodePoint.toUpperCase(0x0041))         // 'A'

    @Test
    def toUpperCase03(): Unit \ Assert = assertEq(expected = 0x0020, CodePoint.toUpperCase(0x0020))         // ' ' (space)

    @Test
    def toUpperCase04(): Unit \ Assert = assertEq(expected = 0x01F1, CodePoint.toUpperCase(0x01F2))         // Dz (single char, title case) => DZ

    @Test
    def toUpperCase05(): Unit \ Assert = assertEq(expected = 0x01F1, CodePoint.toUpperCase(0x01F3))         // dz (single char) => DZ

    @Test
    def toUpperCase06(): Unit \ Assert = assertEq(expected = 0x1f3dd, CodePoint.toUpperCase(0x1f3dd))       // Desert Island pictogram


    /////////////////////////////////////////////////////////////////////////////
    // toTitleCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toTitleCase01(): Unit \ Assert = assertEq(expected = 0x0041, CodePoint.toTitleCase(0x0061))         // 'a' => 'A'

    @Test
    def toTitleCase02(): Unit \ Assert = assertEq(expected = 0x0041, CodePoint.toTitleCase(0x0041))         // 'A'

    @Test
    def toTitleCase03(): Unit \ Assert = assertEq(expected = 0x0020, CodePoint.toTitleCase(0x0020))         // ' ' (space)

    @Test
    def toTitleCase04(): Unit \ Assert = assertEq(expected = 0x01F2, CodePoint.toTitleCase(0x01F2))         // Dz (single char, title case)

    @Test
    def toTitleCase05(): Unit \ Assert = assertEq(expected = 0x01F2, CodePoint.toTitleCase(0x01F3))         // dz (single char) => Dz

    @Test
    def toTitleCase06(): Unit \ Assert = assertEq(expected = 0x1f3dd, CodePoint.toTitleCase(0x1f3dd))       // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // getName                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getName01(): Unit \ Assert = assertSome(CodePoint.getName(0x0061))         // 'a'

    @Test
    def getName02(): Unit \ Assert = assertSome(CodePoint.getName(0x0041))         // 'A'

    @Test
    def getName03(): Unit \ Assert = assertSome(CodePoint.getName(0x1f3dd))        // Desert Island pictogram

    @Test
    def getName04(): Unit \ Assert = assertNone(CodePoint.getName(0x110000))       // out-of-range

    /////////////////////////////////////////////////////////////////////////////
    // getNumericValue                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getNumericValue01(): Unit \ Assert = assertEq(expected = Some(0), CodePoint.getNumericValue(0x0030))       // '0'

    @Test
    def getNumericValue02(): Unit \ Assert = assertEq(expected = Some(1), CodePoint.getNumericValue(0x0031))       // '1'

    @Test
    def getNumericValue03(): Unit \ Assert = assertEq(expected = Some(9), CodePoint.getNumericValue(0x0039))       // '9'

    @Test
    def getNumericValue04(): Unit \ Assert = assertEq(expected = Some(10), CodePoint.getNumericValue(0x0041))      // 'A'

    @Test
    def getNumericValue05(): Unit \ Assert = assertEq(expected = Some(10), CodePoint.getNumericValue(0x0061))      // 'a'

    @Test
    def getNumericValue06(): Unit \ Assert = assertEq(expected = Some(15), CodePoint.getNumericValue(0x0046))      // 'F'

    @Test
    def getNumericValue07(): Unit \ Assert = assertEq(expected = Some(15), CodePoint.getNumericValue(0x0066))      // 'f'

    @Test
    def getNumericValue08(): Unit \ Assert = assertNone(CodePoint.getNumericValue(0x0020))          // ' ' (space)

    @Test
    def getNumericValue09(): Unit \ Assert = assertNone(CodePoint.getNumericValue(0x002b))          // '+'

    @Test
    def getNumericValue10(): Unit \ Assert = assertNone(CodePoint.getNumericValue(0x002d))          // '-'

    @Test
    def getNumericValue11(): Unit \ Assert = assertNone(CodePoint.getNumericValue(0x1f3dd))          // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // charCount                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def charCount01(): Unit \ Assert = assertEq(expected = 1, CodePoint.charCount(0x0031))          // '1'

    @Test
    def charCount02(): Unit \ Assert = assertEq(expected = 1, CodePoint.charCount(0x0041))          // 'A'

    @Test
    def charCount03(): Unit \ Assert = assertEq(expected = 2, CodePoint.charCount(0x1f3dd))          // Desert Island pictogram

    /////////////////////////////////////////////////////////////////////////////
    // toChars                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toChars01(): Unit \ Assert = assertEq(expected = Some(Vector#{'1'}), CodePoint.toChars(0x0031))     // '1'

    @Test
    def toChars02(): Unit \ Assert = assertEq(expected = Some(Vector#{'A'}), CodePoint.toChars(0x0041))     // 'A'

    @Test
    def toChars03(): Unit \ Assert = assertEq(expected = Some(Vector#{'\uD83C', '\uDFDD'}), CodePoint.toChars(0x1f3dd))     // Desert Island pictogram

    @Test
    def toChars04(): Unit \ Assert = assertNone(CodePoint.toChars(0x110000))                 // out-of-range

    /////////////////////////////////////////////////////////////////////////////
    // toBmpChar                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toBmpChar01(): Unit \ Assert = assertEq(expected = Some('1'), CodePoint.toBmpChar(0x0031))     // '1'

    @Test
    def toBmpChar02(): Unit \ Assert = assertEq(expected = Some('A'), CodePoint.toBmpChar(0x0041))     // 'A'

    @Test
    def toBmpChar03(): Unit \ Assert = assertNone(CodePoint.toBmpChar(0x1f3dd))          // Desert Island pictogram

    @Test
    def toBmpChar04(): Unit \ Assert = assertNone(CodePoint.toBmpChar(0x110000))         // out-of-range

    /////////////////////////////////////////////////////////////////////////////
    // toSupplementaryChars                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSupplementaryChars01(): Unit \ Assert = assertNone(CodePoint.toSupplementaryChars(0x0031))     // '1'

    @Test
    def toSupplementaryChars02(): Unit \ Assert = assertNone(CodePoint.toSupplementaryChars(0x0041))     // 'A'

    @Test
    def toSupplementaryChars03(): Unit \ Assert = assertEq(expected = Some(('\uD83C', '\uDFDD')), CodePoint.toSupplementaryChars(0x1F3DD))      // Desert Island pictogram

    @Test
    def toSupplementaryChars04(): Unit \ Assert = assertNone(CodePoint.toSupplementaryChars(0x110000))   // out-of-range

    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = assertEq(expected = "1", CodePoint.toString(0x0031))      // '1'

    @Test
    def toString02(): Unit \ Assert = assertEq(expected = "A", CodePoint.toString(0x0041))      // 'A'

    @Test
    def toString03(): Unit \ Assert = assertEq(expected = "\uD83C\uDFDD", CodePoint.toString(0x1F3DD))     // Desert Island pictogram

    @Test
    def toString04(): Unit \ Assert = assertEq(expected = "", CodePoint.toString(0x110000))     // out-of-range


    /////////////////////////////////////////////////////////////////////////////
    // highSurrogate                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def highSurrogate01(): Unit \ Assert = assertNone(CodePoint.highSurrogate(0x0041))      // 'A'

    @Test
    def highSurrogate02(): Unit \ Assert = assertEq(expected = Some('\uD83C'), CodePoint.highSurrogate(0x1F3DD))     // Desert Island pictogram

    @Test
    def highSurrogate03(): Unit \ Assert = assertNone(CodePoint.highSurrogate(0x110000))     // out-of-range

    /////////////////////////////////////////////////////////////////////////////
    // lowSurrogate                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def lowSurrogate01(): Unit \ Assert = assertNone(CodePoint.lowSurrogate(0x0041))         // 'A'

    @Test
    def lowSurrogate02(): Unit \ Assert = assertEq(expected = Some('\uDFDD'), CodePoint.lowSurrogate(0x1F3DD))     // Desert Island pictogram

    @Test
    def lowSurrogate03(): Unit \ Assert = assertNone(CodePoint.lowSurrogate(0x110000))       // out-of-range

}
