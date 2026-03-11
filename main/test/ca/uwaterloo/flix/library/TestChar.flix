mod TestChar {

    use Assert.{assertEq, assertTrue, assertFalse, assertSome, assertNone};

    /////////////////////////////////////////////////////////////////////////////
    // isAscii                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isAscii01(): Unit \ Assert = assertTrue(Char.isAscii('a'))

    @Test
    def isAscii02(): Unit \ Assert = assertTrue(Char.isAscii('A'))

    @Test
    def isAscii03(): Unit \ Assert = assertTrue(Char.isAscii(' '))

    @Test
    def isAscii04(): Unit \ Assert = assertTrue(Char.isAscii('+'))

    @Test
    def isAscii05(): Unit \ Assert = assertTrue(Char.isAscii('0'))

    @Test
    def isAscii06(): Unit \ Assert = assertTrue(Char.isAscii('9'))

    @Test
    def isAscii07(): Unit \ Assert = assertFalse(Char.isAscii('†'))

    @Test
    def isAscii08(): Unit \ Assert = assertFalse(Char.isAscii('\u00FE'))     // lower-case thorn

    @Test
    def isAscii09(): Unit \ Assert = assertFalse(Char.isAscii('\u00B1'))     // plus-minus sign

    @Test
    def isAscii10(): Unit \ Assert = assertFalse(Char.isAscii('â'))

    @Test
    def isAscii11(): Unit \ Assert = assertFalse(Char.isAscii('Â'))

    @Test
    def isAscii12(): Unit \ Assert = assertFalse(Char.isAscii('\u0081'))

    /////////////////////////////////////////////////////////////////////////////
    // isLetter                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isLetter01(): Unit \ Assert = assertTrue(Char.isLetter('a'))

    @Test
    def isLetter02(): Unit \ Assert = assertTrue(Char.isLetter('A'))

    @Test
    def isLetter03(): Unit \ Assert = assertFalse(Char.isLetter(' '))

    @Test
    def isLetter04(): Unit \ Assert = assertFalse(Char.isLetter('+'))

    @Test
    def isLetter05(): Unit \ Assert = assertFalse(Char.isLetter('0'))

    @Test
    def isLetter06(): Unit \ Assert = assertFalse(Char.isLetter('9'))

    @Test
    def isLetter07(): Unit \ Assert = assertFalse(Char.isLetter('†'))

    @Test
    def isLetter08(): Unit \ Assert = assertTrue(Char.isLetter('\u00FE'))        // lower-case thorn

    @Test
    def isLetter09(): Unit \ Assert = assertFalse(Char.isLetter('\u00B1'))       // plus-minus sign

    @Test
    def isLetter10(): Unit \ Assert = assertTrue(Char.isLetter('â'))

    @Test
    def isLetter11(): Unit \ Assert = assertTrue(Char.isLetter('Â'))

    /////////////////////////////////////////////////////////////////////////////
    // isDigit                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isDigit01(): Unit \ Assert = assertFalse(Char.isDigit('a'))

    @Test
    def isDigit02(): Unit \ Assert = assertFalse(Char.isDigit('A'))

    @Test
    def isDigit03(): Unit \ Assert = assertFalse(Char.isDigit(' '))

    @Test
    def isDigit04(): Unit \ Assert = assertFalse(Char.isDigit('+'))

    @Test
    def isDigit05(): Unit \ Assert = assertTrue(Char.isDigit('0'))

    @Test
    def isDigit06(): Unit \ Assert = assertTrue(Char.isDigit('9'))

    @Test
    def isDigit07(): Unit \ Assert = assertFalse(Char.isDigit('†'))

    @Test
    def isDigit08(): Unit \ Assert = assertFalse(Char.isDigit('\u00FE'))         // lower-case thorn

    @Test
    def isDigit09(): Unit \ Assert = assertFalse(Char.isDigit('\u00B1'))         // plus-minus sign

    @Test
    def isDigit10(): Unit \ Assert = assertFalse(Char.isDigit('â'))

    @Test
    def isDigit11(): Unit \ Assert = assertFalse(Char.isDigit('Â'))

    @Test
    def isDigit12(): Unit \ Assert = assertTrue(Char.isDigit('\u0660'))          // Arabic-Indic digit zero

    @Test
    def isDigit13(): Unit \ Assert = assertTrue(Char.isDigit('\u0669'))          // Arabic-Indic digit nine

    @Test
    def isDigit14(): Unit \ Assert = assertTrue(Char.isDigit('\u0966'))          // Devanagari digit zero

    @Test
    def isDigit15(): Unit \ Assert = assertTrue(Char.isDigit('\u096F'))          // Devanagari digit nine

    /////////////////////////////////////////////////////////////////////////////
    // isLetterOrDigit                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isLetterOrDigit01(): Unit \ Assert = assertTrue(Char.isLetterOrDigit('a'))

    @Test
    def isLetterOrDigit02(): Unit \ Assert = assertTrue(Char.isLetterOrDigit('A'))

    @Test
    def isLetterOrDigit03(): Unit \ Assert = assertFalse(Char.isLetterOrDigit(' '))

    @Test
    def isLetterOrDigit04(): Unit \ Assert = assertFalse(Char.isLetterOrDigit('+'))

    @Test
    def isLetterOrDigit05(): Unit \ Assert = assertTrue(Char.isLetterOrDigit('0'))

    @Test
    def isLetterOrDigit06(): Unit \ Assert = assertTrue(Char.isLetterOrDigit('9'))

    @Test
    def isLetterOrDigit07(): Unit \ Assert = assertFalse(Char.isLetterOrDigit('†'))

    /////////////////////////////////////////////////////////////////////////////
    // isAsciiDigit                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isAsciiDigit01(): Unit \ Assert = assertFalse(Char.isAsciiDigit('a'))

    @Test
    def isAsciiDigit02(): Unit \ Assert = assertFalse(Char.isAsciiDigit('A'))

    @Test
    def isAsciiDigit03(): Unit \ Assert = assertFalse(Char.isAsciiDigit(' '))

    @Test
    def isAsciiDigit04(): Unit \ Assert = assertFalse(Char.isAsciiDigit('+'))

    @Test
    def isAsciiDigit05(): Unit \ Assert = assertTrue(Char.isAsciiDigit('0'))

    @Test
    def isAsciiDigit06(): Unit \ Assert = assertTrue(Char.isAsciiDigit('9'))

    @Test
    def isAsciiDigit07(): Unit \ Assert = assertFalse(Char.isAsciiDigit('†'))

    @Test
    def isAsciiDigit08(): Unit \ Assert = assertFalse(Char.isAsciiDigit('\u00FE'))        // lower-case thorn

    @Test
    def isAsciiDigit09(): Unit \ Assert = assertFalse(Char.isAsciiDigit('\u00B1'))       // plus-minus sign

    @Test
    def isAsciiDigit10(): Unit \ Assert = assertFalse(Char.isAsciiDigit('â'))

    @Test
    def isAsciiDigit11(): Unit \ Assert = assertFalse(Char.isAsciiDigit('Â'))

    @Test
    def isAsciiDigit12(): Unit \ Assert = assertFalse(Char.isAsciiDigit('\u0660'))       // Arabic-Indic digit zero

    @Test
    def isAsciiDigit13(): Unit \ Assert = assertFalse(Char.isAsciiDigit('\u0669'))       // Arabic-Indic digit nine

    @Test
    def isAsciiDigit14(): Unit \ Assert = assertFalse(Char.isAsciiDigit('\u0966'))       // Devanagari digit zero

    @Test
    def isAsciiDigit15(): Unit \ Assert = assertFalse(Char.isAsciiDigit('\u096F'))       // Devanagari digit nine

    /////////////////////////////////////////////////////////////////////////////
    // isOctDigit                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isOctDigit01(): Unit \ Assert = assertFalse(Char.isOctDigit('a'))

    @Test
    def isOctDigit02(): Unit \ Assert = assertFalse(Char.isOctDigit('A'))

    @Test
    def isOctDigit03(): Unit \ Assert = assertFalse(Char.isOctDigit(' '))

    @Test
    def isOctDigit04(): Unit \ Assert = assertFalse(Char.isOctDigit('+'))

    @Test
    def isOctDigit05(): Unit \ Assert = assertTrue(Char.isOctDigit('0'))

    @Test
    def isOctDigit06(): Unit \ Assert = assertFalse(Char.isOctDigit('9'))

    @Test
    def isOctDigit07(): Unit \ Assert = assertFalse(Char.isOctDigit('†'))

    @Test
    def isOctDigit08(): Unit \ Assert = assertFalse(Char.isOctDigit('\u00FE'))        // lower-case thorn

    @Test
    def isOctDigit09(): Unit \ Assert = assertFalse(Char.isOctDigit('\u00B1'))       // plus-minus sign

    @Test
    def isOctDigit10(): Unit \ Assert = assertFalse(Char.isOctDigit('â'))

    @Test
    def isOctDigit11(): Unit \ Assert = assertFalse(Char.isOctDigit('Â'))

    @Test
    def isOctDigit12(): Unit \ Assert = assertTrue(Char.isOctDigit('7'))

    @Test
    def isOctDigit13(): Unit \ Assert = assertFalse(Char.isOctDigit('8'))

    /////////////////////////////////////////////////////////////////////////////
    // isHexDigit                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isHexDigit01(): Unit \ Assert = assertTrue(Char.isHexDigit('a'))

    @Test
    def isHexDigit02(): Unit \ Assert = assertTrue(Char.isHexDigit('A'))

    @Test
    def isHexDigit03(): Unit \ Assert = assertFalse(Char.isHexDigit(' '))

    @Test
    def isHexDigit04(): Unit \ Assert = assertFalse(Char.isHexDigit('+'))

    @Test
    def isHexDigit05(): Unit \ Assert = assertTrue(Char.isHexDigit('0'))

    @Test
    def isHexDigit06(): Unit \ Assert = assertTrue(Char.isHexDigit('9'))

    @Test
    def isHexDigit07(): Unit \ Assert = assertFalse(Char.isHexDigit('†'))

    @Test
    def isHexDigit08(): Unit \ Assert = assertFalse(Char.isHexDigit('\u00FE'))        // lower-case thorn

    @Test
    def isHexDigit09(): Unit \ Assert = assertFalse(Char.isHexDigit('\u00B1'))       // plus-minus sign

    @Test
    def isHexDigit10(): Unit \ Assert = assertFalse(Char.isHexDigit('â'))

    @Test
    def isHexDigit11(): Unit \ Assert = assertFalse(Char.isHexDigit('Â'))

    @Test
    def isHexDigit12(): Unit \ Assert = assertTrue(Char.isHexDigit('7'))

    @Test
    def isHexDigit13(): Unit \ Assert = assertTrue(Char.isHexDigit('8'))

    @Test
    def isHexDigit14(): Unit \ Assert = assertTrue(Char.isHexDigit('F'))

    @Test
    def isHexDigit15(): Unit \ Assert = assertTrue(Char.isHexDigit('f'))

    @Test
    def isHexDigit16(): Unit \ Assert = assertFalse(Char.isHexDigit('G'))

    @Test
    def isHexDigit17(): Unit \ Assert = assertFalse(Char.isHexDigit('g'))

    /////////////////////////////////////////////////////////////////////////////
    // isLowerCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isLowerCase01(): Unit \ Assert = assertTrue(Char.isLowerCase('a'))

    @Test
    def isLowerCase02(): Unit \ Assert = assertFalse(Char.isLowerCase('A'))

    @Test
    def isLowerCase03(): Unit \ Assert = assertFalse(Char.isLowerCase(' '))

    @Test
    def isLowerCase04(): Unit \ Assert = assertFalse(Char.isLowerCase('+'))

    @Test
    def isLowerCase05(): Unit \ Assert = assertFalse(Char.isLowerCase('0'))

    @Test
    def isLowerCase06(): Unit \ Assert = assertFalse(Char.isLowerCase('9'))

    @Test
    def isLowerCase07(): Unit \ Assert = assertFalse(Char.isLowerCase('†'))

    @Test
    def isLowerCase08(): Unit \ Assert = assertTrue(Char.isLowerCase('\u00FE'))        // lower-case thorn

    @Test
    def isLowerCase09(): Unit \ Assert = assertFalse(Char.isLowerCase('\u00B1'))       // plus-minus sign

    @Test
    def isLowerCase10(): Unit \ Assert = assertTrue(Char.isLowerCase('â'))

    @Test
    def isLowerCase11(): Unit \ Assert = assertFalse(Char.isLowerCase('Â'))

    /////////////////////////////////////////////////////////////////////////////
    // isUpperCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isUpperCase01(): Unit \ Assert = assertFalse(Char.isUpperCase('a'))

    @Test
    def isUpperCase02(): Unit \ Assert = assertTrue(Char.isUpperCase('A'))

    @Test
    def isUpperCase03(): Unit \ Assert = assertFalse(Char.isUpperCase(' '))

    @Test
    def isUpperCase04(): Unit \ Assert = assertFalse(Char.isUpperCase('+'))

    @Test
    def isUpperCase05(): Unit \ Assert = assertFalse(Char.isUpperCase('0'))

    @Test
    def isUpperCase06(): Unit \ Assert = assertFalse(Char.isUpperCase('9'))

    @Test
    def isUpperCase07(): Unit \ Assert = assertFalse(Char.isUpperCase('†'))

    @Test
    def isUpperCase08(): Unit \ Assert = assertFalse(Char.isUpperCase('\u00FE'))        // lower case thorn

    @Test
    def isUpperCase09(): Unit \ Assert = assertFalse(Char.isUpperCase('\u00B1'))       // plus-minus sign

    @Test
    def isUpperCase10(): Unit \ Assert = assertFalse(Char.isUpperCase('â'))

    @Test
    def isUpperCase11(): Unit \ Assert = assertTrue(Char.isUpperCase('Â'))

    /////////////////////////////////////////////////////////////////////////////
    // isTitleCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isTitleCase01(): Unit \ Assert = assertFalse(Char.isTitleCase('a'))

    @Test
    def isTitleCase02(): Unit \ Assert = assertFalse(Char.isTitleCase('A'))

    @Test
    def isTitleCase03(): Unit \ Assert = assertFalse(Char.isTitleCase(' '))

    @Test
    def isTitleCase04(): Unit \ Assert = assertTrue(Char.isTitleCase('\u01F2'))      // Dz (single char, upper case)

    @Test
    def isTitleCase05(): Unit \ Assert = assertFalse(Char.isTitleCase('\u01F3'))      // dz (single char, lower case)

    @Test
    def isTitleCase06(): Unit \ Assert = assertTrue(Char.isTitleCase('\u1F99'))      // Greek Capital Letter Eta with Dasia and Prosgegrammeni

    /////////////////////////////////////////////////////////////////////////////
    // isWhitespace                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isWhitespace01(): Unit \ Assert = assertFalse(Char.isWhitespace('a'))

    @Test
    def isWhitespace02(): Unit \ Assert = assertFalse(Char.isWhitespace('A'))

    @Test
    def isWhitespace03(): Unit \ Assert = assertTrue(Char.isWhitespace(' '))

    @Test
    def isWhitespace04(): Unit \ Assert = assertFalse(Char.isWhitespace('+'))

    @Test
    def isWhitespace05(): Unit \ Assert = assertFalse(Char.isWhitespace('0'))

    @Test
    def isWhitespace06(): Unit \ Assert = assertFalse(Char.isWhitespace('9'))

    @Test
    def isWhitespace07(): Unit \ Assert = assertFalse(Char.isWhitespace('†'))

    @Test
    def isWhitespace08(): Unit \ Assert = assertFalse(Char.isWhitespace('\u00FE'))     // lower-case thorn

    @Test
    def isWhitespace09(): Unit \ Assert = assertFalse(Char.isWhitespace('\u00B1'))     // plus-minus sign

    @Test
    def isWhitespace10(): Unit \ Assert = assertFalse(Char.isWhitespace('â'))

    @Test
    def isWhitespace11(): Unit \ Assert = assertFalse(Char.isWhitespace('Â'))

    @Test
    def isWhitespace12(): Unit \ Assert = assertTrue(Char.isWhitespace('\u0009'))    // tab

    @Test
    def isWhitespace13(): Unit \ Assert = assertTrue(Char.isWhitespace('\t'))        // tab

    @Test
    def isWhitespace14(): Unit \ Assert = assertTrue(Char.isWhitespace('\u000A'))    // line-feed

    @Test
    def isWhitespace15(): Unit \ Assert = assertTrue(Char.isWhitespace('\n'))        // line-feed

    @Test
    def isWhitespace16(): Unit \ Assert = assertTrue(Char.isWhitespace('\u000D'))    // carriage-return

    @Test
    def isWhitespace17(): Unit \ Assert = assertTrue(Char.isWhitespace('\r'))        // carriage-return

    /////////////////////////////////////////////////////////////////////////////
    // isDefined                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isDefined01(): Unit \ Assert = assertTrue(Char.isDefined('a'))

    @Test
    def isDefined02(): Unit \ Assert = assertTrue(Char.isDefined('A'))

    /////////////////////////////////////////////////////////////////////////////
    // isISOControl                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isISOControl01(): Unit \ Assert = assertFalse(Char.isISOControl('a'))

    @Test
    def isISOControl02(): Unit \ Assert = assertFalse(Char.isISOControl('A'))

    @Test
    def isISOControl03(): Unit \ Assert = assertTrue(Char.isISOControl('\u0000'))

    @Test
    def isISOControl04(): Unit \ Assert = assertTrue(Char.isISOControl('\u0015'))        // NAK

    /////////////////////////////////////////////////////////////////////////////
    // isMirrored                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isMirrored01(): Unit \ Assert = assertFalse(Char.isMirrored('a'))

    @Test
    def isMirrored02(): Unit \ Assert = assertFalse(Char.isMirrored('A'))

    @Test
    def isMirrored03(): Unit \ Assert = assertTrue(Char.isMirrored('['))

    @Test
    def isMirrored04(): Unit \ Assert = assertTrue(Char.isMirrored(']'))

    /////////////////////////////////////////////////////////////////////////////
    // isSurrogate                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSurrogate01(): Unit \ Assert = assertFalse(Char.isSurrogate('a'))

    @Test
    def isSurrogate02(): Unit \ Assert = assertFalse(Char.isSurrogate('A'))

    @Test
    def isSurrogate03(): Unit \ Assert = assertTrue(Char.isSurrogate('\uD83C'))

    @Test
    def isSurrogate04(): Unit \ Assert = assertTrue(Char.isSurrogate('\uDFDD'))

    /////////////////////////////////////////////////////////////////////////////
    // isSurrogatePair                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def isSurrogatePair01(): Unit \ Assert = assertFalse(Char.isSurrogatePair(high = 'A', low = 'a'))

    @Test
    def isSurrogatePair02(): Unit \ Assert = assertTrue(Char.isSurrogatePair(high = '\uD83C', low = '\uDFDD'))

    /////////////////////////////////////////////////////////////////////////////
    // toLowerCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toLowerCase01(): Unit \ Assert = assertEq(expected = 'a', Char.toLowerCase('a'))

    @Test
    def toLowerCase02(): Unit \ Assert = assertEq(expected = 'a', Char.toLowerCase('A'))

    @Test
    def toLowerCase03(): Unit \ Assert = assertEq(expected = ' ', Char.toLowerCase(' '))

    @Test
    def toLowerCase04(): Unit \ Assert = assertEq(expected = '+', Char.toLowerCase('+'))

    @Test
    def toLowerCase05(): Unit \ Assert = assertEq(expected = '0', Char.toLowerCase('0'))

    @Test
    def toLowerCase06(): Unit \ Assert = assertEq(expected = '9', Char.toLowerCase('9'))

    @Test
    def toLowerCase07(): Unit \ Assert = assertEq(expected = '†', Char.toLowerCase('†'))

    @Test
    def toLowerCase08(): Unit \ Assert = assertEq(expected = '\u00FE', Char.toLowerCase('\u00FE'))     // lower-case thorn

    @Test
    def toLowerCase09(): Unit \ Assert = assertEq(expected = '\u00B1', Char.toLowerCase('\u00B1'))     // plus-minus sign

    @Test
    def toLowerCase10(): Unit \ Assert = assertEq(expected = 'â', Char.toLowerCase('â'))

    @Test
    def toLowerCase11(): Unit \ Assert = assertEq(expected = 'â', Char.toLowerCase('Â'))

    /////////////////////////////////////////////////////////////////////////////
    // toUpperCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toUpperCase01(): Unit \ Assert = assertEq(expected = 'A', Char.toUpperCase('a'))

    @Test
    def toUpperCase02(): Unit \ Assert = assertEq(expected = 'A', Char.toUpperCase('A'))

    @Test
    def toUpperCase03(): Unit \ Assert = assertEq(expected = ' ', Char.toUpperCase(' '))

    @Test
    def toUpperCase04(): Unit \ Assert = assertEq(expected = '+', Char.toUpperCase('+'))

    @Test
    def toUpperCase05(): Unit \ Assert = assertEq(expected = '0', Char.toUpperCase('0'))

    @Test
    def toUpperCase06(): Unit \ Assert = assertEq(expected = '9', Char.toUpperCase('9'))

    @Test
    def toUpperCase07(): Unit \ Assert = assertEq(expected = '†', Char.toUpperCase('†'))

    @Test
    def toUpperCase08(): Unit \ Assert = assertEq(expected = '\u00DE', Char.toUpperCase('\u00FE'))     // thorn (lower to upper)

    @Test
    def toUpperCase09(): Unit \ Assert = assertEq(expected = '\u00B1', Char.toUpperCase('\u00B1'))     // plus-minus sign

    @Test
    def toUpperCase10(): Unit \ Assert = assertEq(expected = 'Â', Char.toUpperCase('â'))

    @Test
    def toUpperCase11(): Unit \ Assert = assertEq(expected = 'Â', Char.toUpperCase('Â'))

    /////////////////////////////////////////////////////////////////////////////
    // toTitleCase                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toTitleCase01(): Unit \ Assert = assertEq(expected = 'A', Char.toTitleCase('a'))

    @Test
    def toTitleCase02(): Unit \ Assert = assertEq(expected = 'A', Char.toTitleCase('A'))

    @Test
    def toTitleCase03(): Unit \ Assert = assertEq(expected = ' ', Char.toTitleCase(' '))

    @Test
    def toTitleCase04(): Unit \ Assert = assertEq(expected = '\u01F2', Char.toTitleCase('\u01F2'))      // Dz (single char)

    @Test
    def toTitleCase05(): Unit \ Assert = assertEq(expected = '\u01F2', Char.toTitleCase('\u01F3'))      // dz (single char)

    /////////////////////////////////////////////////////////////////////////////
    // toBmpCodePoint                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toBmpCodePoint01(): Unit \ Assert = assertEq(expected = 0x0061, Char.toBmpCodePoint('a'))

    @Test
    def toBmpCodePoint02(): Unit \ Assert = assertEq(expected = 0x0041, Char.toBmpCodePoint('A'))

    @Test
    def toBmpCodePoint03(): Unit \ Assert = assertEq(expected = 0x0020, Char.toBmpCodePoint(' '))

    @Test
    def toBmpCodePoint04(): Unit \ Assert = assertEq(expected = 0x01F2, Char.toBmpCodePoint('\u01F2'))      // Dz (single char)

    @Test
    def toBmpCodePoint05(): Unit \ Assert = assertEq(expected = 0x01F3, Char.toBmpCodePoint('\u01F3'))      // dz (single char)

    /////////////////////////////////////////////////////////////////////////////
    // toSupplementaryCodePoint                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toSupplementaryCodePoint01(): Unit \ Assert =
        assertEq(expected = 0x1f3dd, Char.toSupplementaryCodePoint(high = '\uD83C', low = '\uDFDD'))      // Desert Island pictogram


    /////////////////////////////////////////////////////////////////////////////
    // toString                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def toString01(): Unit \ Assert = assertEq(expected = "a", Char.toString('a'))

    @Test
    def toString02(): Unit \ Assert = assertEq(expected = "A", Char.toString('A'))

    @Test
    def toString03(): Unit \ Assert = assertEq(expected = " ", Char.toString(' '))

    @Test
    def toString04(): Unit \ Assert = assertEq(expected = "+", Char.toString('+'))

    @Test
    def toString05(): Unit \ Assert = assertEq(expected = "0", Char.toString('0'))

    @Test
    def toString06(): Unit \ Assert = assertEq(expected = "9", Char.toString('9'))

    @Test
    def toString07(): Unit \ Assert = assertEq(expected = "†", Char.toString('†'))

    @Test
    def toString08(): Unit \ Assert = assertEq(expected = "þ", Char.toString('\u00FE'))    // lower-case thorn

    @Test
    def toString09(): Unit \ Assert = assertEq(expected = "±", Char.toString('\u00B1'))     // plus-minus sign

    @Test
    def toString10(): Unit \ Assert = assertEq(expected = "â", Char.toString('â'))

    @Test
    def toString11(): Unit \ Assert = assertEq(expected = "Â", Char.toString('Â'))

    /////////////////////////////////////////////////////////////////////////////
    // getNumericValue                                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def getNumericValue01(): Unit \ Assert = assertSome(Char.getNumericValue('0'))

    @Test
    def getNumericValue02(): Unit \ Assert = assertSome(Char.getNumericValue('1'))

    @Test
    def getNumericValue03(): Unit \ Assert = assertSome(Char.getNumericValue('9'))

    @Test
    def getNumericValue04(): Unit \ Assert = assertSome(Char.getNumericValue('A'))

    @Test
    def getNumericValue05(): Unit \ Assert = assertSome(Char.getNumericValue('a'))

    @Test
    def getNumericValue06(): Unit \ Assert = assertSome(Char.getNumericValue('F'))

    @Test
    def getNumericValue07(): Unit \ Assert = assertSome(Char.getNumericValue('f'))

    @Test
    def getNumericValue08(): Unit \ Assert = assertNone(Char.getNumericValue(' '))

    @Test
    def getNumericValue09(): Unit \ Assert = assertNone(Char.getNumericValue('+'))

    @Test
    def getNumericValue10(): Unit \ Assert = assertNone(Char.getNumericValue('-'))

    /////////////////////////////////////////////////////////////////////////////
    // digit                                                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def digit01(): Unit \ Assert = assertEq(expected = Some(0), Char.digit(radix = 10, '0'))

    @Test
    def digit02(): Unit \ Assert = assertEq(expected = Some(1), Char.digit(radix = 10, '1'))

    @Test
    def digit03(): Unit \ Assert = assertEq(expected = Some(9), Char.digit(radix = 10, '9'))

    @Test
    def digit04(): Unit \ Assert = assertNone(Char.digit(radix = 10, 'A'))

    @Test
    def digit05(): Unit \ Assert = assertNone(Char.digit(radix = 10, 'a'))

    @Test
    def digit06(): Unit \ Assert = assertNone(Char.digit(radix = 10, 'F'))

    @Test
    def digit07(): Unit \ Assert = assertNone(Char.digit(radix = 10, 'f'))

    @Test
    def digit08(): Unit \ Assert = assertNone(Char.digit(radix = 10, ' '))

    @Test
    def digit09(): Unit \ Assert = assertNone(Char.digit(radix = 10, '+'))

    @Test
    def digit10(): Unit \ Assert = assertNone(Char.digit(radix = 10, '-'))

    @Test
    def digit11(): Unit \ Assert = assertEq(expected = Some(0), Char.digit(radix = 16, '0'))

    @Test
    def digit12(): Unit \ Assert = assertEq(expected = Some(1), Char.digit(radix = 16, '1'))

    @Test
    def digit13(): Unit \ Assert = assertEq(expected = Some(9), Char.digit(radix = 16, '9'))

    @Test
    def digit14(): Unit \ Assert = assertEq(expected = Some(10), Char.digit(radix = 16, 'A'))

    @Test
    def digit15(): Unit \ Assert = assertEq(expected = Some(10), Char.digit(radix = 16, 'a'))

    @Test
    def digit16(): Unit \ Assert = assertEq(expected = Some(15), Char.digit(radix = 16, 'F'))

    @Test
    def digit17(): Unit \ Assert = assertEq(expected = Some(15), Char.digit(radix = 16, 'f'))

    /////////////////////////////////////////////////////////////////////////////
    // forDigit                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def forDigit01(): Unit \ Assert = assertEq(expected = Some('0'), Char.forDigit(radix = 10, 0))

    @Test
    def forDigit02(): Unit \ Assert = assertEq(expected = Some('1'), Char.forDigit(radix = 10, 1))

    @Test
    def forDigit03(): Unit \ Assert = assertEq(expected = Some('9'), Char.forDigit(radix = 10, 9))

    @Test
    def forDigit04(): Unit \ Assert = assertNone(Char.forDigit(radix = 10, 11))              // 'A'

    @Test
    def forDigit05(): Unit \ Assert = assertNone(Char.forDigit(radix = 10, 15))              // 'F'

    @Test
    def forDigit06(): Unit \ Assert = assertNone(Char.forDigit(radix = 10, 256))

    @Test
    def forDigit07(): Unit \ Assert = assertEq(expected = Some('0'), Char.forDigit(radix = 16, 0))

    @Test
    def forDigit08(): Unit \ Assert = assertEq(expected = Some('1'), Char.forDigit(radix = 16, 1))

    @Test
    def forDigit09(): Unit \ Assert = assertEq(expected = Some('9'), Char.forDigit(radix = 16, 9))

    @Test
    def forDigit10(): Unit \ Assert = assertEq(expected = Some('a'), Char.forDigit(radix = 16, 10))

    @Test
    def forDigit11(): Unit \ Assert = assertEq(expected = Some('f'), Char.forDigit(radix = 16, 15))

    /////////////////////////////////////////////////////////////////////////////
    // charValue                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def charValue01(): Unit \ Assert = {
        let c = Char.valueOf('0');
        assertEq(expected = '0', Char.charValue(c))
    }

    @Test
    def charValue02(): Unit \ Assert = {
        let c = Char.valueOf('A');
        assertEq(expected = 'A', Char.charValue(c))
    }

    @Test
    def charValue03(): Unit \ Assert = {
        let c = Char.valueOf('+');
        assertEq(expected = '+', Char.charValue(c))
    }

    /////////////////////////////////////////////////////////////////////////////
    // valueOf                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def valueOf01(): Unit \ Assert = {
        let c = Char.valueOf('0');
        assertTrue(c.equals(Char.valueOf('0')))
    }

    @Test
    def valueOf02(): Unit \ Assert = {
        let c = Char.valueOf('A');
        assertTrue(c.equals(Char.valueOf('A')))
    }

    @Test
    def valueOf03(): Unit \ Assert = {
        let c = Char.valueOf('+');
        assertTrue(c.equals(Char.valueOf('+')))
    }

}
