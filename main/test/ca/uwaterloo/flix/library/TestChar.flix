/*
 * Copyright 2020 Stephen Tetley
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

mod TestChar {

/////////////////////////////////////////////////////////////////////////////
// isAscii                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def isAscii01(): Bool = Char.isAscii('a') == true

@Test
def isAscii02(): Bool = Char.isAscii('A') == true

@Test
def isAscii03(): Bool = Char.isAscii(' ') == true

@Test
def isAscii04(): Bool = Char.isAscii('+') == true

@Test
def isAscii05(): Bool = Char.isAscii('0') == true

@Test
def isAscii06(): Bool = Char.isAscii('9') == true

@Test
def isAscii07(): Bool = Char.isAscii('†') == false

@Test
def isAscii08(): Bool = Char.isAscii('\u00FE') == false     // lower-case thorn

@Test
def isAscii09(): Bool = Char.isAscii('\u00B1') == false     // plus-minus sign

@Test
def isAscii10(): Bool = Char.isAscii('â') == false

@Test
def isAscii11(): Bool = Char.isAscii('Â') == false

@Test
def isAscii12(): Bool = Char.isAscii('\u0081') == false

/////////////////////////////////////////////////////////////////////////////
// isLetter                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def isLetter01(): Bool = Char.isLetter('a') == true

@Test
def isLetter02(): Bool = Char.isLetter('A') == true

@Test
def isLetter03(): Bool = Char.isLetter(' ') == false

@Test
def isLetter04(): Bool = Char.isLetter('+') == false

@Test
def isLetter05(): Bool = Char.isLetter('0') == false

@Test
def isLetter06(): Bool = Char.isLetter('9') == false

@Test
def isLetter07(): Bool = Char.isLetter('†') == false

@Test
def isLetter08(): Bool = Char.isLetter('\u00FE') == true        // lower-case thorn

@Test
def isLetter09(): Bool = Char.isLetter('\u00B1') == false       // plus-minus sign

@Test
def isLetter10(): Bool = Char.isLetter('â') == true

@Test
def isLetter11(): Bool = Char.isLetter('Â') == true

/////////////////////////////////////////////////////////////////////////////
// isDigit                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def isDigit01(): Bool = Char.isDigit('a') == false

@Test
def isDigit02(): Bool = Char.isDigit('A') == false

@Test
def isDigit03(): Bool = Char.isDigit(' ') == false

@Test
def isDigit04(): Bool = Char.isDigit('+') == false

@Test
def isDigit05(): Bool = Char.isDigit('0') == true

@Test
def isDigit06(): Bool = Char.isDigit('9') == true

@Test
def isDigit07(): Bool = Char.isDigit('†') == false

@Test
def isDigit08(): Bool = Char.isDigit('\u00FE') == false         // lower-case thorn

@Test
def isDigit09(): Bool = Char.isDigit('\u00B1') == false         // plus-minus sign

@Test
def isDigit10(): Bool = Char.isDigit('â') == false

@Test
def isDigit11(): Bool = Char.isDigit('Â') == false

@Test
def isDigit12(): Bool = Char.isDigit('\u0660') == true          // Arabic-Indic digit zero

@Test
def isDigit13(): Bool = Char.isDigit('\u0669') == true          // Arabic-Indic digit nine

@Test
def isDigit14(): Bool = Char.isDigit('\u0966') == true          // Devanagari digit zero

@Test
def isDigit15(): Bool = Char.isDigit('\u096F') == true          // Devanagari digit nine

/////////////////////////////////////////////////////////////////////////////
// isLetterOrDigit                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def isLetterOrDigit01(): Bool = Char.isLetterOrDigit('a') == true

@Test
def isLetterOrDigit02(): Bool = Char.isLetterOrDigit('A') == true

@Test
def isLetterOrDigit03(): Bool = Char.isLetterOrDigit(' ') == false

@Test
def isLetterOrDigit04(): Bool = Char.isLetterOrDigit('+') == false

@Test
def iisLetterOrDigit05(): Bool = Char.isLetterOrDigit('0') == true

@Test
def iisLetterOrDigit06(): Bool = Char.isLetterOrDigit('9') == true

@Test
def isLetterOrDigit07(): Bool = Char.isLetterOrDigit('†') == false

/////////////////////////////////////////////////////////////////////////////
// isAsciiDigit                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def isAsciiDigit01(): Bool = Char.isAsciiDigit('a') == false

@Test
def isAsciiDigit02(): Bool = Char.isAsciiDigit('A') == false

@Test
def isAsciiDigit03(): Bool = Char.isAsciiDigit(' ') == false

@Test
def isAsciiDigit04(): Bool = Char.isAsciiDigit('+') == false

@Test
def isAsciiDigit05(): Bool = Char.isAsciiDigit('0') == true

@Test
def isAsciiDigit06(): Bool = Char.isAsciiDigit('9') == true

@Test
def isAsciiDigit07(): Bool = Char.isAsciiDigit('†') == false

@Test
def isAsciiDigit08(): Bool = Char.isAsciiDigit('\u00FE') == false        // lower-case thorn

@Test
def isAsciiDigit09(): Bool = Char.isAsciiDigit('\u00B1') == false       // plus-minus sign

@Test
def isAsciiDigit10(): Bool = Char.isAsciiDigit('â') == false

@Test
def isAsciiDigit11(): Bool = Char.isAsciiDigit('Â') == false

@Test
def isAsciiDigit12(): Bool = Char.isAsciiDigit('\u0660') == false       // Arabic-Indic digit zero

@Test
def isAsciiDigit13(): Bool = Char.isAsciiDigit('\u0669') == false       // Arabic-Indic digit nine

@Test
def isAsciiDigit14(): Bool = Char.isAsciiDigit('\u0966') == false       // Devanagari digit zero

@Test
def isAsciiDigit15(): Bool = Char.isAsciiDigit('\u096F') == false       // Devanagari digit nine

/////////////////////////////////////////////////////////////////////////////
// isOctDigit                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def isOctDigit01(): Bool = Char.isOctDigit('a') == false

@Test
def isOctDigit02(): Bool = Char.isOctDigit('A') == false

@Test
def isOctDigit03(): Bool = Char.isOctDigit(' ') == false

@Test
def isOctDigit04(): Bool = Char.isOctDigit('+') == false

@Test
def isOctDigit05(): Bool = Char.isOctDigit('0') == true

@Test
def isOctDigit06(): Bool = Char.isOctDigit('9') == false

@Test
def isOctDigit07(): Bool = Char.isOctDigit('†') == false

@Test
def isOctDigit08(): Bool = Char.isOctDigit('\u00FE') == false        // lower-case thorn

@Test
def isOctDigit09(): Bool = Char.isOctDigit('\u00B1') == false       // plus-minus sign

@Test
def isOctDigit10(): Bool = Char.isOctDigit('â') == false

@Test
def isOctDigit11(): Bool = Char.isOctDigit('Â') == false

@Test
def isOctDigit12(): Bool = Char.isOctDigit('7') == true

@Test
def isOctDigit13(): Bool = Char.isOctDigit('8') == false

/////////////////////////////////////////////////////////////////////////////
// isHexDigit                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def isHexDigit01(): Bool = Char.isHexDigit('a') == true

@Test
def isHexDigit02(): Bool = Char.isHexDigit('A') == true

@Test
def isHexDigit03(): Bool = Char.isHexDigit(' ') == false

@Test
def isHexDigit04(): Bool = Char.isHexDigit('+') == false

@Test
def isHexDigit05(): Bool = Char.isHexDigit('0') == true

@Test
def isHexDigit06(): Bool = Char.isHexDigit('9') == true

@Test
def isHexDigit07(): Bool = Char.isHexDigit('†') == false

@Test
def isHexDigit08(): Bool = Char.isHexDigit('\u00FE') == false        // lower-case thorn

@Test
def isHexDigit09(): Bool = Char.isHexDigit('\u00B1') == false       // plus-minus sign

@Test
def isHexDigit10(): Bool = Char.isHexDigit('â') == false

@Test
def isHexDigit11(): Bool = Char.isHexDigit('Â') == false

@Test
def isHexDigit12(): Bool = Char.isHexDigit('7') == true

@Test
def isHexDigit13(): Bool = Char.isHexDigit('8') == true

@Test
def isHexDigit14(): Bool = Char.isHexDigit('F') == true

@Test
def isHexDigit15(): Bool = Char.isHexDigit('f') == true

@Test
def isHexDigit16(): Bool = Char.isHexDigit('G') == false

@Test
def isHexDigit17(): Bool = Char.isHexDigit('g') == false

/////////////////////////////////////////////////////////////////////////////
// isLowerCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def isLowerCase01(): Bool = Char.isLowerCase('a') == true

@Test
def isLowerCase02(): Bool = Char.isLowerCase('A') == false

@Test
def isLowerCase03(): Bool = Char.isLowerCase(' ') == false

@Test
def isLowerCase04(): Bool = Char.isLowerCase('+') == false

@Test
def isLowerCase05(): Bool = Char.isLowerCase('0') == false

@Test
def isLowerCase06(): Bool = Char.isLowerCase('9') == false

@Test
def isLowerCase07(): Bool = Char.isLowerCase('†') == false

@Test
def isLowerCase08(): Bool = Char.isLowerCase('\u00FE') == true        // lower-case thorn

@Test
def isLowerCase09(): Bool = Char.isLowerCase('\u00B1') == false       // plus-minus sign

@Test
def isLowerCase10(): Bool = Char.isLowerCase('â') == true

@Test
def isLowerCase11(): Bool = Char.isLowerCase('Â') == false

/////////////////////////////////////////////////////////////////////////////
// isUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def isUpperCase01(): Bool = Char.isUpperCase('a') == false

@Test
def isUpperCase02(): Bool = Char.isUpperCase('A') == true

@Test
def isUpperCase03(): Bool = Char.isUpperCase(' ') == false

@Test
def isUpperCase04(): Bool = Char.isUpperCase('+') == false

@Test
def isUpperCase05(): Bool = Char.isUpperCase('0') == false

@Test
def isUpperCase06(): Bool = Char.isUpperCase('9') == false

@Test
def isUpperCase07(): Bool = Char.isUpperCase('†') == false

@Test
def isUpperCase08(): Bool = Char.isUpperCase('\u00FE') == false        // lower case thorn

@Test
def isUpperCase09(): Bool = Char.isUpperCase('\u00B1') == false       // plus-minus sign

@Test
def isUpperCase10(): Bool = Char.isUpperCase('â') == false

@Test
def isUpperCase11(): Bool = Char.isUpperCase('Â') == true

/////////////////////////////////////////////////////////////////////////////
// isTitleCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def isTitleCase01(): Bool = Char.isTitleCase('a') == false

@Test
def isTitleCase02(): Bool = Char.isTitleCase('A') == false

@Test
def isTitleCase03(): Bool = Char.isTitleCase(' ') == false

@Test
def isTitleCase04(): Bool = Char.isTitleCase('\u01F2') == true      // Dz (single char, upper case)

@Test
def isTitleCase05(): Bool = Char.isTitleCase('\u01F3') == false      // dz (single char, lower case)

@Test
def isTitleCase06(): Bool = Char.isTitleCase('\u1F99') == true      // Greek Capital Letter Eta with Dasia and Prosgegrammeni

/////////////////////////////////////////////////////////////////////////////
// isWhiteSpace                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def isWhiteSpace01(): Bool = Char.isWhiteSpace('a') == false

@Test
def isWhiteSpace02(): Bool = Char.isWhiteSpace('A') == false

@Test
def isWhiteSpace03(): Bool = Char.isWhiteSpace(' ') == true

@Test
def isWhiteSpace04(): Bool = Char.isWhiteSpace('+') == false

@Test
def isWhiteSpace05(): Bool = Char.isWhiteSpace('0') == false

@Test
def isWhiteSpace06(): Bool = Char.isWhiteSpace('9') == false

@Test
def isWhiteSpace07(): Bool = Char.isWhiteSpace('†') == false

@Test
def isWhiteSpace08(): Bool = Char.isWhiteSpace('\u00FE') == false     // lower-case thorn

@Test
def isWhiteSpace09(): Bool = Char.isWhiteSpace('\u00B1') == false     // plus-minus sign

@Test
def isWhiteSpace10(): Bool = Char.isWhiteSpace('â') == false

@Test
def isWhiteSpace11(): Bool = Char.isWhiteSpace('Â') == false

@Test
def isWhiteSpace12(): Bool = Char.isWhiteSpace('\u0009') == true    // tab

@Test
def isWhiteSpace13(): Bool = Char.isWhiteSpace('\t') == true        // tab

@Test
def isWhiteSpace14(): Bool = Char.isWhiteSpace('\u000A') == true    // line-feed

@Test
def isWhiteSpace15(): Bool = Char.isWhiteSpace('\n') == true        // line-feed

@Test
def isWhiteSpace16(): Bool = Char.isWhiteSpace('\u000D') == true    // carriage-return

@Test
def isWhiteSpace17(): Bool = Char.isWhiteSpace('\r') == true        // carriage-return

/////////////////////////////////////////////////////////////////////////////
// isDefined                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def isDefined01(): Bool = Char.isDefined('a') == true

@Test
def isDefined02(): Bool = Char.isDefined('A') == true

/////////////////////////////////////////////////////////////////////////////
// isISOControl                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def isISOControl01(): Bool = Char.isISOControl('a') == false

@Test
def isISOControl02(): Bool = Char.isISOControl('A') == false

@Test
def isISOControl03(): Bool = Char.isISOControl('\u0000') == true

@Test
def isISOControl04(): Bool = Char.isISOControl('\u0015') == true        // NAK

/////////////////////////////////////////////////////////////////////////////
// isMirrored                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def isMirrored01(): Bool = Char.isMirrored('a') == false

@Test
def isMirrored02(): Bool = Char.isMirrored('A') == false

@Test
def isMirrored03(): Bool = Char.isMirrored('[') == true

@Test
def isMirrored04(): Bool = Char.isMirrored(']') == true

/////////////////////////////////////////////////////////////////////////////
// isSurrogate                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def isSurrogate01(): Bool = Char.isSurrogate('a') == false

@Test
def isSurrogate02(): Bool = Char.isSurrogate('A') == false

@Test
def isSurrogate03(): Bool = Char.isSurrogate('\uD83C') == true

@Test
def isSurrogate04(): Bool = Char.isSurrogate('\uDFDD') == true

/////////////////////////////////////////////////////////////////////////////
// isSurrogatePair                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def isSurrogatePair01(): Bool = Char.isSurrogatePair(high = 'A', low = 'a') == false

@Test
def isSurrogatePair02(): Bool = Char.isSurrogatePair(high = '\uD83C', low = '\uDFDD') == true

/////////////////////////////////////////////////////////////////////////////
// toLowerCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def toLowerCase01(): Bool = Char.toLowerCase('a') == 'a'

@Test
def toLowerCase02(): Bool = Char.toLowerCase('A') == 'a'

@Test
def toLowerCase03(): Bool = Char.toLowerCase(' ') == ' '

@Test
def toLowerCase04(): Bool = Char.toLowerCase('+') == '+'

@Test
def toLowerCase05(): Bool = Char.toLowerCase('0') == '0'

@Test
def toLowerCase06(): Bool = Char.toLowerCase('9') == '9'

@Test
def toLowerCase07(): Bool = Char.toLowerCase('†') == '†'

@Test
def toLowerCase08(): Bool = Char.toLowerCase('\u00FE') == '\u00FE'     // lower-case thorn

@Test
def toLowerCase09(): Bool = Char.toLowerCase('\u00B1') == '\u00B1'     // plus-minus sign

@Test
def toLowerCase10(): Bool = Char.toLowerCase('â') == 'â'

@Test
def toLowerCase11(): Bool = Char.toLowerCase('Â') == 'â'

/////////////////////////////////////////////////////////////////////////////
// toUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def toUpperCase01(): Bool = Char.toUpperCase('a') == 'A'

@Test
def toUpperCase02(): Bool = Char.toUpperCase('A') == 'A'

@Test
def toUpperCase03(): Bool = Char.toUpperCase(' ') == ' '

@Test
def toUpperCase04(): Bool = Char.toUpperCase('+') == '+'

@Test
def toUpperCase05(): Bool = Char.toUpperCase('0') == '0'

@Test
def toUpperCase06(): Bool = Char.toUpperCase('9') == '9'

@Test
def toUpperCase07(): Bool = Char.toUpperCase('†') == '†'

@Test
def toUpperCase08(): Bool = Char.toUpperCase('\u00FE') == '\u00DE'     // thorn (lower to upper)

@Test
def toUpperCase09(): Bool = Char.toUpperCase('\u00B1') == '\u00B1'     // plus-minus sign

@Test
def toUpperCase10(): Bool = Char.toUpperCase('â') == 'Â'

@Test
def toUpperCase11(): Bool = Char.toUpperCase('Â') == 'Â'

/////////////////////////////////////////////////////////////////////////////
// toTitleCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def toTitleCase01(): Bool = Char.toTitleCase('a') == 'A'

@Test
def toTitleCase02(): Bool = Char.toTitleCase('A') == 'A'

@Test
def toTitleCase03(): Bool = Char.toTitleCase(' ') == ' '

@Test
def toTitleCase04(): Bool = Char.toTitleCase('\u01F2') == '\u01F2'      // Dz (single char)

@Test
def toTitleCase05(): Bool = Char.toTitleCase('\u01F3') == '\u01F2'      // dz (single char)

/////////////////////////////////////////////////////////////////////////////
// toBmpCodePoint                                                          //
/////////////////////////////////////////////////////////////////////////////

@Test
def toBmpCodePoint01(): Bool = Char.toBmpCodePoint('a') == 0x0061

@Test
def toBmpCodePoint02(): Bool = Char.toBmpCodePoint('A') == 0x0041

@Test
def toBmpCodePoint03(): Bool = Char.toBmpCodePoint(' ') == 0x0020

@Test
def toBmpCodePoint04(): Bool = Char.toBmpCodePoint('\u01F2') == 0x01F2      // Dz (single char)

@Test
def toBmpCodePoint05(): Bool = Char.toBmpCodePoint('\u01F3') == 0x01F3      // dz (single char)

/////////////////////////////////////////////////////////////////////////////
// toSupplementaryCodePoint                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def toSupplementaryCodePoint01(): Bool =
    Char.toSupplementaryCodePoint(high = '\uD83C', low = '\uDFDD') == 0x1f3dd      // Desert Island pictogram


/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def toString01(): Bool = Char.toString('a') == "a"

@Test
def toString02(): Bool = Char.toString('A') == "A"

@Test
def toString03(): Bool = Char.toString(' ') == " "

@Test
def toString04(): Bool = Char.toString('+') == "+"

@Test
def toString05(): Bool = Char.toString('0') == "0"

@Test
def toString06(): Bool = Char.toString('9') == "9"

@Test
def toString07(): Bool = Char.toString('†') == "†"

@Test
def toString08(): Bool = Char.toString('\u00FE') == "þ"    // lower-case thorn

@Test
def toString09(): Bool = Char.toString('\u00B1') == "±"     // plus-minus sign

@Test
def toString10(): Bool = Char.toString('â') == "â"

@Test
def toString11(): Bool = Char.toString('Â') == "Â"

/////////////////////////////////////////////////////////////////////////////
// getNumericValue                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def getNumericValue01(): Bool = Char.getNumericValue('0') == Some(0)

@Test
def getNumericValue02(): Bool = Char.getNumericValue('1') == Some(1)

@Test
def getNumericValue03(): Bool = Char.getNumericValue('9') == Some(9)

@Test
def getNumericValue04(): Bool = Char.getNumericValue('A') == Some(10)

@Test
def getNumericValue05(): Bool = Char.getNumericValue('a') == Some(10)

@Test
def getNumericValue06(): Bool = Char.getNumericValue('F') == Some(15)

@Test
def getNumericValue07(): Bool = Char.getNumericValue('f') == Some(15)

@Test
def getNumericValue08(): Bool = Char.getNumericValue(' ') == None

@Test
def getNumericValue09(): Bool = Char.getNumericValue('+') == None

@Test
def getNumericValue10(): Bool = Char.getNumericValue('-') == None

/////////////////////////////////////////////////////////////////////////////
// digit                                                                   //
/////////////////////////////////////////////////////////////////////////////

@Test
def digit01(): Bool = Char.digit(radix = 10, '0') == Some(0)

@Test
def digit02(): Bool = Char.digit(radix = 10, '1') == Some(1)

@Test
def digit03(): Bool = Char.digit(radix = 10, '9') == Some(9)

@Test
def digit04(): Bool = Char.digit(radix = 10, 'A') == None

@Test
def digit05(): Bool = Char.digit(radix = 10, 'a') == None

@Test
def digit06(): Bool = Char.digit(radix = 10, 'F') == None

@Test
def digit07(): Bool = Char.digit(radix = 10, 'f') == None

@Test
def digit08(): Bool = Char.digit(radix = 10, ' ') == None

@Test
def digit09(): Bool = Char.digit(radix = 10, '+') == None

@Test
def digit10(): Bool = Char.digit(radix = 10, '-') == None

@Test
def digit11(): Bool = Char.digit(radix = 16, '0') == Some(0)

@Test
def digit12(): Bool = Char.digit(radix = 16, '1') == Some(1)

@Test
def digit13(): Bool = Char.digit(radix = 16, '9') == Some(9)

@Test
def digit14(): Bool = Char.digit(radix = 16, 'A') == Some(10)

@Test
def digit15(): Bool = Char.digit(radix = 16, 'a') == Some(10)

@Test
def digit16(): Bool = Char.digit(radix = 16, 'F') == Some(15)

@Test
def digit17(): Bool = Char.digit(radix = 16, 'f') == Some(15)

/////////////////////////////////////////////////////////////////////////////
// forDigit                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def forDigit01(): Bool = Char.forDigit(radix = 10, 0) == Some('0')

@Test
def forDigit02(): Bool = Char.forDigit(radix = 10, 1) == Some('1')

@Test
def forDigit03(): Bool = Char.forDigit(radix = 10, 9) == Some('9')

@Test
def forDigit04(): Bool = Char.forDigit(radix = 10, 11) == None              // 'A'

@Test
def forDigit05(): Bool = Char.forDigit(radix = 10, 15) == None              // 'F'

@Test
def forDigit06(): Bool = Char.forDigit(radix = 10, 256) == None

@Test
def forDigit07(): Bool = Char.forDigit(radix = 16, 0) == Some('0')

@Test
def forDigit08(): Bool = Char.forDigit(radix = 16, 1) == Some('1')

@Test
def forDigit09(): Bool = Char.forDigit(radix = 16, 9) == Some('9')

@Test
def forDigit10(): Bool = Char.forDigit(radix = 16, 10) == Some('a')

@Test
def forDigit11(): Bool = Char.forDigit(radix = 16, 15) == Some('f')

/////////////////////////////////////////////////////////////////////////////
// charValue                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def charValue01(): Bool =
    let c = Char.valueOf('0');
    Char.charValue(c) == '0'

@Test
def charValue02(): Bool =
    let c = Char.valueOf('A');
    Char.charValue(c) == 'A'

@Test
def charValue03(): Bool =
    let c = Char.valueOf('+');
    Char.charValue(c) == '+'

/////////////////////////////////////////////////////////////////////////////
// valueOf                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def valueOf01(): Bool =
    import java.lang.Character.equals(##java.lang.Object): Bool \ {};
    let c = Char.valueOf('0');
    equals(c, checked_cast(Char.valueOf('0')))

@Test
def valueOf02(): Bool =
    import java.lang.Character.equals(##java.lang.Object): Bool \ {};
    let c = Char.valueOf('A');
    equals(c, checked_cast(Char.valueOf('A')))

@Test
def valueOf03(): Bool =
    import java.lang.Character.equals(##java.lang.Object): Bool \ {};
    let c = Char.valueOf('+');
    equals(c, checked_cast(Char.valueOf('+')))

}
