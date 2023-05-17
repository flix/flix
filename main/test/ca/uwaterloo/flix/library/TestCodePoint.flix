/*
 * Copyright 2023 Stephen Tetley
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

mod TestCodePoint {

/////////////////////////////////////////////////////////////////////////////
// isAscii                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def isAscii01(): Bool = CodePoint.isAscii(0x0061) == true       // 'a'

@test
def isAscii02(): Bool = CodePoint.isAscii(0x0041) == true       // 'A'

@test
def isAscii03(): Bool = CodePoint.isAscii(0x0020) == true       // ' '

@test
def isAscii04(): Bool = CodePoint.isAscii(0x00b1) == false      // plus-minus sign

@test
def isAscii05(): Bool = CodePoint.isAscii(0x01F2) == false      // Dz (single char, upper case)

@test
def isAscii06(): Bool = CodePoint.isAscii(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isLetter                                                                //
/////////////////////////////////////////////////////////////////////////////

@test
def isLetter01(): Bool = CodePoint.isLetter(0x0061) == true     // 'a'

@test
def isLetter02(): Bool = CodePoint.isLetter(0x0041) == true     // 'A'

@test
def isLetter03(): Bool = CodePoint.isLetter(0x0020) == false    // ' '

@test
def isLetter04(): Bool = CodePoint.isLetter(0x00b1) == false    // plus-minus sign

@test
def isLetter05(): Bool = CodePoint.isLetter(0x01F2) == true      // Dz (single char, upper case)

@test
def isLetter06(): Bool = CodePoint.isLetter(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isDigit                                                                 //
/////////////////////////////////////////////////////////////////////////////

@test
def isDigit01(): Bool = CodePoint.isDigit(0x0031) == true       // '1'

@test
def isDigit02(): Bool = CodePoint.isDigit(0x0061) == false      // 'a'

@test
def isDigit03(): Bool = CodePoint.isDigit(0x0041) == false      // 'A'

@test
def isDigit04(): Bool = CodePoint.isDigit(0x0020) == false      // ' '

@test
def isDigit05(): Bool = CodePoint.isDigit(0x00b1) == false      // plus-minus sign

@test
def isDigit06(): Bool = CodePoint.isDigit(0x01F2) == false      // Dz (single char, upper case)

@test
def isDigit07(): Bool = CodePoint.isDigit(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isLetterOrDigit                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def isLetterOrDigit01(): Bool = CodePoint.isLetterOrDigit(0x0031) == true       // '1'

@test
def isLetterOrDigit02(): Bool = CodePoint.isLetterOrDigit(0x0061) == true      // 'a'

@test
def isLetterOrDigit03(): Bool = CodePoint.isLetterOrDigit(0x0041) == true      // 'A'

@test
def isLetterOrDigit04(): Bool = CodePoint.isLetterOrDigit(0x0020) == false      // ' '

@test
def isLetterOrDigit05(): Bool = CodePoint.isLetterOrDigit(0x00b1) == false      // plus-minus sign

@test
def isLetterOrDigit06(): Bool = CodePoint.isLetterOrDigit(0x01F2) == true      // Dz (single char, upper case)

@test
def isLetterOrDigit07(): Bool = CodePoint.isLetterOrDigit(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isAsciiDigit                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def isAsciiDigit01(): Bool = CodePoint.isAsciiDigit(0x0031) == true       // '1'

@test
def isAsciiDigit02(): Bool = CodePoint.isAsciiDigit(0x0061) == false     // 'a'

@test
def isAsciiDigit03(): Bool = CodePoint.isAsciiDigit(0x0041) == false     // 'A'

@test
def isAsciiDigit04(): Bool = CodePoint.isAsciiDigit(0x0020) == false      // ' '

@test
def isAsciiDigit05(): Bool = CodePoint.isAsciiDigit(0x00b1) == false      // plus-minus sign

@test
def isAsciiDigit06(): Bool = CodePoint.isAsciiDigit(0x01F2) == false      // Dz (single char, upper case)

@test
def isAsciiDigit07(): Bool = CodePoint.isAsciiDigit(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isOctDigit                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def isOctDigit01(): Bool = CodePoint.isOctDigit(0x0031) == true     // '1'

@test
def isOctDigit02(): Bool = CodePoint.isOctDigit(0x0061) == false    // 'a'

@test
def isOctDigit03(): Bool = CodePoint.isOctDigit(0x0041) == false    // 'A'

@test
def isOctDigit04(): Bool = CodePoint.isOctDigit(0x0020) == false    // ' '

@test
def isOctDigit05(): Bool = CodePoint.isOctDigit(0x00b1) == false    // plus-minus sign

@test
def isOctDigit06(): Bool = CodePoint.isOctDigit(0x01F2) == false    // Dz (single char, upper case)

@test
def isOctDigit07(): Bool = CodePoint.isOctDigit(0x1f3dd) == false   // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isHexDigit                                                              //
/////////////////////////////////////////////////////////////////////////////

@test
def isHexDigit01(): Bool = CodePoint.isHexDigit(0x0031) == true     // '1'

@test
def isHexDigit02(): Bool = CodePoint.isHexDigit(0x0061) == true     // 'a'

@test
def isHexDigit03(): Bool = CodePoint.isHexDigit(0x0041) == true     // 'A'

@test
def isHexDigit04(): Bool = CodePoint.isHexDigit(0x0020) == false    // ' '

@test
def isHexDigit05(): Bool = CodePoint.isHexDigit(0x00b1) == false    // plus-minus sign

@test
def isHexDigit06(): Bool = CodePoint.isHexDigit(0x01F2) == false    // Dz (single char, upper case)

@test
def isHexDigit07(): Bool = CodePoint.isHexDigit(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isLowerCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def isLowerCase01(): Bool = CodePoint.isLowerCase(0x0031) == false    // '1'

@test
def isLowerCase02(): Bool = CodePoint.isLowerCase(0x0061) == true     // 'a'

@test
def isLowerCase03(): Bool = CodePoint.isLowerCase(0x0041) == false    // 'A'

@test
def isLowerCase04(): Bool = CodePoint.isLowerCase(0x0020) == false    // ' '

@test
def isLowerCase05(): Bool = CodePoint.isLowerCase(0x00b1) == false    // plus-minus sign

@test
def isLowerCase06(): Bool = CodePoint.isLowerCase(0x01F2) == false    // Dz (single char, upper case)

@test
def isLowerCase07(): Bool = CodePoint.isLowerCase(0x01F3) == true     // dz (single char)

@test
def isLowerCase08(): Bool = CodePoint.isLowerCase(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def isUpperCase01(): Bool = CodePoint.isUpperCase(0x0031) == false    // '1'

@test
def isUpperCase02(): Bool = CodePoint.isUpperCase(0x0061) == false   // 'a'

@test
def isUpperCase03(): Bool = CodePoint.isUpperCase(0x0041) == true    // 'A'

@test
def isUpperCase04(): Bool = CodePoint.isUpperCase(0x0020) == false    // ' '

@test
def isUpperCase05(): Bool = CodePoint.isUpperCase(0x00b1) == false    // plus-minus sign

@test
def isUpperCase06(): Bool = CodePoint.isUpperCase(0x01F2) == false    // Dz (single char, title case)

@test
def isUpperCase07(): Bool = CodePoint.isUpperCase(0x01F3) == false     // dz (single char)

@test
def isUpperCase08(): Bool = CodePoint.isUpperCase(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@test
def isTitleCase01(): Bool = CodePoint.isTitleCase(0x0031) == false      // '1'

@test
def isTitleCase02(): Bool = CodePoint.isTitleCase(0x0061) == false      // 'a'

@test
def isTitleCase03(): Bool = CodePoint.isTitleCase(0x0041) == false      // 'A'

@test
def isTitleCase04(): Bool = CodePoint.isTitleCase(0x0020) == false      // ' '

@test
def isTitleCase05(): Bool = CodePoint.isTitleCase(0x00b1) == false      // plus-minus sign

@test
def isTitleCase06(): Bool = CodePoint.isTitleCase(0x01F2) == true       // Dz (single char, title case)

@test
def isTitleCase07(): Bool = CodePoint.isTitleCase(0x01F3) == false     // dz (single char)

@test
def isTitleCase08(): Bool = CodePoint.isTitleCase(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isWhiteSpace                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def isWhiteSpace01(): Bool = CodePoint.isWhiteSpace(0x0031) == false      // '1'

@test
def isWhiteSpace02(): Bool = CodePoint.isWhiteSpace(0x0061) == false      // 'a'

@test
def isWhiteSpace03(): Bool = CodePoint.isWhiteSpace(0x0041) == false      // 'A'

@test
def isWhiteSpace04(): Bool = CodePoint.isWhiteSpace(0x0020) == true      // ' '

@test
def isWhiteSpace05(): Bool = CodePoint.isWhiteSpace(0x00b1) == false      // plus-minus sign

@test
def isWhiteSpace06(): Bool = CodePoint.isWhiteSpace(0x01F2) == false      // Dz (single char, title case)

@test
def isWhiteSpace07(): Bool = CodePoint.isWhiteSpace(0x01F3) == false      // dz (single char)

@test
def isWhiteSpace08(): Bool = CodePoint.isWhiteSpace(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isAlphabetic                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def isAlphabetic01(): Bool = CodePoint.isAlphabetic(0x0031) == false      // '1'

@test
def isAlphabetic02(): Bool = CodePoint.isAlphabetic(0x0061) == true       // 'a'

@test
def isAlphabetic03(): Bool = CodePoint.isAlphabetic(0x0041) == true       // 'A'

@test
def isAlphabetic04(): Bool = CodePoint.isAlphabetic(0x0020) == false      // ' '

@test
def isAlphabetic05(): Bool = CodePoint.isAlphabetic(0x00b1) == false      // plus-minus sign

@test
def isAlphabetic06(): Bool = CodePoint.isAlphabetic(0x01F2) == true       // Dz (single char, title case)

@test
def isAlphabetic07(): Bool = CodePoint.isAlphabetic(0x01F3) == true       // dz (single char)

@test
def isAlphabetic08(): Bool = CodePoint.isAlphabetic(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isBmpCodePoint                                                          //
/////////////////////////////////////////////////////////////////////////////

@test
def isBmpCodePoint01(): Bool = CodePoint.isBmpCodePoint(0x0031) == true       // '1'

@test
def isBmpCodePoint02(): Bool = CodePoint.isBmpCodePoint(0x0061) == true       // 'a'

@test
def isBmpCodePoint03(): Bool = CodePoint.isBmpCodePoint(0x0041) == true       // 'A'

@test
def isBmpCodePoint04(): Bool = CodePoint.isBmpCodePoint(0x0020) == true       // ' '

@test
def isBmpCodePoint05(): Bool = CodePoint.isBmpCodePoint(0x00b1) == true       // plus-minus sign

@test
def isBmpCodePoint06(): Bool = CodePoint.isBmpCodePoint(0x01F2) == true       // Dz (single char, title case)

@test
def isBmpCodePoint07(): Bool = CodePoint.isBmpCodePoint(0x01F3) == true       // dz (single char)

@test
def isBmpCodePoint08(): Bool = CodePoint.isBmpCodePoint(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isDefined                                                               //
/////////////////////////////////////////////////////////////////////////////

@test
def isDefined01(): Bool = CodePoint.isDefined(0x0031) == true       // '1'

@test
def isDefined02(): Bool = CodePoint.isDefined(0x0061) == true       // 'a'

@test
def isDefined03(): Bool = CodePoint.isDefined(0x0041) == true       // 'A'

@test
def isDefined04(): Bool = CodePoint.isDefined(0x0020) == true       // ' '

@test
def isDefined05(): Bool = CodePoint.isDefined(0x00b1) == true       // plus-minus sign

@test
def isDefined06(): Bool = CodePoint.isDefined(0x01F2) == true       // Dz (single char, title case)

@test
def isDefined07(): Bool = CodePoint.isDefined(0x01F3) == true       // dz (single char)

@test
def isDefined08(): Bool = CodePoint.isDefined(0x1f3dd) == true      // Desert Island pictogram

@test
def isDefined09(): Bool = CodePoint.isDefined(CodePoint.minValue()-1) == false

@test
def isDefined10(): Bool = CodePoint.isDefined(CodePoint.maxValue()+1) == false

/////////////////////////////////////////////////////////////////////////////
// isIdeographic                                                           //
/////////////////////////////////////////////////////////////////////////////

@test
def isIdeographic01(): Bool = CodePoint.isIdeographic(0x0031) == false      // '1'

@test
def isIdeographic02(): Bool = CodePoint.isIdeographic(0x0061) == false      // 'a'

@test
def isIdeographic03(): Bool = CodePoint.isIdeographic(0x0041) == false      // 'A'

@test
def isIdeographic04(): Bool = CodePoint.isIdeographic(0x0020) == false      // ' '

@test
def isIdeographic05(): Bool = CodePoint.isIdeographic(0x00b1) == false      // plus-minus sign

@test
def isIdeographic06(): Bool = CodePoint.isIdeographic(0x01F2) == false      // Dz (single char, title case)

@test
def isIdeographic07(): Bool = CodePoint.isIdeographic(0x01F3) == false      // dz (single char)

@test
def isIdeographic08(): Bool = CodePoint.isIdeographic(0x1f3dd) == false     // Desert Island pictogram

@test
def isIdeographic09(): Bool = CodePoint.isIdeographic(0x4E00) == true       // CJK Unified Ideograph-4E00

/////////////////////////////////////////////////////////////////////////////
// isISOControl                                                            //
/////////////////////////////////////////////////////////////////////////////

@test
def isISOControl01(): Bool = CodePoint.isISOControl(0x0031) == false      // '1'

@test
def isISOControl02(): Bool = CodePoint.isISOControl(0x0061) == false      // 'a'

@test
def isISOControl03(): Bool = CodePoint.isISOControl(0x0041) == false      // 'A'

@test
def isISOControl04(): Bool = CodePoint.isISOControl(0x1f3dd) == false     // Desert Island pictogram

@test
def isISOControl05(): Bool = CodePoint.isISOControl(0x0000) == true

@test
def isISOControl06(): Bool = CodePoint.isISOControl(0x0015) == true        // NAK

/////////////////////////////////////////////////////////////////////////////
// getNumericValue                                                         //
/////////////////////////////////////////////////////////////////////////////

@test
def getNumericValue01(): Bool = CodePoint.getNumericValue(0x0030) == Some(0)       // '0'

@test
def getNumericValue02(): Bool = CodePoint.getNumericValue(0x0031) == Some(1)       // '1'

@test
def getNumericValue03(): Bool = CodePoint.getNumericValue(0x0039) == Some(9)       // '9'

@test
def getNumericValue04(): Bool = CodePoint.getNumericValue(0x0041) == Some(10)      // 'A'

@test
def getNumericValue05(): Bool = CodePoint.getNumericValue(0x0041) == Some(10)      // 'a'

@test
def getNumericValue06(): Bool = CodePoint.getNumericValue(0x0046) == Some(15)      // 'F'

@test
def getNumericValue07(): Bool = CodePoint.getNumericValue(0x0066) == Some(15)      // 'f'

@test
def getNumericValue08(): Bool = CodePoint.getNumericValue(0x0020) == None          // ' ' (space)

@test
def getNumericValue09(): Bool = CodePoint.getNumericValue(0x002b) == None          // '+'

@test
def getNumericValue10(): Bool = CodePoint.getNumericValue(0x002d) == None          // '-'

@test
def getNumericValue11(): Bool = CodePoint.getNumericValue(0x1f3dd) == None          // Desert Island pictogram

}
