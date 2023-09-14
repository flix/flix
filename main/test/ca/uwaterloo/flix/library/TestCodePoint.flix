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

def isSome(o: Option[a]): Bool = match o {
    case Some(_) => true
    case None    => false
}

def isNone(o: Option[a]): Bool = not (isSome(o))

/////////////////////////////////////////////////////////////////////////////
// isAscii                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def isAscii01(): Bool = CodePoint.isAscii(0x0061) == true       // 'a'

@Test
def isAscii02(): Bool = CodePoint.isAscii(0x0041) == true       // 'A'

@Test
def isAscii03(): Bool = CodePoint.isAscii(0x0020) == true       // ' '

@Test
def isAscii04(): Bool = CodePoint.isAscii(0x00b1) == false      // plus-minus sign

@Test
def isAscii05(): Bool = CodePoint.isAscii(0x01F2) == false      // Dz (single char, upper case)

@Test
def isAscii06(): Bool = CodePoint.isAscii(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isLetter                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def isLetter01(): Bool = CodePoint.isLetter(0x0061) == true     // 'a'

@Test
def isLetter02(): Bool = CodePoint.isLetter(0x0041) == true     // 'A'

@Test
def isLetter03(): Bool = CodePoint.isLetter(0x0020) == false    // ' '

@Test
def isLetter04(): Bool = CodePoint.isLetter(0x00b1) == false    // plus-minus sign

@Test
def isLetter05(): Bool = CodePoint.isLetter(0x01F2) == true      // Dz (single char, upper case)

@Test
def isLetter06(): Bool = CodePoint.isLetter(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isDigit                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def isDigit01(): Bool = CodePoint.isDigit(0x0031) == true       // '1'

@Test
def isDigit02(): Bool = CodePoint.isDigit(0x0061) == false      // 'a'

@Test
def isDigit03(): Bool = CodePoint.isDigit(0x0041) == false      // 'A'

@Test
def isDigit04(): Bool = CodePoint.isDigit(0x0020) == false      // ' '

@Test
def isDigit05(): Bool = CodePoint.isDigit(0x00b1) == false      // plus-minus sign

@Test
def isDigit06(): Bool = CodePoint.isDigit(0x01F2) == false      // Dz (single char, upper case)

@Test
def isDigit07(): Bool = CodePoint.isDigit(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isLetterOrDigit                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def isLetterOrDigit01(): Bool = CodePoint.isLetterOrDigit(0x0031) == true       // '1'

@Test
def isLetterOrDigit02(): Bool = CodePoint.isLetterOrDigit(0x0061) == true      // 'a'

@Test
def isLetterOrDigit03(): Bool = CodePoint.isLetterOrDigit(0x0041) == true      // 'A'

@Test
def isLetterOrDigit04(): Bool = CodePoint.isLetterOrDigit(0x0020) == false      // ' '

@Test
def isLetterOrDigit05(): Bool = CodePoint.isLetterOrDigit(0x00b1) == false      // plus-minus sign

@Test
def isLetterOrDigit06(): Bool = CodePoint.isLetterOrDigit(0x01F2) == true      // Dz (single char, upper case)

@Test
def isLetterOrDigit07(): Bool = CodePoint.isLetterOrDigit(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isAsciiDigit                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def isAsciiDigit01(): Bool = CodePoint.isAsciiDigit(0x0031) == true       // '1'

@Test
def isAsciiDigit02(): Bool = CodePoint.isAsciiDigit(0x0061) == false     // 'a'

@Test
def isAsciiDigit03(): Bool = CodePoint.isAsciiDigit(0x0041) == false     // 'A'

@Test
def isAsciiDigit04(): Bool = CodePoint.isAsciiDigit(0x0020) == false      // ' '

@Test
def isAsciiDigit05(): Bool = CodePoint.isAsciiDigit(0x00b1) == false      // plus-minus sign

@Test
def isAsciiDigit06(): Bool = CodePoint.isAsciiDigit(0x01F2) == false      // Dz (single char, upper case)

@Test
def isAsciiDigit07(): Bool = CodePoint.isAsciiDigit(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isOctDigit                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def isOctDigit01(): Bool = CodePoint.isOctDigit(0x0031) == true     // '1'

@Test
def isOctDigit02(): Bool = CodePoint.isOctDigit(0x0061) == false    // 'a'

@Test
def isOctDigit03(): Bool = CodePoint.isOctDigit(0x0041) == false    // 'A'

@Test
def isOctDigit04(): Bool = CodePoint.isOctDigit(0x0020) == false    // ' '

@Test
def isOctDigit05(): Bool = CodePoint.isOctDigit(0x00b1) == false    // plus-minus sign

@Test
def isOctDigit06(): Bool = CodePoint.isOctDigit(0x01F2) == false    // Dz (single char, upper case)

@Test
def isOctDigit07(): Bool = CodePoint.isOctDigit(0x1f3dd) == false   // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isHexDigit                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def isHexDigit01(): Bool = CodePoint.isHexDigit(0x0031) == true     // '1'

@Test
def isHexDigit02(): Bool = CodePoint.isHexDigit(0x0061) == true     // 'a'

@Test
def isHexDigit03(): Bool = CodePoint.isHexDigit(0x0041) == true     // 'A'

@Test
def isHexDigit04(): Bool = CodePoint.isHexDigit(0x0020) == false    // ' '

@Test
def isHexDigit05(): Bool = CodePoint.isHexDigit(0x00b1) == false    // plus-minus sign

@Test
def isHexDigit06(): Bool = CodePoint.isHexDigit(0x01F2) == false    // Dz (single char, upper case)

@Test
def isHexDigit07(): Bool = CodePoint.isHexDigit(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isLowerCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def isLowerCase01(): Bool = CodePoint.isLowerCase(0x0031) == false    // '1'

@Test
def isLowerCase02(): Bool = CodePoint.isLowerCase(0x0061) == true     // 'a'

@Test
def isLowerCase03(): Bool = CodePoint.isLowerCase(0x0041) == false    // 'A'

@Test
def isLowerCase04(): Bool = CodePoint.isLowerCase(0x0020) == false    // ' '

@Test
def isLowerCase05(): Bool = CodePoint.isLowerCase(0x00b1) == false    // plus-minus sign

@Test
def isLowerCase06(): Bool = CodePoint.isLowerCase(0x01F2) == false    // Dz (single char, upper case)

@Test
def isLowerCase07(): Bool = CodePoint.isLowerCase(0x01F3) == true     // dz (single char)

@Test
def isLowerCase08(): Bool = CodePoint.isLowerCase(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def isUpperCase01(): Bool = CodePoint.isUpperCase(0x0031) == false    // '1'

@Test
def isUpperCase02(): Bool = CodePoint.isUpperCase(0x0061) == false   // 'a'

@Test
def isUpperCase03(): Bool = CodePoint.isUpperCase(0x0041) == true    // 'A'

@Test
def isUpperCase04(): Bool = CodePoint.isUpperCase(0x0020) == false    // ' '

@Test
def isUpperCase05(): Bool = CodePoint.isUpperCase(0x00b1) == false    // plus-minus sign

@Test
def isUpperCase06(): Bool = CodePoint.isUpperCase(0x01F2) == false    // Dz (single char, title case)

@Test
def isUpperCase07(): Bool = CodePoint.isUpperCase(0x01F3) == false     // dz (single char)

@Test
def isUpperCase08(): Bool = CodePoint.isUpperCase(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def isTitleCase01(): Bool = CodePoint.isTitleCase(0x0031) == false      // '1'

@Test
def isTitleCase02(): Bool = CodePoint.isTitleCase(0x0061) == false      // 'a'

@Test
def isTitleCase03(): Bool = CodePoint.isTitleCase(0x0041) == false      // 'A'

@Test
def isTitleCase04(): Bool = CodePoint.isTitleCase(0x0020) == false      // ' '

@Test
def isTitleCase05(): Bool = CodePoint.isTitleCase(0x00b1) == false      // plus-minus sign

@Test
def isTitleCase06(): Bool = CodePoint.isTitleCase(0x01F2) == true       // Dz (single char, title case)

@Test
def isTitleCase07(): Bool = CodePoint.isTitleCase(0x01F3) == false     // dz (single char)

@Test
def isTitleCase08(): Bool = CodePoint.isTitleCase(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isWhiteSpace                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def isWhiteSpace01(): Bool = CodePoint.isWhiteSpace(0x0031) == false      // '1'

@Test
def isWhiteSpace02(): Bool = CodePoint.isWhiteSpace(0x0061) == false      // 'a'

@Test
def isWhiteSpace03(): Bool = CodePoint.isWhiteSpace(0x0041) == false      // 'A'

@Test
def isWhiteSpace04(): Bool = CodePoint.isWhiteSpace(0x0020) == true      // ' '

@Test
def isWhiteSpace05(): Bool = CodePoint.isWhiteSpace(0x00b1) == false      // plus-minus sign

@Test
def isWhiteSpace06(): Bool = CodePoint.isWhiteSpace(0x01F2) == false      // Dz (single char, title case)

@Test
def isWhiteSpace07(): Bool = CodePoint.isWhiteSpace(0x01F3) == false      // dz (single char)

@Test
def isWhiteSpace08(): Bool = CodePoint.isWhiteSpace(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isAlphabetic                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def isAlphabetic01(): Bool = CodePoint.isAlphabetic(0x0031) == false      // '1'

@Test
def isAlphabetic02(): Bool = CodePoint.isAlphabetic(0x0061) == true       // 'a'

@Test
def isAlphabetic03(): Bool = CodePoint.isAlphabetic(0x0041) == true       // 'A'

@Test
def isAlphabetic04(): Bool = CodePoint.isAlphabetic(0x0020) == false      // ' '

@Test
def isAlphabetic05(): Bool = CodePoint.isAlphabetic(0x00b1) == false      // plus-minus sign

@Test
def isAlphabetic06(): Bool = CodePoint.isAlphabetic(0x01F2) == true       // Dz (single char, title case)

@Test
def isAlphabetic07(): Bool = CodePoint.isAlphabetic(0x01F3) == true       // dz (single char)

@Test
def isAlphabetic08(): Bool = CodePoint.isAlphabetic(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isBmpCodePoint                                                          //
/////////////////////////////////////////////////////////////////////////////

@Test
def isBmpCodePoint01(): Bool = CodePoint.isBmpCodePoint(0x0031) == true       // '1'

@Test
def isBmpCodePoint02(): Bool = CodePoint.isBmpCodePoint(0x0061) == true       // 'a'

@Test
def isBmpCodePoint03(): Bool = CodePoint.isBmpCodePoint(0x0041) == true       // 'A'

@Test
def isBmpCodePoint04(): Bool = CodePoint.isBmpCodePoint(0x0020) == true       // ' '

@Test
def isBmpCodePoint05(): Bool = CodePoint.isBmpCodePoint(0x00b1) == true       // plus-minus sign

@Test
def isBmpCodePoint06(): Bool = CodePoint.isBmpCodePoint(0x01F2) == true       // Dz (single char, title case)

@Test
def isBmpCodePoint07(): Bool = CodePoint.isBmpCodePoint(0x01F3) == true       // dz (single char)

@Test
def isBmpCodePoint08(): Bool = CodePoint.isBmpCodePoint(0x1f3dd) == false     // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isDefined                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def isDefined01(): Bool = CodePoint.isDefined(0x0031) == true       // '1'

@Test
def isDefined02(): Bool = CodePoint.isDefined(0x0061) == true       // 'a'

@Test
def isDefined03(): Bool = CodePoint.isDefined(0x0041) == true       // 'A'

@Test
def isDefined04(): Bool = CodePoint.isDefined(0x0020) == true       // ' '

@Test
def isDefined05(): Bool = CodePoint.isDefined(0x00b1) == true       // plus-minus sign

@Test
def isDefined06(): Bool = CodePoint.isDefined(0x01F2) == true       // Dz (single char, title case)

@Test
def isDefined07(): Bool = CodePoint.isDefined(0x01F3) == true       // dz (single char)

@Test
def isDefined08(): Bool = CodePoint.isDefined(0x1f3dd) == true      // Desert Island pictogram

@Test
def isDefined09(): Bool = CodePoint.isDefined(CodePoint.minValue()-1) == false

@Test
def isDefined10(): Bool = CodePoint.isDefined(CodePoint.maxValue()+1) == false

/////////////////////////////////////////////////////////////////////////////
// isIdeographic                                                           //
/////////////////////////////////////////////////////////////////////////////

@Test
def isIdeographic01(): Bool = CodePoint.isIdeographic(0x0031) == false      // '1'

@Test
def isIdeographic02(): Bool = CodePoint.isIdeographic(0x0061) == false      // 'a'

@Test
def isIdeographic03(): Bool = CodePoint.isIdeographic(0x0041) == false      // 'A'

@Test
def isIdeographic04(): Bool = CodePoint.isIdeographic(0x0020) == false      // ' '

@Test
def isIdeographic05(): Bool = CodePoint.isIdeographic(0x00b1) == false      // plus-minus sign

@Test
def isIdeographic06(): Bool = CodePoint.isIdeographic(0x01F2) == false      // Dz (single char, title case)

@Test
def isIdeographic07(): Bool = CodePoint.isIdeographic(0x01F3) == false      // dz (single char)

@Test
def isIdeographic08(): Bool = CodePoint.isIdeographic(0x1f3dd) == false     // Desert Island pictogram

@Test
def isIdeographic09(): Bool = CodePoint.isIdeographic(0x4E00) == true       // CJK Unified Ideograph-4E00

/////////////////////////////////////////////////////////////////////////////
// isISOControl                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def isISOControl01(): Bool = CodePoint.isISOControl(0x0031) == false      // '1'

@Test
def isISOControl02(): Bool = CodePoint.isISOControl(0x0061) == false      // 'a'

@Test
def isISOControl03(): Bool = CodePoint.isISOControl(0x0041) == false      // 'A'

@Test
def isISOControl04(): Bool = CodePoint.isISOControl(0x1f3dd) == false     // Desert Island pictogram

@Test
def isISOControl05(): Bool = CodePoint.isISOControl(0x0000) == true

@Test
def isISOControl06(): Bool = CodePoint.isISOControl(0x0015) == true        // NAK

/////////////////////////////////////////////////////////////////////////////
// isMirrored                                                              //
/////////////////////////////////////////////////////////////////////////////

@Test
def isMirrored01(): Bool = CodePoint.isMirrored(0x0061) == false        // 'a'

@Test
def isMirrored02(): Bool = CodePoint.isMirrored(0x0041) == false        // 'A'

@Test
def isMirrored03(): Bool = CodePoint.isMirrored(0x005B) == true         // '['

@Test
def isMirrored04(): Bool = CodePoint.isMirrored(0x005B) == true         // ']'

@Test
def isMirrored05(): Bool = CodePoint.isMirrored(0x1f3dd) == false       // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isSupplementaryCodePoint                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def isSupplementaryCodePoint01(): Bool = CodePoint.isSupplementaryCodePoint(0x0031) == false      // '1'

@Test
def isSupplementaryCodePoint02(): Bool = CodePoint.isSupplementaryCodePoint(0x0061) == false      // 'a'

@Test
def isSupplementaryCodePoint03(): Bool = CodePoint.isSupplementaryCodePoint(0x1f3dd) == true       // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// isValidCodePoint                                                        //
/////////////////////////////////////////////////////////////////////////////

@Test
def isValidCodePoint01(): Bool = CodePoint.isValidCodePoint(0x0031) == true       // '1'

@Test
def isValidCodePoint02(): Bool = CodePoint.isValidCodePoint(0x0061) == true       // 'a'

@Test
def isValidCodePoint03(): Bool = CodePoint.isValidCodePoint(0x1f3dd) == true       // Desert Island pictogram

@Test
def isValidCodePoint04(): Bool = CodePoint.isValidCodePoint(0x110000) == false      // out-of-range

/////////////////////////////////////////////////////////////////////////////
// toLowerCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def toLowerCase01(): Bool = CodePoint.toLowerCase(0x0061) == 0x0061         // 'a'

@Test
def toLowerCase02(): Bool = CodePoint.toLowerCase(0x0041) == 0x0061         // 'A' => 'a'

@Test
def toLowerCase03(): Bool = CodePoint.toLowerCase(0x0020) == 0x0020         // ' ' (space)

@Test
def toLowerCase04(): Bool = CodePoint.toLowerCase(0x01F2) == 0x01F3         // Dz (single char, title case) => dz

@Test
def toLowerCase05(): Bool = CodePoint.toLowerCase(0x01F3) == 0x01F3         // dz (single char)

@Test
def toLowerCase06(): Bool = CodePoint.toLowerCase(0x1f3dd) == 0x1f3dd       // Desert Island pictogram


/////////////////////////////////////////////////////////////////////////////
// toUpperCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def toUpperCase01(): Bool = CodePoint.toUpperCase(0x0061) == 0x0041        // 'a' => 'A'

@Test
def toUpperCase02(): Bool = CodePoint.toUpperCase(0x0041) == 0x0041         // 'A'

@Test
def toUpperCase03(): Bool = CodePoint.toUpperCase(0x0020) == 0x0020         // ' ' (space)

@Test
def toUpperCase04(): Bool = CodePoint.toUpperCase(0x01F2) == 0x01F1         // Dz (single char, title case) => DZ

@Test
def toUpperCase05(): Bool = CodePoint.toUpperCase(0x01F3) == 0x01F1         // dz (single char) => DZ

@Test
def toUpperCase06(): Bool = CodePoint.toUpperCase(0x1f3dd) == 0x1f3dd       // Desert Island pictogram


/////////////////////////////////////////////////////////////////////////////
// toTitleCase                                                             //
/////////////////////////////////////////////////////////////////////////////

@Test
def toTitleCase01(): Bool = CodePoint.toTitleCase(0x0061) == 0x0041         // 'a' => 'A'

@Test
def toTitleCase02(): Bool = CodePoint.toTitleCase(0x0041) == 0x0041         // 'A'

@Test
def toTitleCase03(): Bool = CodePoint.toTitleCase(0x0020) == 0x0020         // ' ' (space)

@Test
def toTitleCase04(): Bool = CodePoint.toTitleCase(0x01F2) == 0x01F2         // Dz (single char, title case)

@Test
def toTitleCase05(): Bool = CodePoint.toTitleCase(0x01F3) == 0x01F2         // dz (single char) => Dz

@Test
def toTitleCase06(): Bool = CodePoint.toTitleCase(0x1f3dd) == 0x1f3dd       // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// getName                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def getName01(): Bool = CodePoint.getName(0x0061) |> isSome         // 'a'

@Test
def getName02(): Bool = CodePoint.getName(0x0041) |> isSome         // 'A'

@Test
def getName03(): Bool = CodePoint.getName(0x1f3dd) |> isSome        // Desert Island pictogram

@Test
def getName04(): Bool = CodePoint.getName(0x110000) |> isNone       // out-of-range

/////////////////////////////////////////////////////////////////////////////
// getNumericValue                                                         //
/////////////////////////////////////////////////////////////////////////////

@Test
def getNumericValue01(): Bool = CodePoint.getNumericValue(0x0030) == Some(0)       // '0'

@Test
def getNumericValue02(): Bool = CodePoint.getNumericValue(0x0031) == Some(1)       // '1'

@Test
def getNumericValue03(): Bool = CodePoint.getNumericValue(0x0039) == Some(9)       // '9'

@Test
def getNumericValue04(): Bool = CodePoint.getNumericValue(0x0041) == Some(10)      // 'A'

@Test
def getNumericValue05(): Bool = CodePoint.getNumericValue(0x0061) == Some(10)      // 'a'

@Test
def getNumericValue06(): Bool = CodePoint.getNumericValue(0x0046) == Some(15)      // 'F'

@Test
def getNumericValue07(): Bool = CodePoint.getNumericValue(0x0066) == Some(15)      // 'f'

@Test
def getNumericValue08(): Bool = CodePoint.getNumericValue(0x0020) == None          // ' ' (space)

@Test
def getNumericValue09(): Bool = CodePoint.getNumericValue(0x002b) == None          // '+'

@Test
def getNumericValue10(): Bool = CodePoint.getNumericValue(0x002d) == None          // '-'

@Test
def getNumericValue11(): Bool = CodePoint.getNumericValue(0x1f3dd) == None          // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// charCount                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def charCount01(): Bool = CodePoint.charCount(0x0031) == 1          // '1'

@Test
def charCount02(): Bool = CodePoint.charCount(0x0041) == 1          // 'A'

@Test
def charCount03(): Bool = CodePoint.charCount(0x1f3dd) == 2          // Desert Island pictogram

/////////////////////////////////////////////////////////////////////////////
// toChars                                                                 //
/////////////////////////////////////////////////////////////////////////////

@Test
def toChars01(): Bool = CodePoint.toChars(0x0031) == Some(Vector#{'1'})     // '1'

@Test
def toChars02(): Bool = CodePoint.toChars(0x0041) == Some(Vector#{'A'})     // 'A'

@Test
def toChars03(): Bool = CodePoint.toChars(0x1f3dd) == Some(Vector#{'\uD83C', '\uDFDD'})     // Desert Island pictogram

@Test
def toChars04(): Bool = CodePoint.toChars(0x110000) == None                 // out-of-range

/////////////////////////////////////////////////////////////////////////////
// toBmpChar                                                               //
/////////////////////////////////////////////////////////////////////////////

@Test
def toBmpChar01(): Bool = CodePoint.toBmpChar(0x0031) == Some('1')     // '1'

@Test
def toBmpChar02(): Bool = CodePoint.toBmpChar(0x0041) == Some('A')     // 'A'

@Test
def toBmpChar03(): Bool = CodePoint.toBmpChar(0x1f3dd) == None          // Desert Island pictogram

@Test
def toBmpChar04(): Bool = CodePoint.toBmpChar(0x110000) == None         // out-of-range

/////////////////////////////////////////////////////////////////////////////
// toSupplementaryChars                                                    //
/////////////////////////////////////////////////////////////////////////////

@Test
def toSupplementaryChars01(): Bool = CodePoint.toSupplementaryChars(0x0031) == None     // '1'

@Test
def toSupplementaryChars02(): Bool = CodePoint.toSupplementaryChars(0x0041) == None     // 'A'

@Test
def toSupplementaryChars03(): Bool = CodePoint.toSupplementaryChars(0x1F3DD) == Some(('\uD83C', '\uDFDD'))      // Desert Island pictogram

@Test
def toSupplementaryChars04(): Bool = CodePoint.toSupplementaryChars(0x110000) == None   // out-of-range

/////////////////////////////////////////////////////////////////////////////
// toString                                                                //
/////////////////////////////////////////////////////////////////////////////

@Test
def toString01(): Bool = CodePoint.toString(0x0031) == "1"      // '1'

@Test
def toString02(): Bool = CodePoint.toString(0x0041) == "A"      // 'A'

@Test
def toString03(): Bool = CodePoint.toString(0x1F3DD) == "\uD83C\uDFDD"     // Desert Island pictogram

@Test
def toString04(): Bool = CodePoint.toString(0x110000) == ""     // out-of-range


/////////////////////////////////////////////////////////////////////////////
// highSurrogate                                                           //
/////////////////////////////////////////////////////////////////////////////

@Test
def highSurrogate01(): Bool = CodePoint.highSurrogate(0x0041) == None      // 'A'

@Test
def highSurrogate02(): Bool = CodePoint.highSurrogate(0x1F3DD) == Some('\uD83C')     // Desert Island pictogram

@Test
def highSurrogate03(): Bool = CodePoint.highSurrogate(0x110000) == None     // out-of-range

/////////////////////////////////////////////////////////////////////////////
// lowSurrogate                                                            //
/////////////////////////////////////////////////////////////////////////////

@Test
def lowSurrogate01(): Bool = CodePoint.lowSurrogate(0x0041) == None         // 'A'

@Test
def lowSurrogate02(): Bool = CodePoint.lowSurrogate(0x1F3DD) == Some('\uDFDD')     // Desert Island pictogram

@Test
def lowSurrogate03(): Bool = CodePoint.lowSurrogate(0x110000) == None       // out-of-range

}
