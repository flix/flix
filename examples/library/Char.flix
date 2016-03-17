namespace Char {

    ///////////////////////////////////////////////////////////////////////////
    // Char Predicates                                                      ///
    ///////////////////////////////////////////////////////////////////////////

    /**
     *  Returns `true` if the given char `c` is an ASCII character.
     */
    external def isAscii(c: Char): Bool

//- **isLetter(c: Char): Bool**
//    - Returns true iff the character is a letter.
//- **isDigit(c: Char): Bool**
//    - Returns true iff the character is in the range 0...9.
//- **isOctDigit(c: Char): Bool**
//    - Returns true iff the character is in the range 0...7.
//- **isHexDigit(c: Char): Bool**
//    - Returns true iff the character is in the range 0...F.
//- **isLower(c: Char): Bool**
//    - Returns true iff the character is a lowercase letter.
//- **isUpper(c: Char): Bool**
//    - Returns true iff the character is an uppercase letter.
//- **isWhiteSpace(c: Char): Bool**
//    - Returns true iff the character is a white space character.

    ///////////////////////////////////////////////////////////////////////////
    // Char Conversions                                                     ///
    ///////////////////////////////////////////////////////////////////////////

//- **toLower(c: Char): Char**
//    - Converts a letter to its lowercase version.
//    - Returns the original character if it does not have a lowercase version.
//- **toUpper(c: Char): Char**
//    - Converts a letter to its uppercase version.
//    - Returns the original character if it does not have an uppercase version.
//- **toInt(c: Char): Int**
//    - Converts the character to an integer.

    /**
     * Alias for `toInt32`.
     */
    def toInt(c: Char): Int32 = toInt32(c)

    external def toInt8(c: Char): Int8

    external def toInt16(c: Char): Int16

    external def toInt32(c: Char): Int32

    external def toInt64(c: Char): Int64

}
