namespace Char {

    ///////////////////////////////////////////////////////////////////////////
    // Char Predicates                                                      ///
    ///////////////////////////////////////////////////////////////////////////

    /**
     *  Returns `true` if the given char `c` is an ASCII character.
     */
    external def isAscii(c: Char): Bool

    /**
     *  Returns `true` if the given char `c` is a letter character.
     */
    external def isLetter(c: Char): Bool

    /**
     *  Returns `true` if the given char `c` is in the range 0...9.
     */
    external def isDigit(c: Char): Bool

    /**
     *  Returns `true` if the given char `c` is in the range 0...7.
     */
    external def isOctDigit(c: Char): Bool

    /**
     *  Returns `true` if the given char `c` is in the range 0...F.
     */
    external def isHexDigit(c: Char): Bool

    /**
     *  Returns `true` if the given char `c` is lowercase.
     */
    external def isLower(c: Char): Bool

    /**
     *  Returns `true` if the given char `c` is uppercase.
     */
    external def isUpper(c: Char): Bool

    /**
     *  Returns `true` if the given char `c` is a white space character.
     */
    external def isWhiteSpace(c: Char): Bool

    ///////////////////////////////////////////////////////////////////////////
    // Char Conversions                                                     ///
    ///////////////////////////////////////////////////////////////////////////

    /**
     * Converts a letter to its lowercase version.
     *
     * Returns the original character if it does not have a lowercase version.
     */
    external def toLower(c: Char): Char

    /**
     * Converts a letter to its uppercase version.
     *
     * Returns the original character if it does not have a uppercase version.
     */
    external def toUpper(c: Char): Char

    /**
     * Alias for `toInt32`.
     */
    def toInt(c: Char): Int32 = toInt32(c)

    /**
     *  Converts the given character to an Int8. Truncates excess digits.
     */
    external def toInt8(c: Char): Int8

    /**
     *  Converts the given character to an Int16. Truncates excess digits.
     */
    external def toInt16(c: Char): Int16

    /**
     *  Converts the given character to an Int32. Truncates excess digits.
     */
    external def toInt32(c: Char): Int32

    /**
     *  Converts the given character to an Int64. Truncates excess digits.
     */
    external def toInt64(c: Char): Int64

}
