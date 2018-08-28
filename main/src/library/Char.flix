namespace Char {

    ///
    ///  Returns `true` if the given char `c` is an ascii character.
    ///
    pub def isAscii(c: Char): Bool = native method flix.runtime.library.Char.isAscii(c)

    ///
    /// Returns `true` if the given char `c` is a letter character.
    ///
    pub def isLetter(c: Char): Bool = native method flix.runtime.library.Char.isLetter(c)

    ///
    /// Returns `true` if the given char `c` is in the range 0...9.
    ///
    pub def isDigit(c: Char): Bool = native method flix.runtime.library.Char.isDigit(c)

    ///
    /// Returns `true` if the given char `c` is in the range 0...7.
    ///
    pub def isOctDigit(c: Char): Bool = native method flix.runtime.library.Char.isOctDigit(c)

    ///
    /// Returns `true` if the given char `c` is in the range 0...F.
    ///
    pub def isHexDigit(c: Char): Bool = native method flix.runtime.library.Char.isHexDigit(c)

    ///
    /// Returns `true` if the given char `c` is lowercase.
    ///
    pub def isLowerCase(c: Char): Bool = native method flix.runtime.library.Char.isLowerCase(c)

    ///
    /// Returns `true` if the given char `c` is uppercase.
    ///
    pub def isUpperCase(c: Char): Bool = native method flix.runtime.library.Char.isUpperCase(c)

    ///
    /// Returns `true` if the given char `c` is a white space character.
    ///
    pub def isWhiteSpace(c: Char): Bool = native method flix.runtime.library.Char.isWhiteSpace(c)

    ///
    /// Converts a letter to its lowercase version.
    ///
    /// Returns the original character if it does not have a lowercase version.
    ///
    pub def toLowerCase(c: Char): Char = native method flix.runtime.library.Char.toLowerCase(c)

    ///
    /// Converts a letter to its uppercase version.
    ///
    /// Returns the original character if it does not have a uppercase version.
    ///
    pub def toUpperCase(c: Char): Char = native method flix.runtime.library.Char.toUpperCase(c)

    ///
    /// Returns the character `c` as a string.
    ///
    pub def toString(c: Char): Str = native method flix.runtime.library.Char.toString(c)

}