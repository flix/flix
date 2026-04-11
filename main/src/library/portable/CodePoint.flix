///
/// The `CodePoint` module complements the `Char` module. `Char` only
/// represents characters in Unicode's _Basic Multilingual Plane_
/// (U+0000 to U+FFFF).
/// CodePoint extends access to include _supplementary characters_ so all
/// Unicode characters can be represented.
///
pub mod CodePoint {

    ///
    /// Returns the minimum integer value of a Unicode code point.
    ///
    pub def minValue(): Int32 = 0

    ///
    /// Returns the maximum integer value of a Unicode code point.
    ///
    pub def maxValue(): Int32 = 0x10FFFF

    ///
    /// Returns `true` if the given char `c` is an ascii character.
    ///
    pub def isAscii(cp: Int32): Bool =
        0x0000 <= cp and cp <= 0x0080

    ///
    /// Returns `true` if the given code point `cp` represents a letter character.
    ///
    pub def isLetter(cp: Int32): Bool =
        %%CODEPOINT_IS_LETTER%%(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a recognized Unicode digit.
    ///
    pub def isDigit(cp: Int32): Bool =
        %%CODEPOINT_IS_DIGIT%%(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a recognized Unicode letter or digit.
    ///
    pub def isLetterOrDigit(cp: Int32): Bool =
        isLetter(cp) or isDigit(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a character strictly in the
    /// range of ASCII digits 0...9.
    ///
    pub def isAsciiDigit(cp: Int32): Bool =
        isAscii(cp) and isDigit(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a character in the range 0...7.
    ///
    pub def isOctDigit(cp: Int32): Bool =
        0x0030 <= cp and cp <= 0x0037

    ///
    /// Returns `true` if the given code point `cp` represents a character in the range 0...F.
    ///
    pub def isHexDigit(cp: Int32): Bool = match cp {
        case i if 0x0030 <= i and i <= 0x0039 => true
        case i if 0x0041 <= i and i <= 0x0046 => true
        case i if 0x0061 <= i and i <= 0x0066 => true
        case _ => false
    }

    ///
    /// Returns `true` if the given code point `cp` represents a lowercase letter.
    ///
    pub def isLowerCase(cp: Int32): Bool =
        %%CODEPOINT_IS_LOWER_CASE%%(cp)

    ///
    /// Returns `true` if the given code point `cp` represents an uppercase letter.
    ///
    pub def isUpperCase(cp: Int32): Bool =
        %%CODEPOINT_IS_UPPER_CASE%%(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a titlecase letter.
    ///
    pub def isTitleCase(cp: Int32): Bool =
        %%CODEPOINT_IS_TITLE_CASE%%(cp)

    ///
    /// Returns `true` if the given code point `cp` represents a white space character.
    ///
    pub def isWhitespace(cp: Int32): Bool =
        %%CODEPOINT_IS_WHITESPACE%%(cp)

    ///
    /// Returns `true` if the given code point `cp` represents an alphabetic character.
    ///
    pub def isAlphabetic(cp: Int32): Bool =
        %%CODEPOINT_IS_ALPHABETIC%%(cp)

    ///
    /// Returns `true` if the given code point `cp` is within Unicode's Basic Multilingual Plane.
    ///
    pub def isBmpCodePoint(cp: Int32): Bool =
        minValue() <= cp and cp <= 0xFFFF

    ///
    /// Returns `true` if the given code point `cp` is defined.
    ///
    pub def isDefined(cp: Int32): Bool =
        %%CODEPOINT_IS_DEFINED%%(cp)

    ///
    /// Returns `true` if the given code point `cp` is Chinese, Japanese, Korean
    /// or Vietnamese ideograph.
    ///
    pub def isIdeographic(cp: Int32): Bool =
        %%CODEPOINT_IS_IDEOGRAPHIC%%(cp)

    ///
    /// Returns `true` if the given code point `cp` is an ISO control character.
    ///
    pub def isISOControl(cp: Int32): Bool =
        %%CODEPOINT_IS_ISO_CONTROL%%(cp)

    ///
    /// Returns `true` if the given code point `cp` is mirrored.
    ///
    pub def isMirrored(cp: Int32): Bool =
        %%CODEPOINT_IS_MIRRORED%%(cp)

    ///
    /// Returns `true` if the given code point `cp` is in Unicode's supplementary character range.
    ///
    pub def isSupplementaryCodePoint(cp: Int32): Bool =
        0x10000 <= cp and cp <= maxValue()

    ///
    /// Returns `true` if the code point `cp` is between `U+0000` and `U+10FFFF`.
    ///
    pub def isValidCodePoint(cp: Int32): Bool =
        minValue() <= cp and cp <= maxValue()

    ///
    /// Converts a code point representing a letter to its lowercase version.
    ///
    pub def toLowerCase(cp: Int32): Int32 =
        %%CODEPOINT_TO_LOWER_CASE%%(cp)

    ///
    /// Converts a code point representing a letter to its uppercase version.
    ///
    pub def toUpperCase(cp: Int32): Int32 =
        %%CODEPOINT_TO_UPPER_CASE%%(cp)

    ///
    /// Converts a code point representing a letter to its titlecase version.
    ///
    pub def toTitleCase(cp: Int32): Int32 =
        %%CODEPOINT_TO_TITLE_CASE%%(cp)

    ///
    /// Returns the name of the code point `cp`.
    ///
    pub def getName(cp: Int32): Option[String] =
        Object.toOption(%%CODEPOINT_GET_NAME%%(cp))

    ///
    /// Returns the integer value represented by the code point `cp`.
    ///
    pub def getNumericValue(cp: Int32): Option[Int32] =
        match %%CODEPOINT_GET_NUMERIC_VALUE%%(cp) {
            case i if i < 0 => None
            case i          => Some(i)
        }

    ///
    /// Returns the number of Chars needed to represent the code point `cp`.
    ///
    pub def charCount(cp: Int32): Int32 =
        if (isSupplementaryCodePoint(cp)) 2 else 1

    ///
    /// Optionally returns a vector of Chars representing the code point `cp`.
    ///
    pub def toChars(cp: Int32): Option[Vector[Char]] =
        if (not isValidCodePoint(cp)) {
            None
        } else if (isSupplementaryCodePoint(cp)) {
            match toSupplementaryChars(cp) {
                case Some((hi, lo)) => Some(Vector#{hi, lo})
                case None => None
            }
        } else {
            match toBmpChar(cp) {
                case Some(ch) => Some(Vector#{ch})
                case None => None
            }
        }

    ///
    /// Optionally returns a Char representing the code point `cp` if `cp` is within
    /// Unicode's Basic Multilingual Plane.
    ///
    pub def toBmpChar(cp: Int32): Option[Char] =
        if (isValidCodePoint(cp) and isBmpCodePoint(cp))
            String.nth(0, toString(cp))
        else
            None

    ///
    /// Optionally returns a pair of Chars representing the high and low surrogate characters of the
    /// code point `cp` if `cp` is a supplementary character.
    ///
    pub def toSupplementaryChars(cp: Int32): Option[(Char, Char)] =
        if (isSupplementaryCodePoint(cp)) {
            let s = toString(cp);
            match (String.nth(0, s), String.nth(1, s)) {
                case (Some(hi), Some(lo)) => Some((hi, lo))
                case _ => None
            }
        } else
            None

    ///
    /// Returns a String representing the code point `cp`.
    ///
    pub def toString(cp: Int32): String =
        if (not isValidCodePoint(cp))
            ""
        else region rc {
            let sb = StringBuilder.empty(rc);
            StringBuilder.appendCodePoint(cp, sb);
            StringBuilder.toString(sb)
        }

    ///
    /// Optionally returns the high surrogate character of the code point `cp`
    /// if `cp` is a supplementary character.
    ///
    pub def highSurrogate(cp: Int32): Option[Char] =
        match toSupplementaryChars(cp) {
            case Some((hi, _)) => Some(hi)
            case None => None
        }

    ///
    /// Optionally returns the low surrogate character of the code point `cp`
    /// if `cp` is a supplementary character.
    ///
    pub def lowSurrogate(cp: Int32): Option[Char] =
        match toSupplementaryChars(cp) {
            case Some((_, lo)) => Some(lo)
            case None => None
        }

}
