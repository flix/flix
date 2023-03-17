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





namespace Regex {

    use Regex.Pattern
    use Regex.Pattern.{Pattern}

    ///
    /// Represents a compiled Pattern.
    ///
    pub enum Pattern(##java.util.regex.Pattern)

    instance ToString[Pattern] {
        ///
        /// Return the string representation of the Pattern `patt`.
        ///
        pub def toString(patt: Pattern): String =
            import java.util.regex.Pattern.toString(): String \ {};
            let Pattern(p1) = patt;
            toString(p1)
    }


}

namespace Regex/Pattern {

    use Regex.Pattern
    use Regex.Pattern.{Pattern}
    use Regex/Pattern.Flag
    use Regex/Pattern.Flag.{CanonEq, CaseInsenstive,
        Comments, Dotall, Literal, Multiline,
        UnicodeCase, UnicodeCharacterClass, UnixLines
    }

    pub enum Flag with Eq, Order, ToString {
        case CanonEq
        case CaseInsenstive
        case Comments
        case Dotall
        case Literal
        case Multiline
        case UnicodeCase
        case UnicodeCharacterClass
        case UnixLines
    }

    ///
    /// Returns the int value of `flag`.
    ///
    /// Rexeg Pattern Flags predate `Enum` in Java so they are represented as an `int`.
    ///
    def toInt(flag: Flag): Int32 =
        import static get java.util.regex.Pattern.CANON_EQ: Int32 \ {} as get_CANON_EQ;
        import static get java.util.regex.Pattern.CASE_INSENSITIVE: Int32 \ {} as get_CASE_INSENSITIVE;
        import static get java.util.regex.Pattern.COMMENTS: Int32 \ {} as get_COMMENTS;
        import static get java.util.regex.Pattern.DOTALL: Int32 \ {} as get_DOTALL;
        import static get java.util.regex.Pattern.LITERAL: Int32 \ {} as get_LITERAL;
        import static get java.util.regex.Pattern.MULTILINE: Int32 \ {} as get_MULTILINE;
        import static get java.util.regex.Pattern.UNICODE_CASE: Int32 \ {} as get_UNICODE_CASE;
        import static get java.util.regex.Pattern.UNICODE_CHARACTER_CLASS: Int32 \ {} as get_UNICODE_CHARACTER_CLASS;
        import static get java.util.regex.Pattern.UNIX_LINES: Int32 \ {} as get_UNIX_LINES;
        match flag {
            case CanonEq                => get_CANON_EQ()
            case CaseInsenstive         => get_CASE_INSENSITIVE()
            case Comments               => get_COMMENTS()
            case Dotall                 => get_DOTALL()
            case Literal                => get_LITERAL()
            case Multiline              => get_MULTILINE()
            case UnicodeCase            => get_UNICODE_CASE()
            case UnicodeCharacterClass  => get_UNICODE_CHARACTER_CLASS()
            case UnixLines              => get_UNIX_LINES()
        }

    ///
    /// Sum a list of Flags.
    ///
    def sumFlags(flags: List[Flag]): Int32 =
        List.foldLeft((ac,e) -> ac + toInt(e), 0, flags)

    ///
    /// Decompose an Int32 representing a set of flags into a list.
    ///
    def listFlags(x: Int32): List[Flag] =
        let check = y -> {
            let y1 = toInt(y);
            (x &&& y1) == y1
        };
        List.filter(check, CanonEq :: CaseInsenstive :: Comments :: Dotall :: Literal ::
                            Multiline :: UnicodeCase :: UnicodeCharacterClass :: UnixLines :: Nil)


    ///
    /// Compile the regular expression `regex` into a Pattern.
    ///
    /// Returns `Err` if the regex is ill-formed and cannot be compiled.
    ///
    pub def compile(regex: String): Result[String, Pattern] =
        Result.tryCatch(_ -> {
            import static java.util.regex.Pattern.compile(String): ##java.util.regex.Pattern \ {};
            Pattern(compile(regex))
        })


    ///
    /// Compile the regular expression `regex` into a Pattern.
    ///
    /// Returns `Err` if the regex is ill-formed and cannot be compiled.
    ///
    pub def compileWithFlags(regex: String, flags: List[Flag]): Result[String, Pattern] =
        Result.tryCatch(_ -> {
            import static java.util.regex.Pattern.compile(String, Int32): ##java.util.regex.Pattern \ {};
            Pattern(compile(regex, sumFlags(flags)))
        })


    ///
    /// Return the unmatchable pattern - a pattern that will not match any input.
    ///
    pub def unmatchable(): Pattern =
        import static java.util.regex.Pattern.compile(String): ##java.util.regex.Pattern \ {};
        try {
            Pattern(compile("^\\b$"))
        } catch {
            case _: ##java.lang.Exception => bug!("Regex/Pattern.unmatchable")
        }


    pub def quote(s: String): String =
        import static java.util.regex.Pattern.quote(String): String \ {};
        quote(s)

    ///
    /// Return the regular expression used to build this pattern.
    ///
    pub def pattern(patt: Pattern): String =
        import java.util.regex.Pattern.pattern(): String \ {};
        let Pattern(p1) = patt;
        pattern(p1)

    ///
    /// Return the flags used to build this pattern.
    ///
    pub def flags(patt: Pattern): List[Flag] =
        import java.util.regex.Pattern.flags(): Int32 \ {};
        let Pattern(p1) = patt;
        let i = flags(p1);
        listFlags(i)


}
