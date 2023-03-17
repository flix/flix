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

///
/// Alternative implementations of functions from `String.flix` that use regexs
///

namespace Regex/Text {

    use Regex.Pattern
    use Regex.Pattern.{Pattern}

    ///
    /// Splits the string `s` around matches of the pattern `pattern`.
    ///
    pub def split(pattern: {pattern = Pattern}, s: String): List[String] = region rc {
        import java.util.regex.Pattern.split(##java.lang.CharSequence): Array[String, rc] \ Write(rc);
        let _ = rc; // This avoids a redundancy error.
        let Pattern(p1) = pattern.pattern;
        Array.toList(split(p1, unchecked_cast(s as ##java.lang.CharSequence)))
    }


    ///
    /// Create a Matcher for Pattern `patt` on the source String `input`.
    ///
    def newMatcher(_: Region[r], p: Pattern, s: String): ##java.util.regex.Matcher \ Write(r) =
        import java.util.regex.Pattern.matcher(##java.lang.CharSequence): ##java.util.regex.Matcher \ r;
        let Pattern(p1) = p;
        matcher(p1, unchecked_cast(s as ##java.lang.CharSequence))

    ///
    /// Returns string `s` with every match of the pattern `from` replaced by the string `to`.
    ///
    pub def replace(from: {from = Pattern}, to: {to = String}, s: String): String = region rc {
        import java.util.regex.Matcher.replaceAll(String): String \ rc;
        let m1 = newMatcher(rc, from.from, s);
        replaceAll(m1, to.to)
    }

    ///
    /// Returns string `s` with the first match of the regular expression `from` replaced by the string `to`.
    ///
    pub def replaceFirst(from: {from = Pattern}, to: {to = String}, s: String): String = region rc {
        import java.util.regex.Matcher.replaceFirst(String): String \ rc;
        let m1 = newMatcher(rc, from.from, s);
        replaceFirst(m1, to.to)
    }

    ///
    /// Returns `true` if the string `s` starts the pattern `prefix`.
    ///
    pub def startsWith(prefix: {prefix = Pattern}, s: String): Bool = region rc {
        import java.util.regex.Matcher.lookingAt(): Bool \ rc;
        newMatcher(rc, prefix.prefix, s) |> lookingAt
    }

}
