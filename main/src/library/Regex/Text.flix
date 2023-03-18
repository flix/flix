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
    use Regex/Internal.Matcher
    use Regex/Internal.Matcher.{Matcher}

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
    /// Returns string `s` with every match of the pattern `from` replaced by the string `to`.
    ///
    pub def replace(from: {from = Pattern}, to: {to = String}, s: String): String = region rc {
        import java.util.regex.Matcher.replaceAll(String): String \ rc;
        let Matcher(m1) = Regex/Internal/Matcher.new(rc, from.from, s);
        replaceAll(m1, to.to)
    }

    ///
    /// Returns string `s` with the first match of the regular expression `from` replaced by the string `to`.
    ///
    pub def replaceFirst(from: {from = Pattern}, to: {to = String}, s: String): String = region rc {
        import java.util.regex.Matcher.replaceFirst(String): String \ rc;
        let Matcher(m1) = Regex/Internal/Matcher.new(rc, from.from, s);
        replaceFirst(m1, to.to)
    }

    ///
    /// Returns `true` if the string `s` starts the pattern `prefix`.
    ///
    pub def startsWith(prefix: {prefix = Pattern}, s: String): Bool = region rc {
        import java.util.regex.Matcher.lookingAt(): Bool \ rc;
        let Matcher(m1) = Regex/Internal/Matcher.new(rc, prefix.prefix, s);
        lookingAt(m1)
    }

    ///
    /// Returns `true` if the string `input` ends the regular expression pattern `patt`.
    ///
    /// This will be slower than `startsWith` because there is no primitive Java function
    /// to call, instead the matches of `patt` are iterated until the last one is found.
    ///
    pub def endsWith(suffix: {suffix = Pattern}, s: String): Bool = region rc {
        match Regex/Internal/Matcher.lastSubmatch(rc, suffix.suffix, s, Regex/Internal/Matcher.range) {
            case Err(_)  => false
            case Ok(rng) => String.length(s) - 1 == rng.end
        }
    }


    // stripSuffix
    pub def stripSuffix(substr: {substr = Pattern}, s: String): Option[String] = region rc {
        match Regex/Internal/Matcher.lastSubmatch(rc, substr.substr, s, Regex/Internal/Matcher.range) {
            case Err(_) => None
            case Ok(r)  => if (r.end == String.length(s) - 1) Some(String.takeLeft(r.start, s)) else None
        }
    }

}
