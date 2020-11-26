/*
 * Copyright 2020 Rasmus Larsen
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

namespace Pkger/SemVer {
    ///
    /// Represents an instance of a Semantic Versioning (SemVer) version.
    /// It is currently missing pre-release identifiers and build metadata
    ///
    pub enum SemVer {
        case SemVer(Int, Int, Int)
    }

    ///
    /// Compare two SemVer instances according to the ordering rules in the specification
    ///
    def __cmp(l: SemVer, r: SemVer): Int32 = match (l, r) {
        case (SemVer(lMajor, lMinor, lPatch), SemVer(rMajor, rMinor, rPatch)) => {
            let major = lMajor <=> rMajor;
            if (major == 0) {
                let minor = lMinor <=> rMinor;
                if (minor == 0) {
                    lPatch <=> rPatch
                } else {
                    minor
                }
            } else {
                major
            }
        }
    }

    ///
    /// Check whether the `specific` SemVer is compatible with the `bound` SemVer according to the SemVer specification
    ///
    pub def isCompatible(bound: SemVer, specific: SemVer): Bool = {
        match (bound, specific) {
            case (SemVer(0, boundMinor, boundPatch), SemVer(0, specificMinor, specificPatch)) => {
                boundMinor == specificMinor and boundPatch == specificPatch
            }
            case (SemVer(boundMajor, _, _), SemVer(specificMajor, _, _)) => boundMajor == specificMajor
        }
    }

    namespace Parser {
        type alias Parser[a] = String -> Option[(String, a)]

        def runParser(v: Parser[a], s: String): Option[(String, a)] = v(s)

        def flatMap(f: a -> Parser[b], v: Parser[a]): Parser[b] = input -> {
            Option.flatMap(match (inputx, x) -> runParser(f(x), inputx), runParser(v, input))
        }

        def return(x: a): Parser[a] = input -> Some((input, x))

        def empty(): Parser[a] = _ -> None

        def char(x: Char): Parser[Char] = input -> match String.toList(input) {
            case Nil => None
            case Cons(y, ys) => if (y == x) { Some((List.toString(ys), x)) } else { None }
        }

        def span(p: Char -> Bool): Parser[String] = input -> {
            let (l, r) = List.span(p, String.toList(input));
            Some(List.toString(r), List.toString(l))
        }

        def parseNumber(): Parser[Int] = {
            let* s1 = span(Char.isDigit);
            let* s2 = if (String.isEmpty(s1)) empty() else return(s1);
            let n1 = Int32.fromString(s2);
            let n2 = Result.getWithDefault(n1, 0);
            return(n2)
        }

        def parseSemVer(): Parser[Pkger/SemVer.SemVer] = {
            use Pkger/SemVer.SemVer;
            let* n1 = parseNumber();
            let* _ = char('.');
            let* n2 = parseNumber();
            let* _ = char('.');
            let* n3 = parseNumber();
            return(SemVer.SemVer(n1, n2, n3))
        }

        ///
        /// Read a `SemVer` from an `input` String
        ///
        pub def fromString(input: String): Option[Pkger/SemVer.SemVer] = {
            use Option.flatMap;
            let* (_, semver) = runParser(parseSemVer(), input);
            Some(semver)
        }
    }
}
