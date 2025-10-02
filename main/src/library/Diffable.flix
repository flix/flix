/*
 * Copyright 2025 Magnus Madsen
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
/// A trait for types that can be compared and pretty-printed as diffs.
///
/// Useful for debugging and testing, especially when comparing complex data structures.
///
pub trait Diffable[a] with Eq[a], ToString[a] {

    ///
    /// Returns a `RichString` showing the differences between `expected` and `actual`.
    ///
    /// The diff is formatted with colors:
    /// - Red for elements only in `expected` (removed)
    /// - Green for elements only in `actual` (added)
    /// - Yellow/Blue for changed values
    ///
    pub def diff(expected: {expected = a}, actual: a): RichString

}

mod Diffable {

    use RichString.{green, red, text, yellow};

    ///
    /// Formats an added element (present in actual but not in expected).
    ///
    pub def formatAdded(s: String): RichString =
        green("  + ") + text(s)

    ///
    /// Formats a removed element (present in expected but not in actual).
    ///
    pub def formatRemoved(s: String): RichString =
        red("  - ") + text(s)

    ///
    /// Formats a changed element (different values for the same key/position).
    ///
    pub def formatChanged(s: String): RichString =
        yellow("  ~ ") + text(s)

    ///
    /// Builds a diff header showing the expected and actual values.
    ///
    pub def formatHeader(expected: String, actual: String): RichString =
        text("Expected: ") + text(expected) + text("\n") +
        text("Actual:   ") + text(actual) + text("\n") +
        text("\nDiff:\n")

    ///
    /// Returns "No differences" message when values are equal.
    ///
    pub def formatNoDiff(): RichString =
        green("No differences")

}

instance Diffable[Set[a]] with Order[a], ToString[a] {
    pub def diff(expected: {expected = Set[a]}, actual: Set[a]): RichString =
        if (expected#expected == actual)
            Diffable.formatNoDiff()
        else {
            let removed = Set.difference(expected#expected, actual);
            let added = Set.difference(actual, expected#expected);
            let header = Diffable.formatHeader(ToString.toString(expected#expected), ToString.toString(actual));
            let removedLines = Set.toList(removed) |> List.map(x -> Diffable.formatRemoved(ToString.toString(x)));
            let addedLines = Set.toList(added) |> List.map(x -> Diffable.formatAdded(ToString.toString(x)));
            let allLines = removedLines ::: addedLines;
            List.foldLeft((acc, line) -> acc + line + RichString.text("\n"), header, allLines)
        }
}
