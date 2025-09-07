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
/// A rich string composed of multiple styled spans.
///
pub enum RichString {
    case RichString(Chain[RichString.Span])
}

instance Add[RichString] {
    pub def add(x: RichString, y: RichString): RichString = RichString.combine(x, y)
}

instance ToString[RichString] {
    pub def toString(x: RichString): String = RichString.toString(x)
}

mod RichString {

    ///
    /// Represents a color with RGB components.
    ///
    /// Each component is an 8-bit value representing red, green, and blue intensity.
    ///
    pub enum Color {
        case Rgb(Int8, Int8, Int8)  // Red, Green, Blue components
    }

    ///
    /// Represents text styling options.
    ///
    /// Defines the visual appearance of text spans beyond color.
    ///
    pub enum Style {
        case Normal,
        case Bold,
        case Underline
    }

    ///
    /// A text span with content, color, and style.
    ///
    /// A span represents a contiguous piece of text with uniform styling.
    ///
    pub enum Span {
        case Span({text = String, color = Color, style = Style})
    }

    mod Span {
        use RichString.{Color, Style, Span};

        ///
        /// Returns a span with the given `text` and `color` using normal style.
        ///
        pub def withColor(text: String, color: Color): Span =
            Span({text = text, color = color, style = Style.Normal})

    }

    ///
    /// Returns an empty `RichString`.
    ///
    pub def empty(): RichString =
        RichString(Chain.empty())

    ///
    /// Returns a `RichString` containing the given `span`.
    ///
    pub def fromSpan(span: Span): RichString =
        RichString(Chain.singleton(span))

    ///
    /// Returns a `RichString` with the given text `s` in black color.
    ///
    pub def black(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0i8, 0i8, 0i8)))

    ///
    /// Returns a `RichString` with the given text `s` in red color.
    ///
    pub def red(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(127i8, 0i8, 0i8)))

    ///
    /// Returns a `RichString` with the given text `s` in green color.
    ///
    pub def green(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0i8, 127i8, 0i8)))

    ///
    /// Returns a `RichString` with the given text `s` in yellow color.
    ///
    pub def yellow(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(127i8, 127i8, 0i8)))

    ///
    /// Returns a `RichString` with the given text `s` in blue color.
    ///
    pub def blue(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0i8, 0i8, 127i8)))

    ///
    /// Returns a `RichString` with the given text `s` in magenta color.
    ///
    pub def magenta(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(127i8, 0i8, 127i8)))

    ///
    /// Returns a `RichString` with the given text `s` in cyan color.
    ///
    pub def cyan(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0i8, 127i8, 127i8)))

    ///
    /// Returns a `RichString` with the given text `s` in white color.
    ///
    pub def white(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(127i8, 127i8, 127i8)))

    ///
    /// Returns the concatenation of `rs1` and `rs2`.
    ///
    /// The spans from `rs1` appear first, followed by the spans from `rs2`.
    ///
    pub def combine(rs1: RichString, rs2: RichString): RichString = match (rs1, rs2) {
        case (RichString(spans1), RichString(spans2)) =>
            RichString(Chain.append(spans1, spans2))
    }

    ///
    /// Returns the plain text representation of the `RichString` `rs`.
    ///
    /// All styling and color information is discarded.
    ///
    pub def toString(rs: RichString): String = match rs {
        case RichString(spans) =>
            Chain.joinWith(match Span.Span(r) -> r#text, "", spans)
    }

    ///
    /// Returns the string representation of the `RichString` `rs` with ANSI escape codes.
    ///
    /// The returned string includes ANSI color and style codes for terminal display.
    ///
    pub def toAnsiString(rs: RichString): String = match rs {
        case RichString(spans) =>
            Chain.joinWith(match Span.Span(r) -> {
                let Color.Rgb(red, green, blue) = r#color;
                let styleCode = match r#style {
                    case Style.Normal => ""
                    case Style.Bold => "1;"
                    case Style.Underline => "4;"
                };
                let redInt = Int8.toInt32(red);
                let greenInt = Int8.toInt32(green);
                let blueInt = Int8.toInt32(blue);
                "\u001B[${styleCode}38;2;${redInt};${greenInt};${blueInt}m${r#text}\u001B[0m"
            }, "", spans)
    }

}
