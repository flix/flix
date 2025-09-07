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
    /// Represents a color with RGB components or the terminal's default color.
    ///
    /// For RGB, each component is a value from 0 to 255 representing red, green, and blue intensity.
    /// Default represents no color override, using the terminal's current color settings.
    ///
    pub enum Color {
        case Rgb(Int32, Int32, Int32),  // Red, Green, Blue components
        case Default                     // Use terminal's default color
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
    /// A text span with content, foreground color, background color, and style.
    ///
    /// A span represents a contiguous piece of text with uniform styling.
    ///
    pub enum Span {
        case Span({text = String, fgColor = Color, bgColor = Color, style = Style})
    }

    mod Span {
        use RichString.{Color, Style, Span};

        ///
        /// Returns a span with the given text `s` and foreground color `c` using normal style and default background color.
        ///
        pub def withColor(s: String, c: Color): Span =
            Span({text = s, fgColor = c, bgColor = Color.Default, style = Style.Normal})

        ///
        /// Returns a span with the given text `s` and background color `c` using normal style and default foreground color.
        ///
        pub def withBgColor(s: String, c: Color): Span =
            Span({text = s, fgColor = Color.Default, bgColor = c, style = Style.Normal})

        ///
        /// Returns a span with the given text `s`, foreground color, and background color using normal style.
        ///
        pub def withColors(s: String, fgColor: Color, bgColor: Color): Span =
            Span({text = s, fgColor = fgColor, bgColor = bgColor, style = Style.Normal})

    }

    ///
    /// Returns an empty `RichString`.
    ///
    pub def empty(): RichString =
        RichString(Chain.empty())

    ///
    /// Returns a `RichString` with the given text `s` using default colors and normal style.
    ///
    pub def text(s: String): RichString =
        fromSpan(Span.Span({text = s, fgColor = Color.Default, bgColor = Color.Default, style = Style.Normal}))

    ///
    /// Returns `rs` with all spans updated to use the given foreground color `c`.
    ///
    pub def withColor(c: Color, rs: RichString): RichString = match rs {
        case RichString(spans) =>
            RichString(Chain.map(match Span.Span(r) -> 
                Span.Span({text = r#text, fgColor = c, bgColor = r#bgColor, style = r#style}), spans))
    }

    ///
    /// Returns `rs` with all spans updated to use the given background color `c`.
    ///
    pub def withBgColor(c: Color, rs: RichString): RichString = match rs {
        case RichString(spans) =>
            RichString(Chain.map(match Span.Span(r) -> 
                Span.Span({text = r#text, fgColor = r#fgColor, bgColor = c, style = r#style}), spans))
    }

    ///
    /// Returns `rs` with all spans updated to use the given `style`.
    ///
    pub def withStyle(style: Style, rs: RichString): RichString = match rs {
        case RichString(spans) =>
            RichString(Chain.map(match Span.Span(r) -> 
                Span.Span({text = r#text, fgColor = r#fgColor, bgColor = r#bgColor, style = style}), spans))
    }

    ///
    /// Returns a `RichString` containing the given span `s`.
    ///
    pub def fromSpan(s: Span): RichString =
        RichString(Chain.singleton(s))

    ///
    /// Returns a `RichString` with the given text `s` in black color.
    ///
    pub def black(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0, 0, 0)))

    ///
    /// Returns a `RichString` with the given text `s` in red color.
    ///
    pub def red(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(255, 0, 0)))

    ///
    /// Returns a `RichString` with the given text `s` in green color.
    ///
    pub def green(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0, 255, 0)))

    ///
    /// Returns a `RichString` with the given text `s` in yellow color.
    ///
    pub def yellow(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(255, 255, 0)))

    ///
    /// Returns a `RichString` with the given text `s` in blue color.
    ///
    pub def blue(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0, 0, 255)))

    ///
    /// Returns a `RichString` with the given text `s` in magenta color.
    ///
    pub def magenta(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(255, 0, 255)))

    ///
    /// Returns a `RichString` with the given text `s` in cyan color.
    ///
    pub def cyan(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0, 255, 255)))

    ///
    /// Returns a `RichString` with the given text `s` in white color.
    ///
    pub def white(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(255, 255, 255)))

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
                // Collect all codes in a list
                let styleCodes = match r#style {
                    case Style.Normal => Nil
                    case Style.Bold => "1" :: Nil
                    case Style.Underline => "4" :: Nil
                };

                let fgCodes = match r#fgColor {
                    case Color.Rgb(red, green, blue) => "38;2;${red};${green};${blue}" :: Nil
                    case Color.Default => Nil
                };

                let bgCodes = match r#bgColor {
                    case Color.Rgb(red, green, blue) => "48;2;${red};${green};${blue}" :: Nil
                    case Color.Default => Nil
                };

                let allCodes = styleCodes ::: fgCodes ::: bgCodes;

                if (List.isEmpty(allCodes))
                    r#text
                else
                    "\u001B[${List.join(";", allCodes)}m${r#text}\u001B[0m"
            }, "", spans)
    }

}
