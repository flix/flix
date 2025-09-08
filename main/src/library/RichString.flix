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
    pub enum Color with Eq {
        case Rgb(Int32, Int32, Int32),  // Red, Green, Blue components
        case Default                    // Use terminal's default color
    }

    ///
    /// Represents text styling options.
    ///
    /// Defines the visual appearance of text spans beyond color.
    ///
    pub enum Style with Eq, Order {
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
    /// A trait for types that can be formatted as a `RichString`.
    ///
    pub trait Formattable[a] {
        pub def format(x: a): RichString
    }

    instance Formattable[RichString] {
        pub def format(x: RichString): RichString = x
    }

    instance Formattable[Bool] {
        pub def format(x: Bool): RichString = fromString(ToString.toString(x))
    }

    instance Formattable[Char] {
        pub def format(x: Char): RichString = fromString(ToString.toString(x))
    }

    instance Formattable[Int8] {
        pub def format(x: Int8): RichString = fromString(ToString.toString(x))
    }

    instance Formattable[Int16] {
        pub def format(x: Int16): RichString = fromString(ToString.toString(x))
    }

    instance Formattable[Int32] {
        pub def format(x: Int32): RichString = fromString(ToString.toString(x))
    }

    instance Formattable[Int64] {
        pub def format(x: Int64): RichString = fromString(ToString.toString(x))
    }

    instance Formattable[Float32] {
        pub def format(x: Float32): RichString = fromString(ToString.toString(x))
    }

    instance Formattable[Float64] {
        pub def format(x: Float64): RichString = fromString(ToString.toString(x))
    }

    instance Formattable[String] {
        pub def format(x: String): RichString = fromString(x)
    }

    instance Formattable[BigInt] {
        pub def format(x: BigInt): RichString = fromString(ToString.toString(x))
    }

    instance Formattable[BigDecimal] {
        pub def format(x: BigDecimal): RichString = fromString(ToString.toString(x))
    }

    ///
    /// Returns an empty `RichString`.
    ///
    pub def empty(): RichString =
        RichString(Chain.empty())

    ///
    /// Returns a `RichString` with the given text `x` using default colors and normal style.
    ///
    pub def text(x: a): RichString with Formattable[a] =
        Formattable.format(x)

    ///
    /// Returns a `RichString` with the function `f` applied to every span.
    ///
    pub def map(f: Span -> Span, rs: RichString): RichString = match rs {
        case RichString(spans) => RichString(Chain.map(f, spans))
    }

    ///
    /// Returns a `RichString` from given text `x` with all spans updated to use the given foreground color `c`.
    ///
    pub def withColor(c: Color, x: a): RichString with Formattable[a] =
        let rs = Formattable.format(x);
        map(match Span.Span(r) ->
            Span.Span({text = r#text, fgColor = c, bgColor = r#bgColor, style = r#style}), rs)

    ///
    /// Returns a `RichString` from given text `x` with all spans updated to use the given background color `c`.
    ///
    pub def withBgColor(c: Color, x: a): RichString with Formattable[a] =
        let rs = Formattable.format(x);
        map(match Span.Span(r) ->
            Span.Span({text = r#text, fgColor = r#fgColor, bgColor = c, style = r#style}), rs)

    ///
    /// Returns a `RichString` from given text `x` with all spans updated to use the given `style`.
    ///
    pub def withStyle(style: Style, x: a): RichString with Formattable[a] =
        let rs = Formattable.format(x);
        map(match Span.Span(r) ->
            Span.Span({text = r#text, fgColor = r#fgColor, bgColor = r#bgColor, style = style}), rs)

    ///
    /// Returns a `RichString` containing the given span `s`.
    ///
    pub def fromSpan(s: Span): RichString =
        RichString(Chain.singleton(s))

    ///
    /// Returns a `RichString` from the given string `s` using default colors and normal style.
    ///
    pub def fromString(s: String): RichString =
        RichString(Chain.singleton(Span.Span({text = s, fgColor = Color.Default, bgColor = Color.Default, style = Style.Normal})))

    ///
    /// Returns a `RichString` with the given text `x` in black color.
    ///
    pub def black(x: a): RichString with Formattable[a] =
        withColor(Color.Rgb(0, 0, 0), x)

    ///
    /// Returns a `RichString` with the given text `x` in red color.
    ///
    pub def red(x: a): RichString with Formattable[a] =
        withColor(Color.Rgb(205, 49, 49), x)

    ///
    /// Returns a `RichString` with the given text `x` in green color.
    ///
    pub def green(x: a): RichString with Formattable[a] =
        withColor(Color.Rgb(13, 188, 121), x)

    ///
    /// Returns a `RichString` with the given text `x` in yellow color.
    ///
    pub def yellow(x: a): RichString with Formattable[a] =
        withColor(Color.Rgb(229, 192, 123), x)

    ///
    /// Returns a `RichString` with the given text `x` in blue color.
    ///
    pub def blue(x: a): RichString with Formattable[a] =
        withColor(Color.Rgb(36, 114, 200), x)

    ///
    /// Returns a `RichString` with the given text `x` in magenta color.
    ///
    pub def magenta(x: a): RichString with Formattable[a] =
        withColor(Color.Rgb(188, 63, 188), x)

    ///
    /// Returns a `RichString` with the given text `x` in cyan color.
    ///
    pub def cyan(x: a): RichString with Formattable[a] =
        withColor(Color.Rgb(17, 168, 205), x)

    ///
    /// Returns a `RichString` with the given text `x` in white color.
    ///
    pub def white(x: a): RichString with Formattable[a] =
        withColor(Color.Rgb(255, 255, 255), x)

    ///
    /// Returns a `RichString` with the given text `x` on black background.
    ///
    pub def bgBlack(x: a): RichString with Formattable[a] =
        withBgColor(Color.Rgb(0, 0, 0), x)

    ///
    /// Returns a `RichString` with the given text `x` on red background.
    ///
    pub def bgRed(x: a): RichString with Formattable[a] =
        withBgColor(Color.Rgb(205, 49, 49), x)

    ///
    /// Returns a `RichString` with the given text `x` on green background.
    ///
    pub def bgGreen(x: a): RichString with Formattable[a] =
        withBgColor(Color.Rgb(13, 188, 121), x)

    ///
    /// Returns a `RichString` with the given text `x` on yellow background.
    ///
    pub def bgYellow(x: a): RichString with Formattable[a] =
        withBgColor(Color.Rgb(229, 192, 123), x)

    ///
    /// Returns a `RichString` with the given text `x` on blue background.
    ///
    pub def bgBlue(x: a): RichString with Formattable[a] =
        withBgColor(Color.Rgb(36, 114, 200), x)

    ///
    /// Returns a `RichString` with the given text `x` on magenta background.
    ///
    pub def bgMagenta(x: a): RichString with Formattable[a] =
        withBgColor(Color.Rgb(188, 63, 188), x)

    ///
    /// Returns a `RichString` with the given text `x` on cyan background.
    ///
    pub def bgCyan(x: a): RichString with Formattable[a] =
        withBgColor(Color.Rgb(17, 168, 205), x)

    ///
    /// Returns a `RichString` with the given text `x` on white background.
    ///
    pub def bgWhite(x: a): RichString with Formattable[a] =
        withBgColor(Color.Rgb(255, 255, 255), x)

    ///
    /// Returns a `RichString` with the given text `x` in bold style.
    ///
    pub def bold(x: a): RichString with Formattable[a] =
        withStyle(Style.Bold, x)

    ///
    /// Returns a `RichString` with the given text `x` in underline style.
    ///
    pub def underline(x: a): RichString with Formattable[a] =
        withStyle(Style.Underline, x)

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
