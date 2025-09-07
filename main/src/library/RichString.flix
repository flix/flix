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
    pub enum Color {
        case Rgb(Int8, Int8, Int8)  // Red, Green, Blue components
    }

    ///
    /// Represents text styling options.
    ///
    pub enum Style {
        case Normal,
        case Bold,
        case Underline
    }

    ///
    /// A text span with content, color, and style.
    ///
    pub enum Span {
        case Span({text = String, color = Color, style = Style})
    }

    mod Span {
        use RichString.{Color, Style, Span};

        ///
        /// Create a span from text and color with normal style.
        ///
        pub def withColor(text: String, color: Color): Span =
            Span({text = text, color = color, style = Style.Normal})

    }

    ///
    /// Create an empty RichString.
    ///
    pub def empty(): RichString =
        RichString(Chain.empty())

    ///
    /// Create a RichString from a single span.
    ///
    pub def fromSpan(span: Span): RichString =
        RichString(Chain.singleton(span))

    ///
    /// Create a black RichString from text.
    ///
    pub def black(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0i8, 0i8, 0i8)))

    ///
    /// Create a red RichString from text.
    ///
    pub def red(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(127i8, 0i8, 0i8)))

    ///
    /// Create a green RichString from text.
    ///
    pub def green(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0i8, 127i8, 0i8)))

    ///
    /// Create a yellow RichString from text.
    ///
    pub def yellow(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(127i8, 127i8, 0i8)))

    ///
    /// Create a blue RichString from text.
    ///
    pub def blue(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0i8, 0i8, 127i8)))

    ///
    /// Create a magenta RichString from text.
    ///
    pub def magenta(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(127i8, 0i8, 127i8)))

    ///
    /// Create a cyan RichString from text.
    ///
    pub def cyan(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(0i8, 127i8, 127i8)))

    ///
    /// Create a white RichString from text.
    ///
    pub def white(s: String): RichString =
        fromSpan(Span.withColor(s, Color.Rgb(127i8, 127i8, 127i8)))

    ///
    /// Combine two RichStrings into one.
    ///
    pub def combine(rs1: RichString, rs2: RichString): RichString = match (rs1, rs2) {
        case (RichString(spans1), RichString(spans2)) =>
            RichString(Chain.append(spans1, spans2))
    }

    ///
    /// Convert a RichString to plain text.
    ///
    pub def toString(rs: RichString): String = match rs {
        case RichString(spans) =>
            Chain.joinWith(match Span.Span(r) -> r#text, "", spans)
    }

    ///
    /// Convert a RichString to a String with ANSI escape codes.
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
