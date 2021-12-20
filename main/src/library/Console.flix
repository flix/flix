/*
 * Copyright 2017 Magnus Madsen
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
namespace Console {

    /// test
    /// Reads a single line from the console.
    ///
    /// Returns `None` if nothing was entered or if no console is presented.
    ///
    /// See also `System/StdIn.readLines`.
    ///
    pub def readLine(): Option[String] & Impure =
        import static java.lang.System.console();
        import java.io.Console.readLine() as consoleReadLine;
        Object.toOption(console()) |> Option.map(consoleReadLine)

    ///
    /// Prints `s` to the console.
    ///
    /// Note: This function does not print to standard out.
    ///
    /// Warning: If no console is attached nothing is printed.
    ///
    /// Use `print` instead.
    ///
    /// See also `System/StdOut.print`.
    ///
    pub def print(s: String): Unit & Impure =
        import static java.lang.System.console();
        import java.io.Console.writer();
        import java.io.PrintWriter.print(String) as printWriterPrint;
        Object.toOption(console()) |> Option.map(c -> printWriterPrint(writer(c), s));
        ()

    ///
    /// Prints `s` to the console followed by a new line.
    ///
    /// Note: This function does not print to standard out.
    ///
    /// Warning: If no console is attached nothing is printed.
    ///
    /// Use `print` instead.
    ///
    /// See also `System/StdOut.println`.
    ///
    pub def printLine(s: String): Unit & Impure =
        import static java.lang.System.console();
        import java.io.Console.writer();
        import java.io.PrintWriter.println(String) as printWriterPrintln;
        Object.toOption(console()) |> Option.map(c -> printWriterPrintln(writer(c), s));
        ()

    ///
    /// Flushes the console output.
    ///
    /// Note: This function does not flush standard out.
    ///
    /// Warning: If no console is attached nothing happens.
    ///
    /// See also `System/StdOut.flush`.
    ///
    pub def flush(): Unit & Impure =
        import static java.lang.System.console();
        import java.io.Console.flush() as consoleFlush;
        Object.toOption(console()) |> Option.map(consoleFlush);
        ()

    ///
    /// Returns the string `s` formatted with the given RGB color `c`.
    ///
    pub def rgb(c: (Int32, Int32 , Int32), s: String): String =
        match colorSupport() {
            case ColorSupport.None      => s
            case ColorSupport.Ansi      =>
                let fg = downsample(c);
                formatAnsi(fg, s)
            case ColorSupport.TrueColor =>
                formatTrueColor(c, s)
        }

    ///
    /// Returns the string `s` formatted with the default blue color.
    ///
    pub def blue(s: String): String =
        rgb(Blue |> toRGB, s)

    ///
    /// Returns the string `s` formatted with the default cyan color.
    ///
    pub def cyan(s: String): String =
        rgb(Cyan |> toRGB, s)

    ///
    /// Returns the string `s` formatted with the default green color.
    ///
    pub def green(s: String): String =
        rgb(Green |> toRGB, s)

    ///
    /// Returns the string `s` formatted with the default magenta color.
    ///
    pub def magenta(s: String): String =
        rgb(Magenta |> toRGB, s)

    ///
    /// Returns the string `s` formatted with the default red color.
    ///
    pub def red(s: String): String =
        rgb(Red |> toRGB, s)

    ///
    /// Returns the string `s` formatted with the default yellow color.
    ///
    pub def yellow(s: String): String =
        rgb(Yellow |> toRGB, s)

    ///
    /// Returns the string `s` formatted with the default white color.
    ///
    pub def white(s: String): String =
        rgb(White |> toRGB, s)

    ///
    /// Returns the string `s` formatted with the default bright white color.
    ///
    pub def whiteBright(s: String): String =
        rgb(BrightWhite |> toRGB, s)

    // TODO: Add functions for missing colors.

    // TODO: Add functions for backgrounds.

    ///
    /// Ansi Colors.
    ///
    // TODO: Add background colors.
    pub enum Ansi {
        case Black
        case Red
        case Green
        case Yellow
        case Blue
        case Magenta
        case Cyan
        case White
        case BrightBlack
        case BrightRed
        case BrightGreen
        case BrightYellow
        case BrightBlue
        case BrightMagenta
        case BrightCyan
        case BrightWhite
    }

    ///
    /// Returns the foreground color of the given color `c`.
    ///
    def toAnsi(c: Ansi): Int32 =
        match c {
            case Ansi.Black          => 30
            case Ansi.Red            => 31
            case Ansi.Green          => 32
            case Ansi.Yellow         => 33
            case Ansi.Blue           => 34
            case Ansi.Magenta        => 35
            case Ansi.Cyan           => 36
            case Ansi.White          => 37
            case Ansi.BrightBlack    => 90
            case Ansi.BrightRed      => 91
            case Ansi.BrightGreen    => 92
            case Ansi.BrightYellow   => 93
            case Ansi.BrightBlue     => 94
            case Ansi.BrightMagenta  => 95
            case Ansi.BrightCyan     => 96
            case Ansi.BrightWhite    => 97
        }

    ///
    /// Returns the RGB-triplet for the given color `c`.
    ///
    /// Based on the Ubuntu settings.
    ///
    /// See https://en.wikipedia.org/wiki/ANSI_escape_code#Colors
    ///
    def toRGB(c: Ansi): (Int32, Int32, Int32) =
        match c {
            case Ansi.Black          => (  1,   1,   1)
            case Ansi.Red            => (222,  56,  43)
            case Ansi.Green          => ( 57, 181,  74)
            case Ansi.Yellow         => (255, 199,   6)
            case Ansi.Blue           => (  0, 111, 184)
            case Ansi.Magenta        => (118,  38, 113)
            case Ansi.Cyan           => ( 44, 181, 233)
            case Ansi.White          => (204, 204, 204)
            case Ansi.BrightBlack    => (128, 128, 128)
            case Ansi.BrightRed      => (255,   0,   0)
            case Ansi.BrightGreen    => (  0, 255,   0)
            case Ansi.BrightYellow   => (255, 255,   0)
            case Ansi.BrightBlue     => (  0,   0, 255)
            case Ansi.BrightMagenta  => (255,   0, 255)
            case Ansi.BrightCyan     => (  0, 255, 255)
            case Ansi.BrightWhite    => (255, 255, 255)
        }

    ///
    /// Down sample the given color `c` to an ANSI color code.
    ///
    def downsample(c: (Int32, Int32, Int32)): Int32 =
        let candidates = Ansi.Black :: Ansi.Red :: Ansi.Green :: Ansi.Yellow :: Ansi.Blue :: Ansi.Magenta ::
                            Ansi.Cyan :: Ansi.White :: Ansi.BrightBlack :: Ansi.BrightRed :: Ansi.BrightGreen ::
                            Ansi.BrightYellow :: Ansi.BrightBlue :: Ansi.BrightMagenta :: Ansi.BrightCyan ::
                            Ansi.BrightWhite :: Nil;
        nearest(c, Ansi.Black, Int32.maxValue(), candidates) |> toAnsi

    ///
    /// Returns the ANSI color nearest to the given `target` color from the given list of `candidates`.
    ///
    def nearest(target: (Int32, Int32, Int32), bestCandidate: Ansi, bestDistance: Int32, candidates: List[Ansi]): Ansi =
        match candidates {
            case Nil     => bestCandidate
            case currentCandidate :: remainingCandidates =>
                let currentDistance = distanceSquared(target, toRGB(currentCandidate));
                if (currentDistance < bestDistance)
                    nearest(target, currentCandidate, currentDistance, remainingCandidates)
                else
                    nearest(target, bestCandidate, bestDistance, remainingCandidates)
        }

    ///
    /// Returns the square of the distance between the two colors `c1` and `c2`.
    ///
    def distanceSquared(c1: (Int32, Int32, Int32), c2: (Int32, Int32, Int32)): Int32 =
        let (r1, g1, b1) = c1;
        let (r2, g2, b2) = c2;
        (r1 - r2) * (r1 - r2) + (g1 - g2) * (g1 - g2) + (b1 - b2) * (b1 - b2)

    ///
    /// Returns the string `s` formatted with the given ANSI color `fg`.
    ///
    def formatAnsi(fg: Int32, s: String): String =
        let esc = escape();
        esc + "[" + Int32.toString(fg) + "m" + s + esc + "[0m"

    ///
    /// Returns the string `s` formatted with the given RGB color `c`.
    ///
    def formatTrueColor(c: (Int32, Int32, Int32), s: String): String =
        let (r, g, b) = c;
        let esc = escape();
        let red = Int32.toString(r);
        let gre = Int32.toString(g);
        let blu = Int32.toString(b);
        esc + "[38;2;" + red + ";" + gre + ";" + blu + "m" + s + esc + "[0m"

    ///
    /// Returns the escape character as a string.
    ///
    def escape(): String = Char.toString('\u001b')

    ///
    /// An enum that represents the colors supported by a terminal.
    ///
    enum ColorSupport {
        ///
        /// The terminal does not support any colors.
        ///
        case None,

        ///
        /// The terminal supports ANSI colors.
        ///
        case Ansi,

        ///
        /// The terminal supports 24-bit colors.
        ///
        case TrueColor
    }

    ///
    /// Returns the color support of the current terminal.
    ///
    def colorSupport(): ColorSupport =
        if (isTrueColorTerminal() or isWindowsTerminal())
            ColorSupport.TrueColor
        else if (isAnsiTerminal())
            ColorSupport.Ansi
        else
            ColorSupport.None

    ///
    /// Returns `true` if the terminal supports ANSI colors.
    ///
    def isAnsiTerminal(): Bool =
        Environment.getVar("TERM") |>
        Option.exists(p ->
            let keywords = "256" :: "ansi" :: "xterm" :: "screen" :: Nil;
            List.exists(s -> String.contains({substr = s}, p), keywords)
        )

    ///
    /// Returns `true` if the terminal supports 24-bit colors.
    ///
    def isTrueColorTerminal(): Bool =
        Environment.getVar("COLORTERM") |>
        Option.exists(p -> p == "truecolor")

    ///
    /// Returns `true` if the terminal is the Windows Terminal.
    ///
    def isWindowsTerminal(): Bool =
        not (Environment.getVar("WT_SESSION") |> Option.isEmpty)

}
