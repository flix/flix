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

    ///
    /// Alias for `Console.StdIn.readLine`.
    ///
    pub def readLine(): Option[String] & Impure = Console/StdIn.readLine(())

    ///
    /// Alias for `Console.StdOut.print`.
    ///
    pub def print(s: String): Unit & Impure = Console/StdOut.print(s)

    ///
    /// Alias for `Console.StdOut.printLine`.
    ///
    pub def printLine(s: String): Unit & Impure = Console/StdOut.printLine(s)

    ///
    /// Operations on the Standard Input Stream.
    ///
    namespace StdIn {

        ///
        /// Returns the next line read from the standard input stream.
        ///
        pub def readLine(): Option[String] & Impure =
            import flix.runtime.library.Console:readLine();
            let s = readLine();
            (if (Object.isNull(s)) None else Some(s))

    }

    ///
    /// Operations on the Standard Output Stream.
    ///
    namespace StdOut {

        ///
        /// Prints the given string `s` to the standard output stream.
        ///
        pub def print(s: String): Unit & Impure =
            import java.io.PrintStream.print(String);
            getOutputStream().print(s)

        ///
        /// Prints the given string `s` and a newline to the standard output stream.
        ///
        pub def printLine(s: String): Unit & Impure =
            import java.io.PrintStream.println(String);
            getOutputStream().println(s)

        ///
        /// Prints a newline to the standard output stream.
        ///
        pub def newLine(): Unit & Impure =
            import java.io.PrintStream.println();
            getOutputStream().println()

        ///
        /// Flushes the standard output stream.
        ///
        pub def flush(): Unit & Impure =
            import java.io.PrintStream.flush();
            getOutputStream().flush()

        ///
        /// Returns the standard output stream.
        ///
        def getOutputStream(): ##java.io.PrintStream & Impure =
            import get java.lang.System:out as getOut;
            getOut()

    }

    ///
    /// Operations on the Standard Error Stream.
    ///
    namespace StdErr {

        ///
        /// Prints the given string `s` to the standard error stream.
        ///
        pub def print(s: String): Unit & Impure =
            import java.io.PrintStream.print(String);
            getErrorStream().print(s)

        ///
        /// Prints the given string `s` and a newline to the standard error stream.
        ///
        pub def printLine(s: String): Unit & Impure =
            import java.io.PrintStream.println(String);
            getErrorStream().println(s)

        ///
        /// Prints a newline to the standard error stream.
        ///
        pub def newLine(): Unit & Impure =
            import java.io.PrintStream.println();
            getErrorStream().println()

        ///
        /// Flushes the standard error stream.
        ///
        pub def flush(): Unit & Impure =
            import java.io.PrintStream.flush();
            getErrorStream().flush()

        ///
        /// Returns the standard error stream.
        ///
        def getErrorStream(): ##java.io.PrintStream & Impure =
            import get java.lang.System:err as getErr;
            getErr()

    }


    ///
    /// TODO: DOC
    ///
    pub enum Color {
        case AliceBlue
        case AntiqueWhite
        case Aqua
        case Aquamarine
        case Azure
        case Beige
        case Bisque
        case Black
        case BlanchedAlmond
        case Blue
        case BlueViolet
        case Brown
        case BurlyWood
        case CadetBlue
        case Chartreuse
        case Chocolate
        case Coral
        case CornflowerBlue
        case Cornsilk
        case Crimson
        case Cyan
        case DarkBlue
        case DarkCyan
        case DarkGoldenRod
        case DarkGray
        case DarkGrey
        case DarkGreen
        case DarkKhaki
        case DarkMagenta
        case DarkOliveGreen
        case DarkOrange
        case DarkOrchid
        case DarkRed
        case DarkSalmon
        case DarkSeaGreen
        case DarkSlateBlue
        case DarkSlateGray
    }

    ///
    /// TODO: DOC
    ///
    pub def toRgb(c: Color): (Int32, Int32, Int32) = match c {
        case AliceBlue => (240, 248, 255)
        case AntiqueWhite => (	250,235,215)
        case Aqua => (0,255,255)
        case Aquamarine => (	127,255,212)
        case Azure => (240,255,255)
        case Beige => (	245,245,220)
        case Bisque => (	255,228,196)
        case Black => (	0,0,0 )
        case BlanchedAlmond => (	255,235,205)
        case Blue => (	0,0,255 )
        case BlueViolet => (	138,43,226 )
        case Brown => (165,42,42 )
        case BurlyWood => (	222,184,135)
        case CadetBlue => (	95,158,160 )
        case Chartreuse => (	127,255,0 )
        case Chocolate => (	210,105,30)
        case Coral => (	255,127,80 )
        case CornflowerBlue => (100,149,237)
        case Cornsilk => (	255,248,220)
        case Crimson => (	220,20,60 )
        case Cyan => (	0,255,255 )
        case DarkBlue => (	0,0,139 )
        case DarkCyan => (	0,139,139 )
        case DarkGoldenRod => (	184,134,11 )
        case DarkGray => (	169,169,169)
        case DarkGrey => (	169,169,169)
        case DarkGreen => (	0,100,0 )
        case DarkKhaki => (189,183,107)
        case DarkMagenta => (	139,0,139 )
        case DarkOliveGreen => (85,107,47 )
        case DarkOrange => (	255,140,0 )
        case DarkOrchid => (	153,50,204 )
        case DarkRed => (	139,0,0 )
        case DarkSalmon => (	233,150,122)
        case DarkSeaGreen => (	143,188,143)
        case DarkSlateBlue => (	72,61,139 )
        case DarkSlateGray => (	47,79,79 )
    }


    ///
    /// TODO: DOC
    ///
    pub def formatWithColor(s: String, c: Color): String =
        let (r, g, b) = toRgb(c);
        Console.format(s, r, g, b)

    ///
    /// TODO: DOC
    ///
    pub enum ColorSupport {
        case NoColor,
        case BasicColor,
        case Extended,
        case TrueColor
    }

    ///
    /// TODO: DOC
    /// TODO: Split into several boolean functions.
    ///
    pub def colorSupport(): ColorSupport & Impure =
        import java.lang.System:getenv(String);
        if (not Object.isNull(getenv("COLORTERM")) and getenv("COLORTERM") == "truecolor") // TODO "24bit" (case sensitive)
            TrueColor
        else {
            let term = getenv("TERM");
            if (Object.isNull(term))
                NoColor
            else if (String.indexOf("256", term) != None)
                Extended
            else {
                let basic = "screen" :: "xterm" :: "ansi" :: Nil;
                if (List.exists(s -> String.indexOf(s, term) != None, basic))
                    BasicColor
                else
                    NoColor
            }
        }

    ///
    /// TODO: DOC
    ///
    pub def format(s: String, r: Int32, g: Int32, b: Int32): String =
        let esc = Console.escape();
        let red = Int32.toString(r);
        let gre = Int32.toString(g);
        let blu = Int32.toString(b);
        esc + "[38;2;" + red + ";" + gre + ";" + blu + "m" + s + esc + "[0m"

    // TODO: background color.

    // TODO: HEX and RGB helper functions.

    ///
    /// TODO: DOC
    ///
    pub def escape(): String = Char.toString('\u001b')

}