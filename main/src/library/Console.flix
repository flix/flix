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
    /// An enum that represents the colors supported by a terminal.
    ///
    pub enum ColorSupport {
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

    namespace Color {

        ///
        /// Default Colors.
        ///
        pub enum Default {
            case Blue
            case Cyan
            case Green
            case Magenta
            case Red
            case Yellow
            case White
        }

        ///
        /// Namespace for Default Colors.
        ///
        namespace Default {

            ///
            /// Returns the RGB-triplet for the given color `c`.
            ///
            pub def toRGB(c: Console/Color.Default): (Int32, Int32, Int32) =
                use Console/Color.Default;
                match c {
                    case Default.Blue       => (113, 190, 242)
                    case Default.Cyan       => (102, 194, 205)
                    case Default.Green      => (168, 204, 140)
                    case Default.Magenta    => (210, 144, 228)
                    case Default.Red        => (232, 131, 136)
                    case Default.Yellow     => (219, 171, 121)
                    case Default.White      => (185, 191, 202)
                }

        }

        ///
        /// Ansi Colors.
        ///
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
        /// Namespace for Ansi Colors.
        ///
        namespace Ansi {

            ///
            /// Down sample the given color `c` to an ANSI color code.
            ///
            pub def downsample(c: (Int32, Int32, Int32)): Int32 =
                use Console/Color.Ansi;
                let candidates = Ansi.Black :: Ansi.Red :: Ansi.Green :: Ansi.Yellow :: Ansi.Blue :: Ansi.Magenta ::
                                 Ansi.Cyan :: Ansi.White :: Ansi.BrightBlack :: Ansi.BrightRed :: Ansi.BrightGreen ::
                                 Ansi.BrightYellow :: Ansi.BrightBlue :: Ansi.BrightMagenta :: Ansi.BrightCyan ::
                                 Ansi.BrightWhite :: Nil;
                nearest(c, Ansi.Black, Int32.maxValue(), candidates) |> toFG

            ///
            /// Returns the ANSI color nearest to the given `target` color from the given list of `candidates`.
            ///
            def nearest(target: (Int32, Int32, Int32), bestCandidate: Console/Color.Ansi, bestDistance: Int32, candidates: List[Console/Color.Ansi]): Console/Color.Ansi =
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
            /// Returns the foreground color of the given color `c`.
            ///
            pub def toFG(c: Console/Color.Ansi): Int32 =
                use Console/Color.Ansi;
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
            pub def toRGB(c: Console/Color.Ansi): (Int32, Int32, Int32) =
                use Console/Color.Ansi;
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

        }

        ///
        /// Namespace for Web Colors.
        ///
        namespace Web {

            ///
            /// Web Colors.
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
                case DarkSlateGrey
                case DarkTurquoise
                case DarkViolet
                case DeepPink
                case DeepSkyBlue
                case DimGray
                case DimGrey
                case DodgerBlue
                case FireBrick
                case FloralWhite
                case ForestGreen
                case Fuchsia
                case Gainsboro
                case GhostWhite
                case Gold
                case GoldenRod
                case Gray
                case Grey
                case Green
                case GreenYellow
                case HoneyDew
                case HotPink
                case IndianRed
                case Indigo
                case Ivory
                case Khaki
                case Lavender
                case LavenderBlush
                case LawnGreen
                case LemonChiffon
                case LightBlue
                case LightCoral
                case LightCyan
                case LightGoldenRodYellow
                case LightGray
                case LightGrey
                case LightGreen
                case LightPink
                case LightSalmon
                case LightSeaGreen
                case LightSkyBlue
                case LightSlateGray
                case LightSlateGrey
                case LightSteelBlue
                case LightYellow
                case Lime
                case LimeGreen
                case Linen
                case Magenta
                case Maroon
                case MediumAquaMarine
                case MediumBlue
                case MediumOrchid
                case MediumPurple
                case MediumSeaGreen
                case MediumSlateBlue
                case MediumSpringGreen
                case MediumTurquoise
                case MediumVioletRed
                case MidnightBlue
                case MintCream
                case MistyRose
                case Moccasin
                case NavajoWhite
                case Navy
                case OldLace
                case Olive
                case OliveDrab
                case Orange
                case OrangeRed
                case Orchid
                case PaleGoldenRod
                case PaleGreen
                case PaleTurquoise
                case PaleVioletRed
                case PapayaWhip
                case PeachPuff
                case Peru
                case Pink
                case Plum
                case PowderBlue
                case Purple
                case RebeccaPurple
                case Red
                case RosyBrown
                case RoyalBlue
                case SaddleBrown
                case Salmon
                case SandyBrown
                case SeaGreen
                case SeaShell
                case Sienna
                case Silver
                case SkyBlue
                case SlateBlue
                case SlateGray
                case SlateGrey
                case Snow
                case SpringGreen
                case SteelBlue
                case Tan
                case Teal
                case Thistle
                case Tomato
                case Turquoise
                case Violet
                case Wheat
                case White
                case WhiteSmoke
                case Yellow
                case YellowGreen
            }

            ///
            /// Returns the RGB-triplet for the given color `c`.
            ///
            pub def toRGB(c: Color): (Int32, Int32, Int32) = match c {
                case AliceBlue              => (240, 248, 255)
                case AntiqueWhite           => (250, 235, 215)
                case Aqua                   => (  0, 255, 255)
                case Aquamarine             => (127, 255, 212)
                case Azure                  => (240, 255, 255)
                case Beige                  => (245, 245, 220)
                case Bisque                 => (255, 228, 196)
                case Black                  => (  0,   0,   0)
                case BlanchedAlmond         => (255, 235, 205)
                case Blue                   => (  0,  0,  255)
                case BlueViolet             => (138,  43, 226)
                case Brown                  => (165,  42,  42)
                case BurlyWood              => (222, 184, 135)
                case CadetBlue              => ( 95, 158, 160)
                case Chartreuse             => (127, 255,   0)
                case Chocolate              => (210, 105,  30)
                case Coral                  => (255, 127,  80)
                case CornflowerBlue         => (100, 149, 237)
                case Cornsilk               => (255, 248, 220)
                case Crimson                => (220,  20,  60)
                case Cyan                   => (  0, 255, 255)
                case DarkBlue               => (  0,   0, 139)
                case DarkCyan               => (  0, 139, 139)
                case DarkGoldenRod          => (184, 134,  11)
                case DarkGray               => (169, 169, 169)
                case DarkGrey               => (169, 169, 169)
                case DarkGreen              => (  0, 100,   0)
                case DarkKhaki              => (189, 183, 107)
                case DarkMagenta            => (139,   0, 139)
                case DarkOliveGreen         => ( 85, 107,  47)
                case DarkOrange             => (255, 140,   0)
                case DarkOrchid             => (153,  50, 204)
                case DarkRed                => (139,   0,   0)
                case DarkSalmon             => (233, 150, 122)
                case DarkSeaGreen           => (143, 188, 143)
                case DarkSlateBlue          => ( 72,  61, 139)
                case DarkSlateGray          => ( 47,  79,  79)
                case DarkSlateGrey          => ( 47,  79,  79)
                case DarkTurquoise          => (  0, 206, 209)
                case DarkViolet             => (148,   0, 211)
                case DeepPink               => (255,  20, 147)
                case DeepSkyBlue            => (  0, 191, 255)
                case DimGray                => (105, 105, 105)
                case DimGrey                => (105, 105, 105)
                case DodgerBlue             => ( 30, 144, 255)
                case FireBrick              => (178,  34,  34)
                case FloralWhite            => (255, 250, 240)
                case ForestGreen            => ( 34, 139, 34)
                case Fuchsia                => (255,   0, 255)
                case Gainsboro              => (220, 220, 220)
                case GhostWhite             => (248, 248, 255)
                case Gold                   => (255, 215,   0)
                case GoldenRod              => (218, 165,  32)
                case Gray                   => (128, 128, 128)
                case Grey                   => (128, 128, 128)
                case Green                  => (  0, 128,   0)
                case GreenYellow            => (173, 255,  47)
                case HoneyDew               => (240, 255, 240)
                case HotPink                => (255, 105, 180)
                case IndianRed              => (205,  92,  92)
                case Indigo                 => ( 75,   0, 130)
                case Ivory                  => (255, 255, 240)
                case Khaki                  => (240, 230, 140)
                case Lavender               => (230, 230, 250)
                case LavenderBlush          => (255, 240, 245)
                case LawnGreen              => (124, 252,   0)
                case LemonChiffon           => (255, 250, 205)
                case LightBlue              => (173, 216, 230)
                case LightCoral             => (240, 128, 128)
                case LightCyan              => (224, 255, 255)
                case LightGoldenRodYellow   => (250, 250, 210)
                case LightGray              => (211, 211, 211)
                case LightGrey              => (211, 211, 211)
                case LightGreen             => (144, 238, 144)
                case LightPink              => (255, 182, 193)
                case LightSalmon            => (255, 160, 122)
                case LightSeaGreen          => ( 32, 178, 170)
                case LightSkyBlue           => (135, 206, 250)
                case LightSlateGray         => (119, 136, 153)
                case LightSlateGrey         => (119, 136, 153)
                case LightSteelBlue         => (176, 196, 222)
                case LightYellow            => (255, 255, 224)
                case Lime                   => (  0, 255,   0)
                case LimeGreen              => ( 50, 205,  50)
                case Linen                  => (250, 240, 230)
                case Magenta                => (255,   0, 255)
                case Maroon                 => (128,   0,   0)
                case MediumAquaMarine       => (102, 205, 170)
                case MediumBlue             => (  0,   0, 205)
                case MediumOrchid           => (186,  85, 211)
                case MediumPurple           => (147, 112, 219)
                case MediumSeaGreen         => ( 60, 179, 113)
                case MediumSlateBlue        => (123, 104, 238)
                case MediumSpringGreen      => (  0, 250, 154)
                case MediumTurquoise        => ( 72, 209, 204)
                case MediumVioletRed        => (199,  21, 133)
                case MidnightBlue           => ( 25,  25, 112)
                case MintCream              => (245, 255, 250)
                case MistyRose              => (255, 228, 225)
                case Moccasin               => (255, 228, 181)
                case NavajoWhite            => (255, 222, 173)
                case Navy                   => (  0,   0, 128)
                case OldLace                => (253, 245, 230)
                case Olive                  => (128, 128,   0)
                case OliveDrab              => (107, 142,  35)
                case Orange                 => (255, 165,   0)
                case OrangeRed              => (255,  69,   0)
                case Orchid                 => (218, 112, 214)
                case PaleGoldenRod          => (238, 232, 170)
                case PaleGreen              => (152, 251, 152)
                case PaleTurquoise          => (175, 238, 238)
                case PaleVioletRed          => (219, 112, 147)
                case PapayaWhip             => (255, 239, 213)
                case PeachPuff              => (255, 218, 185)
                case Peru                   => (205, 133,  63)
                case Pink                   => (255, 192, 203)
                case Plum                   => (221, 160, 221)
                case PowderBlue             => (176, 224, 230)
                case Purple                 => (128,   0, 128)
                case RebeccaPurple          => (102,  51, 153)
                case Red                    => (255,   0,   0)
                case RosyBrown              => (188, 143, 143)
                case RoyalBlue              => ( 65, 105, 225)
                case SaddleBrown            => (139,  69,  19)
                case Salmon                 => (250, 128, 114)
                case SandyBrown             => (244, 164,  96)
                case SeaGreen               => ( 46, 139,  87)
                case SeaShell               => (255, 245, 238)
                case Sienna                 => (160,  82,  45)
                case Silver                 => (192, 192, 192)
                case SkyBlue                => (135, 206, 235)
                case SlateBlue              => (106,  90, 205)
                case SlateGray              => (112, 128, 144)
                case SlateGrey              => (112, 128, 144)
                case Snow                   => (255, 250, 250)
                case SpringGreen            => (  0, 255, 127)
                case SteelBlue              => ( 70, 130, 180)
                case Tan                    => (210, 180, 140)
                case Teal                   => (  0, 128, 128)
                case Thistle                => (216, 191, 216)
                case Tomato                 => (255,  99,  71)
                case Turquoise              => ( 64, 224, 208)
                case Violet                 => (238, 130, 238)
                case Wheat                  => (245, 222, 179)
                case White                  => (255, 255, 255)
                case WhiteSmoke             => (245, 245, 245)
                case Yellow                 => (255, 255,   0)
                case YellowGreen            => (154, 205,  50)
            }
        }
    }

    // TODO: Namespaces.

    ///
    /// Returns the string `s` formatted with the given RGB color `c`.
    ///
    pub def formatter(c: (Int32, Int32 , Int32), s: String): String =
        match colorSupport() {
            case ColorSupport.None      => s
            case ColorSupport.Ansi      =>
                use Console/Color/Ansi.downsample;
                let fg = downsample(c);
                formatAnsi(fg, s)
            case ColorSupport.TrueColor => formatTrueColor(c, s)
        }

    ///
    /// Returns the string `s` formatted with the given ANSI color `fg`.
    ///
    def formatAnsi(fg: Int32, s: String): String =
        let esc = Console.escape();
        esc + "[" + Int32.toString(fg) + "m" + s + esc + "[0m"

    ///
    /// Returns the string `s` formatted with the given RGB color `c`.
    ///
    def formatTrueColor(c: (Int32, Int32, Int32), s: String): String =
        let (r, g, b) = c;
        let esc = Console.escape();
        let red = Int32.toString(r);
        let gre = Int32.toString(g);
        let blu = Int32.toString(b);
        esc + "[38;2;" + red + ";" + gre + ";" + blu + "m" + s + esc + "[0m"

    ///
    /// Returns the escape character as a string.
    ///
    def escape(): String = Char.toString('\u001b')

    ///
    /// Returns the color support of the current terminal.
    ///
    pub def colorSupport(): ColorSupport =
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
        import java.lang.System:getenv(String);
        let p = getenv("TERM") as & Pure;
        if (Object.isNull(p))
            false
        else {
            let keywords = "256" :: "ansi" :: "xterm" :: "screen" :: Nil;
            List.exists(s -> String.isInfixOf(s, p), keywords)
        }

    ///
    /// Returns `true` if the terminal supports 24-bit colors.
    ///
    def isTrueColorTerminal(): Bool =
        import java.lang.System:getenv(String);
        let p = getenv("COLORTERM") as & Pure;
        if (Object.isNull(p))
            false
        else
            p == "truecolor"

    ///
    /// Returns `true` if the terminal is the Windows Terminal.
    ///
    def isWindowsTerminal(): Bool =
        import java.lang.System:getenv(String);
        let p = getenv("WT_SESSION") as & Pure;
        not Object.isNull(p)

}