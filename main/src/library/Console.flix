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
            import new java.util.Scanner(##java.io.InputStream) as newScanner;
            import java.util.Scanner.hasNextLine() as hasNextLine;
            import java.util.Scanner.nextLine() as nextLine;
            import get java.lang.System:in as in;
            let i = in() as ##java.io.InputStream; // safe up cast
            let s = newScanner(i);
            if (hasNextLine(s))
                Some(nextLine(s))
            else
                None
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
        /// Returns the string `s` formatted with the default blue color.
        ///
        pub def blue(s: String): String =
            use Console/Color.Default.Blue;
            use Console/Color/Default.toRGB;
            use Console.rgb;
            rgb(Blue |> toRGB, s)

        ///
        /// Returns the string `s` formatted with the default cyan color.
        ///
        pub def cyan(s: String): String =
            use Console/Color.Default.Cyan;
            use Console/Color/Default.toRGB;
            use Console.rgb;
            rgb(Cyan |> toRGB, s)

        ///
        /// Returns the string `s` formatted with the default green color.
        ///
        pub def green(s: String): String =
            use Console/Color.Default.Green;
            use Console/Color/Default.toRGB;
            use Console.rgb;
            rgb(Green |> toRGB, s)

        ///
        /// Returns the string `s` formatted with the default magenta color.
        ///
        pub def magenta(s: String): String =
            use Console/Color.Default.Magenta;
            use Console/Color/Default.toRGB;
            use Console.rgb;
            rgb(Magenta |> toRGB, s)

        ///
        /// Returns the string `s` formatted with the default red color.
        ///
        pub def red(s: String): String =
            use Console/Color.Default.Red;
            use Console/Color/Default.toRGB;
            use Console.rgb;
            rgb(Red |> toRGB, s)

        ///
        /// Returns the string `s` formatted with the default yellow color.
        ///
        pub def yellow(s: String): String =
            use Console/Color.Default.Yellow;
            use Console/Color/Default.toRGB;
            use Console.rgb;
            rgb(Yellow |> toRGB, s)

        ///
        /// Returns the string `s` formatted with the default white color.
        ///
        pub def white(s: String): String =
            use Console/Color.Default.White;
            use Console/Color/Default.toRGB;
            use Console.rgb;
            rgb(White |> toRGB, s)

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
        /// Web Colors.
        ///
        pub enum Web {
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
        /// Namespace for Web Colors.
        ///
        namespace Web {

            ///
            /// Returns the RGB-triplet for the given color `c`.
            ///
            pub def toRGB(c: Console/Color.Web): (Int32, Int32, Int32) =
                use Console/Color.Web;
                match c {
                    case Web.AliceBlue              => (240, 248, 255)
                    case Web.AntiqueWhite           => (250, 235, 215)
                    case Web.Aqua                   => (  0, 255, 255)
                    case Web.Aquamarine             => (127, 255, 212)
                    case Web.Azure                  => (240, 255, 255)
                    case Web.Beige                  => (245, 245, 220)
                    case Web.Bisque                 => (255, 228, 196)
                    case Web.Black                  => (  0,   0,   0)
                    case Web.BlanchedAlmond         => (255, 235, 205)
                    case Web.Blue                   => (  0,  0,  255)
                    case Web.BlueViolet             => (138,  43, 226)
                    case Web.Brown                  => (165,  42,  42)
                    case Web.BurlyWood              => (222, 184, 135)
                    case Web.CadetBlue              => ( 95, 158, 160)
                    case Web.Chartreuse             => (127, 255,   0)
                    case Web.Chocolate              => (210, 105,  30)
                    case Web.Coral                  => (255, 127,  80)
                    case Web.CornflowerBlue         => (100, 149, 237)
                    case Web.Cornsilk               => (255, 248, 220)
                    case Web.Crimson                => (220,  20,  60)
                    case Web.Cyan                   => (  0, 255, 255)
                    case Web.DarkBlue               => (  0,   0, 139)
                    case Web.DarkCyan               => (  0, 139, 139)
                    case Web.DarkGoldenRod          => (184, 134,  11)
                    case Web.DarkGray               => (169, 169, 169)
                    case Web.DarkGrey               => (169, 169, 169)
                    case Web.DarkGreen              => (  0, 100,   0)
                    case Web.DarkKhaki              => (189, 183, 107)
                    case Web.DarkMagenta            => (139,   0, 139)
                    case Web.DarkOliveGreen         => ( 85, 107,  47)
                    case Web.DarkOrange             => (255, 140,   0)
                    case Web.DarkOrchid             => (153,  50, 204)
                    case Web.DarkRed                => (139,   0,   0)
                    case Web.DarkSalmon             => (233, 150, 122)
                    case Web.DarkSeaGreen           => (143, 188, 143)
                    case Web.DarkSlateBlue          => ( 72,  61, 139)
                    case Web.DarkSlateGray          => ( 47,  79,  79)
                    case Web.DarkSlateGrey          => ( 47,  79,  79)
                    case Web.DarkTurquoise          => (  0, 206, 209)
                    case Web.DarkViolet             => (148,   0, 211)
                    case Web.DeepPink               => (255,  20, 147)
                    case Web.DeepSkyBlue            => (  0, 191, 255)
                    case Web.DimGray                => (105, 105, 105)
                    case Web.DimGrey                => (105, 105, 105)
                    case Web.DodgerBlue             => ( 30, 144, 255)
                    case Web.FireBrick              => (178,  34,  34)
                    case Web.FloralWhite            => (255, 250, 240)
                    case Web.ForestGreen            => ( 34, 139, 34)
                    case Web.Fuchsia                => (255,   0, 255)
                    case Web.Gainsboro              => (220, 220, 220)
                    case Web.GhostWhite             => (248, 248, 255)
                    case Web.Gold                   => (255, 215,   0)
                    case Web.GoldenRod              => (218, 165,  32)
                    case Web.Gray                   => (128, 128, 128)
                    case Web.Grey                   => (128, 128, 128)
                    case Web.Green                  => (  0, 128,   0)
                    case Web.GreenYellow            => (173, 255,  47)
                    case Web.HoneyDew               => (240, 255, 240)
                    case Web.HotPink                => (255, 105, 180)
                    case Web.IndianRed              => (205,  92,  92)
                    case Web.Indigo                 => ( 75,   0, 130)
                    case Web.Ivory                  => (255, 255, 240)
                    case Web.Khaki                  => (240, 230, 140)
                    case Web.Lavender               => (230, 230, 250)
                    case Web.LavenderBlush          => (255, 240, 245)
                    case Web.LawnGreen              => (124, 252,   0)
                    case Web.LemonChiffon           => (255, 250, 205)
                    case Web.LightBlue              => (173, 216, 230)
                    case Web.LightCoral             => (240, 128, 128)
                    case Web.LightCyan              => (224, 255, 255)
                    case Web.LightGoldenRodYellow   => (250, 250, 210)
                    case Web.LightGray              => (211, 211, 211)
                    case Web.LightGrey              => (211, 211, 211)
                    case Web.LightGreen             => (144, 238, 144)
                    case Web.LightPink              => (255, 182, 193)
                    case Web.LightSalmon            => (255, 160, 122)
                    case Web.LightSeaGreen          => ( 32, 178, 170)
                    case Web.LightSkyBlue           => (135, 206, 250)
                    case Web.LightSlateGray         => (119, 136, 153)
                    case Web.LightSlateGrey         => (119, 136, 153)
                    case Web.LightSteelBlue         => (176, 196, 222)
                    case Web.LightYellow            => (255, 255, 224)
                    case Web.Lime                   => (  0, 255,   0)
                    case Web.LimeGreen              => ( 50, 205,  50)
                    case Web.Linen                  => (250, 240, 230)
                    case Web.Magenta                => (255,   0, 255)
                    case Web.Maroon                 => (128,   0,   0)
                    case Web.MediumAquaMarine       => (102, 205, 170)
                    case Web.MediumBlue             => (  0,   0, 205)
                    case Web.MediumOrchid           => (186,  85, 211)
                    case Web.MediumPurple           => (147, 112, 219)
                    case Web.MediumSeaGreen         => ( 60, 179, 113)
                    case Web.MediumSlateBlue        => (123, 104, 238)
                    case Web.MediumSpringGreen      => (  0, 250, 154)
                    case Web.MediumTurquoise        => ( 72, 209, 204)
                    case Web.MediumVioletRed        => (199,  21, 133)
                    case Web.MidnightBlue           => ( 25,  25, 112)
                    case Web.MintCream              => (245, 255, 250)
                    case Web.MistyRose              => (255, 228, 225)
                    case Web.Moccasin               => (255, 228, 181)
                    case Web.NavajoWhite            => (255, 222, 173)
                    case Web.Navy                   => (  0,   0, 128)
                    case Web.OldLace                => (253, 245, 230)
                    case Web.Olive                  => (128, 128,   0)
                    case Web.OliveDrab              => (107, 142,  35)
                    case Web.Orange                 => (255, 165,   0)
                    case Web.OrangeRed              => (255,  69,   0)
                    case Web.Orchid                 => (218, 112, 214)
                    case Web.PaleGoldenRod          => (238, 232, 170)
                    case Web.PaleGreen              => (152, 251, 152)
                    case Web.PaleTurquoise          => (175, 238, 238)
                    case Web.PaleVioletRed          => (219, 112, 147)
                    case Web.PapayaWhip             => (255, 239, 213)
                    case Web.PeachPuff              => (255, 218, 185)
                    case Web.Peru                   => (205, 133,  63)
                    case Web.Pink                   => (255, 192, 203)
                    case Web.Plum                   => (221, 160, 221)
                    case Web.PowderBlue             => (176, 224, 230)
                    case Web.Purple                 => (128,   0, 128)
                    case Web.RebeccaPurple          => (102,  51, 153)
                    case Web.Red                    => (255,   0,   0)
                    case Web.RosyBrown              => (188, 143, 143)
                    case Web.RoyalBlue              => ( 65, 105, 225)
                    case Web.SaddleBrown            => (139,  69,  19)
                    case Web.Salmon                 => (250, 128, 114)
                    case Web.SandyBrown             => (244, 164,  96)
                    case Web.SeaGreen               => ( 46, 139,  87)
                    case Web.SeaShell               => (255, 245, 238)
                    case Web.Sienna                 => (160,  82,  45)
                    case Web.Silver                 => (192, 192, 192)
                    case Web.SkyBlue                => (135, 206, 235)
                    case Web.SlateBlue              => (106,  90, 205)
                    case Web.SlateGray              => (112, 128, 144)
                    case Web.SlateGrey              => (112, 128, 144)
                    case Web.Snow                   => (255, 250, 250)
                    case Web.SpringGreen            => (  0, 255, 127)
                    case Web.SteelBlue              => ( 70, 130, 180)
                    case Web.Tan                    => (210, 180, 140)
                    case Web.Teal                   => (  0, 128, 128)
                    case Web.Thistle                => (216, 191, 216)
                    case Web.Tomato                 => (255,  99,  71)
                    case Web.Turquoise              => ( 64, 224, 208)
                    case Web.Violet                 => (238, 130, 238)
                    case Web.Wheat                  => (245, 222, 179)
                    case Web.White                  => (255, 255, 255)
                    case Web.WhiteSmoke             => (245, 245, 245)
                    case Web.Yellow                 => (255, 255,   0)
                    case Web.YellowGreen            => (154, 205,  50)
                }

        }
    }

    ///
    /// Returns the string `s` formatted with the given RGB color `c`.
    ///
    pub def rgb(c: (Int32, Int32 , Int32), s: String): String =
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
            List.exists(s -> String.contains(p, s), keywords)
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
