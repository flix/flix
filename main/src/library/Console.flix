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

    ///
    /// TODO: DOC
    ///
    pub def escape(): String = Char.toString('\u001b')

}