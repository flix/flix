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
    pub def readLine(): Str @ IO = Console/StdIn.readLine(())

    ///
    /// Alias for `Console.StdOut.print`.
    ///
    pub def print(s: Str): Unit @ IO = Console/StdOut.print(s)

    ///
    /// Alias for `Console.StdOut.printLine`.
    ///
    pub def printLine(s: Str): Unit @ IO = Console/StdOut.printLine(s)

    ///
    /// Operations on the Standard Input Stream.
    ///
    namespace StdIn {

        ///
        /// Returns the next line read from the standard input stream.
        ///
        pub def readLine(): Str @ IO = native method flix.runtime.library.Console.readLine() as Str @ IO

    }

    ///
    /// Operations on the Standard Output Stream.
    ///
    namespace StdOut {

        ///
        /// Prints the given string `s` to the standard output stream.
        ///
        pub def print(s: Str): Unit @ IO = native method flix.runtime.library.Console.printStdOut(s) as Unit @ IO

        ///
        /// Prints the given string `s` and a newline to the standard output stream.
        ///
        pub def printLine(s: Str): Unit @ IO = native method flix.runtime.library.Console.printLineStdOut(s) as Unit @ IO

        ///
        /// Prints a newline to the standard output stream.
        ///
        pub def newLine(): Unit @ IO = native method flix.runtime.library.Console.newLineStdOut() as Unit @ IO

        ///
        /// Flushes the standard output stream.
        ///
        pub def flush(): Unit @ IO = native method flix.runtime.library.Console.flushStdOut() as Unit @ IO

    }

    ///
    /// Operations on the Standard Error Stream.
    ///
    namespace StdErr {

        ///
        /// Prints the given string `s` to the standard error stream.
        ///
        pub def print(s: Str): Unit @ IO = native method flix.runtime.library.Console.printStdErr(s) as Unit @ IO

        ///
        /// Prints the given string `s` and a newline to the standard error stream.
        ///
        pub def printLine(s: Str): Unit @ IO  = native method flix.runtime.library.Console.printLineStdErr(s) as Unit @ IO

        ///
        /// Prints a newline to the standard error stream.
        ///
        pub def newLine(): Unit @ IO  = native method flix.runtime.library.Console.newLineStdErr() as Unit @ IO

        ///
        /// Flushes the standard error stream.
        ///
        pub def flush(): Unit @ IO  = native method flix.runtime.library.Console.flushStdErr() as Unit @ IO

    }

}