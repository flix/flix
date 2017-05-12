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
    def readLine(u: Unit): Str = Console/StdIn.readLine(())

    ///
    /// Alias for `Console.StdOut.print`.
    ///
    def print(s: Str): Unit = Console/StdOut.print(s)

    ///
    /// Alias for `Console.StdOut.printLine`.
    ///
    def printLine(s: Str): Unit = Console/StdOut.printLine(s)

    ///
    /// Operations on the Standard Input Stream.
    ///
    namespace StdIn {

        ///
        /// Returns the next line read from the standard input stream.
        ///
        def readLine(u: Unit): Str = unsafe native method ca.uwaterloo.flix.runtime.library.Console.readLine()

    }

    ///
    /// Operations on the Standard Output Stream.
    ///
    namespace StdOut {

        ///
        /// Prints the given string `s` to the standard output stream.
        ///
        def print(s: Str): Unit = unsafe native method ca.uwaterloo.flix.runtime.library.Console.printStdOut(s)

        ///
        /// Prints the given string `s` and a newline to the standard output stream.
        ///
        def printLine(s: Str): Unit = unsafe native method ca.uwaterloo.flix.runtime.library.Console.printLineStdOut(s)

        ///
        /// Prints a newline to the standard output stream.
        ///
        def newLine(u: Unit): Unit = unsafe native method ca.uwaterloo.flix.runtime.library.Console.newLineStdOut()

        ///
        /// Flushes the standard output stream.
        ///
        def flush(u: Unit): Unit = unsafe native method ca.uwaterloo.flix.runtime.library.Console.flushStdOut()

    }

    ///
    /// Operations on the Standard Error Stream.
    ///
    namespace StdErr {

        ///
        /// Prints the given string `s` to the standard error stream.
        ///
        def print(s: Str): Unit = unsafe native method ca.uwaterloo.flix.runtime.library.Console.printStdErr(s)

        ///
        /// Prints the given string `s` and a newline to the standard error stream.
        ///
        def printLine(s: Str): Unit = unsafe native method ca.uwaterloo.flix.runtime.library.Console.printLineStdErr(s)

        ///
        /// Prints a newline to the standard error stream.
        ///
        def newLine(u: Unit): Unit = unsafe native method ca.uwaterloo.flix.runtime.library.Console.newLineStdErr()

        ///
        /// Flushes the standard error stream.
        ///
        def flush(u: Unit): Unit = unsafe native method ca.uwaterloo.flix.runtime.library.Console.flushStdErr()

    }

}