/*
 * Copyright 2021 Magnus Madsen
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
mod System {

    ///
    /// Operations on the Standard Input Stream.
    ///
    mod StdIn {

        import java.io.BufferedReader
        import java.io.InputStreamReader
        import java.lang.{System => JSystem}

        ///
        /// Returns an iterator over lines from the standard input stream.
        ///
        /// The iterator ends when the stream is closed or when an error occurs.
        ///
        /// When the iterator is garbage collected the standard input stream is closed.
        /// Thus, the `readLines` function should not be called more than once per program.
        ///
        /// See also `Console.readLine` for reading from the console.
        ///
        pub def readLines(rc: Region[r]): Iterator[String, IO + r, r] \ { r, IO } =
            let br = new BufferedReader(new InputStreamReader(JSystem.in));
            let nextLine = ref None @ rc;
            let next = () -> {
                let l = match deref nextLine {
                    case Some(line) => if (Object.isNull(line)) None else Some(line)
                    case None =>
                        let n = br.readLine();
                        Ref.put(Some(n), nextLine);
                        if (Object.isNull(n)) None else Some(n)
                };
                Ref.put(None, nextLine);
                l
            };
            Iterator.iterate(rc, next)

    }

    ///
    /// Operations on the Standard Output Stream.
    ///
    mod StdOut {

        import java.lang.{System => JSystem}
        import java.io.PrintStream

        ///
        /// Prints the given string `s` to the standard output stream.
        ///
        pub def print(s: String): Unit \ IO =
            getOutputStream().print(s)

        ///
        /// Prints the given string `s` and a newline to the standard output stream.
        ///
        pub def println(s: String): Unit \ IO =
            getOutputStream().print(s)

        ///
        /// Prints a newline to the standard output stream.
        ///
        pub def newLine(): Unit \ IO =
            getOutputStream().println()

        ///
        /// Flushes the standard output stream.
        ///
        pub def flush(): Unit \ IO =
            getOutputStream().flush()

        ///
        /// Returns the standard output stream.
        ///
        def getOutputStream(): PrintStream \ IO = JSystem.out

    }

    ///
    /// Operations on the Standard Error Stream.
    ///
    mod StdErr {

        import java.lang.{System => JSystem}
        import java.io.PrintStream

        ///
        /// Prints the given string `s` to the standard error stream.
        ///
        pub def print(s: String): Unit \ IO =
            getErrorStream().print(s)

        ///
        /// Prints the given string `s` and a newline to the standard error stream.
        ///
        pub def println(s: String): Unit \ IO =
            getErrorStream().println(s)

        ///
        /// Prints a newline to the standard error stream.
        ///
        pub def newLine(): Unit \ IO =
            getErrorStream().println()

        ///
        /// Flushes the standard error stream.
        ///
        pub def flush(): Unit \ IO =
            getErrorStream().flush()

        ///
        /// Returns the standard error stream.
        ///
        def getErrorStream(): PrintStream \ IO = JSystem.err

    }

}
