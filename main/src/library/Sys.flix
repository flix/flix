/*
 *  Copyright 2026 Flix Authors
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

pub mod Sys {

    use Time.TimeUnit

    import java.lang.{Process => JProcess}
    import java.io.InputStream
    import java.io.OutputStream

    ///
    /// A handle for a running process.
    ///
    pub enum ProcessHandle(JProcess)

    ///
    /// A wrapper for the stdin stream of a process.
    ///
    pub enum StdIn(OutputStream)
    instance Writable[StdIn] {
        type Elm = Int8
        type Aef = IO
        pub def write(b: Array[Int8, r], r: StdIn): Result[IoError, Int32] \ r + IO =
            let StdIn.StdIn(pr) = r;
            Writable.write(b, pr)
    }

    ///
    /// A wrapper for the stdout stream of a process.
    ///
    pub enum StdOut(InputStream)
    instance Readable[StdOut] {
        type Elm = Int8
        type Aef = IO
        pub def read(b: Array[Int8, r], r: StdOut): Result[IoError, Int32] \ r + IO =
            let StdOut.StdOut(pr) = r;
            Readable.read(b, pr)
    }

    ///
    /// A wrapper for the stderr stream of a process.
    ///
    pub enum StdErr(InputStream)
    instance Readable[StdErr] {
        type Elm = Int8
        type Aef = IO
        pub def read(b: Array[Int8, r], r: StdErr): Result[IoError, Int32] \ r + IO =
            let StdErr.StdErr(pr) = r;
            Readable.read(b, pr)
    }

    ///
    /// An effect used to access the system environment.
    ///
    pub eff Env {

        ///
        /// Returns the arguments passed to the program via the command line.
        ///
        def getArgs(): List[String]

        ///
        /// Returns an map of the current system environment.
        ///
        def getEnv(): Map[String, String]

        ///
        /// Returns the value of the specified environment variable.
        ///
        def getVar(name: String): Option[String]

        ///
        /// Returns the system property by name.
        ///
        def getProp(name: String): Option[String]

        ///
        /// Returns the operating system name
        ///
        def getOsName(): Option[String]

        ///
        /// Returns the operating system architecture
        ///
        def getOsArch(): Option[String]

        ///
        /// Returns the operating system version
        ///
        def getOsVersion(): Option[String]

        ///
        /// Returns the file separator
        ///
        def getFileSeparator(): String

        ///
        /// Returns the path separator
        ///
        def getPathSeparator(): String

        ///
        /// Returns the system line separator
        ///
        def getLineSeparator(): String

        ///
        /// Returns the user's current working directory
        ///
        def getCurrentWorkingDirectory(): Option[String]

        ///
        /// Returns the default temp file path
        ///
        def getTemporaryDirectory(): Option[String]

        ///
        /// Returns the user's account name
        ///
        def getUserName(): Option[String]

        ///
        /// Returns the user's home directory
        ///
        def getUserHomeDirectory(): Option[String]

        ///
        /// Returns the number of virtual processors available to the JVM.
        ///
        /// The returned value is never less than one.
        ///
        def getVirtualProcessors(): Int32

    }

    ///
    /// An effect used to exit the JVM.
    ///
    pub eff Exit {

        ///
        /// Immediately exits the JVM with the specified `exitCode`.
        ///
        def exit(exitCode: Int32): Void

    }

    ///
    /// An effect used to start a process outside the JVM.
    ///
    pub eff Process {

        ///
        /// Immediately executes the command `cmd` with the arguments `args`, by the path `cwd` and with the environment `env`.
        ///
        def execWithCwdAndEnv(cmd: String, args: List[String], cwd: Option[String], env: Map[String, String]): Result[IoError, ProcessHandle]

        ///
        /// Returns the exit value of the process `ph`.
        ///
        def exitValue(ph: ProcessHandle): Result[IoError, Int32]

        ///
        /// Returns whether the process `ph` is alive.
        ///
        def isAlive(ph: ProcessHandle): Result[IoError, Bool]

        ///
        /// Returns the PID of the process `ph`.
        ///
        def pid(ph: ProcessHandle): Result[IoError, Int64]

        ///
        /// Returns the stdin stream of the process `ph`.
        ///
        def stdin(ph: ProcessHandle): Result[IoError, StdIn]

        ///
        /// Returns the stdout stream of the process `ph`.
        ///
        def stdout(ph: ProcessHandle): Result[IoError, StdOut]

        ///
        /// Returns the stderr stream of the process `ph`.
        ///
        def stderr(ph: ProcessHandle): Result[IoError, StdErr]

        ///
        /// Stops the process `ph`.
        ///
        def stop(ph: ProcessHandle): Result[IoError, Unit]

        ///
        /// Waits for the process `ph` to finish and returns its exit value.
        ///
        def waitFor(ph: ProcessHandle): Result[IoError, Int32]

        ///
        /// Waits at most `time` (in the given `tUnit`) for the process `ph` to finish.
        /// Returns `true` if the process exited, `false` if the timeout elapsed.
        ///
        def waitForTimeout(ph: ProcessHandle, time: Int64, tUnit: TimeUnit): Result[IoError, Bool]

    }

    ///
    /// An effect used to interact with the console.
    ///
    pub eff Console {

        ///
        /// Reads a single line from the console.
        ///
        def readln(): String

        ///
        /// Prints the given string `s` to the standard out.
        ///
        def print(s: String): Unit

        ///
        /// Prints the given string `s` to the standard err.
        ///
        def eprint(s: String): Unit

        ///
        /// Prints the given string `s` to the standard out followed by a new line.
        ///
        def println(s: String): Unit

        ///
        /// Prints the given string `s` to the standard err followed by a new line.
        ///
        def eprintln(s: String): Unit

    }

}
