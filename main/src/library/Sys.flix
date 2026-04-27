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

}
