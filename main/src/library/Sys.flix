/*
 *  Copyright 2024 Magnus Madsen, Maksim Gusev
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
    /// An effect used to start a process outside the JVM.
    ///
    pub eff Process {
        def execWithCwdAndEnv(cmd: String, args: List[String], cwd: Option[String], env: Map[String, String]): Result[IoError, ProcessHandle]
        def exitValue(ph: ProcessHandle): Result[IoError, Int32]
        def isAlive(ph: ProcessHandle): Result[IoError, Bool]
        def pid(ph: ProcessHandle): Result[IoError, Int64]
        def stdin(ph: ProcessHandle): Result[IoError, StdIn]
        def stdout(ph: ProcessHandle): Result[IoError, StdOut]
        def stderr(ph: ProcessHandle): Result[IoError, StdErr]
        def stop(ph: ProcessHandle): Result[IoError, Unit]
        def waitFor(ph: ProcessHandle): Result[IoError, Int32]
        def waitForTimeout(ph: ProcessHandle, time: Int64, tUnit: TimeUnit): Result[IoError, Bool]
    }

}
