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

use IoError.ErrorKind
use IoError.IoError
use ProcessHandle.ProcessHandle

use Process.StdIn.StdIn
use Process.StdOut.StdOut
use Process.StdErr.StdErr

///
/// An effect used to start a process outside the JVM.
///
/// All operations on this effect are infallible.
/// If an operation fails the handler must deal with it.
///
eff Process {

    ///
    /// Immediately executes the command `cmd` with the arguments `args`, by the path `cwd` and with the environmental `env`.
    ///
    def execWithCwdAndEnv(cmd: String, args: List[String], cwd: Option[String], env: Map[String, String]): ProcessHandle

    ///
    /// Returns the exit value of the process.
    ///
    def exitValue(ph: ProcessHandle): Option[Int32]

    ///
    /// Returns status of the process, if it alive or not.
    ///
    def isAlive(ph: ProcessHandle): Bool

    ///
    /// Returns the PID of the process.
    ///
    def pid(ph: ProcessHandle): Int64

    ///
    /// Returns stdin InputStream of the process.
    ///
    def stdin(ph: ProcessHandle): Process.StdIn

    ///
    /// Returns stdout OutputStream of the process.
    ///
    def stdout(ph: ProcessHandle): Process.StdOut

    ///
    /// Returns stderr OutputStream of the process.
    ///
    def stderr(ph: ProcessHandle): Process.StdErr

    ///
    /// Stops the process.
    ///
    def stop(ph: ProcessHandle): Unit

    ///
    /// Waits for process to finish.
    ///
    def waitFor(ph: ProcessHandle): Int32

    ///
    /// Waits a specified amount of time for the process to finish
    ///
    def waitForTimeout(ph: ProcessHandle, time: Int64, tUnit: TimeUnit): Bool

}

pub mod Process {

    ///
    /// Represents stdin of a process.
    ///
    pub enum StdIn(ProcessHandle)

    ///
    /// Represents stdout of a process.
    ///
    pub enum StdOut(ProcessHandle)

    ///
    /// Represents stderr of a process.
    ///
    pub enum StdErr(ProcessHandle)

    ///
    /// Executes the command `cmd` with the arguments `args`.
    ///
    pub def exec(cmd: String, args: List[String]): ProcessHandle \ Process =
        Process.execWithCwdAndEnv(cmd, args, None, Map.empty())

    ///
    /// Executes the command `cmd` with the arguments `args`, by the path `cwd`.
    ///
    pub def execWithCwd(cmd: String, args: List[String], cwd: Option[String]): ProcessHandle \ Process =
        Process.execWithCwdAndEnv(cmd, args, cwd, Map.empty())

    ///
    /// Executes the command `cmd` with the arguments `args` and with the environmental `env`.
    ///
    pub def execWithEnv(cmd: String, args: List[String], env: Map[String, String]): ProcessHandle \ Process =
        Process.execWithCwdAndEnv(cmd, args, None, env)

}

instance Writable[Process.StdIn] {
    type Elm = Int8
    type Aef = IO

    pub def write(b: Array[Int8, r], r: Process.StdIn): Result[IoError, Int32] \ r + IO = {
        let StdIn(ph) = r;
        let ProcessHandle(id) = ph;
        let (ok, n, kind, msg) = %%PROCESS_STDIN_WRITE%%((id, b));
        if (ok) Ok(n) else Err(IoError(IoError.errorKindFromCode(kind), msg))
    }
}

instance Readable[Process.StdOut] {
    type Elm = Int8
    type Aef = IO

    pub def read(b: Array[Int8, r], r: Process.StdOut): Result[IoError, Int32] \ r + IO = {
        let StdOut(ph) = r;
        let ProcessHandle(id) = ph;
        let (ok, n, kind, msg) = %%PROCESS_STDOUT_READ%%((id, b));
        if (ok) Ok(n) else Err(IoError(IoError.errorKindFromCode(kind), msg))
    }
}

instance Readable[Process.StdErr] {
    type Elm = Int8
    type Aef = IO

    pub def read(b: Array[Int8, r], r: Process.StdErr): Result[IoError, Int32] \ r + IO = {
        let StdErr(ph) = r;
        let ProcessHandle(id) = ph;
        let (ok, n, kind, msg) = %%PROCESS_STDERR_READ%%((id, b));
        if (ok) Ok(n) else Err(IoError(IoError.errorKindFromCode(kind), msg))
    }
}

// JVM-only: the default handler implementation lives in `jvm/Process.flix`.
