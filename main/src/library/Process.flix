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

    use IoError.ErrorKind
    use IoError.IoError
    use ProcessHandle.ProcessHandle

    import java.lang.IllegalArgumentException
    import java.lang.IllegalThreadStateException
    import java.lang.InterruptedException
    import java.lang.UnsupportedOperationException
    import java.io.IOException
    import java.lang.ProcessBuilder
    import java.lang.Runtime
    import java.io.{File => JFile}
    import java.io.InputStream
    import java.io.OutputStream
    import java.util.concurrent.{TimeUnit => JTimeUnit}
    use Process.StdIn.StdIn
    use Process.StdErr.StdErr
    use Process.StdOut.StdOut

    pub enum StdIn(OutputStream)
    instance Writable[StdIn] {
        type Elm = Int8
        type Aef = IO
        pub def write(b: Array[Int8, r], r: StdIn): Result[IoError, Int32] \ r + IO =
            let StdIn(pr) = r;
            Writable.write(b, pr)
    }

    pub enum StdOut(InputStream)
    instance Readable[StdOut] {
        type Elm = Int8
        type Aef = IO
        pub def read(b: Array[Int8, r], r: StdOut): Result[IoError, Int32] \ r + IO =
            let StdOut(pr) = r;
            Readable.read(b, pr)
    }

    pub enum StdErr(InputStream)
    instance Readable[StdErr] {
        type Elm
        type Aef = IO
        pub def read(b: Array[Int8, r], r: StdErr): Result[IoError, Int32] \ r + IO =
            let StdErr(pr) = r;
            Readable.read(b, pr)
    }

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

    ///
    /// Handles the `Process` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Process` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> Result[IoError, b] \ (ef - Process) + IO = x ->
        run {
            Ok(f(x))
        } with handler Process {
            def execWithCwdAndEnv(cmd, args, cwdPath, env, k) = region rc {
                try {
                    let arr = List.toArray(rc, cmd :: args);
                    let pb = new ProcessBuilder(arr);
                    foreach(e <- Map.toList(env)) {
                        pb.environment().put(fst(e), snd(e));
                        ()
                    };
                    match cwdPath {
                        case Some(cwd) => pb.directory(new JFile(cwd)); ()
                        case None => ()
                    };
                    let process = pb.start();
                    k(ProcessHandle(process))
                } catch {
                    case ex: IllegalArgumentException  => Err(IoError(ErrorKind.InvalidInput, ex.getMessage()))
                    case ex: IOException               => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }
            }

            def exitValue(ph, k) =
                try {
                    let ProcessHandle(jProc) = ph;
                    k(Some(jProc.exitValue()))
                } catch {
                    case _: IllegalThreadStateException => k(None)
                    case ex: IOException                => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

            def isAlive(ph, k) =
                try {
                    let ProcessHandle(jProc) = ph;
                    k(jProc.isAlive())
                } catch {
                    case ex: IOException => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

            def pid(ph, k) =
                try {
                    let ProcessHandle(jProc) = ph;
                    let pid = jProc.pid();
                    k(pid)
                } catch {
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

            def stdin(ph, k) =
                let ProcessHandle(jProc) = ph;
                k(StdIn(jProc.getOutputStream()))

            def stdout(ph, k) =
                let ProcessHandle(jProc) = ph;
                k(StdOut(jProc.getInputStream()))

            def stderr(ph, k) =
                let ProcessHandle(jProc) = ph;
                k(StdErr(jProc.getErrorStream()))

            def stop(ph, k) =
                try {
                    let ProcessHandle(jProc) = ph;
                    k(jProc.destroy())
                } catch {
                    case ex: IOException => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

            def waitFor(ph, k) =
                try {
                    let ProcessHandle(jProc) = ph;
                    let exitVal = jProc.waitFor();
                    k(exitVal)
                } catch {
                    case ex: InterruptedException => Err(IoError(ErrorKind.Interrupted, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

            def waitForTimeout(ph, time, unit, k) =
                try {
                    let timeMS = time * Int32.toInt64(TimeUnit.toMilliseconds(unit));
                    let ProcessHandle(jProc) = ph;
                    let exitVal = jProc.waitFor(timeMS, JTimeUnit.MILLISECONDS);
                    k(exitVal)
                } catch {
                    case ex: InterruptedException => Err(IoError(ErrorKind.Interrupted, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

        }

    ///
    /// Runs the `Process` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Process` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): Result[IoError, a] \ (ef - Process) + IO = handle(f)()

}

