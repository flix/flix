/*
 *  Copyright 2024 Maksim Gusev
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
eff ProcessWithResult {

    ///
    /// Immediately executes the command `cmd` with the arguments `args`, by the path `cwd` and with the environmental `env`.
    ///
    def execWithCwdAndEnv(cmd: String, args: List[String], cwd: Option[String], env: Map[String, String]): Result[IoError, ProcessHandle]

    ///
    /// Returns the exit value of the process.
    ///
    def exitValue(ph: ProcessHandle): Result[IoError, Int32]

    ///
    /// Returns status of the process, if it alive or not.
    ///
    def isAlive(ph: ProcessHandle): Result[IoError, Bool]

    ///
    /// Returns the PID of the process.
    ///
    def pid(ph: ProcessHandle): Result[IoError, Int64]

    ///
    /// Stops the process.
    ///
    def stop(ph: ProcessHandle): Result[IoError, Unit]

    ///
    /// Waits for process to finish.
    ///
    def waitFor(ph: ProcessHandle): Result[IoError, Int32]

    ///
    /// Waits a specified amount of time for the process to finish
    ///
    def waitForTimeout(ph: ProcessHandle, time: Int64, tUnit: TimeUnit): Result[IoError, Bool]

}

pub mod ProcessWithResult {

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
    import java.util.concurrent.{TimeUnit => JTimeUnit}

    ///
    /// Executes the command `cmd` with the arguments `args`.
    ///
    pub def exec(cmd: String, args: List[String]): Result[IoError, ProcessHandle] \ ProcessWithResult =
        ProcessWithResult.execWithCwdAndEnv(cmd, args, None, Map.empty())

    ///
    /// Executes the command `cmd` with the arguments `args`, by the path `cwd`.
    ///
    pub def execWithCwd(cmd: String, args: List[String], cwd: Option[String]): Result[IoError, ProcessHandle] \ ProcessWithResult =
        ProcessWithResult.execWithCwdAndEnv(cmd, args, cwd, Map.empty())

    ///
    /// Executes the command `cmd` with the arguments `args` and with the environmental `env`.
    ///
    pub def execWithEnv(cmd: String, args: List[String], env: Map[String, String]): Result[IoError, ProcessHandle] \ ProcessWithResult =
        ProcessWithResult.execWithCwdAndEnv(cmd, args, None, env)

    ///
    /// Handles the `ProcessWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `ProcessWithResult` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - ProcessWithResult) + IO = x ->
        run {
            f(x)
        } with handler ProcessWithResult {
            def execWithCwdAndEnv(cmd, args, cwdPath, env, k) = region rc {
                let res = try {
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
                    Ok(ProcessHandle.ProcessHandle(process))
                } catch {
                    case ex: IllegalArgumentException  => Err(IoError(ErrorKind.InvalidInput, ex.getMessage()))
                    case ex: IOException               => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }

            def exitValue(ph, k) =
                try {
                    let ProcessHandle(jProc) = ph;
                    k(Ok(jProc.exitValue()))
                } catch {
                    case ex: IllegalThreadStateException  => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                    case ex: IOException                  => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }

            def isAlive(ph, k) =
                try {
                    let ProcessHandle(jProc) = ph;
                    k(Ok(jProc.isAlive()))
                } catch {
                    case ex: IOException => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }

            def pid(ph, k) =
                try {
                    let ProcessHandle(jProc) = ph;
                    let pid = jProc.pid();
                    k(Ok(pid))
                } catch {
                    case ex: UnsupportedOperationException => k(Err(IoError(ErrorKind.Unsupported, ex.getMessage())))
                    case ex: IOException                   => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }

            def stop(ph, k) =
                try {
                    let ProcessHandle(jProc) = ph;
                    k(Ok(jProc.destroy()))
                } catch {
                    case ex: IOException => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }

            def waitFor(ph, k) =
                try {
                    let ProcessHandle(jProc) = ph;
                    let exitVal = jProc.waitFor();
                    k(Ok(exitVal))
                } catch {
                    case ex: InterruptedException => k(Err(IoError(ErrorKind.Interrupted, ex.getMessage())))
                    case ex: IOException          => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }

            def waitForTimeout(ph, time, unit, k) =
                try {
                    let timeMS = time * Int32.toInt64(TimeUnit.toMilliseconds(unit));
                    let ProcessHandle(jProc) = ph;
                    let exitVal = jProc.waitFor(timeMS, JTimeUnit.MILLISECONDS);
                    k(Ok(exitVal))
                } catch {
                    case ex: InterruptedException => k(Err(IoError(ErrorKind.Interrupted, ex.getMessage())))
                    case ex: IOException          => k(Err(IoError(ErrorKind.Other, ex.getMessage())))
                }

        }

    ///
    /// Runs the `Process` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Process` effect using the `IO` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - ProcessWithResult) + IO = handle(f)()

}

