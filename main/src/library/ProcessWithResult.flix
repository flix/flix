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

    use IoError.IoError
    use ProcessHandle.ProcessHandle

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
            def execWithCwdAndEnv(cmd, args, cwdOpt, env, k) = {
                let res = region rc {
                    let argv = List.toArray(rc, cmd :: args);

                    let envPairs = env
                        |> Map.toList
                        |> List.flatMap(e -> fst(e) :: snd(e) :: Nil);
                    let envArr = List.toArray(rc, envPairs);

                    let (hasCwd, cwdStr) = match cwdOpt {
                        case Some(cwd) => (true, cwd)
                        case None => (false, "")
                    };

                    let (ok, id, kind, msg) = %%PROCESS_EXEC%%((argv, hasCwd, cwdStr, envArr));
                    if (ok) Ok(ProcessHandle(id)) else Err(IoError(IoError.errorKindFromCode(kind), msg))
                };
                k(res)
            }

            def exitValue(ph, k) = {
                let ProcessHandle(id) = ph;
                let (ok, v, kind, msg) = %%PROCESS_EXIT_VALUE%%(id);
                if (ok) k(Ok(v)) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

            def isAlive(ph, k) = {
                let ProcessHandle(id) = ph;
                let (ok, v, kind, msg) = %%PROCESS_IS_ALIVE%%(id);
                if (ok) k(Ok(v)) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

            def pid(ph, k) = {
                let ProcessHandle(id) = ph;
                let (ok, v, kind, msg) = %%PROCESS_PID%%(id);
                if (ok) k(Ok(v)) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

            def stop(ph, k) = {
                let ProcessHandle(id) = ph;
                let (ok, _, kind, msg) = %%PROCESS_STOP%%(id);
                if (ok) k(Ok(())) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

            def waitFor(ph, k) = {
                let ProcessHandle(id) = ph;
                let (ok, v, kind, msg) = %%PROCESS_WAIT_FOR%%(id);
                if (ok) k(Ok(v)) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

            def waitForTimeout(ph, time, unit, k) = {
                let timeMS = time * Int32.toInt64(TimeUnit.toMilliseconds(unit));
                let ProcessHandle(id) = ph;
                let (ok, v, kind, msg) = %%PROCESS_WAIT_FOR_TIMEOUT%%((id, timeMS));
                if (ok) k(Ok(v)) else k(Err(IoError(IoError.errorKindFromCode(kind), msg)))
            }

        }

    ///
    /// Runs the `ProcessWithResult` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `ProcessWithResult` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - ProcessWithResult) + IO = handle(f)()

}
