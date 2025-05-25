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
}

mod ProcessWithResult {

    use IoError.ErrorKind
    use IoError.IoError

    import java.lang.IllegalArgumentException
    import java.lang.NullPointerException
    import java.lang.SecurityException
    import java.io.IOException
    import java.lang.ProcessBuilder
    import java.lang.Runtime
    import java.io.{File => JFile}

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
    /// In other words, re-interprets the `ProcessWithResult` effect using the `Exec` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - ProcessWithResult) + {Exec, IO} = x ->
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
                    Ok(
                        ProcessHandle.ProcessHandle(
                            {stop = () -> process.destroy(),exitValue = () -> process.exitValue(),
                                isAlive = () -> process.isAlive()}
                            )
                        )

                } catch {
                    case ex: IllegalArgumentException  => Err(IoError(ErrorKind.InvalidInput, ex.getMessage()))
                    case ex: NullPointerException      => Err(IoError(ErrorKind.InvalidInput, ex.getMessage()))
                    case ex: SecurityException         => Err(IoError(ErrorKind.PermissionDenied, ex.getMessage()))
                    case ex: IOException               => Err(IoError(ErrorKind.Other, ex.getMessage()))
                };
                k(res)
            }
        }

    ///
    /// Runs the `Process` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Process` effect using the `Exec` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - ProcessWithResult) + {Exec, IO} = handle(f)()

}
