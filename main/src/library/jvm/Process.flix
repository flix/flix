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
/// JVM-only: default handler implementation for `Process`.
///

pub mod Process {

    use IoError.ErrorKind
    use IoError.IoError
    use ProcessHandle.ProcessHandle

    use Process.StdIn.StdIn
    use Process.StdOut.StdOut
    use Process.StdErr.StdErr

    import dev.flix.runtime.Global

    import java.io.IOException
    import java.lang.IllegalArgumentException
    import java.lang.IllegalThreadStateException
    import java.lang.InterruptedException
    import java.lang.ProcessBuilder
    import java.lang.UnsupportedOperationException
    import java.io.{File => JFile}
    import java.util.concurrent.{TimeUnit => JTimeUnit}

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
                    let id = %%NEW_ID%%(());
                    Global.putProcess(id, process);
                    k(ProcessHandle(id))
                } catch {
                    case ex: IllegalArgumentException => Err(IoError(ErrorKind.InvalidInput, ex.getMessage()))
                    case ex: IOException              => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }
            }

            def exitValue(ph, k) =
                try {
                    let ProcessHandle(id) = ph;
                    let jProc = Global.getProcess(id);
                    if (Object.isNull(jProc)) {
                        k(None)
                    } else {
                        k(Some(jProc.exitValue()))
                    }
                } catch {
                    case _: IllegalThreadStateException => k(None)
                    case ex: IOException                => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

            def isAlive(ph, k) =
                try {
                    let ProcessHandle(id) = ph;
                    let jProc = Global.getProcess(id);
                    if (Object.isNull(jProc)) {
                        k(false)
                    } else {
                        k(jProc.isAlive())
                    }
                } catch {
                    case ex: IOException => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

            def pid(ph, k) =
                try {
                    let ProcessHandle(id) = ph;
                    let jProc = Global.getProcess(id);
                    if (Object.isNull(jProc)) {
                        Err(IoError(ErrorKind.Other, "invalid process handle."))
                    } else {
                        let pid = jProc.pid();
                        k(pid)
                    }
                } catch {
                    case ex: UnsupportedOperationException => Err(IoError(ErrorKind.Unsupported, ex.getMessage()))
                    case ex: IOException                   => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

            def stdin(ph, k) =
                k(StdIn(ph))

            def stdout(ph, k) =
                k(StdOut(ph))

            def stderr(ph, k) =
                k(StdErr(ph))

            def stop(ph, k) =
                try {
                    let ProcessHandle(id) = ph;
                    let jProc = Global.getProcess(id);
                    if (Object.isNull(jProc)) {
                        k(())
                    } else {
                        k(jProc.destroy())
                    }
                } catch {
                    case ex: IOException => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

            def waitFor(ph, k) =
                try {
                    let ProcessHandle(id) = ph;
                    let jProc = Global.getProcess(id);
                    if (Object.isNull(jProc)) {
                        Err(IoError(ErrorKind.Other, "invalid process handle."))
                    } else {
                        let exitVal = jProc.waitFor();
                        k(exitVal)
                    }
                } catch {
                    case ex: InterruptedException => Err(IoError(ErrorKind.Interrupted, ex.getMessage()))
                    case ex: IOException          => Err(IoError(ErrorKind.Other, ex.getMessage()))
                }

            def waitForTimeout(ph, time, unit, k) =
                try {
                    let timeMS = time * Int32.toInt64(TimeUnit.toMilliseconds(unit));
                    let ProcessHandle(id) = ph;
                    let jProc = Global.getProcess(id);
                    if (Object.isNull(jProc)) {
                        Err(IoError(ErrorKind.Other, "invalid process handle."))
                    } else {
                        let exitVal = jProc.waitFor(timeMS, JTimeUnit.MILLISECONDS);
                        k(exitVal)
                    }
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
