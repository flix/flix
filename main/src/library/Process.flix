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
eff Process {

    ///
    /// Immediately executes the command `cmd` with the arguments `args`, by the path `cwd` and with the environmental `env`
    ///
    def execWithCwdAndEnv(cmd: String, args: List[String], cwd: Option[String], env: Map[String, String]): ProcessHandle
}

mod Process {

    import java.lang.ProcessBuilder
    import java.lang.Runtime
    import java.io.{File => JFile}

    ///
    /// Executes the command `cmd` with the arguments `args`
    ///
    pub def exec(cmd: String, args: List[String]): ProcessHandle \ Process =
        Process.execWithCwdAndEnv(cmd, args, None, Map.empty())

    ///
    /// Executes the command `cmd` with the arguments `args`, by the path `cwd`
    ///
    pub def execWithCwd(cmd: String, args: List[String], cwd: Option[String]): ProcessHandle \ Process =
        Process.execWithCwdAndEnv(cmd, args, cwd, Map.empty())

    ///
    /// Executes the command `cmd` with the arguments `args` and with the environmental `env`
    ///
    pub def execWithEnv(cmd: String, args: List[String], env: Map[String, String]): ProcessHandle \ Process =
        Process.execWithCwdAndEnv(cmd, args, None, env)

    ///
    /// Handles the `Process` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Process` effect using the `Exec` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Process) + {Exec, IO, Abort} = x ->
        run {
            f(x)
        } with Process {
            def execWithCwdAndEnv(cmd, args, cwd, env, handler) = region rc {
                let arr = List.toArray(rc, cmd :: args);
                let pb = new ProcessBuilder(arr);
                match cwd {
                    case Some("") => Abort.abort("given an empty directory as an argument")
                    case Some(cwd) => pb.directory(new JFile(cwd)); ()
                    case None => ()
                };
                foreach(e <- Map.toList(env)) {
                    pb.environment().put(fst(e), snd(e));
                    ()
                };
                let process = pb.start();
                handler(ProcessHandle.ProcessHandle({stop = () -> process.destroy(), exitValue = () -> process.exitValue(),  
                    isAlive = () -> process.isAlive()})
                    )
            }
        }

    ///
    /// Runs the `Process` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Process` effect using the `Exec` effect.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Process) + {Exec, IO} = handle(f)()

}

