/*
 *  Copyright 2024 Magnus Madsen
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
    /// Immediately executes the command `cmd` passing the arguments `args`.
    ///
    pub def exec(cmd: String, args: List[String]): Unit

}

mod Process {

    import java.lang.ProcessBuilder
    import java.lang.Runtime

    ///
    /// Handles the `Process` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Process` effect using the `Exec` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Process) + Exec = x ->
        try {
            f(x)
        } with Process {
            def exec(cmd, args, k) = region rc {
                let arr = List.toArray(rc, cmd :: args);
                let pb = new ProcessBuilder(arr);
                pb.start();
                k()
            }
        }

    ///
    /// Runs the `Process` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Process` effect using the `Exec` effect.
    ///
    pub def run(f: Unit -> a \ ef): a \ (ef - Process) + Exec = handle(f)()

}
