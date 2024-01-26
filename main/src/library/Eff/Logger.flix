/*
 *  Copyright 2024 Magnus Madsen, Stephen Tetley
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
mod Eff {

    pub eff Logger {
        // Note: The msg is lazy. We could also use Lazy[String] ?
        pub def log(s: Eff.Logger.Severity, msg: Unit -> String): Unit
    }
}

mod Eff.Logger {

    pub enum Severity with Eq, Order, ToString {
        case Fatal
        case Warn
        case Info
        case Debug
        case Trace
    }

    pub def fatal(msg: a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Fatal, () -> "${msg}")

    pub def warn(msg: a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Warn, () -> "${msg}")

    pub def info(msg: a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Info, () -> "${msg}")

    pub def debug(msg: a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Debug, () -> "${msg}")

    pub def trace(msg: a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Trace, () -> "${msg}")

    pub def toListHandler(f: Unit -> a \ ef): (a, List[String]) \ ef = region rc {
        let l = MutList.new(rc);
        let ans = try f() with Logger {
            def log(_, msgF, k) = {
                MutList.push!(msgF(), l);
                k()
            }
        };
        let xs = MutList.toList(l);
        (ans, xs)
    }

    pub def ioHandlerWithSeverity(s: Severity, f: Unit -> a \ ef): a \ ef + IO =
        try f() with Logger {
            def log(s1, msgF, k) = {
                if (s1 <= s) {
                    let t = Time.Instant.now();
                    let msg = msgF();
                    let message = "[${t}] [${s1}] - ${msg}";
                    println(message);
                    k()
                }
                else k()
            }
        }

    pub def infoExample(): Int32 \ Logger = {
        // Using the proper API (i.e. `info` or another of the log functions
        // that wrap the `log` effect) currently fails.
        // info("A1");
        // We have to use the `log` effect directly to avoid a runtime crash.
        do Eff.Logger.log(Severity.Info, _ -> "A1");
        1000
    }

}
