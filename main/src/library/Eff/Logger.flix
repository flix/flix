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

    ///
    /// Enum of the severity levels used by this `Logger`.
    ///
    /// `Trace` is the lowest level of severity, `Fatal` is the highest level.
    pub enum Severity with Eq, Order, ToString {
        case Trace
        case Debug
        case Info
        case Warn
        case Fatal
    }

    ///
    /// A simple, pure handler for `Logger`.
    ///
    /// Run the Logger and collect the log messages in a list.
    ///
    pub def toListHandler(f: Unit -> a \ Logger ): (List[String], a) = region rc {
        let l = MutList.new(rc);
        let ans = try f() with Logger {
            def log(s1, msgF, k) = {
                let msg = msgF();
                let message = "[${s1}] - ${msg}";
                MutList.push!(message, l);
                k()
            }
        };
        let xs = MutList.toList(l);
        (xs, ans)
    }

    ///
    /// An IO handler for `Logger`.
    ///
    /// Run the Logger within the IO effect, the log messages are written
    /// to the console with a timestamp.
    ///
    /// Use severity level `s` to limit the log output - only messages with
    /// severity >= `s` are printed.
    ///
    pub def ioHandlerWithSeverity(s: Severity, f: Unit -> a \ Logger): a \ IO =
        try f() with Logger {
            def log(s1, msgF, k) = {
                if (s1 >= s) {
                    let t = Time.Instant.now();
                    let msg = msgF();
                    let message = "[${t}] [${s1}] - ${msg}";
                    println(message);
                    k()
                }
                else k()
            }
        }

    ///
    /// Report a fatal situation.
    ///
    /// `Fatal` is the highest severity level for a log message (level 5 of 5).
    ///
    pub def fatal(msg: a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Fatal, () -> "${msg}")

    ///
    /// Report a warning message.
    ///
    /// `Warn` is a high severity level for a log message (level 4 of 5).
    ///
    pub def warn(msg: a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Warn, () -> "${msg}")

    ///
    /// Report an info message.
    ///
    /// `Info` is a medium severity level for a log message (level 3 of 5).
    ///
    pub def info(msg: a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Info, () -> "${msg}")

    ///
    /// Report a debug message.
    ///
    /// `Debug` is a low severity level for a log message (level 2 of 5).
    ///
    pub def debug(msg: a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Debug, () -> "${msg}")

    ///
    /// Report a message.
    ///
    /// `Trace` is the lowest severity level for a log message (level 1 of 5).
    ///
    pub def trace(msg: a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Trace, () -> "${msg}")

    ///
    /// Lazy version of `fatal` - the log message is a thunk that gets evaluated when the handler demands it.
    ///
    /// `Fatal` is the highest severity level for a log message (level 5 of 5).
    ///
    pub def lazyFatal(f: Unit -> a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Fatal, () -> "${f()}")

    ///
    /// Lazy version of `warn` - the log message is a thunk that gets
    /// evaluated when the handler demands it.
    ///
    /// `Warn` is a high severity level for a log message (level 4 of 5).
    ///
    pub def lazyWarn(f: Unit -> a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Warn, () -> "${f()}")

    ///
    /// Lazy version of `info` - the log message is a thunk that gets
    /// evaluated when the handler demands it.
    ///
    /// `Info` is a medium severity level for a log message (level 3 of 5).
    ///
    pub def lazyInfo(f: Unit -> a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Info, () -> "${f()}")

    ///
    /// Lazy version of `debug` - the log message is a thunk that gets evaluated
    /// when the handler demands it.
    ///
    /// `Debug` is a low severity level for a log message (level 2 of 5).
    ///
    pub def lazyDebug(f: Unit -> a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Debug, () -> "${f()}")

    ///
    /// Lazy version of `trace` - the log message is a thunk that gets
    /// evaluated when the handler demands it.
    ///
    /// `Trace` is the lowest severity level for a log message (level 1 of 5).
    ///
    pub def lazyTrace(f: Unit -> a): Unit \ Logger with ToString[a] = do Eff.Logger.log(Severity.Trace, () -> "${f()}")




}
