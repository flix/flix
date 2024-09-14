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
/// An effect used to log messages.
///
pub eff Logger {

    ///
    /// Logs the given message `m` at the given severity `s`.
    ///
    pub def log(s: Severity, m: String): Unit

}

mod Logger {

    ///
    /// Logs the message `m` at the `Trace` level.
    ///
    pub def trace(m: a): Unit \ Logger with ToString[a] = do Logger.log(Severity.Trace, "${m}")

    ///
    /// Logs the message `m` at the `Debug` level.
    ///
    pub def debug1(m: a): Unit \ Logger with ToString[a] = do Logger.log(Severity.Debug, "${m}")

    ///
    /// Logs the message `m` at the `Info` level.
    ///
    pub def info(m: a): Unit \ Logger with ToString[a] = do Logger.log(Severity.Info, "${m}")

    ///
    /// Logs the message `m` at the `Warn` level.
    ///
    pub def warn(m: a): Unit \ Logger with ToString[a] = do Logger.log(Severity.Warn, "${m}")

    ///
    /// Logs the message `m` at the `Fatal` level.
    ///
    pub def fatal(m: a): Unit \ Logger with ToString[a] = do Logger.log(Severity.Fatal, "${m}")

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `IO` effect.
    ///
    pub def run(f: Unit -> a \ ef): a \ (ef - Logger) + IO = {
        // Currently try-with typing is broken, so fix it with checked_ecast
        checked_ecast(try {
            f()
        } with Logger {
            def log(s, m, k) = unsafe {
                println("[${s}] ${m}");
                k()
            }
        })
    }

}
