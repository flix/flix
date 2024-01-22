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

    pub enum Severity with Eq, Order {
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

    // TODO: DOC
    pub def toListHandler(f: Unit -> a \ ef + Logger): (a, List[String]) \ ef + IO = region rc {
        let l = MutList.new(rc);
        // TODO: Here we would use a MutList to collect the formatted messages and then return them.
        // A question is whether instead of String, we should have some LogMessage data structure?
        checked_ecast(???)
    }

}
