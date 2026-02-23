/*
 *  Copyright 2025 Magnus Madsen
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
    def log(s: Severity, m: RichString): Unit

}

pub mod Logger {

    use RichString.{gray, cyan, green, yellow, red, bold, text};
    use Severity.{Warn, Fatal, Trace, Debug, Info}

    ///
    /// Logs the message `m` at the `Trace` level.
    ///
    pub def trace(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Trace, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Debug` level.
    ///
    pub def debug(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Debug, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Info` level.
    ///
    pub def info(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Info, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Warn` level.
    ///
    pub def warn(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Warn, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Fatal` level.
    ///
    pub def fatal(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Fatal, Formattable.format(m))

    ///
    /// Handles the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Logger) + IO = x ->
        run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                let prefix = match s {
                    case Trace => gray("[Trace]")
                    case Debug => cyan("[Debug]")
                    case Info  => green("[Info]")
                    case Warn  => yellow("[Warn]")
                    case Fatal => bold(red("[Fatal]"))
                };
                let output = prefix + text(" ") + m;
                println(RichString.toString(output));
                k()
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Logger) + IO = handle(f)()

    ///
    /// Handles the `Logger` effect of the given function `f`.
    /// 
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    pub def handleWithConsole(f: a -> b \ ef): (a -> b \ ef - Logger + Console) = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                let prefix = match s {
                    case Severity.Debug => cyan("[Debug]")
                    case Severity.Fatal => bold(red("[Fatal]"))
                    case Severity.Info  => green("[Info]")
                    case Severity.Trace => gray("[Trace]")
                    case Severity.Warn  => yellow("[Warn]")
                };
                let output = prefix + text(" ") + m;
                Console.println(RichString.toString(output));
                k()
            }

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    pub def runWithConsole(f: Unit -> a \ ef): a \ ef - Logger + Console = handleWithConsole(f)()

    ///
    /// Aborts with the message `m` from `Logger.log` if given function `g` returns true.
    /// Otherwise, lets `Logger` effect propagate.
    ///
    pub def handleWithAbort(g: (Severity, RichString) -> Bool \ ef1, f: a -> b \ ef2): a -> b \ ef1 + ef2 + Abort + Logger = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) =
                if (g(s, m)) {
                    Abort.abort(m)
                } else {
                    Logger.log(s, m); k()
                }
            }

    ///
    /// Aborts with the message `m` from `Logger.log` if given function `g` returns true.
    /// Otherwise, lets `Logger` effect propagate.
    ///
    pub def runWithAbort(g: (Severity, RichString) -> Bool \ ef1, f: Unit -> a \ ef2): a \ ef1 + ef2 + Abort + Logger = 
        handleWithAbort(g, f)() 

}
