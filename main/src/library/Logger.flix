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

    ///
    /// Logs the message `m` at the `Trace` level.
    ///
    pub def trace(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Severity.Trace, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Debug` level.
    ///
    pub def debug(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Severity.Debug, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Info` level.
    ///
    pub def info(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Severity.Info, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Warn` level.
    ///
    pub def warn(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Severity.Warn, Formattable.format(m))

    ///
    /// Logs the message `m` at the `Fatal` level.
    ///
    pub def fatal(m: a): Unit \ (Logger + Formattable.Aef[a]) with Formattable[a] = Logger.log(Severity.Fatal, Formattable.format(m))

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
                    case Severity.Trace => gray("[Trace]")
                    case Severity.Debug => cyan("[Debug]")
                    case Severity.Info  => green("[Info]")
                    case Severity.Warn  => yellow("[Warn]")
                    case Severity.Fatal => bold(red("[Fatal]"))
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
    /// Handles the `Logger` effect of the given function `f` with the `FileWrite` effect.
    /// 
    /// Appends the logged message to the file obtained by running `file` with the 
    /// severity of the `log`.
    ///
    pub def handleWithWriteByLevel(file: Severity -> String, f: a -> b \ ef): a -> b \ (ef - Logger) + FileWrite = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = 
                let filename = file(s);
                let output = "[${ToString.toString(s)}] ${RichString.toString(m)}\n";
                FileWrite.append({str = output}, filename);
                k()
        }
    /// 
    /// Runs the `Logger` effect of the given function `f` with the `FileWrite` effect.
    /// 
    /// Appends the logged message to the file obtained by running `file` with the 
    /// severity of the `log`.
    ///
    pub def runWithWriteByLevel(file: Severity -> String, f: Unit -> a \ ef): a \ (ef - Logger) + FileWrite = 
        handleWithWriteByLevel(file, f)()
}
