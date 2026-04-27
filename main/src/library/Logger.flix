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

pub mod Logger {

    use Sys.Console
    use RichString.{gray, cyan, green, yellow, red, bold, text};
    use Severity.{Warn, Fatal, Trace, Debug, Info}

    ///
    /// An effect used to log messages.
    ///
    pub eff Logger {

        ///
        /// Logs the given message `m` at the given severity `s`.
        ///
        def log(s: Severity, m: RichString): Unit

    }

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
    /// Prints to standard out.
    ///
    pub def handleWithStdOut(f: a -> b \ ef): (a -> b \ ef - Logger + Console) = 
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
        }
    
    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    /// Prints to standard out.
    ///
    pub def runWithStdOut(f: Unit -> a \ ef): a \ ef - Logger + Console = handleWithStdOut(f)()

    ///
    /// Handles the `Logger` effect of the given function `f`.
    /// 
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    /// Prints to standard error.
    ///
    pub def handleWithStdErr(f: a -> b \ ef): (a -> b \ ef - Logger + Console) = 
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
                Console.eprintln(RichString.toString(output));
                k()
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Logger` effect using the `Console` effect.
    ///
    /// Prints to standard error.
    ///
    pub def runWithStdErr(f: Unit -> a \ ef): a \ ef - Logger + Console = handleWithStdErr(f)()

    ///
    /// Handles the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Aborts if an error is encountered while writing to file.
    ///
    pub def handleWithFileWriteAbort(file: String, f: a -> b \ ef): a -> b \ (ef - Logger) + Fs.FileWrite + Abort = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                let output = "[${s}] ${RichString.toPlainString(m)}\n";
                let res = Fs.FileWrite.append({str = output}, file);
                match res {
                    case Err(e) => Abort.abort(RichString.text(e))
                    case Ok() => ()
                };
                k()
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Aborts if an error is encountered while writing to file.
    ///
    pub def runWithFileWriteAbort(file: String, f: Unit -> a \ ef): a \ (ef - Logger) + Fs.FileWrite + Abort =
        handleWithFileWriteAbort(file, f)()

    ///
    /// Handles the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Ignores errors encountered while writing.
    ///
    pub def handleWithFileWrite(file: String, f: a -> b \ ef): a -> b \ (ef - Logger) + Fs.FileWrite = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                let output = "[${s}] ${RichString.toPlainString(m)}\n";
                let _ = Fs.FileWrite.append({str = output}, file);
                k()
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`. Appends the logged to `file`.
    ///
    /// Discards style from the logged messages. 
    /// Ignores errors encountered while writing.
    ///
    pub def runWithFileWrite(file: String, f: Unit -> a \ ef): a \ (ef - Logger) + Fs.FileWrite = 
        handleWithFileWrite(file, f)()

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

    ///
    /// Handles the `Logger` effect of the given function `f`.
    ///
    /// In other words, returns a list of `(Severity, RichString)` that represents and is ordered by the calls to log.
    ///
    pub def handleWithList(f: a -> b \ ef): a -> (b, List[(Severity, RichString)]) \ (ef - Logger) =
        x -> run {
            (f(x), List.empty())
        } with handler Logger {
            def log(s, m, k) = {
                let (r, l) = k();
                (r, (s, m) :: l)
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// In other words, returns a list of `(Severity, RichString)` that represents and is ordered by the calls to log.
    ///
    pub def runWithList(f: Unit -> b \ ef): (b, List[(Severity, RichString)]) \ (ef - Logger) = handleWithList(f)()

    ///
    /// Handles the `Logger` effect of the given function `f`.
    ///
    /// Filters the logging, by the `filter` function.
    ///
    pub def handleWithFilter(filter: (Severity, RichString) -> Bool \ ef1, f: a -> b \ ef2): a -> b \ (ef2 - Logger) + Logger + ef1 = 
        x -> run {
            f(x)
        } with handler Logger {
            def log(s, m, k) = {
                if (filter(s, m)) {
                    Logger.log(s, m);
                    k()
                } else {
                    k()
                }
            }
        }

    ///
    /// Runs the `Logger` effect of the given function `f`.
    ///
    /// Filters the logging, by the `filter` function.
    ///
    pub def runWithFilter(filter: (Severity, RichString) -> Bool \ ef1, f: Unit -> b \ ef2): b \ (ef2 - Logger) + Logger + ef1 = handleWithFilter(filter, f)()
    
    ///
    /// Filters logs with greater or equal severity level
    ///
    pub def runWithSeverityFilter(s: Severity, f: Unit -> b \ ef): b \ (ef - Logger) + Logger =
        run {
            f()
        } with runWithFilter((sev, _) -> Order.greaterEqual(sev, s))

}
