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

pub mod Abort {

    import java.lang.Thread
    import java.lang.StackTraceElement
    import java.lang.System

    ///
    /// An effect used to abort computation with an error message.
    ///
    pub eff Abort {

        ///
        /// Immediately aborts the current computation with the given error message `m`.
        ///
        /// The computation cannot be resumed.
        ///
        def abort(m: RichString): Void

    }

    ///
    /// Aborts the current computation with the given error message `m` enriched with a JVM stack trace.
    ///
    pub def abortWithTrace(m: String): Void \ Abort =
        let trace = unsafe IO {
            let elems = Thread.currentThread().getStackTrace();
            formatStackTrace(Array.toList(elems), 3)
        };
        let nl = RichString.fromString(String.lineSeparator());
        let header = RichString.white("Stack Trace:");
        Abort.abort(RichString.bold(RichString.red(RichString.fromString(m))) + nl + header + nl + trace)

    ///
    /// Handles the `Abort` effect of the given function `f` by printing the error message and exiting.
    ///
    /// In other words, re-interprets the `Abort` effect using the `IO` effect.
    ///
    /// Note: The returned function can still return normally if `abort` is never called by `f`.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Abort) + IO = x ->
        run {
            f(x)
        } with handler Abort {
            def abort(m, _k) = {
                System.out.println(RichString.toString(m));
                unchecked_cast(System.exit(1) as b)
            }
        }

    ///
    /// Handles the `Abort` effect of the given function `f` by converting aborts to Result values.
    ///
    /// Returns a function that
    ///
    /// Returns `Ok(f())` if the computation completes successfully without aborting.
    /// Returns `Err(m)`  if the computation aborts with error message `m`.
    ///
    pub def handleWithResult(f: a -> b \ ef): a -> Result[RichString, b] \ (ef - Abort) = x ->
        run {
            Ok(f(x))
        } with handler Abort {
            def abort(m, _k) = Result.Err(m)
        }


    ///
    /// Runs the `Abort` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Abort` effect using the `IO` effect.
    ///
    /// Note: This function can still return normally if `abort` is never called by `f`.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Abort) + IO = handle(f)()

    ///
    /// Runs the `Abort` effect of the given function `f`.
    ///
    /// Returns `Ok(f())` if the computation completes successfully without aborting.
    /// Returns `Err(m)`  if the computation aborts with error message `m`.
    ///
    pub def runWithResult(f: Unit -> a \ ef): Result[RichString, a] \ (ef - Abort) = handleWithResult(f)()

    ///
    /// Formats a JVM stack trace as a gray RichString.
    ///
    def formatStackTrace(trace: List[StackTraceElement], skip: Int32): RichString \ IO =
        use RichString.{fromString, joinWith, cyan, gray, bold, yellow};
        let nl = fromString(String.lineSeparator());
        let frames = List.drop(skip, trace) |> List.zipWithIndex;
        let maxIdx = List.length(frames) - 1;
        let width  = String.length("${maxIdx}");
        joinWith(match (idx, elem) -> {
            let className  = elem.getClassName();
            let methodName = elem.getMethodName();
            let fileName   = Object.toOption(elem.getFileName()) |> Option.getWithDefault("Unknown Source");
            let lineNumber = elem.getLineNumber();
            let index    = cyan("#${String.padLeft(width, ' ', "${idx}")}");
            let location = gray("(${fileName}:${lineNumber})");
            if (isUserFrame(className))
                let (classPrefix, funcName) = splitClassName(className);
                fromString("  ") + index + fromString(" ") +
                    fromString(classPrefix) +
                    bold(yellow(funcName)) +
                    fromString(".${methodName}") + location
            else
                fromString("  ") + index + fromString(" ") +
                    gray("${className}.${methodName}") + location
        }, nl, frames)

    ///
    /// Splits a user-frame class name into the prefix (up to and including `Def$`)
    /// and the Flix function name (after `Def$`).
    ///
    def splitClassName(className: String): (String, String) =
        let prefix = "Def$";
        match String.indexOfRight(substr = prefix, className) {
            case Some(idx) =>
                let splitAt = idx + String.length(prefix);
                (String.sliceLeft(end = splitAt, className), String.sliceRight(start = splitAt, className))
            case None      => ("", className)
        }

    ///
    /// Returns `true` if the given `className` represents a user-defined Flix function.
    ///
    /// Flix compiles user functions to JVM classes named `Def$functionName` or `Namespace.Def$functionName`.
    ///
    def isUserFrame(className: String): Bool =
        String.startsWith(prefix = "Def$", className) or
        String.contains(substr = ".Def$", className)

}
