/*
 *  Copyright 2024 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Sys.Console {

    use Sys.Console

    import java.lang.System
    import java.io.ByteArrayOutputStream

    ///
    /// An effect used to interact with the console.
    ///
    /// The operations are the interface that a handler implements.
    ///
    /// Programs normally use `print`, `println`, `eprint`, and `eprintln`
    /// which accept any value with a `ToString` instance.
    ///
    pub eff Console {

        ///
        /// Reads a single line from the console.
        ///
        /// Returns `None` if the end of input has been reached.
        ///
        def readln(): Option[String]

        ///
        /// Writes the given string `s` to the standard out.
        ///
        def write(s: String): Unit

        ///
        /// Writes the given string `s` to the standard out followed by a new line.
        ///
        def writeLine(s: String): Unit

        ///
        /// Writes the given string `s` to the standard err.
        ///
        def writeErr(s: String): Unit

        ///
        /// Writes the given string `s` to the standard err followed by a new line.
        ///
        def writeErrLine(s: String): Unit

    }

    ///
    /// Prints the string representation of `x` to the standard out.
    ///
    pub def print(x: a): Unit \ Console with ToString[a] =
        Console.write(ToString.toString(x))

    ///
    /// Prints the string representation of `x` to the standard out followed by a new line.
    ///
    pub def println(x: a): Unit \ Console with ToString[a] =
        Console.writeLine(ToString.toString(x))

    ///
    /// Prints the string representation of `x` to the standard err.
    ///
    pub def eprint(x: a): Unit \ Console with ToString[a] =
        Console.writeErr(ToString.toString(x))

    ///
    /// Prints the string representation of `x` to the standard err followed by a new line.
    ///
    pub def eprintln(x: a): Unit \ Console with ToString[a] =
        Console.writeErrLine(ToString.toString(x))

    ///
    /// Prints prompt `p`, reads a line, and applies `f` to the input.
    ///
    /// If `f` returns `Err(msg)`, prints `msg` to stderr and re-prompts.
    /// If `f` returns `Ok(v)`, returns `Some(v)`.
    ///
    /// Returns `None` if the end of input has been reached.
    ///
    pub def readlnWith(p: a, f: String -> Result[String, b]): Option[b] \ Console with ToString[a] =
        Console.print(p);
        match Console.readln() {
            case None        => None
            case Some(input) => match f(input) {
                case Ok(v)    => Some(v)
                case Err(msg) => Console.eprintln(msg); readlnWith(p, f)
            }
        }

    ///
    /// Prints prompt `p`, reads a line, and parses it as a value of type `b`.
    ///
    /// Leading and trailing whitespace is ignored.
    ///
    /// If the input cannot be parsed, prints an error to stderr and re-prompts.
    ///
    /// Returns `None` if the end of input has been reached.
    ///
    pub def readlnAs(p: a): Option[b] \ Console with ToString[a], FromString[b] =
        readlnWith(p, s -> match FromString.fromString(String.trim(s)) {
            case Some(v) => Ok(v)
            case None    => Err("Invalid input. Please try again.")
        })

    ///
    /// Prints prompt `p` with a yes/no hint and reads a boolean answer.
    ///
    /// Appends `[Y/n]` if `default#default` is `true`, or `[y/N]` if `false`.
    /// Empty input returns `default#default`. Recognizes "y", "yes", "n", "no" (case-insensitive).
    /// Any other input prints an error to stderr and re-prompts.
    ///
    /// Returns `default#default` if the end of input has been reached.
    ///
    pub def confirm(p: a, default: {default = Bool}): Bool \ Console with ToString[a] =
        let hint = if (default#default) "[Y/n]" else "[y/N]";
        let parse = s -> match String.toLowerCase(String.trim(s)) {
            case ""    => Ok(default#default)
            case "y"   => Ok(true)
            case "yes" => Ok(true)
            case "n"   => Ok(false)
            case "no"  => Ok(false)
            case _     => Err("Please answer 'y' or 'n'.")
        };
        readlnWith("${p} ${hint} ", parse) |> Option.getWithDefault(default#default)

    ///
    /// Handles the `Console` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Console` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Console) + IO = x ->
        // NB: System.console() returns null when there is no interactive terminal (e.g. IDE, piped input).
        let reader = match Object.toOption(System.console()) {
            case Some(c) => () -> Object.toOption(c.readLine())
            case None    => () -> readLineFromStdIn()
        };
        run {
            f(x)
        } with handler Console {
            def readln(k)          = k(reader())
            def write(s, k)        = { System.out.print(s); System.out.flush(); k() }
            def writeLine(s, k)    = { System.out.println(s); System.out.flush(); k() }
            def writeErr(s, k)     = { System.err.print(s); System.err.flush(); k() }
            def writeErrLine(s, k) = { System.err.println(s); System.err.flush(); k() }
        }

    ///
    /// Runs the `Console` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Console` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Console) + IO = handle(f)()

    ///
    /// Reads a line from `System.in` without reading past the end of the line.
    ///
    /// Returns `None` if the end of input has been reached before any byte was read.
    ///
    /// A `BufferedReader` reads ahead and would swallow lines meant for a later
    /// reader, e.g. when the `Console` effect is handled more than once.
    /// Reading one byte at a time is cheap because `System.in` is itself buffered.
    ///
    def readLineFromStdIn(): Option[String] \ IO =
        let bytes = new ByteArrayOutputStream();
        // Returns `true` if a newline was consumed and `false` if the end of input was reached.
        def loop() = match System.in.read() {
            case -1 => false // end of input
            case 10 => true  // '\n'
            case b  => bytes.write(b); loop()
        };
        let sawNewline = loop();
        if (not sawNewline and bytes.size() == 0)
            None
        else {
            let s = String.fromBytes(Array.toVector(bytes.toByteArray()));
            Some(Option.getWithDefault(s, String.stripSuffix(substr = "\r", s)))
        }

}
