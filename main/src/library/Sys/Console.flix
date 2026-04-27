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

pub mod Sys.Console {

    use Sys.Console

    import java.lang.System

    ///
    /// An effect used to interact with the console.
    ///
    pub eff Console {

        ///
        /// Reads a single line from the console.
        ///
        def readln(): String

        ///
        /// Prints the given string `s` to the standard out.
        ///
        def print(s: String): Unit

        ///
        /// Prints the given string `s` to the standard err.
        ///
        def eprint(s: String): Unit

        ///
        /// Prints the given string `s` to the standard out followed by a new line.
        ///
        def println(s: String): Unit

        ///
        /// Prints the given string `s` to the standard err followed by a new line.
        ///
        def eprintln(s: String): Unit

    }

    ///
    /// Prints prompt `p`, reads a line, and returns `default` if the input is empty or whitespace.
    ///
    pub def readlnWithDefault(p: a, default: String): String \ Console + Formattable.Aef[a] with Formattable[a] =
        Console.print(prompt(p));
        let input = Console.readln();
        let trimmed = String.trim(input);
        if (String.isEmpty(trimmed)) default else trimmed

    ///
    /// Prints prompt `p`, reads a line, and applies `f` to the input.
    ///
    /// If `f` returns `Err(msg)`, prints `msg` to stderr and re-prompts.
    /// If `f` returns `Ok(v)`, returns `v`.
    ///
    pub def readlnWith(p: a, f: String -> Result[String, b]): b \ Console + Formattable.Aef[a] with Formattable[a] =
        Console.print(prompt(p));
        let input = Console.readln();
        match f(input) {
            case Ok(v)    => v
            case Err(msg) =>
                Console.eprintln(msg);
                readlnWith(p, f)
        }

    ///
    /// Prints prompt `p` with a yes/no hint and reads a boolean answer.
    ///
    /// Appends `[Y/n]` if `default#default` is `true`, or `[y/N]` if `false`.
    /// Empty input returns `default#default`. Recognizes "y", "yes", "n", "no" (case-insensitive).
    /// Any other input returns `default#default`.
    ///
    pub def confirm(p: a, default: {default = Bool}): Bool \ Console + Formattable.Aef[a] with Formattable[a] =
        let hint = if (default#default) " [Y/n] " else " [y/N] ";
        Console.print(prompt(p) + hint);
        let input = String.toLowerCase(String.trim(Console.readln()));
        if (String.isEmpty(input))
            default#default
        else if (input == "y" or input == "yes")
            true
        else if (input == "n" or input == "no")
            false
        else
            default#default

    ///
    /// Prints prompt `p` followed by a numbered list of `options`, then reads a selection.
    ///
    /// Returns `Some(element)` if the user enters a valid number, `None` otherwise.
    ///
    pub def pick(p: a, choices: List[b]): Option[b] \ Console + Formattable.Aef[a] + Formattable.Aef[b] with Formattable[a], Formattable[b] =
        Console.println(prompt(p));
        foreach ((i, opt) <- ForEach.withIndex(choices))
            Console.println("  ${i + 1}) ${prompt(opt)}");
        Console.print("> ");
        let input = String.trim(Console.readln());
        match Int32.fromString(input) {
            case None    => None
            case Some(n) =>
                if (1 <= n and n <= List.length(choices))
                    List.head(List.drop(n - 1, choices))
                else
                    None
        }

    ///
    /// Like `pick`, but re-prompts until the user makes a valid selection.
    ///
    pub def pickWith(p: a, choices: List[b]): b \ Console + Formattable.Aef[a] + Formattable.Aef[b] with Formattable[a], Formattable[b] =
        match pick(p, choices) {
            case Some(v) => v
            case None    => pickWith(p, choices)
        }

    ///
    /// Handles the `Console` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Console` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Console) + IO = x ->
        run {
            f(x)
        } with handler Console {
            def readln(k)      = { k(System.console().readLine()) }
            def print(s, k)    = { System.out.print(s); System.out.flush(); k() }
            def eprint(s, k)   = { System.err.print(s); System.err.flush(); k() }
            def println(s, k)  = { System.out.println(s); System.out.flush(); k() }
            def eprintln(s, k) = { System.err.println(s); System.err.flush(); k() }
        }

    ///
    /// Runs the `Console` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Console` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Console) + IO = handle(f)()

    ///
    /// Returns the string representation of the formattable value `p`.
    ///
    def prompt(p: a): String \ Formattable.Aef[a] with Formattable[a] =
        RichString.toString(Formattable.format(p))

}
