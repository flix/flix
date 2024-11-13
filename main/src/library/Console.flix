/*
 * Copyright 2024 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

///
/// An effect used to interact with the console.
///
eff Console {

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

mod Console {

    import java.lang.System

    ///
    /// Handles the `Console` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `Console` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - Console) + IO = x ->
        try {
            f(x)
        } with Console {
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
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - Console) + IO = handle(f)()

}
