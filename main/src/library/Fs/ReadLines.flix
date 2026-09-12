/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.ReadLines {

    use Fs.FileRead

    ///
    /// An effect used to read the lines of a file.
    ///
    /// Use the `ReadLines.runWithFileRead` handler.
    ///
    pub eff ReadLines {

        ///
        /// Returns a list of all lines in the given file `f`.
        ///
        def readLines(f: String): Result[IoError, List[String]]

    }

    ///
    /// Handles the `ReadLines` effect of the given function `f` using the `FileRead` effect.
    ///
    pub def runWithFileRead(f: Unit -> a \ ef): a \ (ef - ReadLines) + FileRead =
        run { f() } with handler ReadLines {
            def readLines(filename, k) = k(Fs.FileRead.readLines(filename))
        }

}
