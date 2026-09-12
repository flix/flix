/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.ReadFile {

    use Fs.FileRead

    ///
    /// An effect used to read the contents of a file as a string.
    ///
    /// Use the `ReadFile.runWithFileRead` handler.
    ///
    pub eff ReadFile {

        ///
        /// Returns a string of all the contents of the given file `f`.
        ///
        def read(f: String): Result[IoError, String]

    }

    ///
    /// Handles the `ReadFile` effect of the given function `f` using the `FileRead` effect.
    ///
    pub def runWithFileRead(f: Unit -> a \ ef): a \ (ef - ReadFile) + FileRead =
        run { f() } with handler ReadFile {
            def read(filename, k) = k(Fs.FileRead.read(filename))
        }

}
