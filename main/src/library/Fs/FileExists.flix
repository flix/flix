/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.FileExists {

    use Fs.FileTest

    ///
    /// An effect used to check if a file exists.
    ///
    /// Use the `FileExists.runWithFileTest` handler.
    ///
    pub eff FileExists {

        ///
        /// Returns `true` if the given file `f` exists.
        ///
        def exists(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `FileExists` effect of the given function `f` using the `FileTest` effect.
    ///
    pub def runWithFileTest(f: Unit -> a \ ef): a \ (ef - FileExists) + FileTest =
        run { f() } with handler FileExists {
            def exists(filename, k) = k(Fs.FileTest.exists(filename))
        }

}
