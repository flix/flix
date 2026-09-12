/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.IsRegularFile {

    use Fs.FileTest

    ///
    /// An effect used to check if a path is a regular file.
    ///
    /// Use the `IsRegularFile.runWithFileTest` handler.
    ///
    pub eff IsRegularFile {

        ///
        /// Returns `true` if the given file `f` is a regular file.
        ///
        def isRegularFile(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `IsRegularFile` effect of the given function `f` using the `FileTest` effect.
    ///
    pub def runWithFileTest(f: Unit -> a \ ef): a \ (ef - IsRegularFile) + FileTest =
        run { f() } with handler IsRegularFile {
            def isRegularFile(filename, k) = k(Fs.FileTest.isRegularFile(filename))
        }

}
