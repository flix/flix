/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.IsDirectory {

    use Fs.FileTest

    ///
    /// An effect used to check if a path is a directory.
    ///
    /// Use the `IsDirectory.runWithFileTest` handler.
    ///
    pub eff IsDirectory {

        ///
        /// Returns `true` if the given file `f` is a directory.
        ///
        def isDirectory(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `IsDirectory` effect of the given function `f` using the `FileTest` effect.
    ///
    pub def runWithFileTest(f: Unit -> a \ ef): a \ (ef - IsDirectory) + FileTest =
        run { f() } with handler IsDirectory {
            def isDirectory(filename, k) = k(Fs.FileTest.isDirectory(filename))
        }

}
