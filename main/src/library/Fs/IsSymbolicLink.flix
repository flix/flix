/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.IsSymbolicLink {

    use Fs.FileTest

    ///
    /// An effect used to check if a path is a symbolic link.
    ///
    /// Use the `IsSymbolicLink.runWithFileTest` handler.
    ///
    pub eff IsSymbolicLink {

        ///
        /// Returns `true` if the given file `f` is a symbolic link.
        ///
        def isSymbolicLink(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `IsSymbolicLink` effect of the given function `f` using the `FileTest` effect.
    ///
    pub def runWithFileTest(f: Unit -> a \ ef): a \ (ef - IsSymbolicLink) + FileTest =
        run { f() } with handler IsSymbolicLink {
            def isSymbolicLink(filename, k) = k(Fs.FileTest.isSymbolicLink(filename))
        }

}
