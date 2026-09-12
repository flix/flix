/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.MkTempDir {

    use Fs.FileWrite

    ///
    /// An effect used to create a temporary directory.
    ///
    /// Use the `MkTempDir.runWithFileWrite` handler.
    ///
    pub eff MkTempDir {

        ///
        /// Creates a new temporary directory with the given prefix.
        ///
        /// Returns the path to the directory.
        ///
        def mkTempDir(prefix: String): Result[IoError, String]

    }

    ///
    /// Handles the `MkTempDir` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - MkTempDir) + FileWrite =
        run { f() } with handler MkTempDir {
            def mkTempDir(prefix, k) = k(Fs.FileWrite.mkTempDir(prefix))
        }

}
