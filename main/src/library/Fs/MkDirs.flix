/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.MkDirs {

    use Fs.FileWrite

    ///
    /// An effect used to create a directory and all its parent directories.
    ///
    /// Use the `MkDirs.runWithFileWrite` handler.
    ///
    pub eff MkDirs {

        ///
        /// Creates the directory `d` and all its parent directories.
        ///
        def mkDirs(d: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `MkDirs` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - MkDirs) + FileWrite =
        run { f() } with handler MkDirs {
            def mkDirs(d, k) = k(Fs.FileWrite.mkDirs(d))
        }

}
