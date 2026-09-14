/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.MkDir {

    use Fs.FileWrite

    ///
    /// An effect used to create a directory.
    ///
    /// Use the `MkDir.runWithFileWrite` handler.
    ///
    pub eff MkDir {

        ///
        /// Creates the directory `d`.
        ///
        def mkDir(d: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `MkDir` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - MkDir) + FileWrite =
        run { f() } with handler MkDir {
            def mkDir(d, k) = k(Fs.FileWrite.mkDir(d))
        }

}
