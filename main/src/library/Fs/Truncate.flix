/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.Truncate {

    use Fs.FileWrite

    ///
    /// An effect used to truncate a file.
    ///
    /// Use the `Truncate.runWithFileWrite` handler.
    ///
    pub eff Truncate {

        ///
        /// Truncates the given file `f`.
        ///
        def truncate(f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `Truncate` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - Truncate) + FileWrite =
        run { f() } with handler Truncate {
            def truncate(file, k) = k(Fs.FileWrite.truncate(file))
        }

}
