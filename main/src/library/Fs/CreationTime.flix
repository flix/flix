/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.CreationTime {

    use Fs.FileTime

    ///
    /// An effect used to read the creation time of a file.
    ///
    /// Use the `CreationTime.runWithFileTime` handler.
    ///
    pub eff CreationTime {

        ///
        /// Returns the creation time of the given file `f` in milliseconds since the epoch.
        ///
        def creationTime(f: String): Result[IoError, Int64]

    }

    ///
    /// Handles the `CreationTime` effect of the given function `f` using the `FileTime` effect.
    ///
    pub def runWithFileTime(f: Unit -> a \ ef): a \ (ef - CreationTime) + FileTime =
        run { f() } with handler CreationTime {
            def creationTime(filename, k) = k(Fs.FileTime.creationTime(filename))
        }

}
