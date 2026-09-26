/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.AccessTime {

    use Fs.FileTime

    ///
    /// An effect used to read the last access time of a file.
    ///
    /// Use the `AccessTime.runWithFileTime` handler.
    ///
    pub eff AccessTime {

        ///
        /// Returns the last access time of the given file `f` in milliseconds since the epoch.
        ///
        def accessTime(f: String): Result[IoError, Int64]

    }

    ///
    /// Handles the `AccessTime` effect of the given function `f` using the `FileTime` effect.
    ///
    pub def runWithFileTime(f: Unit -> a \ ef): a \ (ef - AccessTime) + FileTime =
        run { f() } with handler AccessTime {
            def accessTime(filename, k) = k(Fs.FileTime.accessTime(filename))
        }

}
