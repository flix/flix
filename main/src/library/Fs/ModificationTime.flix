/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.ModificationTime {

    use Fs.FileTime

    ///
    /// An effect used to read the last modification time of a file.
    ///
    /// Use the `ModificationTime.runWithFileTime` handler.
    ///
    pub eff ModificationTime {

        ///
        /// Returns the last-modified timestamp of the given file `f` in milliseconds since the epoch.
        ///
        def modificationTime(f: String): Result[IoError, Int64]

    }

    ///
    /// Handles the `ModificationTime` effect of the given function `f` using the `FileTime` effect.
    ///
    pub def runWithFileTime(f: Unit -> a \ ef): a \ (ef - ModificationTime) + FileTime =
        run { f() } with handler ModificationTime {
            def modificationTime(filename, k) = k(Fs.FileTime.modificationTime(filename))
        }

}
