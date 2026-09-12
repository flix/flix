/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.FileSize {

    use Fs.FileStat
    use Fs.Size

    ///
    /// An effect used to read the size of a file.
    ///
    /// Use the `FileSize.runWithFileStat` handler.
    ///
    pub eff FileSize {

        ///
        /// Returns the size of the given file `f`.
        ///
        def size(f: String): Result[IoError, Size]

    }

    ///
    /// Handles the `FileSize` effect of the given function `f` using the `FileStat` effect.
    ///
    pub def runWithFileStat(f: Unit -> a \ ef): a \ (ef - FileSize) + FileStat =
        run { f() } with handler FileSize {
            def size(filename, k) = k(Fs.FileStat.size(filename))
        }

}
