/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.IsWritable {

    use Fs.FilePermission

    ///
    /// An effect used to check if a file is writable.
    ///
    /// Use the `IsWritable.runWithFilePermission` handler.
    ///
    pub eff IsWritable {

        ///
        /// Returns `true` if the given file `f` is writable.
        ///
        def isWritable(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `IsWritable` effect of the given function `f` using the `FilePermission` effect.
    ///
    pub def runWithFilePermission(f: Unit -> a \ ef): a \ (ef - IsWritable) + FilePermission =
        run { f() } with handler IsWritable {
            def isWritable(filename, k) = k(Fs.FilePermission.isWritable(filename))
        }

}
