/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

///
/// Effect for checking whether a file is readable, and its handlers.
///
pub mod Fs.IsReadable {

    use Fs.FilePermission

    ///
    /// Effect for checking whether a file is readable.
    ///
    /// Use the `IsReadable.runWithFilePermission` handler.
    ///
    pub eff IsReadable {

        ///
        /// Returns `true` if the given file `f` is readable.
        ///
        def isReadable(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `IsReadable` effect of the given function `f` using the `FilePermission` effect.
    ///
    pub def runWithFilePermission(f: Unit -> a \ ef): a \ (ef - IsReadable) + FilePermission =
        run { f() } with handler IsReadable {
            def isReadable(filename, k) = k(Fs.FilePermission.isReadable(filename))
        }

}
