/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.IsExecutable {

    use Fs.FilePermission

    ///
    /// An effect used to check if a file is executable.
    ///
    /// Use the `IsExecutable.runWithFilePermission` handler.
    ///
    pub eff IsExecutable {

        ///
        /// Returns `true` if the given file `f` is executable.
        ///
        def isExecutable(f: String): Result[IoError, Bool]

    }

    ///
    /// Handles the `IsExecutable` effect of the given function `f` using the `FilePermission` effect.
    ///
    pub def runWithFilePermission(f: Unit -> a \ ef): a \ (ef - IsExecutable) + FilePermission =
        run { f() } with handler IsExecutable {
            def isExecutable(filename, k) = k(Fs.FilePermission.isExecutable(filename))
        }

}
