/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.WriteFile {

    use Fs.FileWrite

    ///
    /// An effect used to write the contents of a file as a string.
    ///
    /// Use the `WriteFile.runWithFileWrite` handler.
    ///
    pub eff WriteFile {

        ///
        /// Writes `str` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def write(data: {str = String}, f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `WriteFile` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - WriteFile) + FileWrite =
        run { f() } with handler WriteFile {
            def write(data, file, k) = k(Fs.FileWrite.write(data, file))
        }

}
