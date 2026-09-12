/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.AppendFile {

    use Fs.FileWrite

    ///
    /// An effect used to append a string to a file.
    ///
    /// Use the `AppendFile.runWithFileWrite` handler.
    ///
    pub eff AppendFile {

        ///
        /// Appends `str` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def append(data: {str = String}, f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `AppendFile` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - AppendFile) + FileWrite =
        run { f() } with handler AppendFile {
            def append(data, file, k) = k(Fs.FileWrite.append(data, file))
        }

}
