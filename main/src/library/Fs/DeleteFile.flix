/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.DeleteFile {

    use Fs.FileWrite

    ///
    /// An effect used to delete a file.
    ///
    /// Use the `DeleteFile.runWithFileWrite` handler.
    ///
    pub eff DeleteFile {

        ///
        /// Deletes the given file `f`.
        ///
        /// Fails if the file does not exist.
        ///
        def delete(f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `DeleteFile` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - DeleteFile) + FileWrite =
        run { f() } with handler DeleteFile {
            def delete(file, k) = k(Fs.FileWrite.delete(file))
        }

}
