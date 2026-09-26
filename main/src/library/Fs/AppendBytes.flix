/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.AppendBytes {

    use Fs.FileWrite

    ///
    /// An effect used to append bytes to a file.
    ///
    /// Use the `AppendBytes.runWithFileWrite` handler.
    ///
    pub eff AppendBytes {

        ///
        /// Appends `data` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `AppendBytes` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - AppendBytes) + FileWrite =
        run { f() } with handler AppendBytes {
            def appendBytes(data, file, k) = k(Fs.FileWrite.appendBytes(data, file))
        }

}
