/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.WriteBytes {

    use Fs.FileWrite

    ///
    /// An effect used to write bytes to a file.
    ///
    /// Use the `WriteBytes.runWithFileWrite` handler.
    ///
    pub eff WriteBytes {

        ///
        /// Writes `data` to the given file `f`.
        ///
        /// Creates file `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeBytes(data: Vector[Int8], f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `WriteBytes` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - WriteBytes) + FileWrite =
        run { f() } with handler WriteBytes {
            def writeBytes(data, file, k) = k(Fs.FileWrite.writeBytes(data, file))
        }

}
