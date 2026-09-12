/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.ReadBytes {

    use Fs.FileRead

    ///
    /// An effect used to read the bytes of a file.
    ///
    /// Use the `ReadBytes.runWithFileRead` handler.
    ///
    pub eff ReadBytes {

        ///
        /// Returns a vector of all the bytes in the given file `f`.
        ///
        def readBytes(f: String): Result[IoError, Vector[Int8]]

    }

    ///
    /// Handles the `ReadBytes` effect of the given function `f` using the `FileRead` effect.
    ///
    pub def runWithFileRead(f: Unit -> a \ ef): a \ (ef - ReadBytes) + FileRead =
        run { f() } with handler ReadBytes {
            def readBytes(filename, k) = k(Fs.FileRead.readBytes(filename))
        }

}
