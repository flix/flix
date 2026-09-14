/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.WriteLines {

    use Fs.FileWrite

    ///
    /// An effect used to write lines to a file.
    ///
    /// Use the `WriteLines.runWithFileWrite` handler.
    ///
    pub eff WriteLines {

        ///
        /// Writes `lines` to the given file `f`.
        ///
        /// Creates `f` if it does not exist. Overwrites it if it exists.
        ///
        def writeLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `WriteLines` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - WriteLines) + FileWrite =
        run { f() } with handler WriteLines {
            def writeLines(data, file, k) = k(Fs.FileWrite.writeLines(data, file))
        }

}
