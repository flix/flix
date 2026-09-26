/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.AppendLines {

    use Fs.FileWrite

    ///
    /// An effect used to append lines to a file.
    ///
    /// Use the `AppendLines.runWithFileWrite` handler.
    ///
    pub eff AppendLines {

        ///
        /// Appends `lines` to the given file `f`.
        ///
        /// Creates the file `f` if it does not exist.
        ///
        def appendLines(data: {lines = List[String]}, f: String): Result[IoError, Unit]

    }

    ///
    /// Handles the `AppendLines` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - AppendLines) + FileWrite =
        run { f() } with handler AppendLines {
            def appendLines(data, file, k) = k(Fs.FileWrite.appendLines(data, file))
        }

}
