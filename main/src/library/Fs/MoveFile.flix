/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.MoveFile {

    use Fs.FileWrite
    use Fs.MoveFile
    use Fs.MoveOption

    ///
    /// An effect used to move (rename) a file or directory.
    ///
    /// Use the `MoveFile.runWithFileWrite` handler.
    ///
    pub eff MoveFile {

        ///
        /// Moves (renames) the file or directory `src` to `dst` with the given `options`.
        ///
        def moveWith(src: {src = String}, dst: String, opts: Set[MoveOption]): Result[IoError, Unit]

    }

    ///
    /// Convenience function: moves `src` to `dst` with no options.
    ///
    pub def move(src: {src = String}, dst: String): Result[IoError, Unit] \ MoveFile =
        Fs.MoveFile.moveWith(src, dst, Set.empty())

    ///
    /// Handles the `MoveFile` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - MoveFile) + FileWrite =
        run { f() } with handler MoveFile {
            def moveWith(src, dst, opts, k) = k(Fs.FileWrite.moveWith(src, dst, opts))
        }

}
