/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

pub mod Fs.CopyFile {

    use Fs.FileWrite
    use Fs.CopyFile
    use Fs.CopyOption

    ///
    /// An effect used to copy a file.
    ///
    /// Use the `CopyFile.runWithFileWrite` handler.
    ///
    pub eff CopyFile {

        ///
        /// Copies the file `src` to `dst` with the given `options`.
        ///
        def copyWith(src: {src = String}, dst: String, opts: Set[CopyOption]): Result[IoError, Unit]

    }

    ///
    /// Convenience function: copies `src` to `dst` with no options.
    ///
    pub def copy(src: {src = String}, dst: String): Result[IoError, Unit] \ CopyFile =
        Fs.CopyFile.copyWith(src, dst, Set.empty())

    ///
    /// Handles the `CopyFile` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def runWithFileWrite(f: Unit -> a \ ef): a \ (ef - CopyFile) + FileWrite =
        run { f() } with handler CopyFile {
            def copyWith(src, dst, opts, k) = k(Fs.FileWrite.copyWith(src, dst, opts))
        }

}
