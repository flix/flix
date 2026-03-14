/*
 * Copyright 2026 Flix Authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

pub mod Fs.FileStat {

    use Fs.FileSystem

    ///
    /// Handles the `FileStat` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileStat` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileStat) + IO = x ->
        checked_ecast(
            Fs.FileSystem.handle(
                handleWithFileSystem(f)
            )(x)
        )

    ///
    /// Runs the `FileStat` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileStat` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileStat) + IO = handle(f)()

    ///
    /// Handles the `FileStat` effect of the given function `f` using the `FileSystem` effect.
    ///
    pub def handleWithFileSystem(f: a -> b \ ef): a -> b \ (ef - FileStat) + FileSystem = x ->
        run { f(x) } with handler FileStat {
            def exists(filename, k)           = k(Fs.FileSystem.exists(filename))
            def isDirectory(filename, k)      = k(Fs.FileSystem.isDirectory(filename))
            def isRegularFile(filename, k)    = k(Fs.FileSystem.isRegularFile(filename))
            def isSymbolicLink(filename, k)   = k(Fs.FileSystem.isSymbolicLink(filename))
            def isReadable(filename, k)       = k(Fs.FileSystem.isReadable(filename))
            def isWritable(filename, k)       = k(Fs.FileSystem.isWritable(filename))
            def isExecutable(filename, k)     = k(Fs.FileSystem.isExecutable(filename))
            def accessTime(filename, k)       = k(Fs.FileSystem.accessTime(filename))
            def creationTime(filename, k)     = k(Fs.FileSystem.creationTime(filename))
            def modificationTime(filename, k) = k(Fs.FileSystem.modificationTime(filename))
            def size(filename, k)             = k(Fs.FileSystem.size(filename))
        }

}
