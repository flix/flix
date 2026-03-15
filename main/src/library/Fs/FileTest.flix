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

pub mod Fs.FileTest {

    use Fs.FileStat

    ///
    /// Handles the `FileTest` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileTest` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - FileTest) + IO = x ->
        checked_ecast(
            Fs.FileSystem.handle(
                y -> checked_ecast(
                    Fs.FileStat.handleWithFileSystem(
                        handleWithFileStat(f)
                    )(y)
                )
            )(x)
        )

    ///
    /// Runs the `FileTest` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FileTest` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - FileTest) + IO = handle(f)()

    ///
    /// Handles the `FileTest` effect of the given function `f` using the `FileStat` effect.
    ///
    pub def handleWithFileStat(f: a -> b \ ef): a -> b \ (ef - FileTest) + FileStat = x ->
        run { f(x) } with handler FileTest {
            def exists(filename, k)         = k(Fs.FileStat.exists(filename))
            def isDirectory(filename, k)    = k(Fs.FileStat.isDirectory(filename))
            def isRegularFile(filename, k)  = k(Fs.FileStat.isRegularFile(filename))
            def isSymbolicLink(filename, k) = k(Fs.FileStat.isSymbolicLink(filename))
        }

}
