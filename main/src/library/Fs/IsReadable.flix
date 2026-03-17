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

pub mod Fs.IsReadable {

    use Fs.FilePermission

    ///
    /// Handles the `IsReadable` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `IsReadable` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - IsReadable) + IO = x ->
        run { f(x) } with handler IsReadable {
            def isReadable(filename, k) = k(Fs.FsLayer.isReadable(filename))
        }

    ///
    /// Runs the `IsReadable` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `IsReadable` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - IsReadable) + IO = handle(f)()

    ///
    /// Handles the `IsReadable` effect of the given function `f` using the `FilePermission` effect.
    ///
    pub def handleWithFilePermission(f: a -> b \ ef): a -> b \ (ef - IsReadable) + FilePermission = x ->
        run { f(x) } with handler IsReadable {
            def isReadable(filename, k) = k(Fs.FilePermission.isReadable(filename))
        }

}
