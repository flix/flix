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

pub mod Fs.DeleteFile {

    use Fs.FileWrite

    ///
    /// Handles the `DeleteFile` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `DeleteFile` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - DeleteFile) + IO = x ->
        run { f(x) } with handler DeleteFile {
            def delete(file, k) = k(Fs.FsLayer.delete(file))
        }

    ///
    /// Runs the `DeleteFile` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `DeleteFile` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - DeleteFile) + IO = handle(f)()

    ///
    /// Handles the `DeleteFile` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def handleWithFileWrite(f: a -> b \ ef): a -> b \ (ef - DeleteFile) + FileWrite = x ->
        run { f(x) } with handler DeleteFile {
            def delete(file, k) = k(Fs.FileWrite.delete(file))
        }

}
