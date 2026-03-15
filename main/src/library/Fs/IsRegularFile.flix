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

pub mod Fs.IsRegularFile {

    use Fs.FileTest

    ///
    /// Handles the `IsRegularFile` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `IsRegularFile` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - IsRegularFile) + IO = x ->
        run { f(x) } with handler IsRegularFile {
            def isRegularFile(filename, k) = k(Fs.FsClient.isRegularFile(filename))
        }

    ///
    /// Runs the `IsRegularFile` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `IsRegularFile` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - IsRegularFile) + IO = handle(f)()

    ///
    /// Handles the `IsRegularFile` effect of the given function `f` using the `FileTest` effect.
    ///
    pub def handleWithFileTest(f: a -> b \ ef): a -> b \ (ef - IsRegularFile) + FileTest = x ->
        run { f(x) } with handler IsRegularFile {
            def isRegularFile(filename, k) = k(Fs.FileTest.isRegularFile(filename))
        }

}
