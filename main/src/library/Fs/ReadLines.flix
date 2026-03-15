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

pub mod Fs.ReadLines {

    use Fs.FileRead

    ///
    /// Handles the `ReadLines` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `ReadLines` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - ReadLines) + IO = x ->
        run { f(x) } with handler ReadLines {
            def readLines(filename, k) = k(Fs.FsLayer.readLines(filename))
        }

    ///
    /// Runs the `ReadLines` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `ReadLines` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - ReadLines) + IO = handle(f)()

    ///
    /// Handles the `ReadLines` effect of the given function `f` using the `FileRead` effect.
    ///
    pub def handleWithFileRead(f: a -> b \ ef): a -> b \ (ef - ReadLines) + FileRead = x ->
        run { f(x) } with handler ReadLines {
            def readLines(filename, k) = k(Fs.FileRead.readLines(filename))
        }

}
