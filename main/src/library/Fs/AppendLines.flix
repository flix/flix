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

pub mod Fs.AppendLines {

    use Fs.FileWrite

    ///
    /// Handles the `AppendLines` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `AppendLines` effect using the `IO` effect.
    ///
    pub def handle(f: a -> b \ ef): a -> b \ (ef - AppendLines) + IO = x ->
        run { f(x) } with handler AppendLines {
            def appendLines(data, file, k) = k(Fs.FsLayer.appendLines(data, file))
        }

    ///
    /// Runs the `AppendLines` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `AppendLines` effect using the `IO` effect.
    ///
    @DefaultHandler
    pub def runWithIO(f: Unit -> a \ ef): a \ (ef - AppendLines) + IO = handle(f)()

    ///
    /// Handles the `AppendLines` effect of the given function `f` using the `FileWrite` effect.
    ///
    pub def handleWithFileWrite(f: a -> b \ ef): a -> b \ (ef - AppendLines) + FileWrite = x ->
        run { f(x) } with handler AppendLines {
            def appendLines(data, file, k) = k(Fs.FileWrite.appendLines(data, file))
        }

}
