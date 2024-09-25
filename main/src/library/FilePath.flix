/*
 *  Copyright 2024 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

///
/// An effect used to interact with the file system.
///
pub eff FilePath {

    ///
    /// Returns `true` if the given file `f` exists.
    ///
    pub def exists(f: String): Bool

    ///
    /// Returns a vector of all lines in the given file `f`.
    ///
    pub def readLines(f: String): List[String]

}

mod FilePath {

    import java.io.{File => JFile}
    import java.nio.file.Files

    ///
    /// Runs the `FilePath` effect of the given function `f`.
    ///
    /// In other words, re-interprets the `FilePath` effect using the `IO` effect.
    ///
    pub def run(f: Unit -> a \ ef): a \ (ef - FilePath) + IO = {
        // Currently try-with typing is broken, so fix it with checked_ecast
        checked_ecast(try {
            f()
        } with FilePath {
            def exists(f, k) = unsafe {
                let jf = new JFile(f);
                k(jf.exists())
            }
            def readLines(f, k) = unsafe {
                let jf = new JFile(f);
                let jp = jf.toPath();
                let jl = Files.readAllLines(jp);
                k(Adaptor.fromList(jl))
            }
        })
    }

}
