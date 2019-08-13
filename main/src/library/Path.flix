/*
 * Copyright 2019 Magnus Madsen
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
namespace Path {

    ///
    /// Returns a path from the given string `s`.
    ///
    pub def new(s: Str): Path =
        let f = native new java.io.File(s : Str);
        Path(f)

    ///
    /// Returns `true` if the given path `p` exists.
    ///
    pub def exists(p: Path): Bool = {
        let Path(javaFile) = p;
        native method java.io.File.exists(javaFile) as Bool
    }

    ///
    /// Returns a list of all the lines in the given path `p`.
    ///
    pub def readLines(p: Path): List[Str] =
        let Path(javaFile) = p;
        let javaPath = native method java.io.File.toPath(javaFile);
        let reader = native method java.nio.file.Files.newBufferedReader(javaPath);
        readAll(reader)

    ///
    /// Reads all lines from the given buffered reader `r` until it is empty.
    ///
    def readAll(r: ##java.io.BufferedReader): List[Str] =
        let line = native method java.io.BufferedReader.readLine(r);
        if (isNull(line))
            Nil
        else
            line :: readAll(r)
}

///
/// Represents a path.
///
type Path = Path(##java.io.File)
