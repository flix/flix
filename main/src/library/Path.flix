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
//
//    ///
//    /// Effectfully returns `true` if the given path `parent` is the direct parent of the path `child`.
//    ///
//    pub eff isParentOf[a](parent: Path, child: Path, k: Result[Bool, IOError] -> a): a

//     pub eff isEmpty[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Effectfully returns a list of all lines in the given text file denoted by path `p`.
    ///
//    pub eff readLines[a](p: Path, k: Result[List[Str], IOError] -> a): a
//
//    ///
//    /// Default handler for `readLines`.
//    ///
//    handler readLines[a](p: Path, k: Result[List[Str], IOError] -> a): a = {
//        let isRegularFileResponse = isRegularFile(p, result -> result);
//        match isRegularFileResponse with {
//            case Ok(true)  => let Path(javaPath) = p;
//                              let reader = native method java.nio.file.Files.newBufferedReader(javaPath);
//                              let flixList = readLinesHelper(Nil, reader);
//                              let _ = native method java.io.BufferedReader.close(reader);
//                              k(Ok(flixList))
//            case Ok(false) => k(Err(IOError("Argument 'p' is not a regular file")))
//            case Err(IOError(errorString)) => k(Err(IOError(errorString)))
//        }
//    }
//
//    def readLinesHelper(fList: List[Str], reader : ##java.io.BufferedReader) : List[Str] = {
//        let readLine = native method java.io.BufferedReader.readLine(reader);
//        if (native method java.util.Objects.isNull(readLine))
//            List.reverse(fList)
//        else
//            readLinesHelper(readLine :: fList, reader)
//    }
//
//    ///
//    /// Effectfully returns a list of all lines in the given text file denoted by path `p` parsed with the given charset `cs`.
//    ///
//    pub eff readLinesWithCharset[a](p: Path, cs: Charset, k: Result[List[Str], IOError] -> a): a
//
//    ///
//    /// Default handler for `readLinesWithCharset`.
//    ///
//    handler readLinesWithCharset[a](p: Path, cs: Charset, k: Result[List[Str], IOError] -> a): a = ???

}

///
/// Represents a path.
///
type Path = Path(##java.io.File)
