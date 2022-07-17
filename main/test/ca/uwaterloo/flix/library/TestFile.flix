/*
 * Copyright 2021 Nina Andrup Pedersen
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

namespace TestFile {

    use File.{exists, isDirectory, isRegularFile, isReadable, isSymbolicLink,
        isWritable, isExecutable, readLines, readLinesWith, read, readWith,
        readLinesIter, readLinesIterWith, readBytes, readBytesWith, readChunks,
        stat, accessTime, creationTime, modificationTime, size};

    /////////////////////////////////////////////////////////////////////////////
    // accessTime                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testAccessTime01(): Bool & Impure =
        Result.isOk(accessTime("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // creationTime                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCreationTime01(): Bool & Impure =
        Result.isOk(creationTime("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // modificationTime                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testModificationTime01(): Bool & Impure =
        Result.isOk(modificationTime("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSize01(): Bool & Impure =
        Result.isOk(size("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // stat                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testStat01(): Bool & Impure =
        Result.isOk(stat("README.md"))

    @test
    def testStat02(): Bool & Impure =
        match stat("README.md") {
            case Ok(v) => v.fileType == File/FileType.File
            case _ => false
        }

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testExists01(): Bool & Impure =
        exists("./README.md") == Ok(true)

    @test
    def testExists02(): Bool & Impure =
        exists("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isDirectory                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsDirectory01(): Bool & Impure =
        isDirectory("./main/test/ca/uwaterloo/flix/library") == Ok(true)

    @test
    def testIsDirectory02(): Bool & Impure =
        isDirectory("./README.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isRegularFile                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsRegularFile01(): Bool & Impure =
        isRegularFile("./main/test/ca/uwaterloo/flix/library") == Ok(false)

    @test
    def testIsRegularFile02(): Bool & Impure =
        isRegularFile("./README.md") == Ok(true)

    /////////////////////////////////////////////////////////////////////////////
    // isReadable                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsReadable01(): Bool & Impure =
        isReadable("./README.md") == Ok(true)

    @test
    def testIsReadable02(): Bool & Impure =
        isReadable("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isSymbolicLink                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsSymbolicLink(): Bool & Impure =
        isSymbolicLink("./README.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isWritable                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsWritable01(): Bool & Impure =
        isWritable("./README.md") == Ok(true)

    @test
    def testIsWritable02(): Bool & Impure =
        isWritable("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isExecutable                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsExecutable(): Bool & Impure =
        isExecutable("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // readLines                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadLines01(): Bool & Impure =
        Result.isOk(readLines("./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // readLinesWith                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadLinesWith01(): Bool & Impure =
        Result.isOk(readLinesWith({charSet = "UTF8"}, "./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // read                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testRead01(): Bool & Impure =
        Result.isOk(read("./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // readWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadWith01(): Bool & Impure =
        Result.isOk(readWith({offset = 2i64, count = 2, charSet = "UTF8"}, "./LICENSE.md"))

    @test
    def testReadWith02(): Bool & Impure =
        readWith({offset = 2i64, count = 2, charSet = "US-ASCII"}, "./LICENSE.md") == Ok("ac")

    @test
    def testReadWith03(): Bool & Impure =
        readWith({offset = 0i64, count = 0, charSet = "US-ASCII"}, "./LICENSE.md") == Ok("")

    @test
    def testReadWith04(): Bool & Impure =
        Result.isErr(readWith({offset = -10i64, count = 0, charSet = "US-ASCII"}, "./LICENSE.md"))

    @test
    def testReadWith05(): Bool & Impure =
        Result.isErr(readWith({offset = 0i64, count = -100, charSet = "US-ASCII"}, "./LICENSE.md"))


    /////////////////////////////////////////////////////////////////////////////
    // readLinesIter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadLinesIter01(): Bool & Impure =
        match readLinesIter("./LICENSE.md") {
            case Iterator(_, next) => Result.isOk(next())
        }

    @test
    def testReadLinesIter02(): Bool & Impure =
        match readLinesIter("./LICENSE.md") {
            case Iterator(done, _) => done() == false
        }

    /////////////////////////////////////////////////////////////////////////////
    // readLinesIterWith                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadLinesIterWithWith01(): Bool & Impure =
        match readLinesIterWith({charSet = "utf8"}, "./LICENSE.md") {
            case Iterator(_, next) => Result.isOk(next())
        }

    /////////////////////////////////////////////////////////////////////////////
    // readBytes                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadBytes01(): Bool & Impure =
        Result.isOk(readBytes("./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // readBytesWith                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadBytesWith01(): Bool & Impure =
        Result.isOk(readBytesWith({offset = 2i64, count = 2}, "./LICENSE.md"))

    @test
    def testReadBytesWith02(): Bool & Impure =
        match readBytesWith({offset = 2i64, count = 2}, "./LICENSE.md") {
            case Ok(v) => v.length == 2
            case _     => false
        }

    @test
    def testReadBytesWith03(): Bool & Impure =
        match readBytesWith({offset = 10i64, count = 4}, "./LICENSE.md") {
            case Ok(v) => v `Array.sameElements` [101i8, 110i8, 115i8, 101i8]
            case _     => false
        }

    @test
    def testReadBytesWith04(): Bool & Impure =
        match readBytesWith({offset = 0i64, count = 0}, "./LICENSE.md") {
            case Ok(v) => Array.isEmpty(v)
            case _     => false
        }

    @test
    def testReadBytesWith05(): Bool & Impure =
        Result.isErr(readBytesWith({offset = -10i64, count = 0}, "./LICENSE.md"))

    @test
    def testReadBytesWith06(): Bool & Impure =
        Result.isErr(readBytesWith({offset = 0i64, count = -10}, "./LICENSE.md"))


    /////////////////////////////////////////////////////////////////////////////
    // readChunks                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadChunks01(): Bool & Impure =
        match readChunks("./LICENSE.md", 2) {
            case Iterator(_, next) => Result.isOk(next())
        }

    @test
    def testReadChunks02(): Bool & Impure =
        match readChunks("./LICENSE.md", 2) {
            case Iterator(done, _) => done() == false
        }

}
