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
    def testAccessTime01(): Bool \ IO =
        Result.isOk(accessTime("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // creationTime                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testCreationTime01(): Bool \ IO =
        Result.isOk(creationTime("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // modificationTime                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testModificationTime01(): Bool \ IO =
        Result.isOk(modificationTime("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testSize01(): Bool \ IO =
        Result.isOk(size("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // stat                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testStat01(): Bool \ IO =
        Result.isOk(stat("README.md"))

    @test
    def testStat02(): Bool \ IO =
        match stat("README.md") {
            case Ok(v) => v.fileType == File/FileType.File
            case _ => false
        }

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testExists01(): Bool \ IO =
        exists("./README.md") == Ok(true)

    @test
    def testExists02(): Bool \ IO =
        exists("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isDirectory                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsDirectory01(): Bool \ IO =
        isDirectory("./main/test/ca/uwaterloo/flix/library") == Ok(true)

    @test
    def testIsDirectory02(): Bool \ IO =
        isDirectory("./README.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isRegularFile                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsRegularFile01(): Bool \ IO =
        isRegularFile("./main/test/ca/uwaterloo/flix/library") == Ok(false)

    @test
    def testIsRegularFile02(): Bool \ IO =
        isRegularFile("./README.md") == Ok(true)

    /////////////////////////////////////////////////////////////////////////////
    // isReadable                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsReadable01(): Bool \ IO =
        isReadable("./README.md") == Ok(true)

    @test
    def testIsReadable02(): Bool \ IO =
        isReadable("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isSymbolicLink                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsSymbolicLink(): Bool \ IO =
        isSymbolicLink("./README.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isWritable                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsWritable01(): Bool \ IO =
        isWritable("./README.md") == Ok(true)

    @test
    def testIsWritable02(): Bool \ IO =
        isWritable("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isExecutable                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testIsExecutable(): Bool \ IO =
        isExecutable("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // readLines                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadLines01(): Bool \ IO =
        Result.isOk(readLines("./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // readLinesWith                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadLinesWith01(): Bool \ IO =
        Result.isOk(readLinesWith({charSet = "UTF8"}, "./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // read                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testRead01(): Bool \ IO =
        Result.isOk(read("./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // readWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadWith01(): Bool \ IO =
        Result.isOk(readWith({offset = 2i64, count = 2, charSet = "UTF8"}, "./LICENSE.md"))

    @test
    def testReadWith02(): Bool \ IO =
        readWith({offset = 2i64, count = 2, charSet = "US-ASCII"}, "./LICENSE.md") == Ok("ac")

    /////////////////////////////////////////////////////////////////////////////
    // readLinesIter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadLinesIter01(): Bool \ IO =
        match readLinesIter("./LICENSE.md") {
            case Iterator(_, next) => Result.isOk(next())
        }

    @test
    def testReadLinesIter02(): Bool \ IO =
        match readLinesIter("./LICENSE.md") {
            case Iterator(done, _) => done() == false
        }

    /////////////////////////////////////////////////////////////////////////////
    // readLinesIterWith                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadLinesIterWithWith01(): Bool \ IO =
        match readLinesIterWith({charSet = "utf8"}, "./LICENSE.md") {
            case Iterator(_, next) => Result.isOk(next())
        }

    /////////////////////////////////////////////////////////////////////////////
    // readBytes                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadBytes01(): Bool \ IO =
        Result.isOk(readBytes("./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // readBytesWith                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadBytesWith01(): Bool \ IO =
        Result.isOk(readBytesWith({offset = 2i64, count = 2}, "./LICENSE.md"))

    @test
    def testReadBytesWith02(): Bool \ IO =
        match readBytesWith({offset = 2i64, count = 2}, "./LICENSE.md") {
            case Ok(v) => v.length == 2
            case _ => false
        }

    /////////////////////////////////////////////////////////////////////////////
    // readChunks                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def testReadChunks01(): Bool \ IO =
        match readChunks("./LICENSE.md", 2) {
            case Iterator(_, next) => Result.isOk(next())
        }

    @test
    def testReadChunks02(): Bool \ IO =
        match readChunks("./LICENSE.md", 2) {
            case Iterator(done, _) => done() == false
        }

}
