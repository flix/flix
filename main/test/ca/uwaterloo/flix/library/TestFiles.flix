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

mod TestFile {

    use Files.{exists, isDirectory, isRegularFile, isReadable, isSymbolicLink,
        isWritable, isExecutable, readLines, readLinesWith, read, readWith,
        readLinesIter, readLinesIterWith, readBytes, readBytesWith, readChunks,
        stat, accessTime, creationTime, modificationTime, size}

    /////////////////////////////////////////////////////////////////////////////
    // accessTime                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testAccessTime01(): Bool \ IO =
        Result.isOk(accessTime("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // creationTime                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testCreationTime01(): Bool \ IO =
        Result.isOk(creationTime("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // modificationTime                                                        //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testModificationTime01(): Bool \ IO =
        Result.isOk(modificationTime("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // size                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testSize01(): Bool \ IO =
        Result.isOk(size("README.md"))

    /////////////////////////////////////////////////////////////////////////////
    // stat                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testStat01(): Bool \ IO =
        Result.isOk(stat("README.md"))

    @Test
    def testStat02(): Bool \ IO =
        match stat("README.md") {
            case Ok(v) => v.fileType == Files.FileType.File
            case _ => false
        }

    /////////////////////////////////////////////////////////////////////////////
    // exists                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testExists01(): Bool \ IO =
        exists("./README.md") == Ok(true)

    @Test
    def testExists02(): Bool \ IO =
        exists("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isDirectory                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsDirectory01(): Bool \ IO =
        isDirectory("./main/test/ca/uwaterloo/flix/library") == Ok(true)

    @Test
    def testIsDirectory02(): Bool \ IO =
        isDirectory("./README.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isRegularFile                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsRegularFile01(): Bool \ IO =
        isRegularFile("./main/test/ca/uwaterloo/flix/library") == Ok(false)

    @Test
    def testIsRegularFile02(): Bool \ IO =
        isRegularFile("./README.md") == Ok(true)

    /////////////////////////////////////////////////////////////////////////////
    // isReadable                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsReadable01(): Bool \ IO =
        isReadable("./README.md") == Ok(true)

    @Test
    def testIsReadable02(): Bool \ IO =
        isReadable("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isSymbolicLink                                                          //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsSymbolicLink(): Bool \ IO =
        isSymbolicLink("./README.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isWritable                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsWritable01(): Bool \ IO =
        isWritable("./README.md") == Ok(true)

    @Test
    def testIsWritable02(): Bool \ IO =
        isWritable("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // isExecutable                                                            //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIsExecutable(): Bool \ IO =
        isExecutable("./fakeREADME.md") == Ok(false)

    /////////////////////////////////////////////////////////////////////////////
    // readLines                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testReadLines01(): Bool \ IO =
        Result.isOk(readLines("./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // readLinesWith                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testReadLinesWith01(): Bool \ IO =
        Result.isOk(readLinesWith({charSet = "UTF8"}, "./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // read                                                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testRead01(): Bool \ IO =
        Result.isOk(read("./LICENSE.md"))

    /////////////////////////////////////////////////////////////////////////////
    // readWith                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testReadWith01(): Bool \ IO =
        Result.isOk(readWith({offset = 2i64, count = 2, charSet = "UTF8"}, "./LICENSE.md"))

    @Test
    def testReadWith02(): Bool \ IO =
        readWith({offset = 2i64, count = 2, charSet = "US-ASCII"}, "./LICENSE.md") == Ok("ac")

    @Test
    def testReadWith03(): Bool \ IO =
        readWith({offset = 0i64, count = 0, charSet = "US-ASCII"}, "./LICENSE.md") == Ok("")

    @Test
    def testReadWith04(): Bool \ IO =
        readWith({offset = -10i64, count = 0, charSet = "US-ASCII"}, "./LICENSE.md") == Ok("")

    @Test
    def testReadWith05(): Bool \ IO =
        Result.isErr(readWith({offset = 0i64, count = -100, charSet = "US-ASCII"}, "./LICENSE.md"))

    @Test
    def testReadWith06(): Bool \ IO =
        Result.isErr(readWith({offset = -10i64, count = 10, charSet = "US-ASCII"}, "./LICENSE.md"))


    /////////////////////////////////////////////////////////////////////////////
    // readLinesIter                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testReadLinesIter01(): Bool \ IO = region rc {
        let iter = readLinesIter(rc, "./LICENSE.md");
        match Iterator.next(iter) {
            case Some(Ok(_)) => true
            case _           => false
        }
    }


    @Test
    def testReadLinesIter02(): Bool \ IO = region rc {
        let iter = readLinesIter(rc, "./LICENSE.md");
        discard Iterator.next(iter);
        match Iterator.next(iter) {
            case Some(Ok(_)) => true
            case _           => false
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // readLinesIterWith                                                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testReadLinesIterWithWith01(): Bool \ IO = region rc {
        let iter = readLinesIterWith(rc, {charSet = "utf8"}, "./LICENSE.md");
        match Iterator.next(iter) {
            case Some(Ok(_)) => true
            case _           => false
        }
    }

    /////////////////////////////////////////////////////////////////////////////
    // readBytes                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testReadBytes01(): Bool \ IO = region rc {
        Result.isOk(readBytes(rc, "./LICENSE.md"))
    }

    /////////////////////////////////////////////////////////////////////////////
    // readBytesWith                                                           //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testReadBytesWith01(): Bool \ IO = region rc {
        Result.isOk(readBytesWith(rc, {offset = 2i64, count = 2}, "./LICENSE.md"))
    }

    @Test
    def testReadBytesWith02(): Bool \ IO = region rc {
        match readBytesWith(rc, {offset = 2i64, count = 2}, "./LICENSE.md") {
            case Ok(v) => Array.length(v) == 2
            case _     => false
        }
    }

    @Test
    def testReadBytesWith03(): Bool \ IO = region rc {
        match readBytesWith(rc, {offset = 10i64, count = 4}, "./LICENSE.md") {
            case Ok(v) => v `Array.sameElements` Array#{101i8, 110i8, 115i8, 101i8} @ rc
            case _     => false
        }
    }

    @Test
    def testReadBytesWith04(): Bool \ IO = region rc {
        match readBytesWith(rc, {offset = 0i64, count = 0}, "./LICENSE.md") {
            case Ok(v) => v `Array.sameElements` Array#{} @ rc
            case _     => false
        }
    }

    @Test
    def testReadBytesWith05(): Bool \ IO = region rc {
        match readBytesWith(rc, {offset = -10i64, count = 0}, "./LICENSE.md") {
            case Ok(v) => v `Array.sameElements` Array#{} @ rc
            case _     => false
        }
    }

    @Test
    def testReadBytesWith06(): Bool \ IO = region rc {
        Result.isErr(readBytesWith(rc, {offset = 0i64, count = -10}, "./LICENSE.md"))
    }

    @Test
    def testReadBytesWith07(): Bool \ IO = region rc {
        Result.isErr(readBytesWith(rc, {offset = -10i64, count = 10}, "./LICENSE.md"))
    }

    /////////////////////////////////////////////////////////////////////////////
    // readChunks                                                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testReadChunks01(): Bool \ IO = region rc {
        let iter = readChunks(rc, 2, "./LICENSE.md");
        match Iterator.next(iter) {
            case Some(Ok(_)) => true
            case _           => false
        }
    }

    @Test
    def testReadChunks02(): Bool \ IO = region rc {
        let iter = readChunks(rc, 2, "./LICENSE.md");
        discard  Iterator.next(iter);
        match Iterator.next(iter) {
            case Some(Ok(_)) => true
            case _           => false
        }
    }

    @Test
    def testReadChunks03(): Bool \ IO = region rc {
        let iter = readChunks(rc, 0, "./LICENSE.md");
        match Iterator.next(iter) {
            case Some(Err(_)) => true
            case _            => false
        }
    }

    @Test
    def testReadChunks04(): Bool \ IO = region rc {
        let iter = readChunks(rc, -10, "./LICENSE.md");
        match Iterator.next(iter) {
            case Some(Err(_)) => true
            case _            => false
        }
    }

}
