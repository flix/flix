/*
 *  Copyright 2025 Ry Wiese
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

 mod TestBufReader {
     use TestReader.mkTestBufReader

    /////////////////////////////////////////////////////////////////////////////
    // read                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def read01(): Bool = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 0);
        let actualDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read02(): Bool = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 8);
        let dest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bytesRead = Reader.read(dest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(dest, expectedDest))
    }

    @test
    def read03(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read04(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read05(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read06(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read07(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read08(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read09(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read10(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read11(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read12(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read13(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read14(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read15(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read16(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read17(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read18(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read19(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read20(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read21(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read22(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read23(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read24(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read25(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read26(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read27(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read28(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read29(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read30(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read31(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read32(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read33(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read34(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read35(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read36(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read37(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read38(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read39(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read40(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read41(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read42(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read43(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read44(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read45(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read46(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read47(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read48(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read49(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read50(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read51(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read52(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read53(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read54(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read55(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read56(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read57(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read58(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read59(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read60(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read61(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read62(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read63(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read64(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read65(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read66(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read67(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read68(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read69(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read70(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read71(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read72(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read73(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read74(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read75(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read76(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read77(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read78(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read79(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read80(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read81(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read82(): Bool = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read83(): Bool = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read84(): Bool = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read85(): Bool = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read86(): Bool = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read87(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read88(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read89(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read90(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read91(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let _ = Reader.read(actualDest, bufReader);
        let bytesRead = Reader.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

}

struct TestReader[r] {
    stream: Array[Int8, r],
    mut cursor: Int32
}

instance Reader[TestReader[rr]] {

    type Aef = rr

    pub def read(buffer: Array[Int8, rb], reader: TestReader[rr]): Result[IoError, Int32] \ rb + rr = TestReader.read(buffer, reader)

}

mod TestReader {

    pub def mkEmptyBufReader(rc: Region[r], capacity: Int32): BufReader[TestReader[r], r] \ r =
        let stream: Array[Int8, r] = Array#{} @ rc;
        mkTestBufReader(rc, capacity, stream)

    pub def mkTestBufReader(rc: Region[r], capacity: Int32, stream: Array[Int8, r]): BufReader[TestReader[r], r] \ r =
        let testReader = fromArray(rc, stream);
        BufReader.withCapacity(rc, capacity, testReader)

    pub def fromArray(rc: Region[r], stream: Array[Int8, r]): TestReader[r] \ r =
        new TestReader @ rc {
            stream = stream,
            cursor = 0
        }

    pub def read(buffer: Array[Int8, rb], reader: TestReader[rr]): Result[IoError, Int32] \ rb + rr =
        let bufferSize = Array.size(buffer);
        let streamSize = Array.size(reader->stream) - reader->cursor;
        if (streamSize > 0) {
            let bytesToRead = Int32.min(bufferSize, streamSize);
            Array.copyInto(
                srcPos = reader->cursor,
                dstPos = 0,
                len = bytesToRead,
                src = reader->stream,
                buffer
            );
            reader->cursor = reader->cursor + bytesToRead;
            Ok(bytesToRead)
        } else {
            Ok(0)
        }

}
