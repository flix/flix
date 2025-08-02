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
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read02(): Bool = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 8);
        let dest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bytesRead = Readable.read(dest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(dest, expectedDest))
    }

    @test
    def read03(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read04(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read05(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read06(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read07(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read08(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read09(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read10(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read11(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read12(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read13(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read14(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read15(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read16(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read17(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read18(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read19(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read20(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read21(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read22(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read23(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read24(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read25(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read26(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read27(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read28(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read29(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read30(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read31(): Bool = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read32(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read33(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read34(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read35(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read36(): Bool = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read37(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read38(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read39(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read40(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read41(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read42(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read43(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read44(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read45(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read46(): Bool = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read47(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read48(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read49(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read50(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read51(): Bool = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read52(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read53(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read54(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read55(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read56(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read57(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read58(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read59(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read60(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read61(): Bool = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read62(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read63(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read64(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read65(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read66(): Bool = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read67(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read68(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read69(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read70(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read71(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read72(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read73(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read74(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read75(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read76(): Bool = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read77(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read78(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read79(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read80(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read81(): Bool = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read82(): Bool = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read83(): Bool = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read84(): Bool = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read85(): Bool = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read86(): Bool = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read87(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDeset = Array#{0i8, 1i8, 2i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDeset))
    }

    @test
    def read88(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        (bytesRead == Ok(3)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read89(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(2)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read90(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    @test
    def read91(): Bool = region rc {
        let capacity: Int32 = 16;
        let array: Array[Int8, rc] = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest: Array[Int8, rc] = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        (bytesRead == Ok(0)) and (Array.sameElements(actualDest, expectedDest))
    }

    /////////////////////////////////////////////////////////////////////////////
    // peek                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def peek01(): Bool = region rc {
        let bufReader: BufReader[Int8, TestReader[Int8, rc], rc] = TestReader.mkEmptyBufReader(rc, 0);
        let peeked = Peekable.peek(bufReader);
        peeked == Ok(None)
    }

    @test
    def peek02(): Bool = region rc {
        let bufReader: BufReader[Int8, TestReader[Int8, rc], rc] = TestReader.mkEmptyBufReader(rc, 0);
        let peeked1 = Peekable.peek(bufReader);
        let peeked2 = Peekable.peek(bufReader);
        peeked1 == Ok(None) and peeked2 == Ok(None)
    }

    @test
    def peek03(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let peeked = Peekable.peek(bufReader);
        peeked == Ok(Some(0i8))
    }

    @test
    def peek04(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let peeked1 = Peekable.peek(bufReader);
        let peeked2 = Peekable.peek(bufReader);
        peeked1 == Ok(Some(0i8)) and peeked2 == Ok(Some(0i8))
    }

    /////////////////////////////////////////////////////////////////////////////
    // skip                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def skip01(): Bool = region rc {
        let bufReader: BufReader[Int8, TestReader[Int8, rc], rc] = TestReader.mkEmptyBufReader(rc, 0);
        let skipped = Peekable.skip(0, bufReader);
        let peeked = Peekable.peek(bufReader);
        peeked == Ok(None) and skipped == Ok(0)
    }

    @test
    def skip02(): Bool = region rc {
        let bufReader: BufReader[Int8, TestReader[Int8, rc], rc] = TestReader.mkEmptyBufReader(rc, 0);
        let skipped = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        peeked == Ok(None) and skipped == Ok(0)
    }

    @test
    def skip03(): Bool = region rc {
        let bufReader: BufReader[Int8, TestReader[Int8, rc], rc] = TestReader.mkEmptyBufReader(rc, 0);
        let skipped = Peekable.skip(-1, bufReader);
        let peeked = Peekable.peek(bufReader);
        peeked == Ok(None) and skipped == Ok(0)
    }

    @test
    def skip04(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(0, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(0) and peeked == Ok(Some(0i8))
    }

    @test
    def skip05(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(-1, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(0) and peeked == Ok(Some(0i8))
    }

    @test
    def skip06(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(1) and peeked == Ok(Some(1i8))
    }

    @test
    def skip07(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(2, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(2) and peeked == Ok(Some(2i8))
    }

    @test
    def skip08(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped1 = Peekable.skip(1, bufReader);
        let skipped2 = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped1 == Ok(1) and skipped2 == Ok(1) and peeked == Ok(Some(2i8))
    }

    @test
    def skip09(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(3, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(3) and peeked == Ok(Some(3i8))
    }

    @test
    def skip10(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(4, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(4) and peeked == Ok(Some(4i8))
    }

    @test
    def skip11(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(5, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(5) and peeked == Ok(Some(5i8))
    }

    @test
    def skip12(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(6, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(6) and peeked == Ok(Some(6i8))
    }

    @test
    def skip13(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(7, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(7) and peeked == Ok(Some(7i8))
    }

    @test
    def skip14(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(8, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip15(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(9, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip16(): Bool = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(100, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip17(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(1) and peeked == Ok(Some(1i8))
    }

    @test
    def skip18(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(2, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(2) and peeked == Ok(Some(2i8))
    }

    @test
    def skip19(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped1 = Peekable.skip(1, bufReader);
        let skipped2 = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped1 == Ok(1) and skipped2 == Ok(1) and peeked == Ok(Some(2i8))
    }

    @test
    def skip20(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(3, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(3) and peeked == Ok(Some(3i8))
    }

    @test
    def skip21(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(4, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(4) and peeked == Ok(Some(4i8))
    }

    @test
    def skip22(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(5, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(5) and peeked == Ok(Some(5i8))
    }

    @test
    def skip23(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(6, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(6) and peeked == Ok(Some(6i8))
    }

    @test
    def skip24(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(7, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(7) and peeked == Ok(Some(7i8))
    }

    @test
    def skip25(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(8, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip26(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(9, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip27(): Bool = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(100, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip28(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(1) and peeked == Ok(Some(1i8))
    }

    @test
    def skip29(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(2, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(2) and peeked == Ok(Some(2i8))
    }

    @test
    def skip30(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped1 = Peekable.skip(1, bufReader);
        let skipped2 = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped1 == Ok(1) and skipped2 == Ok(1) and peeked == Ok(Some(2i8))
    }

    @test
    def skip31(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(3, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(3) and peeked == Ok(Some(3i8))
    }

    @test
    def skip32(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(4, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(4) and peeked == Ok(Some(4i8))
    }

    @test
    def skip33(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(5, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(5) and peeked == Ok(Some(5i8))
    }

    @test
    def skip34(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(6, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(6) and peeked == Ok(Some(6i8))
    }

    @test
    def skip35(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(7, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(7) and peeked == Ok(Some(7i8))
    }

    @test
    def skip36(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(8, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip37(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(9, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip38(): Bool = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(100, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip39(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(1) and peeked == Ok(Some(1i8))
    }

    @test
    def skip40(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(2, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(2) and peeked == Ok(Some(2i8))
    }

    @test
    def skip41(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped1 = Peekable.skip(1, bufReader);
        let skipped2 = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped1 == Ok(1) and skipped2 == Ok(1) and peeked == Ok(Some(2i8))
    }

    @test
    def skip42(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(3, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(3) and peeked == Ok(Some(3i8))
    }

    @test
    def skip43(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(4, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(4) and peeked == Ok(Some(4i8))
    }

    @test
    def skip44(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(5, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(5) and peeked == Ok(Some(5i8))
    }

    @test
    def skip45(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(6, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(6) and peeked == Ok(Some(6i8))
    }

    @test
    def skip46(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(7, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(7) and peeked == Ok(Some(7i8))
    }

    @test
    def skip47(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(8, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip48(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(9, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    @test
    def skip49(): Bool = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(100, bufReader);
        let peeked = Peekable.peek(bufReader);
        skipped == Ok(8) and peeked == Ok(None)
    }

    /////////////////////////////////////////////////////////////////////////////
    // readWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def readWhile01(): Bool = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 0);
        let res = Peekable.readWhile(_ -> true, bufReader);
        match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        }
    }

    @test
    def readWhile02(): Bool = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 0);
        let res = Peekable.readWhile(_ -> false, bufReader);
        match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        }
    }

    @test
    def readWhile03(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(_ -> true, bufReader);
        res == Ok(String.toVector("hello world\n"))
    }

    @test
    def readWhile04(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(_ -> false, bufReader);
        match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        }
    }

    @test
    def readWhile05(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile06(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile07(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile08(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector(""))
    }

    @test
    def readWhile09(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector("world"))
    }

    @test
    def readWhile10(): Bool = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile11(): Bool = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile12(): Bool = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile13(): Bool = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector(""))
    }

    @test
    def readWhile14(): Bool = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector("world"))
    }

    @test
    def readWhile15(): Bool = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile16(): Bool = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile17(): Bool = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile18(): Bool = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector(""))
    }

    @test
    def readWhile19(): Bool = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector("world"))
    }

    @test
    def readWhile20(): Bool = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile21(): Bool = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile22(): Bool = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile23(): Bool = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector(""))
    }

    @test
    def readWhile24(): Bool = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector("world"))
    }

    @test
    def readWhile25(): Bool = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile26(): Bool = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile27(): Bool = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readWhile28(): Bool = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector(""))
    }

    @test
    def readWhile29(): Bool = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector("world"))
    }

    /////////////////////////////////////////////////////////////////////////////
    // readUntil                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def readUntil01(): Bool = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 0);
        let res = Peekable.readUntil(_ -> true, bufReader);
        match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        }
    }

    @test
    def readUntil02(): Bool = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 0);
        let res = Peekable.readUntil(_ -> false, bufReader);
        match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        }
    }

    @test
    def readUntil03(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(_ -> false, bufReader);
        res == Ok(String.toVector("hello world\n"))
    }

    @test
    def readUntil04(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(_ -> true, bufReader);
        match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        }
    }

    @test
    def readUntil05(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil06(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil07(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil08(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector(""))
    }

    @test
    def readUntil09(): Bool = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector("world"))
    }

    @test
    def readUntil10(): Bool = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil11(): Bool = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil12(): Bool = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil13(): Bool = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector(""))
    }

    @test
    def readUntil14(): Bool = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector("world"))
    }

    @test
    def readUntil15(): Bool = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil16(): Bool = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil17(): Bool = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil18(): Bool = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector(""))
    }

    @test
    def readUntil19(): Bool = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector("world"))
    }

    @test
    def readUntil20(): Bool = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil21(): Bool = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil22(): Bool = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil23(): Bool = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector(""))
    }

    @test
    def readUntil24(): Bool = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector("world"))
    }

    @test
    def readUntil25(): Bool = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil26(): Bool = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil27(): Bool = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        res == Ok(String.toVector("hello world"))
    }

    @test
    def readUntil28(): Bool = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector(""))
    }

    @test
    def readUntil29(): Bool = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        word1 == Ok(String.toVector("hello")) and word2 == Ok(String.toVector("world"))
    }

    /////////////////////////////////////////////////////////////////////////////
    // readLine                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def readLine01(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine02(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\n'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine03(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\r'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine04(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\r', '\n'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine05(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello world")
    }

    @test
    def readLine06(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello world")
    }

    @test
    def readLine07(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\r'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello world")
    }

    @test
    def readLine08(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\r', '\n'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello world")
    }

    @test
    def readLine09(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine10(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("world")
    }

    @test
    def readLine11(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine12(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("world")
    }

    @test
    def readLine13(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine14(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("world")
    }

    @test
    def readLine15(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine16(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine17(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("world")
    }

    @test
    def readLine18(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine19(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine20(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("world")
    }

    @test
    def readLine21(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine22(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine23(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("world")
    }

    @test
    def readLine24(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine25(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("world")
    }

    @test
    def readLine26(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine27(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine28(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("world")
    }

    @test
    def readLine29(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine30(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine31(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("world")
    }

    @test
    def readLine32(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine33(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine34(): Bool \ IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("world")
    }

    @test
    def readLine35(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\n', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine36(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\n', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine37(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\r', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine38(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\r', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine39(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\r', '\n', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine40(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\r', '\n', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

    @test
    def readLine41(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\n', '\r', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine42(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\n', '\r', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("")
    }

    @test
    def readLine43(): Bool \ IO = region rc {
        let capacity = 8;
        let array = Array#{'\n', '\r', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        line == Ok("hello")
    }

}

struct TestReader[t, r] {
    stream: Array[t, r],
    mut cursor: Int32
}

instance Readable[TestReader[t, rr]] {

    type Elm = t

    type Aef = rr

    pub def read(buffer: Array[t, rb], reader: TestReader[t, rr]): Result[IoError, Int32] \ rb + rr = TestReader.read(buffer, reader)

}

mod TestReader {

    pub def mkEmptyBufReader(rc: Region[r], capacity: Int32): BufReader[t, TestReader[t, r], r] \ r =
        let stream: Array[t, r] = Array#{} @ rc;
        mkTestBufReader(rc, capacity, stream)

    pub def mkTestBufReader(rc: Region[r], capacity: Int32, stream: Array[t, r]): BufReader[t, TestReader[t, r], r] \ r =
        let testReader = fromArray(rc, stream);
        BufReader.withCapacity(rc, capacity, testReader)

    pub def fromArray(rc: Region[r], stream: Array[t, r]): TestReader[t, r] \ r =
        new TestReader @ rc {
            stream = stream,
            cursor = 0
        }

    pub def read(buffer: Array[t, rb], reader: TestReader[t, rr]): Result[IoError, Int32] \ rb + rr =
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
