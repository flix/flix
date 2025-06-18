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
use Bool.∧

    /////////////////////////////////////////////////////////////////////////////
    // read                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @test
    def read01(): Bool = readEmptyStream(0)

    @test
    def read02(): Bool = readEmptyStream(8)

    def readEmptyStream(bufferCapacity: Int32): Bool = region rc {
        let array: Array[Int8, rc] = Array#{} @ rc;
        let reader = TestReader.fromArray(rc, array);
        let bufReader = BufReader.withCapacity(rc, bufferCapacity, reader);
        let dest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bytesRead = Reader.read(dest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(0)) ∧ (Array.sameElements(dest, expectedDest))
    }

    @test
    def read03(): Bool = readDestLengthEqStreamLength(0)

    @test
    def read04(): Bool = readDestLengthEqStreamLength(1)

    @test
    def read05(): Bool = readDestLengthEqStreamLength(2)

    @test
    def read06(): Bool = readDestLengthEqStreamLength(3)

    @test
    def read07(): Bool = readDestLengthEqStreamLength(4)

    @test
    def read08(): Bool = readDestLengthEqStreamLength(5)

    @test
    def read09(): Bool = readDestLengthEqStreamLength(6)

    @test
    def read10(): Bool = readDestLengthEqStreamLength(7)

    @test
    def read11(): Bool = readDestLengthEqStreamLength(8)

    @test
    def read12(): Bool = readDestLengthEqStreamLength(9)

    @test
    def read13(): Bool = readDestLengthEqStreamLength(10)

    @test
    def read14(): Bool = readDestLengthEqStreamLength(16)

    def readDestLengthEqStreamLength(bufferCapacity: Int32): Bool = region rc {
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let reader = TestReader.fromArray(rc, array);
        let bufReader = BufReader.withCapacity(rc, bufferCapacity, reader);
        let dest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Reader.read(dest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        (bytesRead == Ok(8)) ∧ (Array.sameElements(dest, expectedDest))
    }

    @test
    def read15(): Bool = readDestLengthGtStreamLength(0)

    @test
    def read16(): Bool = readDestLengthGtStreamLength(1)

    @test
    def read17(): Bool = readDestLengthGtStreamLength(2)

    @test
    def read18(): Bool = readDestLengthGtStreamLength(3)

    @test
    def read19(): Bool = readDestLengthGtStreamLength(4)

    @test
    def read20(): Bool = readDestLengthGtStreamLength(5)

    @test
    def read21(): Bool = readDestLengthGtStreamLength(6)

    @test
    def read22(): Bool = readDestLengthGtStreamLength(7)

    @test
    def read23(): Bool = readDestLengthGtStreamLength(8)

    @test
    def read24(): Bool = readDestLengthGtStreamLength(9)

    @test
    def read25(): Bool = readDestLengthGtStreamLength(10)

    @test
    def read26(): Bool = readDestLengthGtStreamLength(16)

    def readDestLengthGtStreamLength(bufferCapacity: Int32): Bool = region rc {
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let reader = TestReader.fromArray(rc, array);
        let bufReader = BufReader.withCapacity(rc, bufferCapacity, reader);
        let dest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Reader.read(dest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        (bytesRead == Ok(8)) ∧ (Array.sameElements(dest, expectedDest))
    }

    @test
    def read27(): Bool = readDestLengthLtStreamLength(0)

    @test
    def read28(): Bool = readDestLengthLtStreamLength(1)

    @test
    def read29(): Bool = readDestLengthLtStreamLength(2)

    @test
    def read30(): Bool = readDestLengthLtStreamLength(3)

    @test
    def read31(): Bool = readDestLengthLtStreamLength(4)

    @test
    def read32(): Bool = readDestLengthLtStreamLength(5)

    @test
    def read33(): Bool = readDestLengthLtStreamLength(6)

    @test
    def read34(): Bool = readDestLengthLtStreamLength(7)

    @test
    def read35(): Bool = readDestLengthLtStreamLength(8)

    @test
    def read36(): Bool = readDestLengthLtStreamLength(9)

    @test
    def read37(): Bool = readDestLengthLtStreamLength(10)

    @test
    def read38(): Bool = readDestLengthLtStreamLength(11)

    @test
    def read39(): Bool = readDestLengthLtStreamLength(16)

    def readDestLengthLtStreamLength(bufferCapacity: Int32): Bool = region rc {
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let reader = TestReader.fromArray(rc, array);
        let bufReader = BufReader.withCapacity(rc, bufferCapacity, reader);
        let dest = Array.repeat(rc, 3, 0i8);

        let bytesRead1 = Reader.read(dest, bufReader);
        let expected1 = Array#{0i8, 1i8, 2i8} @ rc;
        let _ = Assert.eq(Ok(3), bytesRead1);
        let _ = Assert.eq(true, Array.sameElements(dest, expected1));

        let bytesRead2 = Reader.read(dest, bufReader);
        let expected2 = Array#{3i8, 4i8, 5i8} @ rc;
        let _ = Assert.eq(Ok(3), bytesRead2);
        let _ = Assert.eq(true, Array.sameElements(dest, expected2));

        let bytesRead3 = Reader.read(dest, bufReader);
        let expected3 = Array#{6i8, 7i8, 5i8} @ rc;
        let _ = Assert.eq(Ok(2), bytesRead3);
        let _ = Assert.eq(true, Array.sameElements(dest, expected3));

        let bytesRead4 = Reader.read(dest, bufReader);
        let expected4 = Array#{6i8, 7i8, 5i8} @ rc;
        let _ = Assert.eq(Ok(0), bytesRead4);
        let _ = Assert.eq(true, Array.sameElements(dest, expected4));

        let bytesRead5 = Reader.read(dest, bufReader);
        let expected5 = Array#{6i8, 7i8, 5i8} @ rc;
        let _ = Assert.eq(Ok(0), bytesRead5);
        Array.sameElements(dest, expected5)
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
