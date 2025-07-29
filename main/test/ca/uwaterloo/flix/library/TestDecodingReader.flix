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

mod TestDecodingReader {
    use TestReader.mkDecodingReader
    import java.nio.charset.StandardCharsets
    import java.nio.charset.Charset

    @test
    def read01(): Bool \ IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 11);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        charsRead == Ok(11) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read02(): Bool \ IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'h', 'e', 'l'} @ rc;
        charsRead == Ok(3) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read03(): Bool \ IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'l', 'o', ' '} @ rc;
        charsRead == Ok(3) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read04(): Bool \ IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'w', 'o', 'r'} @ rc;
        charsRead == Ok(3) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read05(): Bool \ IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'l', 'd', 'r'} @ rc;
        charsRead == Ok(2) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read06(): Bool \ IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'l', 'd', 'r'} @ rc;
        charsRead == Ok(0) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read07(): Bool \ IO = region rc {
        // é is a two byte char
        let bytes = Array#{104i8, -61i8, -87i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 11);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'h', 'é', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        charsRead == Ok(11) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read08(): Bool \ IO = region rc {
        // é is a two byte char
        let bytes = Array#{-61i8, -87i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 10);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'é', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        charsRead == Ok(10) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read09(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{104i8, -30i8, -126i8, -84i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 11);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'h', '€', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        charsRead == Ok(11) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read10(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 10);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        charsRead == Ok(10) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read11(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '0', '0'} @ rc;
        charsRead == Ok(1) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read12(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '0'} @ rc;
        charsRead == Ok(1) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read13(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        charsRead == Ok(2) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read14(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        charsRead == Ok(2) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read15(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        charsRead == Ok(2) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read16(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        charsRead == Ok(2) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read17(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        charsRead == Ok(2) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read18(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        charsRead == Ok(2) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read19(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        charsRead == Ok(2) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read20(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        charsRead == Ok(1) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read21(): Bool \ IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        charsRead == Ok(0) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read22(): Bool \ IO = region rc {
        // -61i8 is a lead byte and we provide a continuation byte
        let bytes = Array#{-61i8, -87i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 1);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'é'} @ rc;
        charsRead == Ok(1) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read23(): Bool \ IO = region rc {
        // -61i8 is a lead byte but we provide no continuation byte
        let bytes = Array#{-61i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 1);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'�'} @ rc;
        charsRead == Ok(1) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read24(): Bool \ IO = region rc {
        // -61i8 is a lead byte and we provide a continuation byte
        let bytes = Array#{-61i8, -87i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'é', '0'} @ rc;
        charsRead == Ok(1) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read25(): Bool \ IO = region rc {
        // -61i8 is a lead byte but we provide no continuation byte
        let bytes = Array#{-61i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'�', '0'} @ rc;
        charsRead == Ok(1) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read26(): Bool \ IO = region rc {
        // -61i8 is a lead byte but not a valid continuation byte
        let bytes = Array#{-61i8, -61i8, -87i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'�', 'é'} @ rc;
        charsRead == Ok(2) and Array.sameElements(actualDest, expectedDest)
    }

    @test
    def read27(): Bool \ IO = region rc {
        // -61i8 is a lead byte but 108i8 is not a valid continuation byte
        let bytes = Array#{104i8, 101i8, 108i8, -61i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 12);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'h', 'e', 'l', '�', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        charsRead == Ok(12) and Array.sameElements(actualDest, expectedDest)
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
    import java.nio.charset.CodingErrorAction
    import java.nio.charset.StandardCharsets

    pub def mkEmptyDecodingReader(rc: Region[r]): DecodingReader[TestReader[Int8, r], r] \ IO + r =
        let bytes: Array[Int8, r] = Array#{} @ rc;
        let byteReader = fromArray(rc, bytes);
        let utf8 = CharacterSet.CharacterSet(StandardCharsets.UTF_8);
        DecodingReader.wrap(rc, utf8, byteReader)

    pub def mkDecodingReader(rc: Region[r], bytes: Array[Int8, r]): DecodingReader[TestReader[Int8, r], r] \ IO + r =
        let byteReader = fromArray(rc, bytes);
        let utf8 = CharacterSet.CharacterSet(StandardCharsets.UTF_8);
        DecodingReader.wrap(rc, utf8, byteReader)

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
