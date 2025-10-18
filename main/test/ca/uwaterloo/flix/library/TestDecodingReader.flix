mod TestDecodingReader {
    use Assert.{assertEq, assertTrue}
    use TestReader.mkDecodingReader
    import java.nio.charset.StandardCharsets
    import java.nio.charset.Charset

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


    @Test
    def read01(): Unit \ Assert + IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 11);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        assertEq(expected = Ok(11), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read02(): Unit \ Assert + IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'h', 'e', 'l'} @ rc;
        assertEq(expected = Ok(3), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read03(): Unit \ Assert + IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'l', 'o', ' '} @ rc;
        assertEq(expected = Ok(3), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read04(): Unit \ Assert + IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'w', 'o', 'r'} @ rc;
        assertEq(expected = Ok(3), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read05(): Unit \ Assert + IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'l', 'd', 'r'} @ rc;
        assertEq(expected = Ok(2), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read06(): Unit \ Assert + IO = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'l', 'd', 'r'} @ rc;
        assertEq(expected = Ok(0), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read07(): Unit \ Assert + IO = region rc {
        // é is a two byte char
        let bytes = Array#{104i8, -61i8, -87i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 11);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'h', 'é', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        assertEq(expected = Ok(11), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read08(): Unit \ Assert + IO = region rc {
        // é is a two byte char
        let bytes = Array#{-61i8, -87i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 10);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'é', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        assertEq(expected = Ok(10), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read09(): Unit \ Assert + IO = region rc {
        // € is a three byte char
        let bytes = Array#{104i8, -30i8, -126i8, -84i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 11);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'h', '€', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        assertEq(expected = Ok(11), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read10(): Unit \ Assert + IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, 108i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 10);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        assertEq(expected = Ok(10), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read11(): Unit \ Assert + IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 3);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '0', '0'} @ rc;
        assertEq(expected = Ok(1), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read12(): Unit \ Assert + IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '0'} @ rc;
        assertEq(expected = Ok(1), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read13(): Unit \ Assert + IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        assertEq(expected = Ok(2), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read14(): Unit \ Assert + IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        assertEq(expected = Ok(2), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read15(): Unit \ Assert + IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        assertEq(expected = Ok(2), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read16(): Unit \ Assert + IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        assertEq(expected = Ok(2), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read17(): Unit \ Assert + IO = region rc {
        // € is a three byte char
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let _ = Readable.read(actualDest, reader);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'€', '€'} @ rc;
        assertEq(expected = Ok(2), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read18(): Unit \ Assert + IO = region rc {
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
        assertEq(expected = Ok(2), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read19(): Unit \ Assert + IO = region rc {
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
        assertEq(expected = Ok(2), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read20(): Unit \ Assert + IO = region rc {
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
        assertEq(expected = Ok(1), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read21(): Unit \ Assert + IO = region rc {
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
        assertEq(expected = Ok(0), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read22(): Unit \ Assert + IO = region rc {
        // -61i8 is a lead byte and we provide a continuation byte
        let bytes = Array#{-61i8, -87i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 1);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'é'} @ rc;
        assertEq(expected = Ok(1), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read23(): Unit \ Assert + IO = region rc {
        // -61i8 is a lead byte but we provide no continuation byte
        let bytes = Array#{-61i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 1);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'�'} @ rc;
        assertEq(expected = Ok(1), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read24(): Unit \ Assert + IO = region rc {
        // -61i8 is a lead byte and we provide a continuation byte
        let bytes = Array#{-61i8, -87i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'é', '0'} @ rc;
        assertEq(expected = Ok(1), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read25(): Unit \ Assert + IO = region rc {
        // -61i8 is a lead byte but we provide no continuation byte
        let bytes = Array#{-61i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'�', '0'} @ rc;
        assertEq(expected = Ok(1), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read26(): Unit \ Assert + IO = region rc {
        // -61i8 is a lead byte but not a valid continuation byte
        let bytes = Array#{-61i8, -61i8, -87i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 2);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'�', 'é'} @ rc;
        assertEq(expected = Ok(2), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read27(): Unit \ Assert + IO = region rc {
        // -61i8 is a lead byte but 108i8 is not a valid continuation byte
        let bytes = Array#{104i8, 101i8, 108i8, -61i8, 108i8, 111i8, 32i8, 119i8, 111i8, 114i8, 108i8, 100i8} @ rc;
        let reader = mkDecodingReader(rc, bytes);
        let actualDest = Array.empty(rc, 12);
        let charsRead = Readable.read(actualDest, reader);
        let expectedDest = Array#{'h', 'e', 'l', '�', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        assertEq(expected = Ok(12), charsRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

}

