mod TestBufReader {

    use Assert.{assertEq, assertTrue};
    use TestReader.mkTestBufReader

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

    /////////////////////////////////////////////////////////////////////////////
    // read                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def read01(): Unit \ Assert = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 0);
        let actualDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read02(): Unit \ Assert = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 8);
        let dest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bytesRead = Readable.read(dest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(dest, expectedDest))
    }

    @Test
    def read03(): Unit \ Assert = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read04(): Unit \ Assert = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read05(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read06(): Unit \ Assert = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read07(): Unit \ Assert = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read08(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read09(): Unit \ Assert = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read10(): Unit \ Assert = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read11(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read12(): Unit \ Assert = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read13(): Unit \ Assert = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read14(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 8, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read15(): Unit \ Assert = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read16(): Unit \ Assert = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read17(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read18(): Unit \ Assert = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read19(): Unit \ Assert = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read20(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read21(): Unit \ Assert = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read22(): Unit \ Assert = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read23(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read24(): Unit \ Assert = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read25(): Unit \ Assert = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read26(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 12, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8, 0i8, 0i8, 0i8, 0i8} @ rc;
        assertEq(expected = Ok(8), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read27(): Unit \ Assert = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read28(): Unit \ Assert = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read29(): Unit \ Assert = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read30(): Unit \ Assert = region rc {
        let capacity = 0;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read31(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read32(): Unit \ Assert = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read33(): Unit \ Assert = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read34(): Unit \ Assert = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read35(): Unit \ Assert = region rc {
        let capacity = 1;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read36(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read37(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read38(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read39(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read40(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read41(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read42(): Unit \ Assert = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read43(): Unit \ Assert = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read44(): Unit \ Assert = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read45(): Unit \ Assert = region rc {
        let capacity = 3;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read46(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read47(): Unit \ Assert = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read48(): Unit \ Assert = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read49(): Unit \ Assert = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read50(): Unit \ Assert = region rc {
        let capacity = 4;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read51(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read52(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read53(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read54(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read55(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read56(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read57(): Unit \ Assert = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read58(): Unit \ Assert = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read59(): Unit \ Assert = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read60(): Unit \ Assert = region rc {
        let capacity = 6;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read61(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read62(): Unit \ Assert = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read63(): Unit \ Assert = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read64(): Unit \ Assert = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read65(): Unit \ Assert = region rc {
        let capacity = 7;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read66(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read67(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read68(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read69(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read70(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read71(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read72(): Unit \ Assert = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read73(): Unit \ Assert = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read74(): Unit \ Assert = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read75(): Unit \ Assert = region rc {
        let capacity = 9;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read76(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read77(): Unit \ Assert = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read78(): Unit \ Assert = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read79(): Unit \ Assert = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read80(): Unit \ Assert = region rc {
        let capacity = 10;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read81(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read82(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read83(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read84(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read85(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read86(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read87(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{0i8, 1i8, 2i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read88(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{3i8, 4i8, 5i8} @ rc;
        assertEq(expected = Ok(3), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read89(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(2), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read90(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let actualDest = Array.repeat(rc, 3, 0i8);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let _ = Readable.read(actualDest, bufReader);
        let bytesRead = Readable.read(actualDest, bufReader);
        let expectedDest = Array#{6i8, 7i8, 5i8} @ rc;
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    @Test
    def read91(): Unit \ Assert = region rc {
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
        assertEq(expected = Ok(0), bytesRead);
        assertTrue(Array.sameElements(actualDest, expectedDest))
    }

    /////////////////////////////////////////////////////////////////////////////
    // peek                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def peek01(): Unit \ Assert = region rc {
        let bufReader: BufReader[Int8, TestReader[Int8, rc], rc] = TestReader.mkEmptyBufReader(rc, 0);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def peek02(): Unit \ Assert = region rc {
        let bufReader: BufReader[Int8, TestReader[Int8, rc], rc] = TestReader.mkEmptyBufReader(rc, 0);
        let peeked1 = Peekable.peek(bufReader);
        let peeked2 = Peekable.peek(bufReader);
        assertEq(expected = Ok(None), peeked1);
        assertEq(expected = Ok(None), peeked2)
    }

    @Test
    def peek03(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(Some(0i8)), peeked)
    }

    @Test
    def peek04(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let peeked1 = Peekable.peek(bufReader);
        let peeked2 = Peekable.peek(bufReader);
        assertEq(expected = Ok(Some(0i8)), peeked1);
        assertEq(expected = Ok(Some(0i8)), peeked2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // skip                                                                    //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def skip01(): Unit \ Assert = region rc {
        let bufReader: BufReader[Int8, TestReader[Int8, rc], rc] = TestReader.mkEmptyBufReader(rc, 0);
        let skipped = Peekable.skip(0, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(None), peeked);
        assertEq(expected = Ok(0), skipped)
    }

    @Test
    def skip02(): Unit \ Assert = region rc {
        let bufReader: BufReader[Int8, TestReader[Int8, rc], rc] = TestReader.mkEmptyBufReader(rc, 0);
        let skipped = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(None), peeked);
        assertEq(expected = Ok(0), skipped)
    }

    @Test
    def skip03(): Unit \ Assert = region rc {
        let bufReader: BufReader[Int8, TestReader[Int8, rc], rc] = TestReader.mkEmptyBufReader(rc, 0);
        let skipped = Peekable.skip(-1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(None), peeked);
        assertEq(expected = Ok(0), skipped)
    }

    @Test
    def skip04(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(0, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(0), skipped);
        assertEq(expected = Ok(Some(0i8)), peeked)
    }

    @Test
    def skip05(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(-1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(0), skipped);
        assertEq(expected = Ok(Some(0i8)), peeked)
    }

    @Test
    def skip06(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(1), skipped);
        assertEq(expected = Ok(Some(1i8)), peeked)
    }

    @Test
    def skip07(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(2, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(2), skipped);
        assertEq(expected = Ok(Some(2i8)), peeked)
    }

    @Test
    def skip08(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped1 = Peekable.skip(1, bufReader);
        let skipped2 = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(1), skipped1);
        assertEq(expected = Ok(1), skipped2);
        assertEq(expected = Ok(Some(2i8)), peeked)
    }

    @Test
    def skip09(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(3, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(3), skipped);
        assertEq(expected = Ok(Some(3i8)), peeked)
    }

    @Test
    def skip10(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(4, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(4), skipped);
        assertEq(expected = Ok(Some(4i8)), peeked)
    }

    @Test
    def skip11(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(5, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(5), skipped);
        assertEq(expected = Ok(Some(5i8)), peeked)
    }

    @Test
    def skip12(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(6, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(6), skipped);
        assertEq(expected = Ok(Some(6i8)), peeked)
    }

    @Test
    def skip13(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(7, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(7), skipped);
        assertEq(expected = Ok(Some(7i8)), peeked)
    }

    @Test
    def skip14(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(8, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip15(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(9, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip16(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(100, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip17(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(1), skipped);
        assertEq(expected = Ok(Some(1i8)), peeked)
    }

    @Test
    def skip18(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(2, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(2), skipped);
        assertEq(expected = Ok(Some(2i8)), peeked)
    }

    @Test
    def skip19(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped1 = Peekable.skip(1, bufReader);
        let skipped2 = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(1), skipped1);
        assertEq(expected = Ok(1), skipped2);
        assertEq(expected = Ok(Some(2i8)), peeked)
    }

    @Test
    def skip20(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(3, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(3), skipped);
        assertEq(expected = Ok(Some(3i8)), peeked)
    }

    @Test
    def skip21(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(4, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(4), skipped);
        assertEq(expected = Ok(Some(4i8)), peeked)
    }

    @Test
    def skip22(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(5, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(5), skipped);
        assertEq(expected = Ok(Some(5i8)), peeked)
    }

    @Test
    def skip23(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(6, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(6), skipped);
        assertEq(expected = Ok(Some(6i8)), peeked)
    }

    @Test
    def skip24(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(7, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(7), skipped);
        assertEq(expected = Ok(Some(7i8)), peeked)
    }

    @Test
    def skip25(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(8, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip26(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(9, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip27(): Unit \ Assert = region rc {
        let capacity = 5;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(100, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip28(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(1), skipped);
        assertEq(expected = Ok(Some(1i8)), peeked)
    }

    @Test
    def skip29(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(2, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(2), skipped);
        assertEq(expected = Ok(Some(2i8)), peeked)
    }

    @Test
    def skip30(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped1 = Peekable.skip(1, bufReader);
        let skipped2 = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(1), skipped1);
        assertEq(expected = Ok(1), skipped2);
        assertEq(expected = Ok(Some(2i8)), peeked)
    }

    @Test
    def skip31(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(3, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(3), skipped);
        assertEq(expected = Ok(Some(3i8)), peeked)
    }

    @Test
    def skip32(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(4, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(4), skipped);
        assertEq(expected = Ok(Some(4i8)), peeked)
    }

    @Test
    def skip33(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(5, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(5), skipped);
        assertEq(expected = Ok(Some(5i8)), peeked)
    }

    @Test
    def skip34(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(6, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(6), skipped);
        assertEq(expected = Ok(Some(6i8)), peeked)
    }

    @Test
    def skip35(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(7, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(7), skipped);
        assertEq(expected = Ok(Some(7i8)), peeked)
    }

    @Test
    def skip36(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(8, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip37(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(9, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip38(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(100, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip39(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(1), skipped);
        assertEq(expected = Ok(Some(1i8)), peeked)
    }

    @Test
    def skip40(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(2, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(2), skipped);
        assertEq(expected = Ok(Some(2i8)), peeked)
    }

    @Test
    def skip41(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped1 = Peekable.skip(1, bufReader);
        let skipped2 = Peekable.skip(1, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(1), skipped1);
        assertEq(expected = Ok(1), skipped2);
        assertEq(expected = Ok(Some(2i8)), peeked)
    }

    @Test
    def skip42(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(3, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(3), skipped);
        assertEq(expected = Ok(Some(3i8)), peeked)
    }

    @Test
    def skip43(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(4, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(4), skipped);
        assertEq(expected = Ok(Some(4i8)), peeked)
    }

    @Test
    def skip44(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(5, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(5), skipped);
        assertEq(expected = Ok(Some(5i8)), peeked)
    }

    @Test
    def skip45(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(6, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(6), skipped);
        assertEq(expected = Ok(Some(6i8)), peeked)
    }

    @Test
    def skip46(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(7, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(7), skipped);
        assertEq(expected = Ok(Some(7i8)), peeked)
    }

    @Test
    def skip47(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(8, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip48(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(9, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    @Test
    def skip49(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{0i8, 1i8, 2i8, 3i8, 4i8, 5i8, 6i8, 7i8} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let skipped = Peekable.skip(100, bufReader);
        let peeked = Peekable.peek(bufReader);
        assertEq(expected = Ok(8), skipped);
        assertEq(expected = Ok(None), peeked)
    }

    /////////////////////////////////////////////////////////////////////////////
    // readWhile                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def readWhile01(): Unit \ Assert = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 0);
        let res = Peekable.readWhile(_ -> true, bufReader);
        assertTrue(match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        })
    }

    @Test
    def readWhile02(): Unit \ Assert = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 0);
        let res = Peekable.readWhile(_ -> false, bufReader);
        assertTrue(match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        })
    }

    @Test
    def readWhile03(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(_ -> true, bufReader);
        assertEq(expected = Ok(String.toVector("hello world\n")), res)
    }

    @Test
    def readWhile04(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(_ -> false, bufReader);
        assertTrue(match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        })
    }

    @Test
    def readWhile05(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile06(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile07(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile08(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("")), word2)
    }

    @Test
    def readWhile09(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("world")), word2)
    }

    @Test
    def readWhile10(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile11(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile12(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile13(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("")), word2)
    }

    @Test
    def readWhile14(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("world")), word2)
    }

    @Test
    def readWhile15(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile16(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile17(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile18(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("")), word2)
    }

    @Test
    def readWhile19(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("world")), word2)
    }

    @Test
    def readWhile20(): Unit \ Assert = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile21(): Unit \ Assert = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile22(): Unit \ Assert = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile23(): Unit \ Assert = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("")), word2)
    }

    @Test
    def readWhile24(): Unit \ Assert = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("world")), word2)
    }

    @Test
    def readWhile25(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile26(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile27(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readWhile(c -> c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readWhile28(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("")), word2)
    }

    @Test
    def readWhile29(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readWhile(c -> c != ' ' and c != '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("world")), word2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // readUntil                                                               //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def readUntil01(): Unit \ Assert = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 0);
        let res = Peekable.readUntil(_ -> true, bufReader);
        assertTrue(match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        })
    }

    @Test
    def readUntil02(): Unit \ Assert = region rc {
        let bufReader = TestReader.mkEmptyBufReader(rc, 0);
        let res = Peekable.readUntil(_ -> false, bufReader);
        assertTrue(match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        })
    }

    @Test
    def readUntil03(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(_ -> false, bufReader);
        assertEq(expected = Ok(String.toVector("hello world\n")), res)
    }

    @Test
    def readUntil04(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(_ -> true, bufReader);
        assertTrue(match res {
            case Ok(read) => Vector.isEmpty(read)
            case _ => false
        })
    }

    @Test
    def readUntil05(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil06(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil07(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil08(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("")), word2)
    }

    @Test
    def readUntil09(): Unit \ Assert = region rc {
        let capacity = 2;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("world")), word2)
    }

    @Test
    def readUntil10(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil11(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil12(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil13(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("")), word2)
    }

    @Test
    def readUntil14(): Unit \ Assert = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("world")), word2)
    }

    @Test
    def readUntil15(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil16(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil17(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil18(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("")), word2)
    }

    @Test
    def readUntil19(): Unit \ Assert = region rc {
        let capacity = 11;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("world")), word2)
    }

    @Test
    def readUntil20(): Unit \ Assert = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil21(): Unit \ Assert = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil22(): Unit \ Assert = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil23(): Unit \ Assert = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("")), word2)
    }

    @Test
    def readUntil24(): Unit \ Assert = region rc {
        let capacity = 12;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("world")), word2)
    }

    @Test
    def readUntil25(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil26(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil27(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n', 'j', 'u', 'n', 'k', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let res = Peekable.readUntil(c -> c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello world")), res)
    }

    @Test
    def readUntil28(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("")), word2)
    }

    @Test
    def readUntil29(): Unit \ Assert = region rc {
        let capacity = 16;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = mkTestBufReader(rc, capacity, array);
        let word1 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        let _ = Peekable.skip(1, bufReader);
        let word2 = Peekable.readUntil(c -> c == ' ' or c == '\n', bufReader);
        assertEq(expected = Ok(String.toVector("hello")), word1);
        assertEq(expected = Ok(String.toVector("world")), word2)
    }

    /////////////////////////////////////////////////////////////////////////////
    // readLine                                                                //
    /////////////////////////////////////////////////////////////////////////////
    @Test
    def readLine01(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine02(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\n'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine03(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\r'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine04(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\r', '\n'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine05(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello world"), line)
    }

    @Test
    def readLine06(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\n'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello world"), line)
    }

    @Test
    def readLine07(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\r'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello world"), line)
    }

    @Test
    def readLine08(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '\r', '\n'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello world"), line)
    }

    @Test
    def readLine09(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine10(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("world"), line)
    }

    @Test
    def readLine11(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine12(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("world"), line)
    }

    @Test
    def readLine13(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine14(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("world"), line)
    }

    @Test
    def readLine15(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine16(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine17(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("world"), line)
    }

    @Test
    def readLine18(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine19(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine20(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("world"), line)
    }

    @Test
    def readLine21(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine22(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine23(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("world"), line)
    }

    @Test
    def readLine24(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine25(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("world"), line)
    }

    @Test
    def readLine26(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine27(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine28(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("world"), line)
    }

    @Test
    def readLine29(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine30(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine31(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\n', '\n', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("world"), line)
    }

    @Test
    def readLine32(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine33(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine34(): Unit \ Assert + IO = region rc {
        let capacity = 6;
        let array = Array#{'h', 'e', 'l', 'l', 'o', '\r', '\r', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("world"), line)
    }

    @Test
    def readLine35(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\n', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine36(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\n', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine37(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\r', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine38(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\r', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine39(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\r', '\n', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine40(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\r', '\n', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

    @Test
    def readLine41(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\n', '\r', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine42(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\n', '\r', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok(""), line)
    }

    @Test
    def readLine43(): Unit \ Assert + IO = region rc {
        let capacity = 8;
        let array = Array#{'\n', '\r', 'h', 'e', 'l', 'l', 'o'} @ rc;
        let bufReader = TestReader.mkTestBufReader(rc, capacity, array);
        let _ = bufReader |> Peekable.readLine;
        let _ = bufReader |> Peekable.readLine;
        let line = bufReader |> Peekable.readLine;
        assertEq(expected = Ok("hello"), line)
    }

}
