mod Test.Portable.TextEncoding {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    enum TestWriter[t: Type, r: Eff]({
        stream = MutList[t, r]
    })

    instance Writable[TestWriter[t, r]] {
        type Elm = t
        type Aef = r
        pub def write(buffer: Array[t, rc], writer: TestWriter[t, r]): Result[IoError, Int32] \ rc + r = TestWriter.write(buffer, writer)
    }

    mod TestWriter {
        pub def mkUtf8(stream: MutList[t, r]): EncodingWriter[TestWriter[t, r]] =
            EncodingWriter.wrap(CharacterSet.utf8(), fromMutList(stream))

        pub def fromMutList(stream: MutList[t, r]): TestWriter[t, r] =
            TestWriter({ stream = stream })

        pub def write(buffer: Array[t, rc], writer: TestWriter[t, r]): Result[IoError, Int32] \ rc + r =
            let TestWriter({ stream }) = writer;
            let size = Array.length(buffer);
            stream |> MutList.pushAll(buffer |> Array.toList);
            Ok(size)
    }

    enum LimitedTestWriter[t: Type, r: Eff]({
        stream = Array[t, r]
    })

    instance Writable[LimitedTestWriter[t, r]] {
        type Elm = t
        type Aef = r
        pub def write(buffer: Array[t, rc], writer: LimitedTestWriter[t, r]): Result[IoError, Int32] \ rc + r = LimitedTestWriter.write(buffer, writer)
    }

    mod LimitedTestWriter {
        pub def mkUtf8(stream: Array[t, r]): EncodingWriter[LimitedTestWriter[t, r]] =
            EncodingWriter.wrap(CharacterSet.utf8(), fromArray(stream))

        pub def fromArray(stream: Array[t, r]): LimitedTestWriter[t, r] =
            LimitedTestWriter({ stream = stream })

        pub def write(buffer: Array[t, rc], writer: LimitedTestWriter[t, r]): Result[IoError, Int32] \ rc + r =
            let LimitedTestWriter({ stream }) = writer;
            let bytesToWrite = Int32.min(Array.length(buffer), Array.length(stream));
            Array.copyInto(srcPos = 0, dstPos = 0, len = bytesToWrite, src = buffer, stream);
            Ok(bytesToWrite)
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
        pub def mkUtf8(rc: Region[r], bytes: Array[Int8, r]): DecodingReader[TestReader[Int8, r], r] \ r =
            DecodingReader.wrap(rc, CharacterSet.utf8(), fromArray(rc, bytes))

        pub def fromArray(rc: Region[r], stream: Array[t, r]): TestReader[t, r] \ r =
            new TestReader @ rc {
                stream = stream,
                cursor = 0
            }

        pub def read(buffer: Array[t, rb], reader: TestReader[t, rr]): Result[IoError, Int32] \ rb + rr =
            let remaining = Array.length(reader->stream) - reader->cursor;
            if (remaining > 0) {
                let n = Int32.min(Array.length(buffer), remaining);
                Array.copyInto(srcPos = reader->cursor, dstPos = 0, len = n, src = reader->stream, buffer);
                reader->cursor = reader->cursor + n;
                Ok(n)
            } else {
                Ok(0)
            }
    }

    @Test
    def encodeAscii01(): Unit = region rc {
        let actual = MutList.empty(rc);
        let writer = TestWriter.mkUtf8(actual);
        let chars = Array#{'h', 'e', 'l', 'l', 'o'} @ rc;
        let written = writer |> Writable.write(chars);
        let expected = 104i8 :: 101i8 :: 108i8 :: 108i8 :: 111i8 :: Nil;
        let _ = assertTrue(written == Ok(5), "encodeAscii01 failed: chars written.");
        assertTrue(MutList.toList(actual) == expected, "encodeAscii01 failed: bytes.")
    }

    @Test
    def encodeUtf8AndSurrogates01(): Unit = region rc {
        let actual = MutList.empty(rc);
        let writer = TestWriter.mkUtf8(actual);
        let chars = Array#{'é', ' ', '€', ' ', '\uD83C', '\uDFDD'} @ rc;
        let written = writer |> Writable.write(chars);
        let expected = -61i8 :: -87i8 :: 32i8 :: -30i8 :: -126i8 :: -84i8 :: 32i8 :: -16i8 :: -97i8 :: -113i8 :: -99i8 :: Nil;
        let _ = assertTrue(written == Ok(6), "encodeUtf8AndSurrogates01 failed: chars written.");
        assertTrue(MutList.toList(actual) == expected, "encodeUtf8AndSurrogates01 failed: bytes.")
    }

    @Test
    def encodeMalformedSurrogate01(): Unit = region rc {
        let actual = MutList.empty(rc);
        let writer = TestWriter.mkUtf8(actual);
        let chars = Array#{'\uD83C', 'A'} @ rc;
        let written = writer |> Writable.write(chars);
        let expected = 63i8 :: 65i8 :: Nil;
        let _ = assertTrue(written == Ok(2), "encodeMalformedSurrogate01 failed: chars written.");
        assertTrue(MutList.toList(actual) == expected, "encodeMalformedSurrogate01 failed: replacement bytes.")
    }

    @Test
    def encodePartialWrites01(): Unit = region rc {
        let actual = Array.empty(rc, 2);
        let writer = LimitedTestWriter.mkUtf8(actual);
        let chars = Array#{'h', 'é', 'l'} @ rc;
        let written = writer |> Writable.write(chars);
        let expected = -87i8 :: 108i8 :: Nil;
        let _ = assertTrue(written == Ok(3), "encodePartialWrites01 failed: chars written.");
        assertTrue(actual |> Array.toList == expected, "encodePartialWrites01 failed: final buffer contents.")
    }

    @Test
    def decodeAscii01(): Unit = region rc {
        let bytes = Array#{104i8, 101i8, 108i8, 108i8, 111i8} @ rc;
        let reader = TestReader.mkUtf8(rc, bytes);
        let actual = Array.empty(rc, 5);
        let read = Readable.read(actual, reader);
        let expected = Array#{'h', 'e', 'l', 'l', 'o'} @ rc;
        let _ = assertTrue(read == Ok(5), "decodeAscii01 failed: chars read.");
        assertTrue(Array.sameElements(actual, expected), "decodeAscii01 failed: chars.")
    }

    @Test
    def decodeUtf8Chunked01(): Unit = region rc {
        let bytes = Array#{-30i8, -126i8, -84i8, -30i8, -126i8, -84i8} @ rc;
        let reader = TestReader.mkUtf8(rc, bytes);
        let actual = Array.empty(rc, 1);
        let expected = Array#{'€'} @ rc;
        let first = Readable.read(actual, reader);
        let _ = assertTrue(first == Ok(1), "decodeUtf8Chunked01 failed: first read count.");
        let _ = assertTrue(Array.sameElements(actual, expected), "decodeUtf8Chunked01 failed: first read.");
        let second = Readable.read(actual, reader);
        let _ = assertTrue(second == Ok(1), "decodeUtf8Chunked01 failed: second read count.");
        assertTrue(Array.sameElements(actual, expected), "decodeUtf8Chunked01 failed: second read.")
    }

    @Test
    def decodeSupplementary01(): Unit = region rc {
        let bytes = Array#{-16i8, -97i8, -113i8, -99i8} @ rc;
        let reader = TestReader.mkUtf8(rc, bytes);
        let actual = Array.empty(rc, 2);
        let read = Readable.read(actual, reader);
        let expected = Array#{'\uD83C', '\uDFDD'} @ rc;
        let _ = assertTrue(read == Ok(2), "decodeSupplementary01 failed: chars read.");
        assertTrue(Array.sameElements(actual, expected), "decodeSupplementary01 failed: chars.")
    }

    @Test
    def decodeMalformed01(): Unit = region rc {
        let bytes = Array#{104i8, -61i8, 108i8} @ rc;
        let reader = TestReader.mkUtf8(rc, bytes);
        let actual = Array.empty(rc, 3);
        let read = Readable.read(actual, reader);
        let expected = Array#{'h', '\uFFFD', 'l'} @ rc;
        let _ = assertTrue(read == Ok(3), "decodeMalformed01 failed: chars read.");
        assertTrue(Array.sameElements(actual, expected), "decodeMalformed01 failed: malformed replacement.")
    }

    @Test
    def decodeIncompleteEof01(): Unit = region rc {
        let bytes = Array#{-61i8} @ rc;
        let reader = TestReader.mkUtf8(rc, bytes);
        let actual = Array.empty(rc, 1);
        let read = Readable.read(actual, reader);
        let expected = Array#{'\uFFFD'} @ rc;
        let _ = assertTrue(read == Ok(1), "decodeIncompleteEof01 failed: chars read.");
        assertTrue(Array.sameElements(actual, expected), "decodeIncompleteEof01 failed: eof replacement.")
    }

    pub def runAll(): Unit = {
        let _ = encodeAscii01();
        let _ = encodeUtf8AndSurrogates01();
        let _ = encodeMalformedSurrogate01();
        let _ = encodePartialWrites01();
        let _ = decodeAscii01();
        let _ = decodeUtf8Chunked01();
        let _ = decodeSupplementary01();
        let _ = decodeMalformed01();
        let _ = decodeIncompleteEof01();
        ()
    }

}
