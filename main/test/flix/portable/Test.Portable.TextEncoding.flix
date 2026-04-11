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
    def encode01(): Unit = region rc {
        let actual = MutList.empty(rc);
        let writer = TestWriter.mkUtf8(actual);
        let chars = Array#{'é', ' ', '€', ' ', '\uD83C', '\uDFDD'} @ rc;
        let written = writer |> Writable.write(chars);
        let expected = -61i8 :: -87i8 :: 32i8 :: -30i8 :: -126i8 :: -84i8 :: 32i8 :: -16i8 :: -97i8 :: -113i8 :: -99i8 :: Nil;
        let _ = assertTrue(written == Ok(6), "encode01 failed: chars written.");
        assertTrue(MutList.toList(actual) == expected, "encode01 failed: bytes.")
    }

    @Test
    def decode01(): Unit = region rc {
        let bytes = Array#{-16i8, -97i8, -113i8, -99i8} @ rc;
        let reader = TestReader.mkUtf8(rc, bytes);
        let actual = Array.empty(rc, 2);
        let read = Readable.read(actual, reader);
        let expected = Array#{'\uD83C', '\uDFDD'} @ rc;
        let _ = assertTrue(read == Ok(2), "decode01 failed: chars read.");
        assertTrue(Array.sameElements(actual, expected), "decode01 failed: chars.")
    }

    @Test
    def replacement01(): Unit = region rc {
        let bytes = Array#{104i8, -61i8, 108i8} @ rc;
        let reader = TestReader.mkUtf8(rc, bytes);
        let actual = Array.empty(rc, 3);
        let read = Readable.read(actual, reader);
        let expected = Array#{'h', '\uFFFD', 'l'} @ rc;
        let _ = assertTrue(read == Ok(3), "replacement01 failed: chars read.");
        assertTrue(Array.sameElements(actual, expected), "replacement01 failed: malformed replacement.")
    }

    pub def runAll(): Unit = {
        let _ = encode01();
        let _ = decode01();
        let _ = replacement01();
        ()
    }

}
