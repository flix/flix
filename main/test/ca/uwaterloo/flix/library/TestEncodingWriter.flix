mod TestEncodingWriter {
    use Assert.{assertEq};
    use LimitedTestWriter.mkLimitedEncodingWriter
    use TestWriter.mkEncodingWriter

    enum TestWriter[t: Type, r: Eff]({
        stream = MutList[t, r]
    })

    instance Writable[TestWriter[t, r]] {
        type Elm = t
        type Aef = r
        pub def write(buffer: Array[t, rc], writer: TestWriter[t, r]): Result[IoError, Int32] \ rc + r = TestWriter.write(buffer, writer)

    }

    mod TestWriter {
        import java.nio.charset.StandardCharsets

        pub def mkEncodingWriter(stream: MutList[t, r]): EncodingWriter[TestWriter[t, r]] \ IO =
            let testWriter = TestEncodingWriter.TestWriter.fromMutList(stream);
            let utf8 = CharacterSet.CharacterSet(StandardCharsets.UTF_8);
            EncodingWriter.wrap(utf8, testWriter)

        pub def fromMutList(stream: MutList[t, r]): TestWriter[t, r] =
            TestWriter({ stream = stream })

        pub def write(buffer: Array[t, rc], writer: TestWriter[t, r]): Result[IoError, Int32] \ rc + r =
            let TestWriter({ stream }) = writer;
            let bufferSize = Array.size(buffer);
            stream |> MutList.pushAll(buffer |> Array.toList);
            Ok(bufferSize)

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
        import java.nio.charset.StandardCharsets

        pub def mkLimitedEncodingWriter(stream: Array[t, r]): EncodingWriter[LimitedTestWriter[t, r]] \ IO =
            let testWriter = TestEncodingWriter.LimitedTestWriter.fromMutList(stream);
            let utf8 = CharacterSet.CharacterSet(StandardCharsets.UTF_8);
            EncodingWriter.wrap(utf8, testWriter)

        pub def fromMutList(stream: Array[t, r]): LimitedTestWriter[t, r] =
            LimitedTestWriter({ stream = stream })

        pub def write(buffer: Array[t, rc], writer: LimitedTestWriter[t, r]): Result[IoError, Int32] \ rc + r =
            let LimitedTestWriter({ stream }) = writer;
            let bufferSize = Array.size(buffer);
            let streamSize = Array.size(stream);
            let bytesToWrite = Int32.min(bufferSize, streamSize);
            Array.copyInto(
                srcPos = 0,
                dstPos = 0,
                len = bytesToWrite,
                src = buffer,
                stream
            );
            Ok(bytesToWrite)

    }


    @Test
    def write01(): Unit \ Assert + IO = region rc {
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars = Array#{} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = Nil;
        assertEq(expected = Ok(0), charsWritten);
        assertEq(expected = expectedDest, actualDest |> MutList.toList)
    }

    @Test
    def write02(): Unit \ Assert + IO = region rc {
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = 104i8 :: 101i8 :: 108i8 :: 108i8 :: 111i8 :: 32i8 :: 119i8 :: 111i8 :: 114i8 :: 108i8 :: 100i8 :: Nil;
        assertEq(expected = Ok(11), charsWritten);
        assertEq(expected = expectedDest, actualDest |> MutList.toList)
    }

    @Test
    def write03(): Unit \ Assert + IO = region rc {
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars = Array#{'h', 'e', 'l', 'l', 'o'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = 104i8 :: 101i8 :: 108i8 :: 108i8 :: 111i8 :: Nil;
        assertEq(expected = Ok(5), charsWritten);
        assertEq(expected = expectedDest, actualDest |> MutList.toList)
    }

    @Test
    def write04(): Unit \ Assert + IO = region rc {
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars1 = Array#{'h', 'e', 'l', 'l', 'o'} @ rc;
        let _ = writer |> Writable.write(chars1);
        let chars2 = Array#{' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars2);
        let expectedDest = 104i8 :: 101i8 :: 108i8 :: 108i8 :: 111i8 :: 32i8 :: 119i8 :: 111i8 :: 114i8 :: 108i8 :: 100i8 :: Nil;
        assertEq(expected = Ok(6), charsWritten);
        assertEq(expected = expectedDest, actualDest |> MutList.toList)
    }

    @Test
    def write05(): Unit \ Assert + IO = region rc {
        // é is a two byte char
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars = Array#{'h', 'é', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = 104i8 :: -61i8 :: -87i8 :: 108i8 :: 108i8 :: 111i8 :: 32i8 :: 119i8 :: 111i8 :: 114i8 :: 108i8 :: 100i8 :: Nil;
        assertEq(expected = Ok(11), charsWritten);
        assertEq(expected = expectedDest, actualDest |> MutList.toList)
    }

    @Test
    def write06(): Unit \ Assert + IO = region rc {
        // € is a two byte char
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars = Array#{'h', '€', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = 104i8 :: -30i8 :: -126i8 :: -84i8 :: 108i8 :: 108i8 :: 111i8 :: 32i8 :: 119i8 :: 111i8 :: 114i8 :: 108i8 :: 100i8 :: Nil;
        assertEq(expected = Ok(11), charsWritten);
        assertEq(expected = expectedDest, actualDest |> MutList.toList)
    }

    @Test
    def write07(): Unit \ Assert + IO = region rc {
        let actualDest = Array#{} @ rc;
        let writer = mkLimitedEncodingWriter(actualDest);
        let chars = Array#{} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = Nil;
        assertEq(expected = Ok(0), charsWritten);
        assertEq(expected = expectedDest, actualDest |> Array.toList)
    }

    @Test
    def write08(): Unit \ Assert + IO = region rc {
        let actualDest = Array.empty(rc, 2);
        let writer = mkLimitedEncodingWriter(actualDest);
        let chars = Array#{'a', 'b', 'c'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        // the first underlying write leaves {'a', 'b'}
        // the second underlying write leaves {'c', 'b'} ('b' is not overwritten)
        let expectedDest = 99i8 :: 98i8 :: Nil;
        assertEq(expected = Ok(3), charsWritten);
        assertEq(expected = expectedDest, actualDest |> Array.toList)
    }

    @Test
    def write09(): Unit \ Assert + IO = region rc {
        let actualDest = Array.empty(rc, 2);
        let writer = mkLimitedEncodingWriter(actualDest);
        let chars = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        // the second to last underlying write leaves {'r', 'l'}
        // the last underlying write leaves {'d', 'l'} ('l' is not overwritten)
        let expectedDest = 100i8 :: 108i8 :: Nil;
        assertEq(expected = Ok(11), charsWritten);
        assertEq(expected = expectedDest, actualDest |> Array.toList)
    }

    @Test
    def write10(): Unit \ Assert + IO = region rc {
        let actualDest = Array.empty(rc, 2);
        let writer = mkLimitedEncodingWriter(actualDest);
        let chars = Array#{'h', 'é', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        // num chars divisible by dest size, so last underlying write leaves {'l', 'd'}
        let expectedDest = 108i8 :: 100i8 :: Nil;
        assertEq(expected = Ok(11), charsWritten);
        assertEq(expected = expectedDest, actualDest |> Array.toList)
    }

    @Test
    def write11(): Unit \ Assert + IO = region rc {
        let actualDest = Array.empty(rc, 2);
        let writer = mkLimitedEncodingWriter(actualDest);
        let chars = Array#{'h', '€', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        // the second to last underlying write leaves {'r', 'l'}
        // the last underlying write leaves {'d', 'l'} ('l' is not overwritten)
        let expectedDest = 100i8 :: 108i8 :: Nil;
        assertEq(expected = Ok(11), charsWritten);
        assertEq(expected = expectedDest, actualDest |> Array.toList)
    }

}
