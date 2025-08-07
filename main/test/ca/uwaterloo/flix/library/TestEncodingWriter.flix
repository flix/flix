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

mod TestEncodingWriter {
    use LimitedTestWriter.mkLimitedEncodingWriter
    use TestWriter.mkEncodingWriter

    @test
    def write01(): Bool \ IO = region rc {
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars = Array#{} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = Nil;
        charsWritten == Ok(0) and actualDest |> MutList.toList == expectedDest
    }

    @test
    def write02(): Bool \ IO = region rc {
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = 104i8 :: 101i8 :: 108i8 :: 108i8 :: 111i8 :: 32i8 :: 119i8 :: 111i8 :: 114i8 :: 108i8 :: 100i8 :: Nil;
        charsWritten == Ok(11) and actualDest |> MutList.toList == expectedDest
    }

    @test
    def write03(): Bool \ IO = region rc {
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars = Array#{'h', 'e', 'l', 'l', 'o'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = 104i8 :: 101i8 :: 108i8 :: 108i8 :: 111i8 :: Nil;
        charsWritten == Ok(5) and actualDest |> MutList.toList == expectedDest
    }

    @test
    def write04(): Bool \ IO = region rc {
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars1 = Array#{'h', 'e', 'l', 'l', 'o'} @ rc;
        let _ = writer |> Writable.write(chars1);
        let chars2 = Array#{' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars2);
        let expectedDest = 104i8 :: 101i8 :: 108i8 :: 108i8 :: 111i8 :: 32i8 :: 119i8 :: 111i8 :: 114i8 :: 108i8 :: 100i8 :: Nil;
        charsWritten == Ok(6) and actualDest |> MutList.toList == expectedDest
    }

    @test
    def write05(): Bool \ IO = region rc {
        // é is a two byte char
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars = Array#{'h', 'é', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = 104i8 :: -61i8 :: -87i8 :: 108i8 :: 108i8 :: 111i8 :: 32i8 :: 119i8 :: 111i8 :: 114i8 :: 108i8 :: 100i8 :: Nil;
        charsWritten == Ok(11) and actualDest |> MutList.toList == expectedDest
    }

    @test
    def write06(): Bool \ IO = region rc {
        // € is a two byte char
        let actualDest = MutList.empty(rc);
        let writer = mkEncodingWriter(actualDest);
        let chars = Array#{'h', '€', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = 104i8 :: -30i8 :: -126i8 :: -84i8 :: 108i8 :: 108i8 :: 111i8 :: 32i8 :: 119i8 :: 111i8 :: 114i8 :: 108i8 :: 100i8 :: Nil;
        charsWritten == Ok(11) and actualDest |> MutList.toList == expectedDest
    }

    @test
    def write07(): Bool \ IO = region rc {
        let actualDest = Array#{} @ rc;
        let writer = mkLimitedEncodingWriter(actualDest);
        let chars = Array#{} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        let expectedDest = Nil;
        charsWritten == Ok(0) and actualDest |> Array.toList == expectedDest
    }

    @test
    def write08(): Bool \ IO = region rc {
        let actualDest = Array.empty(rc, 2);
        let writer = mkLimitedEncodingWriter(actualDest);
        let chars = Array#{'a', 'b', 'c'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        // the first underlying write leaves {'a', 'b'}
        // the second underlying write leaves {'c', 'b'} ('b' is not overwritten)
        let expectedDest = 99i8 :: 98i8 :: Nil;
        charsWritten == Ok(3) and actualDest |> Array.toList == expectedDest
    }

    @test
    def write09(): Bool \ IO = region rc {
        let actualDest = Array.empty(rc, 2);
        let writer = mkLimitedEncodingWriter(actualDest);
        let chars = Array#{'h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        // the second to last underlying write leaves {'r', 'l'}
        // the last underlying write leaves {'d', 'l'} ('l' is not overwritten)
        let expectedDest = 100i8 :: 108i8 :: Nil;
        charsWritten == Ok(11) and actualDest |> Array.toList == expectedDest
    }

    @test
    def write10(): Bool \ IO = region rc {
        let actualDest = Array.empty(rc, 2);
        let writer = mkLimitedEncodingWriter(actualDest);
        let chars = Array#{'h', 'é', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        // num chars divisible by dest size, so last underlying write leaves {'l', 'd'}
        let expectedDest = 108i8 :: 100i8 :: Nil;
        charsWritten == Ok(11) and actualDest |> Array.toList == expectedDest
    }

    @test
    def write11(): Bool \ IO = region rc {
        let actualDest = Array.empty(rc, 2);
        let writer = mkLimitedEncodingWriter(actualDest);
        let chars = Array#{'h', '€', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd'} @ rc;
        let charsWritten = writer |> Writable.write(chars);
        // the second to last underlying write leaves {'r', 'l'}
        // the last underlying write leaves {'d', 'l'} ('l' is not overwritten)
        let expectedDest = 100i8 :: 108i8 :: Nil;
        charsWritten == Ok(11) and actualDest |> Array.toList == expectedDest
    }

}

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
        let testWriter = TestWriter.fromMutList(stream);
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
        let testWriter = LimitedTestWriter.fromMutList(stream);
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
