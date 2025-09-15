mod TestWritable {
    import java.lang.System

    use Assert.{assertEq, assertTrue}

    pub enum TestWriter[r: Eff](Region[r], MutList[Char, r])

    instance Writable[TestWriter[rw]] {
        type Elm = Char
        type Aef = rw
        pub def write(buffer: Array[Char, rb], writer: TestWriter[rw]): Result[IoError, Int32] \ rb + rw = TestWriter.write(buffer, writer)
    }

    mod TestWriter {
        use TestWritable.TestWriter

        import java.lang.String

        pub def empty(rc: Region[r]): TestWriter[r] \ r = TestWriter(rc, MutList.empty(rc))

        pub def write(buffer: Array[Char, rb], writer: TestWriter[rw]): Result[IoError, Int32] \ rb + rw =
            let TestWriter.TestWriter(_, mutList) = writer;
            mutList |> MutList.pushAll(buffer |> Array.toList);
            Ok(buffer |> Array.size)

        pub def contents(writer: TestWriter[r]): String \ r + IO =
            let TestWriter.TestWriter(rc, mutList) = writer;
            new String(mutList |> MutList.toArray(rc))

    }

    @Test
    pub def testWriteString01(): Unit \ {Assert, IO} = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeString("", writer);
        let contents = TestWriter.contents(writer);
        assertEq(expected = Ok(()), result);
        assertEq(expected = "", contents)
    }

    @Test
    pub def testWriteString02(): Unit \ {Assert, IO} = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeString("hello world", writer);
        let contents = TestWriter.contents(writer);
        assertEq(expected = Ok(()), result);
        assertEq(expected = "hello world", contents)
    }

    @Test
    pub def testWriteString03(): Unit \ {Assert, IO} = region rc {
        let writer = TestWriter.empty(rc);
        let result1 = Writable.writeString("hello", writer);
        let result2 = Writable.writeString(" ", writer);
        let result3 = Writable.writeString("world", writer);
        let contents = TestWriter.contents(writer);
        assertEq(expected = Ok(()), result1);
        assertEq(expected = Ok(()), result2);
        assertEq(expected = Ok(()), result3);
        assertEq(expected = "hello world", contents)
    }

    @Test
    pub def testWriteString04(): Unit \ {Assert, IO} = region rc {
        let writer = TestWriter.empty(rc);
        let result1 = Writable.writeString("", writer);
        let result2 = Writable.writeString("hello world", writer);
        let result3 = Writable.writeString("", writer);
        let contents = TestWriter.contents(writer);
        assertEq(expected = Ok(()), result1);
        assertEq(expected = Ok(()), result2);
        assertEq(expected = Ok(()), result3);
        assertEq(expected = "hello world", contents)
    }

    @Test
    pub def testWriteNewLine01(): Unit \ {Assert, IO, Env} = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeNewLine(writer);
        let contents = TestWriter.contents(writer);
        assertEq(expected = Ok(()), result);
        assertEq(expected = newLine(), contents)
    }

    @Test
    pub def testWriteNewLine02(): Unit \ {Assert, IO, Env} = region rc {
        let writer = TestWriter.empty(rc);
        let result1 = Writable.writeString("hello world", writer);
        let result2 = Writable.writeNewLine(writer);
        let contents = TestWriter.contents(writer);
        assertEq(expected = Ok(()), result1);
        assertEq(expected = Ok(()), result2);
        assertEq(expected = "hello world" ++ newLine(), contents)
    }

    @Test
    pub def testWriteLine01(): Unit \ {Assert, IO, Env} = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeLine("", writer);
        let contents = TestWriter.contents(writer);
        assertEq(expected = Ok(()), result);
        assertEq(expected = newLine(), contents)
    }

    @Test
    pub def testWriteLine02(): Unit \ {Assert, IO, Env} = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeLine("hello world", writer);
        let contents = TestWriter.contents(writer);
        assertEq(expected = Ok(()), result);
        assertEq(expected = "hello world" ++ newLine(), contents)
    }

    @Test
    pub def testWriteLine03(): Unit \ {Assert, IO, Env} = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeLine(newLine(), writer);
        let contents = TestWriter.contents(writer);
        assertEq(expected = Ok(()), result);
        assertEq(expected = newLine() ++ newLine(), contents)
    }

    @Test
    pub def testWriteLine04(): Unit \ {Assert, IO, Env} = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeLine("hello" ++ newLine() ++ "world", writer);
        let contents = TestWriter.contents(writer);
        assertEq(expected = Ok(()), result);
        assertEq(expected = "hello" ++ newLine() ++ "world" ++ newLine(), contents)
    }

    def newLine(): String \ IO + Env = System.lineSeparator()

}
