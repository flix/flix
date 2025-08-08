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

mod TestWritable {
    import java.lang.System

    @test
    pub def testWriteString01(): Bool \ IO = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeString("", writer);
        let contents = TestWriter.contents(writer);
        result == Ok(()) and contents == ""
    }

    @test
    pub def testWriteString02(): Bool \ IO = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeString("hello world", writer);
        let contents = TestWriter.contents(writer);
        result == Ok(()) and contents == "hello world"
    }

    @test
    pub def testWriteString03(): Bool \ IO = region rc {
        let writer = TestWriter.empty(rc);
        let result1 = Writable.writeString("hello", writer);
        let result2 = Writable.writeString(" ", writer);
        let result3 = Writable.writeString("world", writer);
        let contents = TestWriter.contents(writer);
        result1 == Ok(()) and result2 == Ok(()) and result3 == Ok(()) and contents == "hello world"
    }

    @test
    pub def testWriteString04(): Bool \ IO = region rc {
        let writer = TestWriter.empty(rc);
        let result1 = Writable.writeString("", writer);
        let result2 = Writable.writeString("hello world", writer);
        let result3 = Writable.writeString("", writer);
        let contents = TestWriter.contents(writer);
        result1 == Ok(()) and result2 == Ok(()) and result3 == Ok(()) and contents == "hello world"
    }

    @test
    pub def testWriteNewLine01(): Bool \ IO + Sys = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeNewLine(writer);
        let contents = TestWriter.contents(writer);
        result == Ok(()) and contents == newLine()
    }

    @test
    pub def testWriteNewLine02(): Bool \ IO + Sys = region rc {
        let writer = TestWriter.empty(rc);
        let result1 = Writable.writeString("hello world", writer);
        let result2 = Writable.writeNewLine(writer);
        let contents = TestWriter.contents(writer);
        result1 == Ok(()) and result2 == Ok(()) and contents == "hello world" ++ newLine()
    }

    @test
    pub def testWriteLine01(): Bool \ IO + Sys = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeLine("", writer);
        let contents = TestWriter.contents(writer);
        result == Ok(()) and contents == newLine()
    }

    @test
    pub def testWriteLine02(): Bool \ IO + Sys = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeLine("hello world", writer);
        let contents = TestWriter.contents(writer);
        result == Ok(()) and contents == "hello world" ++ newLine()
    }

    @test
    pub def testWriteLine03(): Bool \ IO + Sys = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeLine(newLine(), writer);
        let contents = TestWriter.contents(writer);
        result == Ok(()) and contents == newLine() ++ newLine()
    }

    @test
    pub def testWriteLine04(): Bool \ IO + Sys = region rc {
        let writer = TestWriter.empty(rc);
        let result = Writable.writeLine("hello" ++ newLine() ++ "world", writer);
        let contents = TestWriter.contents(writer);
        result == Ok(()) and contents == "hello" ++ newLine() ++ "world" ++ newLine()
    }

    def newLine(): String \ IO + Sys = System.getProperty("line.separator")

}

enum TestWriter[r: Eff](Region[r], MutList[Char, r])

instance Writable[TestWriter[rw]] {
    type Elm = Char
    type Aef = rw
    pub def write(buffer: Array[Char, rb], writer: TestWriter[rw]): Result[IoError, Int32] \ rb + rw = TestWriter.write(buffer, writer)
}

mod TestWriter {
    import java.lang.String

    pub def empty(rc: Region[r]): TestWriter[r] \ r = TestWriter.TestWriter(rc, MutList.empty(rc))

    pub def write(buffer: Array[Char, rb], writer: TestWriter[rw]): Result[IoError, Int32] \ rb + rw =
        let TestWriter.TestWriter(_, mutList) = writer;
        mutList |> MutList.pushAll(buffer |> Array.toList);
        Ok(buffer |> Array.size)

    pub def contents(writer: TestWriter[r]): String \ r + IO =
        let TestWriter.TestWriter(rc, mutList) = writer;
        new String(mutList |> MutList.toArray(rc))

}
