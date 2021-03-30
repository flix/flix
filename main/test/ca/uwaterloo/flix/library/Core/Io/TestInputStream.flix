/*
 * Copyright 2020 Rasmus Larsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

use Core/Io/InputStream.{available, close, fromBuffer, InputStream, readBuffer, readBufferWithOffset, readByte, readAll};
use Core/Io/InputStream.InputStream.InputStream;
use Core/Io/IOError.IOError;

namespace Core/Io/TestInputStream {
    @test
    def fromBuffer(): InputStream & Impure = fromBuffer([0 as Int8, 1 as Int8, 2 as Int8])

    @test
    def available01(): Bool & Impure = {
        available(testBuffer()) |> resultEq(3)
    }

    @test
    def available02(): Bool & Impure = {
        let b = testBuffer();
        readByte(b)
            |> Result.flatMap(_ -> available(b))
            |> resultEq(2)
    }

    @test
    def available03(): Bool & Impure =  {
        let b = testBuffer();
        let a = [0 as Int8; 3];
        let r = readBuffer(a, b)
            |> Result.flatMap(c -> available(b) |> Result.map(d -> c == 3 and d == 0));
        Result.getWithDefault(r, false)
    }

    @test
    def readByte01(): Bool & Impure = {
        let b = testBuffer();
        let r = readByte(b);
        let m = Result.map(n -> n == 0, r);
        Result.getWithDefault(m, false)
    }

    @test
    def readByte02(): Bool & Impure = {
        let b = testBuffer();
        let r1 = readByte(b);
        let r2 = readByte(b);
        let m = Result.flatMap(v1 -> Result.map(v2 -> v1 == 0 and v2 == 1, r2), r1);
        Result.getWithDefault(m, false)
    }

    @test
    def readByte03(): Bool & Impure = {
        let b = testBuffer();
        let r1 = readByte(b);
        let r2 = readByte(b);
        let r3 = readByte(b);
        let m = Result.flatMap(v1 -> Result.flatMap(v2 -> Result.map(v3 -> v1 == 0 and v2 == 1 and v3 == 2, r3), r2), r1);
        Result.getWithDefault(m, false)
    }

    @test
    def readBuffer01(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 2];
        let g = readBuffer(a, b) |> resultEq(2);
        let a0 = a[0] == 0 as Int8;
        let a1 = a[1] == 1 as Int8;
        g and a0 and a1
    }

    @test
    def readBuffer02(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 4];
        let g = readBuffer(a, b) |> resultEq(3);
        let a0 = a[0] == 0 as Int8;
        let a1 = a[1] == 1 as Int8;
        let a2 = a[2] == 2 as Int8;
        let a3 = a[3] == -1 as Int8;
        g and a0 and a1 and a2 and a3
    }

    @test
    def readBuffer03(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 0];
        let r = readBuffer(a, b) |> resultEq(0);
        let p = available(b) |> resultEq(3);
        r and p
    }

    @test
    def readBufferWithOffset01(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 4];
        let g = readBufferWithOffset(a, 1, 3, b) |> resultEq(3);
        let a0 = a[0] == -1 as Int8;
        let a1 = a[1] == 0 as Int8;
        let a2 = a[2] == 1 as Int8;
        let a3 = a[3] == 2 as Int8;
        g and a0 and a1 and a2 and a3
    }

    @test
    def readBufferWithOffset02(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 4];
        let r = readBufferWithOffset(a, 1, -1, b);
        let s = resultSwap(r);
        let m = Result.map(e -> e == IOError.IncorrectBufferSize, s);
        Result.isErr(r) and Result.getWithDefault(m, false)
    }

    @test
    def readBufferWithOffset03(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 1];
        let g = readBufferWithOffset(a, 0, 1, b) |> resultEq(1);
        g and a[0] == 0 as Int8
    }

    @test
    def readBufferWithOffset04(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 4];
        let g = readBufferWithOffset(a, 0, 4, b) |> resultEq(3);
        g and (a[0] == 0 as Int8) and (a[1] == 1 as Int8) and (a[2] == 2 as Int8) and a[3] == -1 as Int8
    }

    @test
    def readAll01(): Bool & Impure = {
        let b = testBuffer();
        let r = readAll(b);
        Result.map(Array.length, r) |> resultEq(3)
    }

    @test
    def readAll02(): Bool & Impure = {
        let b = fromBuffer([1 as Int8; 2049]);
        let r = readAll(b);
        Result.map(Array.length, r) |> resultEq(2049)
    }

    def testBuffer(): InputStream & Impure = fromBuffer([0 as Int8, 1 as Int8, 2 as Int8])

    def resultEq[a, e](i: a, r: Result[a, e]): Bool with Eq[a] = Result.getWithDefault(Result.map(v -> i == v, r), false)

    def resultSwap(r: Result[a, b]): Result[b, a] = match r {
        case Ok(x) => Err(x)
        case Err(x) => Ok(x)
    }
}
