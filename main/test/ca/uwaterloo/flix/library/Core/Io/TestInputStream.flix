use Core/Io/InputStream.{available, close, fromBuffer, InputStream, readByte, readBuffer, readBufferWithLength};
use Core/Io/InputStream.InputStream.InputStream;
use Core/Io/IOError.IOError;

namespace Core/Io/TestInputStream {
    def testBuffer(): InputStream & Impure = fromBuffer([0 as Int8, 1 as Int8, 2 as Int8])

    def resultEq(i: Int32, r: Result[Int32, err]): Bool = Result.getWithDefault(Result.map(v -> i == v, r), false)

    @test
    def fromBuffer(): InputStream & Impure = fromBuffer([0 as Int8, 1 as Int8, 2 as Int8])

    @test
    def available01(): Bool & Impure = {
        let r = available(testBuffer()) |> Result.map(a -> a == 3) ;
        Result.getWithDefault(r, false)
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
            |> Result.flatMap(c -> available(b) |> Result.map(d -> c == 3 && d == 0));
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
        let m = Result.flatMap(v1 -> Result.map(v2 -> v1 == 0 && v2 == 1, r2), r1);
        Result.getWithDefault(m, false)
    }

    @test
    def readByte03(): Bool & Impure = {
        let b = testBuffer();
        let r1 = readByte(b);
        let r2 = readByte(b);
        let r3 = readByte(b);
        let m = Result.flatMap(v1 -> Result.flatMap(v2 -> Result.map(v3 -> v1 == 0 && v2 == 1 && v3 == 2, r3), r2), r1);
        Result.getWithDefault(m, false)
    }

    @test
    def readBuffer01(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 2];
        let g = readBuffer(a, b) |> resultEq(2);
        let a0 = a[0] == 0 as Int8;
        let a1 = a[1] == 1 as Int8;
        g && a0 && a1
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
        g && a0 && a1 && a2 && a3
    }

    @test
    def readBuffer03(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 0];
        let r = readBuffer(a, b) |> resultEq(0);
        let p = available(b) |> resultEq(3);
        r && p
    }

    @test
    def readBufferWithLength01(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 4];
        let g = readBufferWithLength(a, 1, 3, b) |> resultEq(3);
        let a0 = a[0] == -1 as Int8;
        let a1 = a[1] == 0 as Int8;
        let a2 = a[2] == 1 as Int8;
        let a3 = a[3] == 2 as Int8;
        g && a0 && a1 && a2 && a3
    }

    def resultSwap(r: Result[a, b]): Result[b, a] = match r {
        case Ok(x) => Err(x)
        case Err(x) => Ok(x)
    }

    @test
    def readBufferWithLength02(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 4];
        let r = readBufferWithLength(a, 1, -1, b);
        let s = resultSwap(r);
        let m = Result.map(e -> e == IOError.IndexOutOfBoundsException, s);
        Result.isErr(r) && Result.getWithDefault(m, false)
    }

    @test
    def readBufferWithLength03(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 1];
        let g = readBufferWithLength(a, 0, 1, b) |> resultEq(1);
        g && a[0] == 0 as Int8
    }

    @test
    def readBufferWithLength04(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 4];
        let g = readBufferWithLength(a, 0, 4, b) |> resultEq(3);
        g && a[0] == 0 as Int8 && a[1] == 1 as Int8 && a[2] == 2 as Int8 && a[3] == -1 as Int8
    }

    @test
    // TODO: `close` is `nop` on an `InputStream` backed by a byte array
    def close(): Bool & Impure = {
        let b = testBuffer();
        let o = b |> close |> Result.isOk;
        o
    }
}