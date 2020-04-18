use Core/Io/InputStream.{available, close, fromBuffer, InputStream, readByte, readBuffer, readBufferWithLength};
use Core/Io/InputStream.InputStream.InputStream;

namespace TestInputStream {
    def testBuffer(): InputStream & Impure = fromBuffer([0 as Int8, 1 as Int8, 2 as Int8])

    @test
    def fromBuffer(): InputStream & Impure = fromBuffer([0 as Int8, 1 as Int8, 2 as Int8])

    @test
    def available(): Bool & Impure = {
        let r = available(testBuffer()) |> Result.map(a -> a == 3) ;
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
        let r = readByte(b);
        let f = Result.flatMap(_ -> readByte(b), r);
        let m = Result.map(n -> n == 1, f);
        Result.getWithDefault(m, false)
    }

    @test
    def readBuffer01(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 2];
        let r = readBuffer(a, b);
        let m = Result.map(n -> n == 2, r);
        let g = Result.getWithDefault(m, false);
        let a0 = a[0] == 0 as Int8;
        let a1 = a[1] == 1 as Int8;
        g && a0 && a1
    }

    @test
    def readBuffer02(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 4];
        let r = readBuffer(a, b);
        let m = Result.map(n -> n == 3, r);
        let g = Result.getWithDefault(m, false);
        let a0 = a[0] == 0 as Int8;
        let a1 = a[1] == 1 as Int8;
        let a2 = a[2] == 2 as Int8;
        let a3 = a[3] == -1 as Int8;
        g && a0 && a1 && a2 && a3
    }

    @test
    def readBufferWithLength01(): Bool & Impure = {
        let b = testBuffer();
        let a = [-1 as Int8; 4];
        let r = readBufferWithLength(a, 1, 3, b);
        let m = Result.map(n -> n == 3, r);
        let g = Result.getWithDefault(m, false);
        let a0 = a[0] == -1 as Int8;
        let a1 = a[1] == 0 as Int8;
        let a2 = a[2] == 1 as Int8;
        let a3 = a[3] == 2 as Int8;
        g && a0 && a1 && a2 && a3
    }

    @test
    // TODO: `close` is `nop` on an `InputStream` backed by a byte array
    def close(): Bool & Impure = {
        let b = testBuffer();
        let o = b |> close |> Result.isOk;
        o
    }
}