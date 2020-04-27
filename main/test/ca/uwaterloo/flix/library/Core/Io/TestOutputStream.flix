use Core/Io/IOError.IOError;
use Core/Io/OutputStream.{OutputStream, writeBuffer, writeBufferWithOffset, writeByte};
use Core/Io/OutputStream.OutputStream.OutputStream;
use Result.flatMap;

namespace Core/Io/TestOutputStream {
    @test
    def writeByte01(): Bool & Impure = {
        let b = testBuffer();
        let _ = writeByte(10 as Int8, b);
        let a = readTestBuffer(b);
        Array.length(a) == 1 && a[0] == 10 as Int8
    }

    @test
    def writeByte02(): Bool & Impure = {
        let b = testBuffer();
        let _ = writeByte(52 as Int8, b);
        let _ = writeByte(76 as Int8, b);
        let a = readTestBuffer(b);
        Array.length(a) == 2 && a[0] == 52 as Int8 && a[1] == 76 as Int8
    }

    @test
    def writeByte03(): Bool & Impure = {
        let e = [52 as Int8; 1000];
        let b = testBuffer();
        let _ = Array.map(i -> writeByte(i, b), e);
        let a = readTestBuffer(b);
        Array.length(a) == 1000 && Array.forall(i -> i == 52 as Int8, a)
    }

    @test
    def writeBuffer01(): Bool & Impure = {
        let e = [42 as Int8; 1];
        let b = testBuffer();
        let _ = writeBuffer(e, b);
        let a = readTestBuffer(b);
        Array.length(a) == 1 && a[0] == 42 as Int8
    }

    @test
    def writeBuffer02(): Bool & Impure = {
        let e = [42 as Int8; 420];
        let b = testBuffer();
        let _ = writeBuffer(e, b);
        let a = readTestBuffer(b);
        Array.length(a) == 420 && Array.forall(i ->i == 42 as Int8, a)
    }

    @test
    def writeBuffer03(): Bool & Impure = {
        let e = [42 as Int8; 0];
        let b = testBuffer();
        let _ = writeBuffer(e, b);
        let a = readTestBuffer(b);
        Array.length(a) == 0
    }

    @test
    def writeBufferWithOffset01(): Bool & Impure = {
        let e = [1 as Int8, 2 as Int8, 3 as Int8];
        let b = testBuffer();
        let _ = writeBufferWithOffset(e, 1, 2, b);
        let a = readTestBuffer(b);
        Array.length(a) == 2  && a[0] == 2 as Int8 && a[1] == 3 as Int8
    }

    @test
    def writeBufferWithOffset02(): Bool & Impure = {
        let e = [-1 as Int8; 10];
        let b = testBuffer();
        let r = writeBufferWithOffset(e, 2, 10, b);
        let a = readTestBuffer(b);
        Array.length(a) == 0 && Result.isErr(r)
    }

    @test
    def writeBufferWithOffset03(): Bool & Impure = {
        let e = [-1 as Int8; 10];
        let b = testBuffer();
        let r = writeBufferWithOffset(e, 10, 1, b);
        let a = readTestBuffer(b);
        Array.length(a) == 0 && Result.isErr(r)
    }

    def testBuffer(): OutputStream & Impure = {
        import new java.io.ByteArrayOutputStream(Int) as newByteArrayOuputStream;
        let stream = newByteArrayOuputStream(0);
        OutputStream(stream as ##java.io.OutputStream)
    }

    def readTestBuffer(s: OutputStream): Array[Int8] & Impure = {
        import java.io.ByteArrayOutputStream.toByteArray();
        let OutputStream(javaStream) = s;
        let byteArrayOutputStream = javaStream as ##java.io.ByteArrayOutputStream;
        byteArrayOutputStream.toByteArray()
    }

    // def resultEq(i: a, r: Result[a, e]): Bool = Result.getWithDefault(Result.map(v -> i == v, r), false)
}