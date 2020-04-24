use Core/Io/IOError.IOError;

namespace Core/Io/OutputStream {
    pub opaque type OutputStream = ##java.io.OutputStream

    pub def open(p: Path): Result[OutputStream, IOError] & Impure = try {
        import new java.io.FileOutputStream(##java.io.File) as newFileOutputStream;
        let Path(javaFile) = p;
        let outputStream = newFileOutputStream(javaFile);
        Ok(OutputStream(outputStream as ##java.io.OutputStream))
    } catch {
        case _: ##java.io.FileNotFoundException => Err(IOError.FileNotFound(p))
    }

    pub def writeByte(b: Int8, s: OutputStream): Result[Unit, IOError] & Impure = try {
        import java.io.OutputStream.write(Int32);
        let OutputStream(javaStream) = s;
        javaStream.write(b as Int32);
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("An I/O error occurred in Core/Io/OutputStream.writeByte"))
    }

    pub def writeBuffer(b: Array[Int8], s: OutputStream): Result[Unit, IOError] & Impure = try {
        import java.io.OutputStream.write(Array[Int8]);
        let OutputStream(javaStream) = s;
        javaStream.write(b);
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("An I/O error occurred in Core/Io/OutputStream.writeBuffer"))
    }

    pub def writeBufferWithOffset(b: Array[Int8], o: Int32, l: Int32, s: OutputStream): Result[Unit, IOError] & Impure = try {
        import java.io.OutputStream.write(Array[Int8], Int32, Int32);
        let OutputStream(javaStream) = s;
        javaStream.write(b, o, l);
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("An I/O error occurred in Core/Io/OutputStream.writeBufferWithOffset"))
        case _: ##java.lang.IndexOutOfBoundsException => Err(IOError.IncorrectBufferSize)
    }

    pub def flush(s: OutputStream): Result[Unit, IOError] & Impure = try {
        import java.io.OutputStream.flush();
        let OutputStream(javaStream) = s;
        javaStream.flush();
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("An I/O error occurred in Core/Io/OutputStream.flush"))
    }

    pub def close(s: OutputStream): Result[Unit, IOError] & Impure = try {
        import java.io.OutputStream.close();
        let OutputStream(javaStream) = s;
        javaStream.close();
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("An I/O error occurred in Core/Io/OutputStream.close"))
    }
}