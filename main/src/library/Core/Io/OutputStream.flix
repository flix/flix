use Core/Io/IOError.IOError;

namespace Core/Io/OutputStream {
    pub opaque type OutputStream = ##java.io.OutputStream

    ///
    /// Writes a single byte into the `s` stream.
    ///
    pub def writeByte(b: Int8, s: OutputStream): Result[Unit, IOError] & Impure = try {
        import java.io.OutputStream.write(Int32);
        let OutputStream(javaStream) = s;
        javaStream.write(b as Int32);
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("An I/O error occurred in Core/Io/OutputStream.writeByte"))
    }

    ///
    /// Writes the buffer `b` into the `s` stream.
    ///
    pub def writeBuffer(b: Array[Int8], s: OutputStream): Result[Unit, IOError] & Impure = try {
        import java.io.OutputStream.write(Array[Int8]);
        let OutputStream(javaStream) = s;
        javaStream.write(b);
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("An I/O error occurred in Core/Io/OutputStream.writeBuffer"))
    }

    ///
    /// Writes `l` elements of the buffer `b` into the `s` stream, starting from index `o` in the buffer.
    ///
    pub def writeBufferWithOffset(b: Array[Int8], o: Int32, l: Int32, s: OutputStream): Result[Unit, IOError] & Impure = try {
        import java.io.OutputStream.write(Array[Int8], Int32, Int32);
        let OutputStream(javaStream) = s;
        javaStream.write(b, o, l);
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("An I/O error occurred in Core/Io/OutputStream.writeBufferWithOffset"))
        case _: ##java.lang.IndexOutOfBoundsException => Err(IOError.IncorrectBufferSize)
    }

    ///
    /// Sends all bytes to the underlying resource of `s`.
    ///
    pub def flush(s: OutputStream): Result[Unit, IOError] & Impure = try {
        import java.io.OutputStream.flush();
        let OutputStream(javaStream) = s;
        javaStream.flush();
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("An I/O error occurred in Core/Io/OutputStream.flush"))
    }

    ///
    /// Closes the underlying resource of `s`.
    ///
    pub def close(s: OutputStream): Result[Unit, IOError] & Impure = try {
        import java.io.OutputStream.close();
        let OutputStream(javaStream) = s;
        javaStream.close();
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("An I/O error occurred in Core/Io/OutputStream.close"))
    }
}