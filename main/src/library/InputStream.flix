namespace InputStream {
    ///
    /// Wraps the `java.io.InputStream.available()` method that returns the number of available bytes
    ///
    /// See the Java documentation for more details
    ///
    pub def available(stream: InputStream): Result[Int32, Error] & Impure = try {
        import java.io.InputStream.available();

        let InputStream(javaStream) = stream;

        Ok(javaStream.available())
    } catch {
        case _: ##java.io.IOException => Err(IOException)
    }

    ///
    /// Wraps the `java.io.InputStream.read()` method that reads a single byte from the stream.
    ///
    /// See the Java documentation for more details
    ///
    pub def readByte(stream: InputStream): Result[Int32, Error] & Impure = try {
        import java.io.InputStream.read();

        let InputStream(javaStream) = stream;

        Ok(javaStream.read())
    } catch {
        case _: ##java.io.IOException => Err(IOException)
    }

    ///
    /// Wraps the `java.io.InputStream.read(byte[])` method that reads a number of bytes from the stream.
    ///
    /// See the Java documentation for more details
    ///
    pub def readBuffer(buffer: Array[Int8], stream: InputStream): Result[Int32, Error] & Impure = try {
        import java.io.InputStream.read(Array[Int8]);

        let InputStream(javaStream) = stream;

        Ok(javaStream.read(buffer))
    } catch {
        case _: ##java.io.IOException => Err(IOException),
        case _: ##java.lang.NullPointerException => Err(NullPointerException)
    }

    ///
    /// Wraps the `java.io.InputStream.read(byte[], int, int)` method that reads a specified number of bytes from the stream.
    ///
    /// See the Java documentation for more details
    ///
    pub def readWithLength(buffer: Array[Int8], offset: Int32, length: Int32, stream: InputStream): Result[Int32, Error] & Impure = try {
        import java.io.InputStream.read(Array[Int8], Int32, Int32);

        let InputStream(javaStream) = stream;
        let readBytes = javaStream.read(buffer, offset, length);

        Ok(readBytes)
    } catch {
        case _: ##java.io.IOException => Err(IOException)
        case _: ##java.lang.NullPointerException => Err(NullPointerException)
        case _: ##java.lang.IndexOutOfBoundsException => Err(IndexOutOfBoundsException)
    }

    ///
    /// Wraps the `java.io.InputStream.close()` method that frees the resource.
    ///
    /// See the Java documentation for more details
    ///
    pub def close(stream: InputStream): Result[Unit, Error] & Impure = try {
        import java.io.InputStream.close();
        let InputStream(javaStream) = stream;
        Ok(javaStream.close())
    } catch {
        case _: ##java.io.IOException => Err(IOException)
    }

    ///
    /// Represents an error when operating on an InputStream
    ///
    pub enum Error {
        case IOException
        case NullPointerException
        case IndexOutOfBoundsException
    }
}

pub opaque type InputStream = ##java.io.InputStream
