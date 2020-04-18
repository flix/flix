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

namespace Core/Io/InputStream {
    pub opaque type InputStream = ##java.io.InputStream

    ///
    /// Represents an error when operating on an InputStream
    ///
    pub enum Error {
        case IOException
        case IndexOutOfBoundsException
    }

    ///
    /// Creates an `InputStream` from `b`
    ///
    pub def fromBuffer(b: Array[Int8]): InputStream & Impure = {
        import new java.io.ByteArrayInputStream(Array[Int8]) as newByteArrayInputStream;

        let byteStream = newByteArrayInputStream(b);

        InputStream(byteStream as ##java.io.InputStream)
    }

    ///
    /// Returns the number of bytes available in `s`
    ///
    pub def available(s: InputStream): Result[Int32, Error] & Impure = try {
        import java.io.InputStream.available();

        let InputStream(javaStream) = s;

        Ok(javaStream.available())
    } catch {
        case _: ##java.io.IOException => Err(IOException)
    }

    ///
    /// Reads a single byte from `s`. If the end of the stream is reached `-1` is returned
    ///
    pub def readByte(s: InputStream): Result[Int32, Error] & Impure = try {
        import java.io.InputStream.read();

        let InputStream(javaStream) = s;

        Ok(javaStream.read())
    } catch {
        case _: ##java.io.IOException => Err(IOException)
    }

    ///
    /// Reads a number of bytes from `s` into `b`. Returns the number of bytes read. -1 is returned if no bytes are available
    ///
    pub def readBuffer(b: Array[Int8], s: InputStream): Result[Int32, Error] & Impure = try {
        import java.io.InputStream.read(Array[Int8]);

        let InputStream(javaStream) = s;

        Ok(javaStream.read(b))
    } catch {
        case _: ##java.io.IOException => Err(IOException)
    }

    ///
    /// Reads a number of bytes from `s` into `b`, starting at index `o`, reading at most `l` bytes.
    /// Returns the number of bytes read. -1 is returned if no bytes are available.
    ///
    pub def readBufferWithLength(b: Array[Int8], o: Int32, l: Int32, s: InputStream): Result[Int32, Error] & Impure = try {
        import java.io.InputStream.read(Array[Int8], Int32, Int32);

        let InputStream(javaStream) = s;
        let readBytes = javaStream.read(b, o, l);

        Ok(readBytes)
    } catch {
        case _: ##java.io.IOException => Err(IOException)
        case _: ##java.lang.IndexOutOfBoundsException => Err(IndexOutOfBoundsException)
    }

    ///
    /// Closes `s` and frees the resource
    ///
    pub def close(stream: InputStream): Result[Unit, Error] & Impure = try {
        import java.io.InputStream.close();

        let InputStream(javaStream) = stream;

        Ok(javaStream.close())
    } catch {
        case _: ##java.io.IOException => Err(IOException)
    }
}


