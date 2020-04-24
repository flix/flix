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

use Core/Io/IOError.IOError;

namespace Core/Io/InputStream {
    pub opaque type InputStream = ##java.io.InputStream

    pub def open(p: Path): Result[InputStream, IOError] & Impure = try {
        import new java.io.FileInputStream(##java.io.File) as newFileInputStream;
        let Path(javaFile) = p;
        let inputStream = newFileInputStream(javaFile);
        Ok(InputStream(inputStream as ##java.io.InputStream))
    } catch {
        case _: ##java.io.FileNotFoundException => Err(IOError.FileNotFound(p))
    }

    ///
    /// Creates an `InputStream` from `b`.
    ///
    pub def fromBuffer(b: Array[Int8]): InputStream & Impure = {
        import new java.io.ByteArrayInputStream(Array[Int8]) as newByteArrayInputStream;
        let byteStream = newByteArrayInputStream(b);
        InputStream(byteStream as ##java.io.InputStream)
    }

    ///
    /// Returns the number of bytes available in `s`.
    ///
    pub def available(s: InputStream): Result[Int32, IOError] & Impure = try {
        import java.io.InputStream.available();
        let InputStream(javaStream) = s;
        Ok(javaStream.available())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("I/O Error in Core/Io/InputStream.available"))
    }

    ///
    /// Reads a single byte from `s`. If the end of the stream is reached `-1` is returned.
    ///
    pub def readByte(s: InputStream): Result[Int32, IOError] & Impure = try {
        import java.io.InputStream.read();
        let InputStream(javaStream) = s;
        Ok(javaStream.read())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("I/O Error in Core/Io/InputStream.readByte"))
    }

    ///
    /// Reads a number of bytes from `s` into `b`. Returns the number of bytes read. -1 is returned if no bytes are available.
    ///
    pub def readBuffer(b: Array[Int8], s: InputStream): Result[Int32, IOError] & Impure = try {
        import java.io.InputStream.read(Array[Int8]);
        let InputStream(javaStream) = s;
        let readBytes = javaStream.read(b);
        Ok(readBytes)
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("I/O Error in Core/Io/InputStream.readBuffer"))
    }

    ///
    /// Reads a number of bytes from `s` into `b`, starting at index `o`, reading at most `l` bytes.
    /// Returns the number of bytes read. -1 is returned if no bytes are available.
    ///
    pub def readBufferWithOffset(b: Array[Int8], o: Int32, l: Int32, s: InputStream): Result[Int32, IOError] & Impure = try {
        import java.io.InputStream.read(Array[Int8], Int32, Int32);
        let InputStream(javaStream) = s;
        let readBytes = javaStream.read(b, o, l);
        Ok(readBytes)
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("I/O Error in Core/Io/InputStream.readBufferWithLength"))
        case _: ##java.lang.IndexOutOfBoundsException => Err(IOError.IncorrectBufferSize)
    }

    ///
    /// Read the whole stream into a byte array
    ///
    pub def readFully(s: InputStream): Result[Array[Int8], IOError] & Impure = {
        let step = () -> {
            let arr = [0 as Int8; 1024];
            Result.map(r -> if (r <= 0) None else Some(arr[0..r]), readBuffer(arr, s))
        };
        List.unfoldWithOkIter(step)
            |> List.sequenceResult
            |> Result.map(List.fold((acc, elem) -> Array.append(acc, elem), []))
    }

    ///
    /// Closes `s` and frees the resource.
    ///
    pub def close(stream: InputStream): Result[Unit, IOError] & Impure = try {
        import java.io.InputStream.close();
        let InputStream(javaStream) = stream;
        Ok(javaStream.close())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("I/O Error in Core/Io/InputStream.close"))
    }
}


