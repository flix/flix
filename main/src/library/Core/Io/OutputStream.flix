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

use Core/Io/File.File;
use Core/Io/IOError.IOError;
use Core/Io/OutputStream.{OutputStream};

namespace Core/Io/OutputStream {
    pub opaque type OutputStream = ##java.io.OutputStream

    ///
    /// Opens the specified `File` into an `OutputStream`
    ///
    pub def new(f: File): Result[OutputStream, IOError] & Impure = {
        Core/Io/File.newOutputStream(f)
    }

    ///
    /// Writes a single byte `b` into the `s` stream.
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
