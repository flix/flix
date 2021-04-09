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

use Core/Io/OutputStream.OutputStream;
use Core/Io/OutputStream.OutputStream.OutputStream;
use Core/Io/IOError.IOError;
use Result.flatMap;

namespace Core/Io/ZipOutput {
    pub opaque type ZipOutput = ##java.util.zip.ZipOutputStream

    ///
    /// Constructs a `ZipOutput` from an `OutputStream`
    ///
    pub def new(s: OutputStream): ZipOutput & Impure = {
        import new java.util.zip.ZipOutputStream(##java.io.OutputStream) as newZipOutputStream;
        let OutputStream(javaStream) = s;
        let zipOutput = newZipOutputStream(javaStream);
        ZipOutput(zipOutput)
    }

    ///
    /// Writes `a` to the entry specified by `n` in the `z` `ZipOutput`
    ///
    pub def write(n: String, a: Array[Int8], z: ZipOutput): Result[Unit, IOError] & Impure = try {
        import new java.util.zip.ZipEntry(String) as newEntry;
        import java.util.zip.ZipOutputStream.putNextEntry(##java.util.zip.ZipEntry);
        import java.util.zip.ZipOutputStream.closeEntry();
        use Core/Io/OutputStream.writeBuffer;
        let ZipOutput(javaOutput) = z;
        let outputStream = OutputStream(javaOutput as ##java.io.OutputStream);
        let entry = newEntry(n);
        javaOutput.putNextEntry(entry);
        let r = writeBuffer(a, outputStream);
        javaOutput.closeEntry();
        let* _ = r;
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("I/O error occurred in Core/Io/ZipOutput.write"))
        case _: ##java.util.zip.ZipException => Err(IOError.MalformedZip)
    }

    ///
    /// Closes the `z` `ZipOutput` file
    ///
    pub def close(z: ZipOutput): Result[Unit, IOError] & Impure = try {
        import java.util.zip.ZipOutputStream.close();
        let ZipOutput(javaOutput) = z;
        javaOutput.close();
        Ok(())
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("I/O error occurred in Core/Io/ZipOutput.close"))
    }
}
