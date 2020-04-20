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

use Core/Io/InputStream.InputStream;
use Core/Io/IOError.IOError;

namespace Core/Io/Zip {
    pub opaque type Zip = ##java.util.zip.ZipFile

    pub type alias ZipEntry = {
        comment: String,
        compressedSize: Int64,
        crc: Int64,
        // creationTime: ##java.nio.file.attribute.FileTime
        extra: Array[Int8],
        // lastAccessTime: ##java.nio.file.attribute.FileTime
        // lastModifiedTime: ##java.nio.file.attribute.FileTime
        method: Int32, // TODO: enum?
        name: String,
        size: Int64,
        time: Int64,
        isDirectory: Bool,
        stream: InputStream
    }

    ///
    /// Constructs a `Zip` from a `Path`.
    ///
    pub def new(path: Path): Result[Zip, IOError] & Impure = try {
        import new java.util.zip.ZipFile(##java.io.File) as newZipFile;
        let Path(javaFile) = path;
        let zipFile = newZipFile(javaFile);
        Ok(Zip(zipFile))
    } catch {
        case _: ##java.io.IOException => Err(IOError.IOException)
        case _: ##java.util.zip.ZipException => Err(IOError.ZipException)
    }

    ///
    /// Reads all entries in `z` into `InputStream`s.
    ///
    pub def entries(z: Zip): Result[List[ZipEntry], IOError] & Impure = try {
        import java.util.Enumeration.hasMoreElements();
        import java.util.Enumeration.nextElement();
        import java.util.zip.ZipFile.entries();
        import java.util.zip.ZipFile.getInputStream(##java.util.zip.ZipEntry);
        import java.util.zip.ZipEntry.getComment();
        import java.util.zip.ZipEntry.getCompressedSize();
        import java.util.zip.ZipEntry.getCrc();
        import java.util.zip.ZipEntry.getExtra();
        import java.util.zip.ZipEntry.getMethod();
        import java.util.zip.ZipEntry.getName();
        import java.util.zip.ZipEntry.getSize();
        import java.util.zip.ZipEntry.getTime();
        import java.util.zip.ZipEntry.isDirectory();

        use Core/Io/InputStream.InputStream.InputStream;
        let Zip(javaZip) = z;
        let e = entries(javaZip);
        let step = () -> {
            if (!e.hasMoreElements()) {
                None
            } else {
                let entry = e.nextElement() as ##java.util.zip.ZipEntry;
                let entryStream = javaZip.getInputStream(entry);
                let entryRec = {
                    comment = entry.getComment(),
                    compressedSize = entry.getCompressedSize(),
                    crc = entry.getCrc(),
                    extra = entry.getExtra(),
                    method = entry.getMethod(),
                    name = entry.getName(),
                    size = entry.getSize(),
                    time = entry.getTime(),
                    isDirectory = entry.isDirectory(),
                    stream = InputStream(entryStream)
                };

                Some(entryRec)
            }
        };
        Ok(List.unfoldWithIter(step))
    } catch {
        case _: ##java.lang.IllegalStateException => Err(IOError.IllegalStateException)
        case _: ##java.util.zip.ZipException => Err(IOError.ZipException)
        case _: ##java.io.IOException => Err(IOError.IOException)
        case _: ##java.util.NoSuchElementException => Err(IOError.NoSuchElementException)
    }

    ///
    /// Closes `z` and frees the resource.
    ///
    pub def close(z: Zip): Result[Unit, IOError] & Impure = try {
        import java.util.zip.ZipFile.close();
        let Zip(javaZip) = z;
        Ok(javaZip.close())
    } catch {
        case _: ##java.lang.IllegalStateException => Err(IOError.IllegalStateException)
    }
}