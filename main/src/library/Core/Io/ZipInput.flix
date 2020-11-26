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
use Core/Io/InputStream.InputStream;
use Core/Io/IOError.IOError;
use Result.flatMap;

namespace Core/Io/ZipInput {
    pub opaque type ZipInput = ##java.util.zip.ZipFile

    ///
    /// An entry in a `ZipInput`.
    ///
    pub type alias ZipEntry = {
        name: String,
        isDirectory: Bool,
        stream: InputStream
    }

    ///
    /// Constructs a `ZipInput` from a `Path`.
    ///
    pub def new(f: File): Result[ZipInput, IOError] & Impure = try {
        import new java.util.zip.ZipFile(##java.io.File) as newZipFile;
        let* e = Core/Io/File.exists(f);
        let* javaFile = if (e) {
            let File.File(javaFile) = f;
            Ok(javaFile)
        } else {
            Err(IOError.FileNotFound(f))
        };
        let zipFile = newZipFile(javaFile);
        Ok(ZipInput(zipFile))
    } catch {
        case _: ##java.io.IOException => Err(IOError.Generic("I/O Error in Core/Io/Zip.new"))
        case _: ##java.util.zip.ZipException => Err(IOError.MalformedZip)
    }

    ///
    /// Get the `##java.io.InputStream` for a given `##java.util.zip.ZipEntry` in a `##java.util.zip.ZipFile`.
    ///
    /// This is a workaround for try-catch blocks being buggy!
    ///
    def entriesTryCatchHelper(z: ##java.util.zip.ZipFile, entry: ##java.util.zip.ZipEntry): Result[##java.io.InputStream, IOError] & Impure= try {
        import java.util.zip.ZipFile.getInputStream(##java.util.zip.ZipEntry);
            Ok(z.getInputStream(entry) as ##java.io.InputStream)
        } catch {
            case _: ##java.util.NoSuchElementException => Err(IOError.ElementNotFound("Couldn't find entry"))
        }

    ///
    /// Reads all entries in `z` into a list of `ZipEntry`.
    ///
    pub def entries(z: ZipInput): Result[List[ZipEntry], IOError] & Impure = try {
        import java.util.Enumeration.hasMoreElements();
        import java.util.Enumeration.nextElement();
        import java.util.zip.ZipFile.entries();
        import java.util.zip.ZipEntry.getName();
        import java.util.zip.ZipEntry.isDirectory();

        use Core/Io/InputStream.InputStream.InputStream;
        let ZipInput(javaZip) = z;
        let e = entries(javaZip);
        let step = () -> {
            if (not e.hasMoreElements()) {
                Ok(None)
            } else {
                let entry = e.nextElement() as ##java.util.zip.ZipEntry;
                let* es = entriesTryCatchHelper(javaZip, entry);
                let entryRec = {
                    name = entry.getName(),
                    isDirectory = entry.isDirectory(),
                    stream = InputStream(es)
                };
                Ok(Some(entryRec))
            }
        };
        List.unfoldWithOkIter(step)
    } catch {
        case _: ##java.util.zip.ZipException => Err(IOError.MalformedZip)
        case _: ##java.lang.IllegalStateException => Err(IOError.IllegalState)
        case _: ##java.io.IOException => Err(IOError.Generic("I/O Exception in Core/Io/Zip.entries"))
    }

    ///
    /// Closes `z` and frees the resource.
    ///
    pub def close(z: ZipInput): Result[Unit, IOError] & Impure = try {
        import java.util.zip.ZipFile.close();
        let ZipInput(javaZip) = z;
        Ok(javaZip.close())
    } catch {
        case _: ##java.lang.IllegalStateException => Err(IOError.IllegalState)
    }
}