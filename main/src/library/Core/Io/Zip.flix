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

namespace Core/Io/Zip {
    ///
    /// Represents an error when operating on a Zip file
    ///
    pub enum Error {
        case IOException
        case ZipException
        case IllegalStateException
        case NoSuchElementException
    }

    pub opaque type Zip = ##java.util.zip.ZipFile

    ///
    /// Constructs a `Zip` from a `Path`
    ///
    pub def new(path: Path): Result[Zip, Error] & Impure = try {
        import new java.util.zip.ZipFile(##java.io.File) as newZipFile;

        let Path(javaFile) = path;
        let zipFile = newZipFile(javaFile);
        Ok(Zip(zipFile))
    } catch {
        case _: ##java.io.IOException => Err(IOException)
        case _: ##java.util.zip.ZipException => Err(ZipException)
    }

    ///
    /// Reads all entries in `z` into `InputStream`s
    ///
    pub def entries(z: Zip): Result[List[InputStream], Error] & Impure = try {
        import java.util.Enumeration.hasMoreElements();
        import java.util.Enumeration.nextElement();
        import java.util.zip.ZipFile.entries();
        import java.util.zip.ZipFile.getInputStream(##java.util.zip.ZipEntry);

        use Core/Io/InputStream.InputStream.InputStream;

        let Zip(javaZip) = z;
        let e = entries(javaZip);
        let step = () -> {
            if (!e.hasMoreElements()) {
                None
            } else {
                let entry = e.nextElement() as ##java.util.zip.ZipEntry;
                let entryStream = javaZip.getInputStream(entry);
                Some(InputStream(entryStream))
            }
        };

        Ok(List.unfoldWithIter(step))
    } catch {
        case _: ##java.lang.IllegalStateException => Err(IllegalStateException)
        case _: ##java.util.zip.ZipException => Err(ZipException)
        case _: ##java.io.IOException => Err(IOException)
        case _: ##java.util.NoSuchElementException => Err(NoSuchElementException)
    }

    ///
    /// Closes `z` and frees the resource
    ///
    pub def close(z: Zip): Result[Unit, Error] & Impure = try {
        import java.util.zip.ZipFile.close();

        let Zip(javaZip) = z;

        Ok(javaZip.close())
    } catch {
        case _: ##java.lang.IllegalStateException => Err(IllegalStateException)
    }
}