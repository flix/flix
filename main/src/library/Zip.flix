namespace Zip {
    ///
    /// Wraps the construction of a `Zip` from a `Path`
    ///
    /// See the Java documentation for `java.util.zip.ZipFile` for more details
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
    /// Reads all entries in the `Zip` into `InputStream`s
    ///
    /// See the Java documentation for `java.util.zip.ZipFile` for more details
    ///
    pub def entries(zip: Zip): Result[List[InputStream], Error] & Impure = try {
        import java.util.Enumeration.hasMoreElements();
        import java.util.Enumeration.nextElement();
        import java.util.zip.ZipFile.entries();
        import java.util.zip.ZipFile.getInputStream(##java.util.zip.ZipEntry);

        let Zip(javaZip) = zip;
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
    /// Closes the `Zip` and frees the resource
    ///
    /// See the Java documentation for `java.util.zip.ZipFile` for more details
    ///
    pub def close(zip: Zip): Result[Unit, Error] & Impure = try {
        import java.util.zip.ZipFile.close();

        let Zip(javaZip) = zip;

        Ok(javaZip.close())
    } catch {
        case _: ##java.lang.IllegalStateException => Err(IllegalStateException)
    }

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
}