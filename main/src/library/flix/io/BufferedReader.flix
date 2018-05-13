type BufferedReader = BufferedReader(##java.io.BufferedReader)

namespace BufferedReader {

    ///
    /// Returns a new BufferedReader from the given Path `p`
    /// TODO: Consider choosing charsets and opening modes
    pub def new(p: Path): BufferedReader = unsafe {
        let Path(javaPath) = p;
        BufferedReader(native method java.nio.file.Files.newBufferedReader(javaPath))
        // TODO: This throws IOException when file does not exist. Do we create it?
    }

    ///
    /// Effectfully closes the given BufferedReader, freeing any resources used by it
    ///
    pub eff close[a](reader: BufferedReader, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `close`
    ///
    handler close[a](reader: BufferedReader, k: Result[Unit, IOError] -> a): a = ???

    ///
    /// Effectfully marks the present position in the stream, for future calls to reset()
    ///
    pub eff mark[a](reader: BufferedReader, readAheadLimit: Int, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `mark`
    ///
    handler mark[a](reader: BufferedReader, readAheadLimit: Int, k: Result[Unit, IOError] -> a): a = ???

    ///
    /// Effectfully reads a single character from the stream and returns it as an integer
    /// Returns -1 if end-of-file is the read character
    /// TODO: Consider returning read character as a char instead
    pub eff read[a](reader: BufferedReader, k: Result[Int32, IOError] -> a): a

    ///
    /// Default handler for `read`
    ///
    handler read[a](reader: BufferedReader, k: Result[Int32, IOError] -> a): a = ???

    ///
    /// Effectfully reads a specified number of characters into the provided array from a given offset
    /// Returns -1 if the first character encountered is end-of-file.
    /// Otherwise reads until end-of-file encountered
    ///
    pub eff readIntoBuffer[a](reader: BufferedReader, array: Array[Char], offset: Int32, number: Int32, k: Result[Int32, IOError] -> a): a

    ///
    /// Default handler for `readIntoBuffer`
    ///
    handler readIntoBuffer[a](reader: BufferedReader, array: Array[Char], offset: Int32, number: Int32, k: Result[Int32, IOError] -> a): a = ???

    ///
    /// Effectfully reads an entire line from the BufferedReader. Returns None if end-of-file is reached
    ///
    pub eff readLine[a](reader: BufferedReader, k: Result[Option[Str], IOError] -> a): a

    ///
    /// Default handler for `readLine`
    ///
    handler readLine[a](reader: BufferedReader, k: Result[Option[Str], IOError] -> a): a = unsafe {
        try {
            let BufferedReader(javaReader) = reader;
            let readString = native method java.io.BufferedReader.readLine(javaReader);
            if (native method java.util.Objects.isNull(readString))
                k(Ok(None))
            else
                k(Ok(Some(readString)))
        }
        catch {
            case ex: ##java.io.IOException => k(Err(IOError("An unexpected IOEXception was thrown by BufferedReader.readLine")))
        }
    }

    ///
    /// Effectfully tells whether the BufferedReader is ready to be read from
    ///
    pub eff ready[a](reader: BufferedReader, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `ready`
    ///
    handler ready[a](reader: BufferedReader, k: Result[Bool, IOError] -> a): a = unsafe {
        try {
            let BufferedReader(javaReader) = reader;
            k(Ok(native method java.io.BufferedReader.ready(javaReader)))
        }
        catch {
            case ex: ##java.io.IOException => k(Err(IOError("An unexpected IOEXception was thrown by BufferedReader.ready")))
        }
    }

    ///
    /// Effectfully resets the BufferedReader to the most recent mark, if any where set and not invalidated
    ///
    pub eff reset[a](reader: BufferedReader, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `reset`
    ///
    handler reset[a](reader: BufferedReader, k: Result[Unit, IOError] -> a): a = ???

    ///
    /// Effectfully skips n characters ahead in the provided BufferedReader. Returns the amount of characters actually skipped
    ///
    pub eff skip[a](reader: BufferedReader, n: Int32, k: Result[Int64, IOError] -> a): a

    ///
    /// Default handler for `skip`
    ///
    handler skip[a](reader: BufferedReader, n: Int32, k: Result[Int64, IOError] -> a): a = unsafe {
        try {
            let BufferedReader(javaReader) = reader;
            k(Ok(native method java.io.BufferedReader.skip(javaReader, n)))
        }
        catch {
            case ex: ##java.io.IOException => k(Err(IOError("An unexpected IOEXception was thrown by BufferedReader.skip")))
        }
    }
}
