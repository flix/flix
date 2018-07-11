type BufferedWriter = BufferedWriter(##java.io.BufferedWriter)

namespace BufferedWriter {

    ///
    /// Returns a new BufferedReader from the given Path `p`
    /// TODO: Consider choosing charsets and opening modes
    /// TODO: Currently this is opened for 'w' and truncates.
    pub def new(p: Path): BufferedWriter = unsafe {
        let Path(javaPath) = p;
        BufferedWriter(native method java.nio.file.Files.newBufferedWriter(javaPath, [| |] : Array[##java.nio.file.OpenOption]))
    }

    ///
    /// Effectfully closes the given BufferedWriter, flushing it first before freeing any resources used by it
    ///
    pub eff close[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `close`
    ///
    handler close[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a = unsafe {
        try {
            let BufferedWriter(javaWriter) = writer;
            let _ = native method java.io.BufferedWriter.close(javaWriter);
            k(Ok(()))
        }
        catch {
            case ex: ##java.io.IOException => k(Err(IOError("An unexpected IOException was thrown by BufferedWriter.close")))
        }
    }

    ///
    /// Effectfully flushes the given BufferedWriter
    ///
    pub eff flush[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `flush`
    ///
    handler flush[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a = unsafe {
        try {
            let BufferedWriter(javaWriter) = writer;
            let _ = native method java.io.BufferedWriter.flush(javaWriter);
            k(Ok(()))
        }
        catch {
            case ex: ##java.io.IOException => k(Err(IOError("An unexpected IOException was thrown by BufferedWriter.flush")))
        }
    }

    ///
    /// Effectfully writes a system dependent line separator using the BufferedWriter
    ///
    pub eff newLine[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `newLine`
    ///
    handler newLine[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a = unsafe {
        try {
            let BufferedWriter(javaWriter) = writer;
            let _ = native method java.io.BufferedWriter.newLine(javaWriter);
            k(Ok(()))
        }
        catch {
            case ex: ##java.io.IOException => k(Err(IOError("An unexpected IOException was thrown by BufferedWriter.newLine")))
        }
    }

    ///
    /// Effectfully writes a single character to the BufferedWriter
    /// TODO: Consider using char instead of int
    pub eff write[a](writer: BufferedWriter, c: Int32, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `write`
    /// TODO: Does not write before .flush called - should we do this automatically here?
    handler write[a](writer: BufferedWriter, c: Int32, k: Result[Unit, IOError] -> a): a = unsafe {
        try {
            let BufferedWriter(javaWriter) = writer;
            let _ = native method java.io.BufferedWriter.write(javaWriter, c);
            k(Ok(()))
        }
        catch {
            case ex: ##java.io.IOException => k(Err(IOError("An unexpected IOException was thrown by BufferedWriter.write")))
        }
    }

    ///
    /// Effectfully reads characters from the provided array starting from given offset
    /// Reads up to a provided number of characters and writes all characters read
    /// to the BufferedWriter
    ///
    pub eff writeFromBuffer[a](writer: BufferedWriter, array: Array[Char], offset: Int32, number: Int32, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `writeFromBuffer`
    ///
    handler writeFromBuffer[a](writer: BufferedWriter, array: Array[Char], offset: Int32, number: Int32, k: Result[Unit, IOError] -> a): a = ???

    ///
    /// Effectfully writes a provided string to the BufferedWriter
    ///
    pub eff writeString[a](writer: BufferedWriter, s: Str, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `writeString`
    ///
    handler writeString[a](writer: BufferedWriter, s: Str, k: Result[Unit, IOError] -> a): a = unsafe {
        try {
            let BufferedWriter(javaWriter) = writer;
            // TODO: Consider whether to let user determine offset below - requires signature change
            let _ = native method java.io.BufferedWriter.write(javaWriter, s : Str, 0, String.length(s));
            k(Ok(()))
        }
        catch {
            case ex: ##java.io.IOException => k(Err(IOError("An unexpected IOException was thrown by BufferedWriter.writeString")))
        }
    }

}
