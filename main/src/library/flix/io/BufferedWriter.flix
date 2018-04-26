type BufferedWriter = BufferedWriter(##java.io.BufferedWriter)

namespace BufferedWriter {

    ///
    /// Returns a new BufferedReader from the given Path `p`
    /// TODO: Consider choosing charsets and opening modes
    pub def new(p: Path): BufferedWriter = ???

    ///
    /// Closes the given BufferedWriter, flushing it first before freeing any resources used by it
    ///
    pub def close(writer: BufferedWriter): Unit = ???

    ///
    /// Flushes the given BufferedWriter
    ///
    pub def flush(writer: BufferedWriter): Unit = ???

    ///
    /// Writes a system dependent line separator using the BufferedWriter
    ///
    pub def newLine(writer: BufferedWriter): Unit = ???

    ///
    /// Writes a single character to the BufferedWriter
    /// TODO: Consider using char instead of int
    pub def write(writer: BufferedWriter, c: Int32): Unit = ???

    ///
    /// Reads characters from the provided array starting from given offset
    /// Reads up to a provided number of characters and writes all characters read
    /// to the BufferedWriter
    ///
    pub def writeFromBuffer(writer: BufferedWriter, array: Array[Char], offset: Int32, number: Int32): Unit = ???

    ///
    /// Writes a provided string to the BufferedWriter
    ///
    pub def writeString(writer: BufferedWriter, s: Str): Unit = ???

}
