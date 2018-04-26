type BufferedReader = BufferedReader(##java.io.BufferedReader)

namespace BufferedReader {

    ///
    /// Returns a new BufferedReader from the given Path `p`
    /// TODO: Consider choosing charsets and opening modes
    pub def new(p: Path): BufferedReader = ???

    ///
    /// Closes the given BufferedReader, freeing any resources used by it
    ///
    pub def close(reader: BufferedReader): Unit = ???


    ///
    /// Marks the present position in the stream, for future calls to reset()
    ///
    pub def mark(reader: BufferedReader, readAheadLimit: Int): Unit = ???

    ///
    /// Reads a single character from the stream and returns it as an integer
    /// Returns -1 if end-of-file is the read character
    /// TODO: Consider returning read character as a char instead
    pub def read(reader: BufferedReader): Int32 = ???

    ///
    /// Reads a specified number of characters into the provided array from a given offset
    /// Returns -1 if the first character encountered is end-of-file.
    /// Otherwise reads until end-of-file encountered
    ///
    pub def readIntoBuffer(reader: BufferedReader, array: Array[Char], offset: Int32, number: Int32): Int32 = ???

    ///
    /// Reads an entire line from the BufferedReader. Returns None if end-of-file is reached
    ///
    pub def readLine(reader: BufferedReader): Option[Str] = ???

    ///
    /// Tells whether the BufferedReader is ready to be read from
    ///
    pub def ready(reader: BufferedReader): Bool = ???

    ///
    /// Resets the BufferedReader to the most recent mark, if any where set and not invalidated
    ///
    pub def reset(reader: BufferedReader): Unit = ???

    ///
    /// Skips n characters ahead in the provided BufferedReader. Returns the amount of characters actually skipped
    ///
    pub def skip(reader: BufferedReader, n: Int64): Int64 = ???

}
