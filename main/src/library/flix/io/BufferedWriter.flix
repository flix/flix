type BufferedWriter = BufferedWriter(##java.io.BufferedWriter)

namespace BufferedWriter {

    ///
    /// Returns a new BufferedReader from the given Path `p`
    /// TODO: Consider choosing charsets and opening modes
    pub def new(p: Path): BufferedWriter = ???

    ///
    /// Effectfully closes the given BufferedWriter, flushing it first before freeing any resources used by it
    ///
    pub eff close[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `close`
    ///
    handler close[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a = ???

    ///
    /// Effectfully flushes the given BufferedWriter
    ///
    pub eff flush[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `flush`
    ///
    handler flush[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a = ???

    ///
    /// Effectfully writes a system dependent line separator using the BufferedWriter
    ///
    pub eff newLine[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `newLine`
    ///
    handler newLine[a](writer: BufferedWriter, k: Result[Unit, IOError] -> a): a = ???

    ///
    /// Effectfully writes a single character to the BufferedWriter
    /// TODO: Consider using char instead of int
    pub eff write[a](writer: BufferedWriter, c: Int32, k: Result[Unit, IOError] -> a): a

    ///
    /// Default handler for `write`
    ///
    handler write[a](writer: BufferedWriter, c: Int32, k: Result[Unit, IOError] -> a): a = ???

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
    handler writeString[a](writer: BufferedWriter, s: Str, k: Result[Unit, IOError] -> a): a = ???

}
