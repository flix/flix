type InputStream = InputStream(##java.io.InputStream)

namespace InputStream {

    // TODO: Uniqueness.

    ///
    /// Effectfully returns an input stream from the given path `s`.
    ///
    pub eff new(p: Path): Result[InputStream, IOError]

    ///
    /// Default handler for `new`.
    ///
    handler new[a](p: Path, k: Result[InputStream, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Effectfully returns an input stream from the given path `s` with the given open options `opts`.
    ///
    pub eff newWithOpts(p: Path, opts: Set[OpenOption]): Result[InputStream, IOError]

    ///
    /// Default handler for `newWithOpts`.
    ///
    handler newWithOpts[a](p: Path, k: Result[InputStream, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Effectfully reads a single character from the given input stream `is`.
    ///
    pub eff read(is: InputStream): Result[Char, IOError]

    ///
    /// Default handler for `read`.
    ///
    handler read[a](is: InputStream): a = ??? // TODO: Implement

    ///
    /// Effectfully reads a single line from the given input stream `is`.
    ///
    pub eff readLine(is: InputStream): Result[Option[Str], IOError]

    ///
    /// Default handler for `readLine`.
    ///
    handler readLine[a](is: InputStream): a = ??? // TODO: Implement

    ///
    /// Effectfully reads all lines from the given input stream `is`.
    ///
    pub eff readLines(is: InputStream): Result[List[Str], IOError]

    ///
    /// Default handler for `readLines`.
    ///
    handler readLines[a](is: InputStream): a = ??? // TODO: Implement

    ///
    /// Effectfully reads all lines from the given input stream `is` with the given charset `cs`.
    ///
    pub eff readLinesWithCharset[a](is: InputStream, cs: Charset, k: Result[List[Str], IOError] -> a): a

    ///
    /// Default handler for `readLinesWithCharset`.
    ///
    handler readLinesWithCharset[a](is: InputStream, cs: Charset, k: Result[List[Str], IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Effectfully reads all bytes of the given input stream `is` into a new array.
    ///
    pub eff readBytes(os: OutputStream): Result[(), IOError]

    ///
    /// Default handler for `readBytes`.
    ///
    handler readBytes[a](is: InputStream): a = ??? // TODO: Implement

    ///
    /// Effectfully pipes the given input stream `is` into the output stream `os`.
    ///
    pub eff pipe(os: OutputStream): Result[(), IOError]

    ///
    /// Default handler for `pipe`.
    ///
    handler pipe[a](os: OutputStream): a = ??? // TODO: Implement

    ///
    /// Effectfully closes the given input stream `is`.
    ///
    pub eff close(is: InputStream): Result[(), IOError]

    ///
    /// Default handler for `close`.
    ///
    handler close[a](is: InputStream): a = ??? // TODO: Implement

}
