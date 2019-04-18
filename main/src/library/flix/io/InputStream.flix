pub type InputStream = InputStream(##java.io.InputStream)

namespace InputStream {

    // TODO: Uniqueness.

    ///
    /// Effectfully returns an input stream from the given path `s`.
    /// Use path newInputStream to implement this
    pub eff new[a](p: Path, k: Result[InputStream, IOError] -> a): a

    ///
    /// Default handler for `new`.
    ///
    handler new[a](p: Path, k: Result[InputStream, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Effectfully returns an input stream from the given path `s` with the given open options `opts`.
    ///
    pub eff newWithOpts[a](p: Path, k: Result[InputStream, IOError] -> a): a

    ///
    /// Default handler for `newWithOpts`.
    ///
    handler newWithOpts[a](p: Path, k: Result[InputStream, IOError] -> a): a = ???

    ///
    /// Effectfully reads a single character from the given input stream `is`.
    ///
    pub eff read[a](is: InputStream, k: Result[Char, IOError] -> a): a

    ///
    /// Default handler for `read`.
    ///
    handler read[a](is: InputStream, k: Result[Char, IOError] -> a): a = ???

    ///
    /// Effectfully reads a single line from the given input stream `is`.
    ///
    pub eff readLine[a](is: InputStream, k: Result[Option[Str], IOError] -> a): a

    ///
    /// Default handler for `readLine`.
    ///
    handler readLine[a](is: InputStream, k: Result[Option[Str], IOError] -> a): a = ???

    ///
    /// Effectfully reads all lines from the given input stream `is`.
    ///
    pub eff readLines[a](is: InputStream, k: Result[List[Str], IOError] -> a): a

    ///
    /// Default handler for `readLines`.
    ///
    handler readLines[a](is: InputStream, k: Result[List[Str], IOError] -> a): a = ???

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
    pub eff readBytes[a](is: InputStream, k: Result[(), IOError] -> a): a

    ///
    /// Default handler for `readBytes`.
    ///
    handler readBytes[a](is: InputStream, k: Result[(), IOError] -> a): a = ???

    ///
    /// Effectfully pipes the given input stream `is` into the output stream `os`.
    ///
    pub eff pipe[a](in: InputStream, os: OutputStream, k: Result[(), IOError] -> a): a

    ///
    /// Default handler for `pipe`.
    ///
    handler pipe[a](in: InputStream, os: OutputStream, k: Result[(), IOError] -> a): a = ???

    ///
    /// Effectfully closes the given input stream `is`.
    ///
    pub eff close[a](is: InputStream, k: Result[(), IOError] -> a): a

    ///
    /// Default handler for `close`.
    ///
    handler close[a](is: InputStream, k: Result[(), IOError] -> a): a = ???

}
