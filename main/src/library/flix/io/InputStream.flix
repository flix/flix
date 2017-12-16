type InputStream = InputStream(##java.io.InputStream)

namespace InputStream {

    // TODO: Uniqueness.

    ///
    /// Effectfully returns an input stream from the given path `s`.
    ///
    pub eff new(p: Path): Result[InputStream, IOError]

    ///
    /// Effectfully returns an input stream from the given path `s` with the given open options `opts`.
    ///
    pub eff newWithOpts(p: Path, opts: Set[OpenOption]): Result[InputStream, IOError]




    ///
    /// Default handler for `new`.
    ///
    handler new[a](p: Path, k: Result[InputStream, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Default handler for `newWithOpts`.
    ///
    handler newWithOpts[a](p: Path, k: Result[InputStream, IOError] -> a): a = ??? // TODO: Implement

    // TODO def buffered: BufferedInputStream
    // TODO: def bytes: Iterator[Byte]
    // TODO: def lines(implicit charset: Charset = DefaultCharset): Iterator[String]
    // TODO: final def pipeTo(out: OutputStream, buffer: Array[Byte]):
    // TODO: def pipeTo(out: OutputStream, bufferSize: Int =

}
