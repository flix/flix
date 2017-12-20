type OutputStream = OutputStream(##java.io.OutputStream)

namespace OutputStream {

    ///
    /// Effectfully returns an output stream for the given path `s`.
    ///
    pub eff new(p: Path): Result[OutputStream, IOError]

    ///
    /// Default handler for `new`.
    ///
    handler new[a](p: Path, k: Result[OutputStream, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Effectfully returns an output stream for the given path `s` with the given open options `opts`.
    ///
    pub eff newWithOpts(p: Path, opts: Set[OpenOption]): Result[OutputStream, IOError]

    ///
    /// Default handler for `new`.
    ///
    handler newWithOpts[a](p: Path, opts: Set[OpenOption], k: Result[OutputStream, IOError] -> a): a = ??? // TODO: Implement

    // TODO: Uniqueness.

}
