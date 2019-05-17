pub type OutputStream = OutputStream(##java.io.OutputStream)

namespace OutputStream {

    ///
    /// Effectfully returns an output stream for the given path `s`.
    ///
    pub eff new[a](p: Path, k: Result[OutputStream, IOError] -> a): a

    ///
    /// Default handler for `new`.
    ///
    handler new[a](p: Path, k: Result[OutputStream, IOError] -> a): a = ???

    ///
    /// Effectfully returns an output stream for the given path `s` with the given open options `opts`.
    ///
    pub eff newWithOpts[a](p: Path, opts: Set[OpenOption], k: Result[OutputStream, IOError] -> a): a

    ///
    /// Default handler for `new`.
    ///
    handler newWithOpts[a](p: Path, opts: Set[OpenOption], k: Result[OutputStream, IOError] -> a): a = ???

}
