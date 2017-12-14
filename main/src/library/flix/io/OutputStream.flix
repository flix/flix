type OutputStream = OutputStream(##java.io.OutputStream)

namespace OutputStream {

    // TODO: Uniqueness.

    ///
    /// Effectfully returns an output stream from the given path `s`.
    ///
    pub eff new(p: Path): Result[OutputStream, IOError]




    ///
    /// Default handler for `new`.
    ///
    handler new[a](p: Path, k: Result[OutputStream, IOError] -> a): a = ??? // TODO: Implement

}
