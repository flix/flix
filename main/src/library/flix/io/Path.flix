// Challenges and solutions:
//
// 1. How to deal with variadic methods: Use array literals [| ... |]
// 2. How to port overloaded methods? Use `withXYZ` style.
// 3. How to deal with null? Fields and method calls should return Option[X].
// 4. How to deal with overloaded methods? Use ascription of type arguments.
// 5. How to deal with different types of IOErrors, e.g. FileNotFound, FileIsDirectory etc.? Open question

// TODO: What should the structure of this enum be?
pub enum IOError {
    case FileNotFound,
    case IOErrorMsg(Str)
}

type Path           =   Path(##java.nio.file.Path)
type InputStream    =   InputStream(##java.io.InputStream)
type OutputStream   =   OutputStream(##java.io.OutputStream)

// TODO: It would be nice to have imports like:  import ##java.io.File (makes File available as ##File)

namespace Path {

    // dummy test method.
    def f(): Result[Bool, IOError] = Path.new("./foo.flix").isReadable()

    ///
    /// Returns a Path from the given string `s`.
    ///
    pub def new(s: Str): Path = unsafe {
        Path(native method java.nio.file.Paths.get(s, [| |] : Array[Str]))
    }

    ///
    /// Returns `true` if the given path `p` is readable.
    ///
    pub def isReadable(p: Path): Result[Bool, IOError] @ IO = match p with {
        case Path(realPath) =>
            Ok(unsafe native method java.nio.file.Files.isReadable(realPath))
    }

    // TODO: Where should the function below be organized?

    pub def copyToPath(i: unique InputStream, p: Path): Result[(), IOError] @ IO = ???

    // TODO: What namespace does such as function belong in?

    pub def copyToStream(is: unique InputStream, os: borrow OutputStream): Result[(), IOError] @ IO = ???

    pub def newInputStream(p: Path): unique Result[InputStream, IOError] @ IO = ???

    // TODO: Add openoptions argument.
    pub def newInputStreamWithOpenOptions(p: Path): unique Result[InputStream, IOError] @ IO = ???

    pub eff readAllLines(p: Path): List[Str] @ IO

    // TODO: Add charset argument
    pub def readAllLinesWithCharset(p: Path): List[Str] @ IO = ???

}
