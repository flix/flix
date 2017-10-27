// TODO: Overall challenges.
// 1. How to deal with variadic methods that take optional arguments? Suggestion => Hack around it with reflection.
// 2. How to support the above without overloading in Flix? Suggestion => Use `withXYZ` suffix.
// 3. How to deal with different types of IOErrors, e.g. FileNotFound, FileIsDirectory etc.? Suggestion introduce IOError? Look at rust?
// 4. How to deal with nulls? Suggestion return Option[X].
// 5. How to deal with overloaded java methods? Suggestion => Allow ascription of arguments.
 
// TODO: We need or want this?
pub enum IOError {
    case FileNotFound,
    case IOErrorMsg(Str)
}

type Path           =   Path(##java.nio.file.Path)
type InputStream    =   InputStream(##java.io.InputStream)
type OutputStream   =   OutputStream(##java.io.OutputStream)

namespace Path {

    def f(): Result[Bool, IOError] = Path.new("./foo.flix").isExecutable()

    ///
    /// Returns a Path from the given string `s`.
    ///
    pub def new(s: Str): Path =
        // TODO: Notice this ugly hack because we lack array literals...
        // TODO: It is possible to determine if method has varargs and then rewrite to call it with the empty array.
        let fileObj = unsafe native new java.io.File(s: Str);
        let pathObj = unsafe native method java.io.File.toPath(fileObj: ##java.io.File);
            Path(pathObj)



    ///
    /// Returns `true` if the given path `p` exists.
    ///
    pub def exists(p: Path): Result[Bool, IOError] @ IO = unsafe match p with {
        case Path(realPath) =>
            let result = native method java.nio.file.Files.exists(realPath: ##java.nio.file.Path, /* TODO */ ???);
                match result with {
                    case Ok(bool) => Ok(bool)
                    case Err(ex)  => Err(IOErrorMsg("get msg from ex"))
                }
        }

    ///
    /// Returns `true` if the given path `p` is readable.
    ///
    pub def isReadable(p: Path): Result[Bool, IOError] @ IO = match p with {
        case Path(realPath) =>
            Ok(unsafe native method java.nio.file.Files.isReadable(realPath: ##java.nio.file.Path))
    }

    ///
    /// Returns `true` if the given path `p` exists and is executable.
    ///
    pub def isExecutable(p: Path): Result[Bool, IOError] @ IO = match p with {
        case Path(realPath) =>
            Ok(unsafe native method java.nio.file.Files.isExecutable(realPath: ##java.nio.file.Path))
    }



    // TODO: What namespace does such as function belong in?
    pub def copyToPath(i: unique InputStream, p: Path): Result[(), IOError] @ IO =
        let InputStream(realInputStream) = i;
        let Path(realPath) = p;
            copyStreamToPathX(realInputStream, realPath)

    // TODO: What namespace does such as function belong in?
    pub def copyToStream(is: unique InputStream, os: borrow OutputStream): Result[(), IOError] @ IO =
        ???

    // TODO: What about OpenOption? Probably needs a `newInputStreamWith` ...
    // TODO: Shape of result?
    pub def newInputStream(p: Path): unique Result[InputStream, IOError] @ IO =
        p |> unwrapPath |> (x -> ???)

    // TODO: Need a withCharset variant.
    pub def readAllLines(p: Path): List[Str] @ IO =
        p |> unwrapPath |> (x -> ???)

    // TODO: Need a withOptions variant.
    // TODO: Need an array
    pub def write(p: Path, bytes: List[Int8]): Result[(), IOError] @ IO = ???


    def copyStreamToPathX(i: ##java.io.InputStream, p: ##java.nio.file.Path): Result[(), IOError] = ???
    // TODO: The below will not work because copy takes three arguments, the last which is an array, which we currently lack.
//        unsafe {
//            native method java.nio.file.Files.copy(i: ##java.io.InputStream, p: ##java.nio.file.Path)
//        }

    def copyToStreamX(is: /*unique*/ InputStream, /* borrow */ dst: OutputStream): Result[(), IOError] = ???

    def unwrapPath(p: Path): ##java.nio.Path = match p with {
        case Path(realPath) => realPath
    }

}
