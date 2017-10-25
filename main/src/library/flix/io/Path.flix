def f(): Path = Path.getPath("./foo.flix")

type Path           =   Path(##java.nio.file.Path)
type InputStream    =   InputStream(##java.io.InputStream)
type OutputStream   =   OutputStream(##java.io.OutputStream)

// TODO: Overall challenges.
// 1. How to deal with variadic methods that take optional arguments? Suggestion => Hack around it with reflection.
// 2. How to support the above without overloading in Flix? Suggestion => Use `withXYZ` suffix.
// 3. How to deal with different types of IOErrors, e.g. FileNotFound, FileIsDirectory etc.? Suggestion introduce IOError? Look at rust?
// 4. How to deal with nulls? Suggestion return Option[X].
// 5. How to deal with overloaded java methods? Suggestion => Allow ascription of arguments.
 
// TODO: We need or want this?
pub enum IOError {
    case FileNotFound
}

namespace Path {

    pub def getPath(s: Str): Path =
        // TODO: Notice this ugly hack because we lack array literals...
        // TODO: It is possible to determine if method has varargs and then rewrite to call it with the empty array.
        let fileObj = unsafe native new java.io.File(s: Str);
        let pathObj = unsafe native method java.io.File.toPath(fileObj: ##java.io.File);
            Path(pathObj)

    pub def isExecutable(p: Path): Result[Bool, IOError] @ IO =
        p |> unwrapPath |> isExecutableX

    // TODO: What namespace does such as function belong in?
    pub def copyStreamToPath(i: /*unique*/ InputStream, p: Path): Result[(), IOError] @ IO =
        let InputStream(realInputStream) = i;
        let Path(realPath) = p;
            copyStreamToPathX(realInputStream, realPath)

    // TODO: What namespace does such as function belong in?
    pub def copyStreamToStream(is: unique InputStream, os: borrow OutputStream): Result[(), IOError] @ IO =
        ???

    // TODO: What about OpenOption? Probably needs a `newInputStreamWith` ...
    // TODO: Shape of result?
    pub def newInputStream(p: Path): /*unique*/ Result[InputStream, IOError] @ IO =
        p |> unwrapPath |> (x -> ???)

    // TODO: Need a withCharset variant.
    pub def readAllLines(p: Path): List[Str] @ IO =
        p |> unwrapPath |> (x -> ???)

    // TODO: Need a withOptions variant.
    // TODO: Need an array
    pub def write(p: Path, bytes: List[Int8]): Result[(), IOError] @ IO = ???

    // TODO: Think about return type.
    def isExecutableX(p: ##java.nio.file.Path): Result[Bool, IOError] =
        unsafe {
            native method java.nio.file.Files.isExecutable(p: ##java.nio.file.Path)
        }

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
