// Challenges and solutions:
//
// 1. How to deal with variadic methods: Use array literals [| ... |]
// 2. How to port overloaded methods? Use `withXYZ` style.
// 3. How to deal with null? Fields and method calls should return Option[X].
// 4. How to deal with overloaded methods? Use ascription of type arguments.
// 5. How to deal with different types of IOErrors, e.g. FileNotFound, FileIsDirectory etc.? Open question

//    pub def copyToPath(i: unique InputStream, p: Path): Result[(), IOError] @ IO = ???
//    pub def copyToStream(is: unique InputStream, os: borrow OutputStream): Result[(), IOError] @ IO = ???

// TODO: Need some kind of walker... and how does that work with algebraic effects?
//public static Path walkFileTree(Path start,
                //Set<FileVisitOption> options,
                //int maxDepth,
                //FileVisitor<? super Path> visitor)
                         //throws IOException

// TODO: Where and how to define this?
type IOError = IOError(Str)

// TODO: Where and how to define this?
type Charset = Charset(Str)

type Path = Path(##java.nio.file.Path)

namespace Path {

    ///
    /// Returns a Path from the given string `s`.
    ///
    pub def new(s: Str): Path = unsafe {
        Path(native method java.nio.file.Paths.get(s, [| |] : Array[Str]))
    }

    ///
    /// Effectfully returns `true` if the given path `p` is a directory.
    ///
    pub eff isDirectory(p: Path): Result[Bool, IOError]

    ///
    /// Effectfully returns `true` if the given path `p` is executable.
    ///
    pub eff isExecutable(p: Path): Result[Bool, IOError]

    ///
    /// Effectfully returns `true` if the given path `p` is hidden.
    ///
    pub eff isHidden(p: Path): Result[Bool, IOError]

    ///
    /// Effectfully returns `true` if the given path `p` is readable.
    ///
    pub eff isReadable(p: Path): Result[Bool, IOError]

    ///
    /// Effectfully returns `true` if the given path `p` is a regular file.
    ///
    pub eff isRegularFile(p: Path): Result[Bool, IOError]

    ///
    /// Effectfully returns `true` if the two given paths `p1` and `p2` refers to the same file.
    ///
    pub eff isSameFile(p1: Path, p2: Path): Result[Bool, IOError]

    ///
    /// Effectfully returns `true` if the given path `p` is a symbolic link.
    ///
    pub eff isSymbolicLink(p: Path): Result[Bool, IOError]

    ///
    /// Effectfully returns `true` if the given path `p` is writable.
    ///
    pub eff isWritable(p: Path): Result[Bool, IOError]

    ///
    /// Effectfully returns a list of all lines in the given text file denoted by path `p`.
    ///
    pub eff readLines(p: Path): Result[List[Str], IOError]

    ///
    /// Effectfully returns a list of all lines in the given text file denoted by path `p` parsed with the given charset `cs`.
    ///
    pub eff readLinesWithCharset(p: Path, cs: Charset): Result[List[Str], IOError]

    ///
    /// Effectfully creates a new and empty file for the given path `p`.
    ///
    pub eff createFile(p: Path): Result[Path, IOError]

    ///
    /// Effectfully creates a new directory for the given path `p`.
    ///
    /// Requires that the path directory of the path `p` exists.
    ///
    pub eff createDirectory(p: Path): Result[Path, IOError]

    ///
    /// Effectfully creates a new directory for the given path `p`.
    ///
    /// Creates all parent directories as needed.
    ///
    pub eff createDirectories(p: Path): Result[Path, IOError]

    ///
    /// Effectfully copies the source file path `src` to the destination path `dst`.
    ///
    pub eff copyFileTo(src: Path, dst: Path): Result[(), IOError]

    ///
    /// Effectfully copies the source directory path `src` to the destination directory `dst`.
    ///
    pub eff copyDirectoryTo(src: Path, dst: Path): Result[(), IOError]


// TODO: move.
// File.newTemporaryDirectory()
   //File.newTemporaryFile()

    ///
    /// Effectfully deletes the for the given path `p`.
    ///
    pub eff deleteFile(p: Path): Result[Path, IOError]

    ///
    /// Effectfully deletes the directory (and its sub-directories) for the given path `p`.
    ///
    pub eff deleteDirectory(p: Path): Result[Path, IOError]

// TODO: size

//file.extension
//file.contentType
//isEmpty
// file.isParentOf; file.isChildOf; file.isSiblingOf; file.siblings
// file.append(text)
// file.appendText
// file.appendLine(text)
// file.appendByreArray
// bytes // byteArray
// children
// contains
// contentType
// isChildOf.
// isParentOf
// lines
// nonEmpty?
// renameTo
// writeText
// writeByteArray
// writeBytes

    ///
    /// Default handler for `isDirectory`.
    ///
    handler isDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Default handler for `isExecutable`.
    ///
    handler isExecutable[a](p: Path, k: Result[Bool, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Default handler for `isHidden`.
    ///
    handler isHidden[a](p: Path, k: Result[Bool, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Default handler for `isReadable`.
    ///
    handler isReadable[a](p: Path, k: Result[Bool, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Default handler for `isRegularFile`.
    ///
    handler isRegularFile[a](p: Path, k: Result[Bool, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Default handler for `isSameFile`.
    ///
    handler isSameFile[a](p: Path, k: Result[Bool, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Default handler for `isSymbolicLink`.
    ///
    handler isSymbolicLink[a](p: Path, k: Result[Bool, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Default handler for `isWritable`.
    ///
    handler isWritable[a](p: Path, k: Result[Bool, IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Default handler for `readLines`.
    ///
    handler readLines[a](p: Path, k: Result[List[Str], IOError] -> a): a = ??? // TODO: Implement

    ///
    /// Default handler for `readLinesWithCharset`.
    ///
    handler readLinesWithCharset[a](p: Path, cs: Charset, k: Result[List[Str], IOError] -> a): a = ??? // TODO: Implement

}
