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
    /// Returns the extension of the given path `p`.
    ///
    pub def extension(p: Path): Option[Str] = ???

    ///
    /// Effectfully returns a list of files in the given directory `path`.
    ///
    pub eff children(path: Path): Result[List[Path], IOError]

    ///
    /// Default handler for `isParentOf`.
    ///
    handler children[a](path: Path, k: Result[List[Path], IOError] -> a): a = ???

// TODO file.siblings

    ///
    /// Effectfully returns `true` if the given path `parent` is the direct parent of the path `child`.
    ///
    pub eff isParentOf(parent: Path, child: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isParentOf`.
    ///
    handler isParentOf[a](parent: Path, child: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `child` is a direct child of the path `parent`.
    ///
    pub eff isChildOf(child: Path, parent: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isChildOf`.
    ///
    handler isChildOf[a](child: Path, parent: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p1` is a sibling of the path `p2`.
    ///
    pub eff isSiblingOf(p1: Path, p2: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isSiblingOf`.
    ///
    handler isSiblingOf[a](p1: Path, p2: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is a directory.
    ///
    pub eff isDirectory(p: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isDirectory`.
    ///
    handler isDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` does not exist or is an empty file or directory.
    ///
    pub eff isEmpty(p: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isEmpty`.
    ///
    handler isEmpty[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is executable.
    ///
    pub eff isExecutable(p: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isExecutable`.
    ///
    handler isExecutable[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is hidden.
    ///
    pub eff isHidden(p: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isHidden`.
    ///
    handler isHidden[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is readable.
    ///
    pub eff isReadable(p: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isReadable`.
    ///
    handler isReadable[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is a regular file.
    ///
    pub eff isRegularFile(p: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isRegularFile`.
    ///
    handler isRegularFile[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the two given paths `p1` and `p2` refers to the same file.
    ///
    pub eff isSameFile(p1: Path, p2: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isSameFile`.
    ///
    handler isSameFile[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is a symbolic link.
    ///
    pub eff isSymbolicLink(p: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isSymbolicLink`.
    ///
    handler isSymbolicLink[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is writable.
    ///
    pub eff isWritable(p: Path): Result[Bool, IOError]

    ///
    /// Default handler for `isWritable`.
    ///
    handler isWritable[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns a list of all lines in the given text file denoted by path `p`.
    ///
    pub eff readLines(p: Path): Result[List[Str], IOError]

    ///
    /// Default handler for `readLines`.
    ///
    handler readLines[a](p: Path, k: Result[List[Str], IOError] -> a): a = ???

    ///
    /// Effectfully returns a list of all lines in the given text file denoted by path `p` parsed with the given charset `cs`.
    ///
    pub eff readLinesWithCharset(p: Path, cs: Charset): Result[List[Str], IOError]

    ///
    /// Default handler for `readLinesWithCharset`.
    ///
    handler readLinesWithCharset[a](p: Path, cs: Charset, k: Result[List[Str], IOError] -> a): a = ???

    ///
    /// Effectfully creates a new and empty file for the given path `p`.
    ///
    pub eff createFile(p: Path): Result[Path, IOError]

    ///
    /// Default handler for `createFile`.
    ///
    handler createFile[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully creates a new directory for the given path `p`.
    ///
    /// Requires that the path directory of the path `p` exists.
    ///
    pub eff createDirectory(p: Path): Result[Path, IOError]

    ///
    /// Default handler for `createDirectory`.
    ///
    handler createDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully creates a new directory for the given path `p`.
    ///
    /// Creates all parent directories as needed.
    ///
    pub eff createDirectories(p: Path): Result[Path, IOError]

    ///
    /// Default handler for `createDirectories`.
    ///
    handler createDirectories[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully copies the source file path `src` to the destination path `dst`.
    ///
    pub eff copyFileTo(src: Path, dst: Path): Result[(), IOError]

    ///
    /// Default handler for `copyFileTo`.
    ///
    handler copyFileTo[a](p: Path, dst: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully copies the source directory path `src` to the destination directory `dst`.
    ///
    pub eff copyDirectoryTo(src: Path, dst: Path): Result[(), IOError]

    ///
    /// Default handler for `copyDirectoryTo`.
    ///
    handler copyDirectoryTo[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully moves the file from the `src` path to the `dst` path.
    ///
    pub eff moveFile(src: Path, dst: Path): Result[(), IOError]

    ///
    /// Default handler for `moveFile`.
    ///
    handler moveFile[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully moves the directory from the `src` path to the `dst` path.
    ///
    pub eff moveDirectory(src: Path, dst: Path): Result[(), IOError]

    ///
    /// Default handler for `moveFile`.
    ///
    handler moveDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully deletes the file for the given path `p`.
    ///
    pub eff deleteFile(p: Path): Result[Path, IOError]

    ///
    /// Default handler for `deleteFile`.
    ///
    handler deleteFile[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully deletes the directory (and its sub-directories) for the given path `p`.
    ///
    pub eff deleteDirectory(p: Path): Result[Path, IOError]

    ///
    /// Default handler for `deleteDirectory`.
    ///
    handler deleteDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns the size of the given path `p`.
    ///
    pub eff size(p: Path): Result[Int64, IOError]

    ///
    /// Default handler for `size`.
    ///
    handler size[a](p: Path, k: Result[Int64, IOError] -> a): a = ???

    ///
    /// Effectfully returns a path that references a new temporary directory.
    ///
    pub eff newTemporaryDirectory(): Result[Path, IOError]

    ///
    /// Default handler for `newTemporaryDirectory`.
    ///
    handler newTemporaryDirectory[a](k: Result[Path, IOError] -> a): a = ???

    ///
    /// Effectfully returns a path that references a new temporary file.
    ///
    pub eff newTemporaryFile(): Result[Path, IOError]

    ///
    /// Default handler for `newTemporaryFile`.
    ///
    handler newTemporaryFile[a](k: Result[Path, IOError] -> a): a = ???

// file.append(text)
// file.appendText
// file.appendLine(text)
// file.appendByreArray
// writeText
// writeByteArray
// writeBytes

}
