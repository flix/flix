// Challenges and solutions:
//
// 1. How to deal with variadic methods: Use array literals [| ... |]
// 2. How to port overloaded methods? Use `withXYZ` style.
// 3. How to deal with null? Fields and method calls should return Option[X].
// 4. How to deal with overloaded methods? Use ascription of type arguments.
// 5. How to deal with different types of IOErrors, e.g. FileNotFound, FileIsDirectory etc.? Open question

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
    //TODO: Testing pull requests. Also return type should be Option[Str] without "No extension"
    pub def extension(p: Path): Str = unsafe {
        let Path(javaPath) = p;
        let shortPath = native method java.nio.file.Path.getFileName(javaPath);
        let myString = native method java.nio.file.Path.toString(shortPath);
        let dotIndex = native method java.lang.String.lastIndexOf(myString, "." : Str);
        match dotIndex with {
            case -1 => "No extension"
            case num => let actualIndex = dotIndex + 1;
                        native method java.lang.String.substring(myString, actualIndex)
        }
    }

    ///
    /// Effectfully returns a list of files in the given directory `path`.
    ///
    pub eff children[a](path: Path, k: Result[List[Path], IOError] -> a): a

    ///
    /// Default handler for `children`.
    ///
    handler children[a](path: Path, k: Result[List[Path], IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `parent` is the direct parent of the path `child`.
    ///
    pub eff isParentOf[a](parent: Path, child: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isParentOf`.
    ///
    handler isParentOf[a](parent: Path, child: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `child` is a direct child of the path `parent`.
    ///
    pub eff isChildOf[a](child: Path, parent: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isChildOf`.
    ///
    handler isChildOf[a](child: Path, parent: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p1` is a sibling of the path `p2`.
    ///
    pub eff isSiblingOf[a](p1: Path, p2: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isSiblingOf`.
    ///
    handler isSiblingOf[a](p1: Path, p2: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is a directory.
    ///
    pub eff isDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isDirectory`.
    ///
    handler isDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` does not exist or is an empty file or directory.
    ///
    pub eff isEmpty[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isEmpty`.
    ///
    handler isEmpty[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is executable.
    ///
    pub eff isExecutable[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isExecutable`.
    ///
    handler isExecutable[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is hidden.
    ///
    pub eff isHidden[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isHidden`.
    ///
    handler isHidden[a](p: Path, k: Result[Bool, IOError] -> a): a = unsafe {
        let Path(javaPath) = p;
        let answer = native method java.nio.file.Files.isHidden(javaPath);
        k(Ok(answer))
        //TODO: This throws java.nio.file.NoSuchFileException when used on non-existing file
    }

    ///
    /// Effectfully returns `true` if the given path `p` is readable.
    ///
    pub eff isReadable[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isReadable`.
    ///
    handler isReadable[a](p: Path, k: Result[Bool, IOError] -> a): a = unsafe {
        let Path(javaPath) = p;
        let answer = native method java.nio.file.Files.isReadable(javaPath);
        k(Ok(answer))
        //TODO: Returns false if Path p does not exist. Is this sufficient?
    }

    ///
    /// Effectfully returns `true` if the given path `p` is a regular file.
    ///
    pub eff isRegularFile[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isRegularFile`.
    ///
    handler isRegularFile[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the two given paths `p1` and `p2` refers to the same file.
    ///
    pub eff isSameFile[a](p1: Path, p2: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isSameFile`.
    ///
    handler isSameFile[a](p1: Path, p2: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns `true` if the given path `p` is a symbolic link.
    ///
    pub eff isSymbolicLink[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isSymbolicLink`.
    ///
    handler isSymbolicLink[a](p: Path, k: Result[Bool, IOError] -> a): a = unsafe {
        let Path(javaPath) = p;
        let answer = native method java.nio.file.Files.isSymbolicLink(javaPath);
        k(Ok(answer))
        //TODO: Returns false if Path p does not exist. Is this sufficient?
    }

    ///
    /// Effectfully returns `true` if the given path `p` is writable.
    ///
    pub eff isWritable[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isWritable`.
    ///
    handler isWritable[a](p: Path, k: Result[Bool, IOError] -> a): a = unsafe {
        let Path(javaPath) = p;
        let answer = native method java.nio.file.Files.isWritable(javaPath);
        k(Ok(answer))
        //TODO: Returns false if Path p does not exist. Is this sufficient?
    }

    ///
    /// Effectfully returns a list of all lines in the given text file denoted by path `p`.
    ///
    pub eff readLines[a](p: Path, k: Result[List[Str], IOError] -> a): a

    ///
    /// Default handler for `readLines`.
    ///
    handler readLines[a](p: Path, k: Result[List[Str], IOError] -> a): a = ???

    ///
    /// Effectfully returns a list of all lines in the given text file denoted by path `p` parsed with the given charset `cs`.
    ///
    pub eff readLinesWithCharset[a](p: Path, cs: Charset, k: Result[List[Str], IOError] -> a): a

    ///
    /// Default handler for `readLinesWithCharset`.
    ///
    handler readLinesWithCharset[a](p: Path, cs: Charset, k: Result[List[Str], IOError] -> a): a = ???

    ///
    /// Effectfully creates a new and empty file for the given path `p`.
    ///
    pub eff createFile[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `createFile`.
    ///
    handler createFile[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully creates a new directory for the given path `p`.
    ///
    /// Requires that the path directory of the path `p` exists.
    ///
    pub eff createDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `createDirectory`.
    ///
    handler createDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully creates a new directory for the given path `p`.
    ///
    /// Creates all parent directories as needed.
    ///
    pub eff createDirectories[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `createDirectories`.
    ///
    handler createDirectories[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully copies the source file path `src` to the destination path `dst`.
    ///
    pub eff copyFileTo[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `copyFileTo`.
    ///
    handler copyFileTo[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully copies the source directory path `src` to the destination directory `dst`.
    ///
    pub eff copyDirectoryTo[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `copyDirectoryTo`.
    ///
    handler copyDirectoryTo[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully moves the file from the `src` path to the `dst` path.
    ///
    pub eff moveFile[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `moveFile`.
    ///
    handler moveFile[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully moves the directory from the `src` path to the `dst` path.
    ///
    pub eff moveDirectory[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `moveFile`.
    ///
    handler moveDirectory[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully deletes the file for the given path `p`.
    ///
    pub eff deleteFile[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `deleteFile`.
    ///
    handler deleteFile[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully deletes the directory (and its sub-directories) for the given path `p`.
    ///
    pub eff deleteDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `deleteDirectory`.
    ///
    handler deleteDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = ???

    ///
    /// Effectfully returns the size of the given path `p`.
    ///
    pub eff size[a](p: Path, k: Result[Int64, IOError] -> a): a

    ///
    /// Default handler for `size`.
    ///
    handler size[a](p: Path, k: Result[Int64, IOError] -> a): a = ???

    ///
    /// Effectfully returns a path that references a new temporary directory.
    ///
    pub eff newTemporaryDirectory[a](k: Result[Path, IOError] -> a): a

    ///
    /// Default handler for `newTemporaryDirectory`.
    ///
    handler newTemporaryDirectory[a](k: Result[Path, IOError] -> a): a = ???

    ///
    /// Effectfully returns a path that references a new temporary file.
    ///
    pub eff newTemporaryFile[a](k: Result[Path, IOError] -> a): a

    ///
    /// Default handler for `newTemporaryFile`.
    ///
    handler newTemporaryFile[a](k: Result[Path, IOError] -> a): a = ???

}
