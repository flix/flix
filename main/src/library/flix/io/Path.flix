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
pub type IOError = IOError(Str)

// TODO: Where and how to define this?
pub type Charset = Charset(Str)

type Path = Path(##java.nio.file.Path)

namespace Path {

    ///
    /// Returns a Path from the given string `s`.
    ///
    pub def new(s: Str): Path = {
        Path(native method java.nio.file.Paths.get(s, [ ] : Array[Str]))
    }

    ///
    /// Returns the extension of the given path `p`.
    ///
    pub def extension(p: Path): Option[Str] = {
        let Path(javaPath) = p;
        let shortPath = native method java.nio.file.Path.getFileName(javaPath);
        let myString = native method java.nio.file.Path.toString(shortPath);
        let dotIndex = native method java.lang.String.lastIndexOf(myString, "." : Str);
        match dotIndex with {
            case -1 => None
            case _ => let actualIndex = dotIndex + 1;
                      Some(native method java.lang.String.substring(myString, actualIndex) as Str)
        }
    }

    ///
    /// Checks whether the given path `p` exists
    ///
    pub def exists(p: Path): Bool = {
        let Path(javaPath) = p;
        native method java.nio.file.Files.exists(javaPath, [ ] : Array[##java.nio.file.LinkOption]) as Bool
    }

    ///
    /// Effectfully returns a list of files in the given directory `path`.
    ///
    pub eff children[a](path: Path, k: Result[List[Path], IOError] -> a): a

    ///
    /// Default handler for `children`.
    ///
    handler children[a](path: Path, k: Result[List[Path], IOError] -> a): a = {
        let directoryResponse = isDirectory(path, result -> result);
        match directoryResponse with {
            case Ok(true) => let Path(javaPath) = path;
                             let dirStream = native method java.nio.file.Files.newDirectoryStream(javaPath);
                             let dirIterator = native method java.nio.file.DirectoryStream.iterator(dirStream);
                             let childList : List[Path] = Nil;
                             let childList = childrenHelper(dirIterator, childList);
                             //let _ = native method java.nio.file.DirectoryStream.close(dirStream); TODO: Have this line compile
                             k(Ok(childList))
            case Ok(false) => k(Err(IOError("The provided path is not a directory. Children is only available for directories")))
            case Err(IOError(errorString)) => k(Err(IOError(errorString)))
        }
    }

    def childrenHelper(dirIterator: ##java.util.Iterator, childList: List[Path]): List[Path] = {
        if (native method java.util.Iterator.hasNext(dirIterator))
            let fetchedPath = native method java.util.Iterator.next(dirIterator);
            let childList = Path(fetchedPath) :: childList;
            childrenHelper(dirIterator, childList)
        else
            List.reverse(childList)
    }

    ///
    /// Effectfully returns `true` if the given path `parent` is the direct parent of the path `child`.
    ///
    pub eff isParentOf[a](parent: Path, child: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isParentOf`.
    ///
    handler isParentOf[a](parent: Path, child: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(child) && exists(parent))
            //TODO: Could this simply reuse isChildOf with flipped call-order? What if user implements own handler for isChild?
            k(Ok(isParentOfHelper(parent, child)))
        else
            k(Err(IOError("One or both arguments 'p1' and 'p2' do not point to an existing file or directory")))
    }

    def isParentOfHelper(parent: Path, child: Path): Bool = {
        let Path(parentPath) = parent;
        let Path(childPath) = child;
        let parentOfChild = native method java.nio.file.Path.getParent(childPath);
        if (native method java.util.Objects.isNull(parentOfChild))
            false
        else
            if (native method java.nio.file.Path.equals(parentOfChild, parentPath))
                true
            else
                isParentOfHelper(parent, Path(parentOfChild))
    }

    ///
    /// Effectfully returns `true` if the given path `child` is a direct child of the path `parent`.
    ///
    pub eff isChildOf[a](child: Path, parent: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isChildOf`.
    ///
    handler isChildOf[a](child: Path, parent: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(child) && exists(parent))
            k(Ok(isChildOfHelper(child, parent)))
        else
            k(Err(IOError("One or both arguments 'child' and 'parent' do not point to an existing file or directory")))
    }

    def isChildOfHelper(child: Path, parent: Path) : Bool = {
        let Path(childPath) = child;
        let Path(parentPath) = parent;
        let parentOfChild = native method java.nio.file.Path.getParent(childPath);
        if (native method java.util.Objects.isNull(parentOfChild))
            false
        else
            if (native method java.nio.file.Path.equals(parentOfChild, parentPath))
                true
            else
                isChildOfHelper(Path(parentOfChild), parent)
    }

    ///
    /// Effectfully returns `true` if the given path `p1` is a sibling of the path `p2`.
    ///
    pub eff isSiblingOf[a](p1: Path, p2: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isSiblingOf`.
    ///
    handler isSiblingOf[a](p1: Path, p2: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(p1) && exists(p2))
            let Path(javaPath1) = p1;
            let Path(javaPath2) = p2;
            let parent1 = native method java.nio.file.Path.getParent(javaPath1);
            let parent2 = native method java.nio.file.Path.getParent(javaPath2);
            let answer = native method java.nio.file.Path.equals(parent1, parent2);
            k(Ok(answer))
        else
            k(Err(IOError("One or both arguments 'p1' and 'p2' do not point to an existing file or directory")))
    }

    ///
    /// Effectfully returns `true` if the given path `p` is a directory.
    ///
    pub eff isDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isDirectory`.
    ///
    handler isDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(p))
            let Path(javaPath) = p;
            let answer = native method java.nio.file.Files.isDirectory(javaPath, [ ] : Array[##java.nio.file.LinkOption]);
            k(Ok(answer))
        else
            k(Err(IOError("Argument 'p' does not point to any existing file or directory")))
    }

    ///
    /// Effectfully returns `true` if the given path `p` does not exist or is an empty file or directory.
    ///
    pub eff isEmpty[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isEmpty`.
    ///
    handler isEmpty[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        let directoryHandler = isDirectory(p, result -> result);
        match directoryHandler with {
            case Ok(true) =>  let Path(javaPath) = p;
                              let dirStream = native method java.nio.file.Files.newDirectoryStream(javaPath);
                              let dirIterator = native method java.nio.file.DirectoryStream.iterator(dirStream);
                              k(Ok(!native method java.util.Iterator.hasNext(dirIterator)))
            case Ok(false) => let readLinesResponse = readLines(p, result -> result);
                              match readLinesResponse with {
                                  case Ok(Nil) => k(Ok(true))
                                  case Err(IOError(errorString)) => k(Err(IOError(errorString)))
                                  case _       => k(Ok(false))
                              }
            case Err(IOError(errorString)) => k(Err(IOError(errorString)))
        }
    }

    ///
    /// Effectfully returns `true` if the given path `p` is executable.
    ///
    pub eff isExecutable[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isExecutable`.
    ///
    handler isExecutable[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(p))
            let Path(javaPath) = p;
            let answer = native method java.nio.file.Files.isExecutable(javaPath);
            k(Ok(answer))
        else
            k(Err(IOError("Argument 'p' does not point to any file")))
            //TODO: Experiment with unExecutable files and determine if we can use .exists on them
    }

    ///
    /// Effectfully returns `true` if the given path `p` is hidden.
    ///
    pub eff isHidden[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isHidden`.
    ///
    handler isHidden[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(p))
            let Path(javaPath) = p;
            let answer = native method java.nio.file.Files.isHidden(javaPath);
            k(Ok(answer))
        else
            k(Err(IOError("Argument 'p' does not point to any existing file or directory")))
    }

    ///
    /// Effectfully returns `true` if the given path `p` is readable.
    ///
    pub eff isReadable[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isReadable`.
    ///
    handler isReadable[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(p))
            let Path(javaPath) = p;
            let answer = native method java.nio.file.Files.isReadable(javaPath);
            k(Ok(answer))
        else
            k(Err(IOError("Argument 'p' does not point to any existing file or directory")))
    }

    ///
    /// Effectfully returns `true` if the given path `p` is a regular file.
    ///
    pub eff isRegularFile[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isRegularFile`.
    ///
    handler isRegularFile[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(p))
            let Path(javaPath) = p;
            let answer = native method java.nio.file.Files.isRegularFile(javaPath, [ ] : Array[##java.nio.file.LinkOption]);
            k(Ok(answer))
        else
            k(Err(IOError("Argument 'p' does not point to any existing file or directory")))
    }

    ///
    /// Effectfully returns `true` if the two given paths `p1` and `p2` refers to the same file.
    ///
    pub eff isSameFile[a](p1: Path, p2: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isSameFile`.
    ///
    handler isSameFile[a](p1: Path, p2: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(p1) && exists(p2))
            let Path(javaPath1) = p1;
            let Path(javaPath2) = p2;
            let answer = native method java.nio.file.Files.isSameFile(javaPath1, javaPath2);
            k(Ok(answer))
        else
            k(Err(IOError("Not both arguments 'p1' and 'p2' refer to an existing Path")))
        // When used on a symbolic link, only returns true when used on original link and not a copy.
    }

    ///
    /// Effectfully returns `true` if the given path `p` is a symbolic link.
    ///
    pub eff isSymbolicLink[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isSymbolicLink`.
    ///
    handler isSymbolicLink[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(p))
            let Path(javaPath) = p;
            let answer = native method java.nio.file.Files.isSymbolicLink(javaPath);
            k(Ok(answer))
        else
            k(Err(IOError("Argument 'p' does not point to any existing file or directory")))
    }

    ///
    /// Effectfully returns `true` if the given path `p` is writable.
    ///
    pub eff isWritable[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `isWritable`.
    ///
    handler isWritable[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(p))
            let Path(javaPath) = p;
            let answer = native method java.nio.file.Files.isWritable(javaPath);
            k(Ok(answer))
        else
            k(Err(IOError("Argument 'p' does not point to any existing file or directory")))
    }

    ///
    /// Effectfully returns a list of all lines in the given text file denoted by path `p`.
    ///
    pub eff readLines[a](p: Path, k: Result[List[Str], IOError] -> a): a

    ///
    /// Default handler for `readLines`.
    ///
    handler readLines[a](p: Path, k: Result[List[Str], IOError] -> a): a = {
        let isRegularFileResponse = isRegularFile(p, result -> result);
        match isRegularFileResponse with {
            case Ok(true)  => let Path(javaPath) = p;
                              let reader = native method java.nio.file.Files.newBufferedReader(javaPath);
                              let flixList: List[Str] = Nil;
                              let flixList = readLinesHelper(flixList, reader);
                              let _ = native method java.io.BufferedReader.close(reader);
                              k(Ok(flixList))
            case Ok(false) => k(Err(IOError("Argument 'p' is not a regular file")))
            case Err(IOError(errorString)) => k(Err(IOError(errorString)))
        }
    }

    def readLinesHelper(fList: List[Str], reader : ##java.io.BufferedReader) : List[Str] = {
        let readLine = native method java.io.BufferedReader.readLine(reader);
        if (native method java.util.Objects.isNull(readLine))
            List.reverse(fList)
        else
            let fList = readLine :: fList;
            readLinesHelper(fList, reader)
    }

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
    handler createFile[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        try {
            let Path(javaPath) = p;
            let _ = native method java.nio.file.Files.createFile(javaPath, [ ] : Array[##java.nio.file.attribute.FileAttribute]);
            k(Ok(true))
        }
        catch {
            case ex: ##java.nio.file.FileAlreadyExistsException => k(Err(IOError("Could not create file at path 'p' as it already exists")))
            case ex: ##java.nio.file.NoSuchFileException        => k(Err(IOError("One of the folders in path 'p' does not exist")))
        }
    }

    ///
    /// Effectfully creates a new directory for the given path `p`.
    ///
    /// Requires that the path directory of the path `p` exists.
    ///
    pub eff createDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `createDirectory`.
    ///
    handler createDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        try {
            let Path(javaPath) = p;
            let _ = native method java.nio.file.Files.createDirectory(javaPath, [ ] : Array[##java.nio.file.attribute.FileAttribute]);
            k(Ok(true))
        }
        catch {
            case ex: ##java.nio.file.FileAlreadyExistsException => k(Err(IOError("Could not create directory at path 'p' as it already exists")))
            case ex: ##java.nio.file.NoSuchFileException        => k(Err(IOError("One of the folders in path 'p' does not exist. Use Path.createDirectories to create them automatically")))
        }
    }

    ///
    /// Effectfully creates a new directory for the given path `p`.
    ///
    /// Creates all parent directories as needed.
    ///
    pub eff createDirectories[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `createDirectories`.
    ///
    handler createDirectories[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        try {
            let Path(javaPath) = p;
            let _ = native method java.nio.file.Files.createDirectories(javaPath, [ ] : Array[##java.nio.file.attribute.FileAttribute]);
            k(Ok(true))
        }
        catch {
            case ex: ##java.nio.file.FileAlreadyExistsException => k(Err(IOError("Could not create directory at path 'p' as it already exists")))
        }
    }

    ///
    /// Effectfully copies the source file path `src` to the destination path `dst`.
    ///
    pub eff copyFileTo[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `copyFileTo`.
    ///
    handler copyFileTo[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a = {
        let directoryResponse = isDirectory(src, result -> result);
        match directoryResponse with {
            case Ok(true)  => k(Err(IOError("Path 'src' is a directory and not a file")))
            case Ok(false) => if (exists(dst))
                                  k(Err(IOError("Path 'dst' already exists. Overwriting is not currently possible")))
                              else
                                  let Path(javaPath1) = src;
                                  let Path(javaPath2) = dst;
                                  let _ = native method java.nio.file.Files.copy(javaPath1 : ##java.nio.file.Path,
                                                                                 javaPath2 : ##java.nio.file.Path,
                                                                                 [ ]     : Array[##java.nio.file.StandardCopyOption]);
                                  k(Ok(true))
            case Err(IOError(errorString)) => k(Err(IOError(errorString)))
        }
    }

    ///
    /// Effectfully copies the source directory path `src` to the destination directory `dst`.
    ///
    pub eff copyDirectoryTo[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `copyDirectoryTo`.
    ///
    handler copyDirectoryTo[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a = {
        let directoryResponse = isDirectory(src, result -> result);
        match directoryResponse with {
            case Ok(true)  => if (exists(dst))
                                  k(Err(IOError("Path 'dst' already exists. Overwriting is not currently possible")))
                              else
                                  let Path(javaPath1) = src;
                                  let Path(javaPath2) = dst;
                                  let _ = native method java.nio.file.Files.copy(javaPath1 : ##java.nio.file.Path,
                                                                                 javaPath2 : ##java.nio.file.Path,
                                                                                 [ ]     : Array[##java.nio.file.StandardCopyOption]);
                                  k(Ok(true))
            case Ok(false) => k(Err(IOError("Path 'src' is a file and not a directory")))
            case Err(IOError(errorString)) => k(Err(IOError(errorString)))
        }
    }

    ///
    /// Effectfully moves the file from the `src` path to the `dst` path.
    ///
    pub eff moveFile[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `moveFile`.
    ///
    handler moveFile[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a = {
        try {
            let handlerResponse = isRegularFile(src, result -> result);
            match handlerResponse with {
                case Ok(true)  => let Path(javaPath1) = src;
                                  let Path(javaPath2) = dst;
                                  let _ = native method java.nio.file.Files.move(javaPath1, javaPath2, [ ] : Array[##java.nio.file.StandardCopyOption]);
                                  k(Ok(true))
                case Ok(false) => k(Err(IOError("Parameter 'src' of moveFile is not a regular file")))
                case Err(IOError(errorString)) => k(Err(IOError(errorString)))
            }
        }
        catch {
            case ex: ##java.nio.file.FileAlreadyExistsException => k(Err(IOError("Destination 'dst' already exists. Overwriting is not currently possible")))
        }
    }

    ///
    /// Effectfully moves the directory from the `src` path to the `dst` path.
    ///
    pub eff moveDirectory[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `moveDirectory`.
    ///
    handler moveDirectory[a](src: Path, dst: Path, k: Result[Bool, IOError] -> a): a = {
        try {
            let handlerResponse = isDirectory(src, result -> result);
            match handlerResponse with {
                case Ok(true)  => let Path(javaPath1) = src;
                                  let Path(javaPath2) = dst;
                                  let _ = native method java.nio.file.Files.move(javaPath1, javaPath2, [ ] : Array[##java.nio.file.StandardCopyOption]);
                                  k(Ok(true))
                case Ok(false) => k(Err(IOError("Parameter 'src' of moveFile is not a directory")))
                case Err(IOError(errorString)) => k(Err(IOError(errorString)))
            }
        }
        catch {
            case ex: ##java.nio.file.FileAlreadyExistsException => k(Err(IOError("Destination 'dst' already exists. Overwriting is not currently possible")))
            case ex: ##java.io.IOException                      => k(Err(IOError("Unable to copy non-empty folder to other filestore")))
        }
    }

    ///
    /// Effectfully deletes the file for the given path `p`.
    ///
    pub eff deleteFile[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `deleteFile`.
    ///
    handler deleteFile[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        if (exists(p))
            let Path(javaPath) = p;
            let directoryResponse = isDirectory(p, result -> result);
            match directoryResponse with {
                case Ok(true) => k(Err(IOError("The provided Path 'p' points to a directory and not a file")))
                case Ok(false) => let _ = native method java.nio.file.Files.delete(javaPath);
                                  k(Ok(true))
                case Err(IOError(errorString)) => k(Err(IOError(errorString)))
            }
        else
            k(Err(IOError("Argument 'p' does not point to any existing file or directory")))
    }

    ///
    /// Effectfully deletes the directory (and its sub-directories) for the given path `p`.
    ///
    pub eff deleteDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a

    ///
    /// Default handler for `deleteDirectory`.
    ///
    handler deleteDirectory[a](p: Path, k: Result[Bool, IOError] -> a): a = {
        try {
            let directoryResponse = isDirectory(p, result -> result);
            match directoryResponse with {
                case Ok(true) => let Path(javaPath) = p;
                                 let _ = native method java.nio.file.Files.delete(javaPath);
                                 k(Ok(true))
                case Ok(false) => k(Err(IOError("The provided Path 'p' points to a file and not a directory")))
                case Err(IOError(errorString)) => k(Err(IOError(errorString)))
            }
        }
        catch {
            case ex: ##java.nio.file.DirectoryNotEmptyException => k(Err(IOError("Directory pointed to by 'p' is not empty. This is required")))
        }
        //Notice that this requires sub directories to be empty as is. (DirectoryNotEmptyException)
        //Will require calls to deleteFile when walking fileTree to implement subdirectory deletion fully.
        //TODO: Consider whether to add a deleteDirectory(true) version for auto-deleting subdirectories?
    }

    ///
    /// Effectfully returns the size of the given path `p`.
    ///
    pub eff size[a](p: Path, k: Result[Int64, IOError] -> a): a

    ///
    /// Default handler for `size`.
    ///
    handler size[a](p: Path, k: Result[Int64, IOError] -> a): a = {
        let handlerResponse = isRegularFile(p, result -> result);
        match handlerResponse with {
            case Ok(true)  => let Path(javaPath) = p;
                              let sizeAnswer = native method java.nio.file.Files.size(javaPath);
                              k(Ok(sizeAnswer))
            case Ok(false) => k(Err(IOError("Path.Size currently only supports regular files")))
            //TODO: java.nio.files does not have system-independent .size() for directories. Write own here.
            //Possible to use own children method for this purpose
            case Err(IOError(errorString)) => k(Err(IOError(errorString)))
        }
    }

    ///
    /// Effectfully returns a path that references a new temporary directory.
    ///
    pub eff newTemporaryDirectory[a](k: Result[Path, IOError] -> a): a

    ///
    /// Default handler for `newTemporaryDirectory`.
    ///
    handler newTemporaryDirectory[a](k: Result[Path, IOError] -> a): a = {
        try {
            let prefixString = "FlixTemp";
            let returnedPath = native method java.nio.file.Files.createTempDirectory(prefixString, [ ] : Array[##java.nio.file.attribute.FileAttribute]);
            k(Ok(Path(returnedPath)))
        }
        catch {
            case ex: ##java.io.IOException => k(Err(IOError("Either an I/O error occured, or the temporary-file directory does not exist")))
        }
    }

    ///
    /// Effectfully returns a path that references a new temporary file.
    ///
    pub eff newTemporaryFile[a](k: Result[Path, IOError] -> a): a

    ///
    /// Default handler for `newTemporaryFile`.
    ///
    handler newTemporaryFile[a](k: Result[Path, IOError] -> a): a = {
        try {
            let prefixString = "TempFlixFile";
            let suffixString = ".tmp";
            let returnedPath = native method java.nio.file.Files.createTempFile(prefixString, suffixString, [ ] : Array[##java.nio.file.attribute.FileAttribute]);
            k(Ok(Path(returnedPath)))
        }
        catch {
            case ex: ##java.io.IOException => k(Err(IOError("Either an I/O error occured, or the temporary-file directory does not exist")))
        }
    }

}
