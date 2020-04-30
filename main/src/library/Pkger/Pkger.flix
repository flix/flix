use Core/Io/File.{add, File};
use Result.flatMap;

namespace Pkger/Pkger {
    pub enum InitError {
        case PathAlreadyExists(File)
        case GenericIoError
    }

    // Create a new package. Everything but the name of the package can be populated with sensible defaults from the environment
    pub def initPackage(name: String): /* Package */ Result[File, InitError] & Impure = {
        let getProjectDir = (base) -> base.add(name);
        let getSourceDir = (base) -> getProjectDir(base).add("src");
        let getMainSourceFile = (base) -> getSourceDir(base).add("Main.flix");

        let newFile = (content) -> (path) -> {
            Core/Io/File.writeLines(path, Array.toList(content))
        };

        let* workingDir = Core/Io/File.workingDirectory();

        // Ensure that none of the paths the init process creates are currently present on the system
        let pathExists = [getProjectDir, getMainSourceFile, getSourceDir]
            |> Array.map(f -> {
                    let* path = f(workingDir);
                    let* exists = Core/Io/File.exists(path);
                    Ok((path, exists))
                })
            |> Array.filterMap(Result.toOption)
            |> Array.find(snd);

        match pathExists {
            case None => {
                // Create all directories required for the package
                let dir = Result.map(getProjectDir, workingDir);
                let* _ = Result.flatMap(Core/Io/File.mkdirs, dir)

                let newHistoryFile = newFile(["### v0.1.0", "  Initial Release"]);
                let* _ = Result.flatMap(newHistoryFile, Result.map(getHistoryFile, workingDir));

                let newLicenseFile = newFile(["<<ENTER LICENSE INFORMATION>>"]);
                Result.flatMap(newLicenseFile, Result.map(getLicenseFile, workingDir));

                let newReadmeFile = newFile(["# " + name, "", "Enter some useful information", ""]);
                Result.flatMap(newReadmeFile, Result.map(getReadmeFile, workingDir));

                let newMainSourceFile = newFile(["// The main entry point.", "def main(): Unit & Impure = Console.printLine(" + quote("Hello, World") + ")"]);
                Result.flatMap(newMainSourceFile, Result.map(getMainSourceFile, workingDir));

                let newMainTestFile = newFile(["@test", "def testMain01(): Bool & Impure = main() == ()"]);
                Result.flatMap(newMainTestFile, Result.map(getMainTestFile, workingDir));

                Result.mapErr(_ -> GenericIoError, Result.map(getSourceDir, workingDir))
            }
            case Some((d, _)) => Err(PathAlreadyExists(d))
        }
    }

    pub enum PackagingError {
        case NoManifestInRoot(File)
        case RootNotDirectory(File)
        case NoChildren(File)
        case IOError(IOError)
    }

    def mapIOError(r: Result[a, IOError]): Result[a, PackagingError] = Result.mapErr(err -> IOError(err), r)

    def _mapBool(f: a -> Result[Bool, t] & e, a: a): Bool & e = Result.getWithDefault(f(a), false)

    def _allChildren(root: File): Result[List[File], PackagingError] & Impure = {
        use Core/Io/File.{add, exists, getChildren, isDirectory};
        let* childrenRes = getChildren(root) |> mapIOError;
        let* children = match childrenRes {
            case Some()
        }
        // let neg = b -> !b;
        // let files = List.filter(f -> mapBool(isDirectory >> neg, f) && mapBool(exists, f));
        ???
    }

    pub def filesForPackaging(root: File): Result[List[File], PackagingError] & Impure = {
        use Core/Io/File.{add, exists, getChildren, isDirectory};
        let* isDir = isDirectory(root) |> mapIOError;
        let* _ = if (isDir) Ok(()) else Err(RootNotDirectory(root));
        let* optionalChildren = getChildren(root) |> mapIOError;
        // We know `root` is a directory
        let children = Option.getWithDefault(optionalChildren, Nil);
        let packageFile = add(root, "package.toml");
        let* packageFileExists = exists(packageFile) |> mapIOError;
        let* packageFileIsDir = isDirectory(packageFile) |> mapIOError;
        let* _ = if (packageFileExists && !packageFileIsDir) Ok(()) else Err(NoManifestInRoot(root));
        Ok(children)
    }

}