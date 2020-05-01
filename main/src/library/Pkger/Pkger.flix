use Core/Io/File.{canonical, File, getParent, mkdirs, new, resolve};
use Core/Io/IOError.IOError;
use Result.flatMap;

namespace Pkger/Pkger {
    pub enum InitError {
        case PathAlreadyExists(File)
        case InitIOError(IOError)
    }

    //def IOErrorToInit(err: IOError): InitError {}

    def mapIOErrorInit(r: Result[a, IOError]): Result[a, InitError] = Result.mapErr(err -> InitIOError(err), r)

    ///
    /// Create a new directory with called `name` in the current working directory and initialize a project called `name` inside of it.
    ///
    @test
    pub def initPackage(name: String): Result[Unit, InitError] & Impure = {
        let getProjectDir = (base) -> base.resolve(name);
        let getSourceDir = (base) -> getProjectDir(base).resolve("src");
        let getMainSourceFile = (base) -> getSourceDir(base).resolve("Main.flix");

        let newFile = (content) -> (path) -> {
            let* optParent = getParent(path) |> mapIOErrorInit;
            let* _ = match optParent {
                case Some(parent) => mkdirs(parent) |> mapIOErrorInit
                case _ => Ok(true)
            };
            Core/Io/File.writeLines(path, Array.toList(content)) |> mapIOErrorInit
        };

        let workingDir = new(".");

        // Ensure that none of the paths the init process creates are currently present on the system
        let pathExists = [getProjectDir, getMainSourceFile, getSourceDir]
            |> Array.map(f -> {
                    let path = f(workingDir);
                    let* exists = Core/Io/File.exists(path);
                    Ok((path, exists))
                })
            |> Array.filterMap(Result.toOption)
            |> Array.find(snd);

        match pathExists {
            case None => {
                let quote = s -> Char.toString('"') + s + Char.toString('"');

                let newMainSourceFile = newFile(["// The main entry point.", "def main(): Unit & Impure = Console.printLine(" + quote("Hello, World") + ")"]);
                let* _ = newMainSourceFile(getMainSourceFile(workingDir));

                Ok(())
            }
            case Some((d, _)) => {
                let* c = canonical(d) |> mapIOErrorInit;
                Err(PathAlreadyExists(c))
            }
        }
    }
    /*

    pub enum PackagingError {
        case RootNotDirectory(File)
        case NoChildren(File)
        case IOError(IOError)
    }

    def mapIOErrorPackaging(r: Result[a, IOError]): Result[a, PackagingError] = Result.mapErr(err -> IOError(err), r)

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
    */

}

pub def main(): Result[Unit, Pkger/Pkger.InitError] & Impure = Pkger/Pkger.initPackage("package_test")