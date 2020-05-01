use Core/Io/File.{canonical, exists, File, getParent, isDirectory, mkdirs, new, resolve};
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
        let getMainSourceFile = (base) -> getProjectDir(base).resolve("Main.flix");

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
        let pathExists = [getProjectDir, getMainSourceFile]
            |> Array.map(f -> {
                    let path = f(workingDir);
                    let* e = exists(path);
                    Ok((path, e))
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

    pub enum PackagingError {
        case RootNotDirectory(File)
        case NoSourceFile
        case PackagingIOError(IOError)
    }

    def mapIOErrorPackaging(r: Result[a, IOError]): Result[a, PackagingError] = Result.mapErr(err -> PackagingIOError(err), r)

    @test
    pub def buildFiles(rootString: String): Array[##java.io.File] & Impure = {
        let res = {
            let root = new(rootString);
            let* isDir = isDirectory(root) |> mapIOErrorPackaging;
            let* _ = if (isDir) Ok(()) else Err(RootNotDirectory(root));
            // We know `root` is a directory
            let sourceFile = resolve(root, "Main.flix");
            let* sourceFileExists = exists(sourceFile) |> mapIOErrorPackaging;
            let* children = if (sourceFileExists) Ok([sourceFile]) else Err(NoSourceFile);
            Ok(children)
        };
        match res {
            case Ok(arr) => Array.map(a -> { let File.File(javaFile) = a; javaFile }, arr),
            case _ => {
                Console.printLine("empty");
                []
            }
        }
    }
}

// Example usage
// pub def main(): Result[Unit, Pkger/Pkger.InitError] & Impure = Pkger/Pkger.initPackage("package_test")
// pub def main(): List[File] & Impure = Pkger/Pkger.buildFiles("package_test") |> Array.toList |> List.map(a -> { File.File(a) })