// Could add an enum specifying which component can be updated safely (e.g. `^` and `~` in NPM)
pub opaque type SemVer = (Int, Option[(Int, Option[Int])], String)

// Same type as `SemVer`, but all `Option` chaining is gone
pub opaque type SpecificSemVer = (Int, Int, Int, String)

pub enum License {
    case MIT,
    case GPLv3
    case Apache2
    case BSD
    case Other(String)
}

pub opaque type Author = { name: String
                         // Is there an email datatype, or is it just commonly a String?
                         , email: String
                         }

pub opaque type Package = { name: String
                          , version: SpecificSemVer
                          , authors: List[Author]
                          , description: String
                          // This could be mandatory
                          , license: Option[License]
                          // Mapping package dependency names to the semver pattern
                          , depends: List[(String, SemVer)]
                          , homepage: Option[String] // Url?
                          }

pub opaque type LockedPackage = { name: String
                                // All transitive dependencies, their specific version, and where they were retrieved from.
                                , transitive: List[(String, List[SpecificSemVer])]
                                }

// Add a new package to the package. Handle superseding old versions
pub def addDependency(self: Package, package: String, version: SemVer): Package = ???

// Remove a package from the list of dependencies
pub def removeDependency(self: Package, package: String): Package = ???

// Resolve dependency tree and create a list of all dependencies and their transitive dependencies
pub def lockPackage(self: Package): List[LockedPackage] = ???

def _newDirectory(p: Path): Unit = ???

// Create a new package. Everything but the name of the package can be populated with sensible defaults from the environment
pub def initPackage(name: String): /* Package */ Result[Path, ErrorFull] & Impure = {
    let getProjectDir = (base) -> Path.add(base, name);

    let getBuildDir = (base) -> Path.add(getProjectDir(base), "build");
    let getSourceDir = (base) -> Path.add(getProjectDir(base), "src");
    let getTestDir = (base) -> Path.add(getProjectDir(base), "test");

    let getPackageFile = (base) -> Path.add(getProjectDir(base), "package.sn");
    let getHistoryFile = (base) -> Path.add(getProjectDir(base), "HISTORY.md");
    let getLicenseFile = (base) -> Path.add(getProjectDir(base), "LICENSE.md");
    let getReadmeFile = (base) -> Path.add(getProjectDir(base), "README.md");

    let getMainSourceFile = (base) -> Path.add(getSourceDir(base), "Main.flix");
    let getMainTestFile = (base) -> Path.add(getTestDir(base), "TestMain.flix");

    let workingDir = Path.workingDirectory();

    let files = Array.toList([getPackageFile, getHistoryFile, getLicenseFile, getReadmeFile, getMainSourceFile, getMainTestFile]);
    let dirs = Array.toList([getBuildDir, getSourceDir, getTestDir]);

    let combined: List[Path -> Path] = List.flatten(Array.toList([files, dirs]));
    let _pathExists = List.find(f -> {
        let dir = Result.map(f, workingDir);
        match Result.flatMap(Path.exists, dir) {
            case Ok(exists) => exists
            case _ => false
        }
    }, combined);

    let _test = List.map(f -> {
        let dir = Result.map(f, workingDir);
        Result.flatMap(Path.mkdirs, dir)
    }, dirs);


    Result.mapErr(_ -> GenericIoError, Result.map(getSourceDir, workingDir))
}

pub enum ErrorFull {
    case ProjectDirExists
    case GenericIoError
}

pub def main(): Result[Path, ErrorFull] & Impure = initPackage("package_test")
