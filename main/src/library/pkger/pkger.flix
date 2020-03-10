// Could add an enum specifying which component can be updated safely (e.g. `^` and `~` in NPM)
pub type alias SemVer = (Int, Option[(Int, Option[Int])], Option[String])

// Same type as `SemVer`, but all `Option` chaining is gone
pub type alias SpecificSemVer = (Int, Int, Int, Option[String])

// TODO: Temp public
pub def isWithinBounds(specific: SpecificSemVer, bound: SemVer): Bool = {
    let (postFirstMajor, boundMajor) = match bound {
        case (major, _, _) => (major > 0, major)
    };

    if (postFirstMajor) {
        match specific {
            case (major, minor, patch, Some(tag)) => match bound {
                case (boundMajor, Some((boundMinor, Some(boundPatch))), Some(boundTag)) => major == boundMajor && minor == boundMinor && patch == boundPatch && tag == boundTag
                case _ => false
            }
            case (major, _, _, None) => boundMajor == major
        }
    } else {
        // TODO
        false
    }
}

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

// Create a new package. Everything but the name of the package can be populated with sensible defaults from the environment
pub def initPackage(name: String): /* Package */ Result[Path, ErrorFull] & Impure = {
    let getProjectDir = (base) -> Path.add(base, name);

    // Project directories
    let getBuildDir = (base) -> Path.add(getProjectDir(base), "build");
    let getSourceDir = (base) -> Path.add(getProjectDir(base), "src");
    let getTestDir = (base) -> Path.add(getProjectDir(base), "test");

    // Top-level files
    let getPackageFile = (base) -> Path.add(getProjectDir(base), "package.sn");
    let getHistoryFile = (base) -> Path.add(getProjectDir(base), "HISTORY.md");
    let getLicenseFile = (base) -> Path.add(getProjectDir(base), "LICENSE.md");
    let getReadmeFile = (base) -> Path.add(getProjectDir(base), "README.md");

    // Source files
    let getMainSourceFile = (base) -> Path.add(getSourceDir(base), "Main.flix");
    let getMainTestFile = (base) -> Path.add(getTestDir(base), "TestMain.flix");

    let newFile = (content) -> (path) -> {
        Path.writeLines(path, Array.toList(content))
    };

    let workingDir = Path.workingDirectory();

    let files = Array.toList([getPackageFile, getHistoryFile, getLicenseFile, getReadmeFile, getMainSourceFile, getMainTestFile]);
    let dirs = Array.toList([getBuildDir, getSourceDir, getTestDir]);

    // Ensure that none of the path the init process creates are currently present on the system
    let combined = List.flatten(Array.toList([files, dirs]));
    let pathExists = List.find(f -> {
        let dir = Result.map(f, workingDir);
        match Result.flatMap(Path.exists, dir) {
            case Ok(exists) => exists
            // Error occurred, detect error as file being present for now
            case _ => true
        }
    }, combined);

    if (Option.isEmpty(pathExists)) {
        // Create all directories required for the package
        List.map(f -> {
            let dir = Result.map(f, workingDir);
            Result.flatMap(Path.mkdirs, dir)
        }, dirs);

        let quote = Char.toString('"');

        // TODO: Abstract this
        let newPackageFile = newFile(["(package", "  (name " + name + ")", "  (version " + quote + "0.1.0" + quote + ")", ")"]);
        Result.flatMap(newPackageFile, Result.map(getPackageFile, workingDir));

        let newHistoryFile = newFile(["### v0.1.0", "  Initial Release"]);
        Result.flatMap(newHistoryFile, Result.map(getHistoryFile, workingDir));

        let newLicenseFile = newFile(["<<ENTER LICENSE INFORMATION>>"]);
        Result.flatMap(newLicenseFile, Result.map(getLicenseFile, workingDir));

        let newReadmeFile = newFile(["# " + name, "", "Enter some useful information", ""]);
        Result.flatMap(newReadmeFile, Result.map(getReadmeFile, workingDir));

        let newMainSourceFile = newFile(["// The main entry point.", "def main(): Unit & Impure = Console.printLine(" + quote + "Hello, World" + quote + ")"]);
        Result.flatMap(newMainSourceFile, Result.map(getMainSourceFile, workingDir));

        let newMainTestFile = newFile(["@test", "def testMain01(): Bool & Impure = main() == ()"]);
        Result.flatMap(newMainTestFile, Result.map(getMainTestFile, workingDir));

        // Output something that can be distinguished easily
        Result.mapErr(_ -> GenericIoError, Result.map(getSourceDir, workingDir))
    } else {
        Err(GenericIoError)
    }
}

pub enum ErrorFull {
    case ProjectDirExists
    case GenericIoError
}

pub def main(): Result[Path, ErrorFull] & Impure = initPackage("package_test")
