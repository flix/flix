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

// Create a new package. Everything but the name of the package can be populated with sensible defaults from the environment
pub def initPackage(name: String): /* Package */ Result[Path, Error] & Impure = {
    let getProjectDir = base -> Path.add(base, name);

    let flatMap = Result.flatMap;
    let mapErr = Result.mapErr;
    let map = Result.map;

    let workingDir = Path.workingDirectory(); // Result[Path, IOError]
    let projectDir = map(d -> getProjectDir(d), workingDir); // Result[Path, IOError]
    let projectDirExists = flatMap(Path.exists, projectDir); // Result[Bool, IOError]
    let mapErred = mapErr(_ -> GenericIoError, projectDirExists); // Result[Bool, Error]

    flatMap(b -> if (!b) mapErr(_ -> GenericIoError, projectDir) else {Err(ProjectDirExists) as & Impure}, mapErred) // Result[Path, Error]
}

pub enum Error {
    case ProjectDirExists
    case GenericIoError
}

def const(a: a): b -> a = _ -> a

pub def main(): Result[Path, Error] & Impure = initPackage("test")
