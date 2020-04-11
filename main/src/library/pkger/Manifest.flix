namespace Manifest {
    pub enum License {
        case MIT
        case GPLv3
        case Apache2
        case BSD
        case Other(String)
    }

    pub type alias Author = { name: String
                            // Is there an email datatype, or is it just commonly a String?
                            , email: String
                            }

    pub type alias Package = { name: String
                             , version: SemVer.SemVer
                             , authors: List[Author]
                             , description: String
                             // This could be mandatory
                             , license: Option[License]
                             // Mapping package dependency names to the semver pattern
                             , depends: Map[String, SemVer.SemVer]
                             , home: Option[String] // Url?
                             }

    pub type alias LockedPackage = { name: String
                                   , version: SemVer.SemVer
                                   // All transitive dependencies, their specific version, and where they were retrieved from.
                                   , transitive: Map[String, SemVer.SemVer]
                                   }

    def newLockedPackage(name: String, version: SemVer.SemVer, transitive: Map[String, SemVer.SemVer]): LockedPackage = {
        {name = name, version = version, transitive = transitive}
    }

    // Add a new package to the package. Handle superseding old versions
    pub def addDependency(self: Package, package: String, version: SemVer.SemVer): Package = {
        use Map.insert;
        let selfx = self.removeDependency(package);
        let addedPackage = insert(package, version, selfx.depends);
        { depends = addedPackage | self }
    }

    // Remove a package from the list of dependencies
    pub def removeDependency(self: Package, package: String): Package = {
        let removedPackage = Map.delete(package, self.depends);
        { depends = removedPackage | self }
    }

    def packageRepository(package: String, version: SemVer.SemVer): Map[SemVer.SemVer, Map[String, SemVer.SemVer]] = {
        use Map.{empty, filterWithKey, getWithDefault, insert};
        use SemVer.isWithinBounds;

        let package_one_versions = empty()
            |> insert((0, 1, 0), empty())
            |> insert((0, 1, 1), empty())
            |> insert((0, 2, 0), empty())
            |> insert((0, 2, 1), empty() |> insert("package_two", (0, 1, 0)));

        let package_two_versions = empty()
            |> insert((0, 1, 0), empty())
            |> insert((0, 1, 1), empty())
            |> insert((0, 2, 0), empty() |> insert("package_one", (0, 1, 0)));

        let all_packages = empty()
            |> insert("package_one", package_one_versions)
            |> insert("package_two", package_two_versions);

        let versions = getWithDefault(package, empty(), all_packages);

        let compat_versions = filterWithKey((k, _) -> isWithinBounds(version, k), versions);

        compat_versions
    }

    // TODO: Add error node here or return Result from `lockPackageHelper`?
    enum DependencyTree {
        case DepNode(String, SemVer.SemVer, List[DependencyTree])
    }

    def newer(k: SemVer.SemVer, v: a, kx: SemVer.SemVer, vx: a): (SemVer.SemVer,  a) = {
        use SemVer.{cmpSemVer, Ord};
        let cmp = cmpSemVer(k, kx);
        if (cmp == Ord.Lt) (kx, vx) else (k, v)
    }

    def lockPackageHelper(package: String, bound: SemVer.SemVer): DependencyTree = {
        use Map.{toList, mapWithKey, valuesOf};
        use List.{map, find, filterMap};

        let dependencies = packageRepository(package, bound)
            |> Map.reduceWithKey(newer);

        match dependencies {
            case Some((version, dependencies)) => {
                DepNode(package, version, dependencies |> mapWithKey((p, b) -> lockPackageHelper(p, b)) |> valuesOf)
            }
            case None => {
                // TODO: No packages with that name and version combo
                ???
            }
        }
    }

    def flattenDependencyTree(tree: DependencyTree): List[(String, SemVer.SemVer, List[(String, SemVer.SemVer)])] = {
        match tree {
            case DepNode(package, version, dependencies) => {
                let x = List.flatMap(flattenDependencyTree, dependencies);
                let flatDep = List.flatMap(match (p, v, d) -> (p, v) :: d, x);
                (package, version, flatDep) :: x
            }
        }
    }

    // Resolve dependency tree and create a list of all dependencies and their transitive dependencies
    pub def lockPackage(self: Package): List[LockedPackage] = {
        DepNode( self.name
               , self.version
               , Map.toList(self.depends) |> List.map(match (p, b) -> lockPackageHelper(p, b))
               )
            |> flattenDependencyTree
            |> List.map(match (package, version, dependencies) -> newLockedPackage(package, version, List.toMap(dependencies)))
    }

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

        // Ensure that none of the paths the init process creates are currently present on the system
        let pathExists = [files, dirs]
            |> Array.toList
            |> List.flatten
            |> List.map(f -> {
                    let dir = Result.map(f, workingDir);
                    match Result.flatMap(Path.exists, dir) {
                        case Ok(exists) => exists,
                        case _ => true
                    }
                })
            |> List.find(x -> x);

        if (Option.isEmpty(pathExists)) {
            // Create all directories required for the package
            List.map(f -> {
                let dir = Result.map(f, workingDir);
                Result.flatMap(Path.mkdirs, dir)
            }, dirs);

            let quote = (s) -> Char.toString('"') + s + Char.toString('"');

            // TODO: Abstract this
            let newPackageFile = newFile([
                "(package",
                "  (name " + name + ")",
                "  (version " + quote("0.1.0") + ")",
                // Empty list/empty value?
                "  (authors ((name " + quote("Rasmus Larsen") + ") (email " + quote("doctorherbstein@gmail.com") + ")))",
                // Should description be quoted?
                "  (description " + quote(name) + ")",
             // "  (license GPLv3)",
             // "  (license (name LGPLv2)",
                "  (license (file " + quote("LICENSE") + ")",
                "  (depends (other_package " + quote("1.2.3") + "))",
                "  (home " + quote("https://github.com/Herbstein/package_one") + ")",
                ")"
            ]);
            Result.flatMap(newPackageFile, Result.map(getPackageFile, workingDir));

            let newHistoryFile = newFile(["### v0.1.0", "  Initial Release"]);
            Result.flatMap(newHistoryFile, Result.map(getHistoryFile, workingDir));

            let newLicenseFile = newFile(["<<ENTER LICENSE INFORMATION>>"]);
            Result.flatMap(newLicenseFile, Result.map(getLicenseFile, workingDir));

            let newReadmeFile = newFile(["# " + name, "", "Enter some useful information", ""]);
            Result.flatMap(newReadmeFile, Result.map(getReadmeFile, workingDir));

            let newMainSourceFile = newFile(["// The main entry point.", "def main(): Unit & Impure = Console.printLine(" + quote("Hello, World") + ")"]);
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

    namespace Test {
        def transitiveContains(mx: Map[String, SemVer.SemVer], m: (String, SemVer.SemVer)): Bool = {
            Map.exists((k, v) -> match m { case (p, s) => p == k && s == v }, mx)
        }

        def containedIn(xs: List[Manifest.LockedPackage], package: Manifest.LockedPackage): Bool = {
            use Manifest.LockedPackage;

            let transitive = (p) -> Map.forall((k, v) -> transitiveContains(p.transitive, (k, v)), package.transitive);

            List.exists(p -> {
                p.name == package.name
                    && p.version == package.version
                    && transitive(p)
            }, xs)
        }

        def testLocking(): Bool & Impure = {
            use Test.test;
            use Manifest.{License, LockedPackage, lockPackage, newLockedPackage};
            use Map.{empty, insert};

            let package = { name = "some_package"
                          , version = (0, 1, 0)
                          , authors = {name = "Some Guy", email = "test@test.com"} :: Nil
                          , description = "Some package"
                          // This could be mandatory
                          , license = Some(License.MIT)
                          // Mapping package dependency names to the semver pattern
                          , depends = empty() |> insert("package_one", (0, 1, 0))
                          , home = None
                          };

            let locked = lockPackage(package);

            let check = newLockedPackage("some_package",  (0,1,0), empty() |> insert("package_one", (0, 1, 1)))
                :: newLockedPackage("package_one", (0,1,1), empty())
                :: Nil;

            let first = test("lockingIsExpected1", () -> List.forall(containedIn(locked), check));

            let package2 = {
                name = "another_package",
                version = (0, 1, 0),
                authors = {name = "Some Guy", email = "test@test.com"} :: Nil,
                description = "Some package",
                license = Some(License.MIT),
                depends = empty() |> insert("package_one", (0, 2, 0)),
                home = None
            };

            let locked2 = lockPackage(package2);

            let check2 = newLockedPackage("another_package", (0, 1, 0), empty() |> insert("package_one", (0, 2, 1)) |> insert("package_two", (0, 1, 1)))
                :: newLockedPackage("package_one", (0, 2, 1), empty() |> insert("package_two", (0, 1, 1)))
                :: newLockedPackage("package_two", (0, 1, 1), empty())
                :: Nil;

            let second = test("lockingIsExpected2", () -> List.forall(containedIn(locked2), check2));

            first && second
        }

        pub def allTests(): Bool & Impure = {
            testLocking()
        }
    }
}