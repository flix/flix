namespace Manifest {
    pub type alias Author = { name: String
                            , email: String
                            }

    pub type alias Package = { name: String
                             , version: SemVer.SemVer
                             , authors: List[Author]
                             , description: String
                             , license: String
                             , depends: Map[String, SemVer.SemVer]
                             , home: Option[String]
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
        use SemVer.{isCompatible, SemVer};

        let package_one_versions = empty()
            |> insert(SemVer.SemVer(0, 1, 0), empty())
            |> insert(SemVer.SemVer(0, 1, 1), empty())
            |> insert(SemVer.SemVer(0, 2, 0), empty())
            |> insert(SemVer.SemVer(0, 2, 1), empty() |> insert("package_two", SemVer.SemVer(0, 1, 0)));

        let package_two_versions = empty()
            |> insert(SemVer.SemVer(0, 1, 0), empty())
            |> insert(SemVer.SemVer(0, 1, 1), empty())
            |> insert(SemVer.SemVer(0, 2, 0), empty() |> insert("package_one", SemVer.SemVer(0, 1, 0)));

        let all_packages = empty()
            |> insert("package_one", package_one_versions)
            |> insert("package_two", package_two_versions);

        let versions = getWithDefault(package, empty(), all_packages);

        let compat_versions = filterWithKey((k, _) -> isCompatible(version, k), versions);

        compat_versions
    }

    // TODO: Add error node here or return Result from `lockPackageHelper`?
    enum DependencyTree {
        case DepNode(String, SemVer.SemVer, List[DependencyTree])
    }

    def newer(k: SemVer.SemVer, v: a, kx: SemVer.SemVer, vx: a): (SemVer.SemVer,  a) = {
        let cmp = k <=> kx;
        if (cmp < 0) (kx, vx) else (k, v)
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
    pub def initPackage(name: String): /* Package */ Result[Path, InitError] & Impure = {
        use Path.add;

        let getProjectDir = (base) -> base.add(name);

        // Project directories
        let getBuildDir = (base) -> getProjectDir(base).add("build");
        let getSourceDir = (base) -> getProjectDir(base).add("src");
        let getTestDir = (base) -> getProjectDir(base).add("test");

        // Top-level files
        let getPackageFile = (base) -> getProjectDir(base).add("package.sn");
        let getHistoryFile = (base) -> getProjectDir(base).add("HISTORY.md");
        let getLicenseFile = (base) -> getProjectDir(base).add("LICENSE.md");
        let getReadmeFile = (base) -> getProjectDir(base).add("README.md");

        // Source files
        let getMainSourceFile = (base) -> getSourceDir(base).add("Main.flix");
        let getMainTestFile = (base) -> getTestDir(base).add("TestMain.flix");

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
                    use Result.flatMap;
                    let* path: Path = Result.map(f, workingDir);
                    let* exists: Bool = Path.exists(path);
                    Ok((path, exists))
                })
            |> List.filterMap(Result.toOption)
            |> List.find(snd);

        match pathExists {
            case None => {
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

                Result.mapErr(_ -> GenericIoError, Result.map(getSourceDir, workingDir))
            }
            case Some((d, _)) => Err(PathAlreadyExists(d))
        }
    }

    pub enum InitError {
        case PathAlreadyExists(Path)
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
            use Manifest.{LockedPackage, lockPackage, newLockedPackage};
            use Map.{empty, insert};
            use SemVer.SemVer;

            let package = { name = "some_package"
                          , version = SemVer.SemVer(0, 1, 0)
                          , authors = {name = "Some Guy", email = "test@test.com"} :: Nil
                          , description = "Some package"
                          // This could be mandatory
                          , license = "MIT"
                          // Mapping package dependency names to the semver pattern
                          , depends = empty() |> insert("package_one", SemVer.SemVer(0, 1, 0))
                          , home = None
                          };

            let locked = lockPackage(package);

            let check = newLockedPackage("some_package",  SemVer.SemVer(0,1,0), empty() |> insert("package_one", SemVer.SemVer(0, 1, 1)))
                :: newLockedPackage("package_one", SemVer.SemVer(0,1,1), empty())
                :: Nil;

            let first = test("lockingIsExpected1", () -> List.forall(containedIn(locked), check));

            let package2 = {
                name = "another_package",
                version = SemVer.SemVer(0, 1, 0),
                authors = {name = "Some Guy", email = "test@test.com"} :: Nil,
                description = "Some package",
                license = "MIT",
                depends = empty() |> insert("package_one", SemVer.SemVer(0, 2, 0)),
                home = None
            };

            let locked2 = lockPackage(package2);

            let check2 = newLockedPackage("another_package", SemVer.SemVer(0, 1, 0), empty() |> insert("package_one", SemVer.SemVer(0, 2, 1)) |> insert("package_two", SemVer.SemVer(0, 1, 1)))
                :: newLockedPackage("package_one", SemVer.SemVer(0, 2, 1), empty() |> insert("package_two", SemVer.SemVer(0, 1, 1)))
                :: newLockedPackage("package_two", SemVer.SemVer(0, 1, 1), empty())
                :: Nil;

            let second = test("lockingIsExpected2", () -> List.forall(containedIn(locked2), check2));

            first && second
        }

        pub def allTests(): Bool & Impure = {
            testLocking()
        }
    }
}