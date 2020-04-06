namespace Package {
    pub enum License {
        case MIT
        case GPLv3
        case Apache2
        case BSD
        case Other(String)
    }

    pub opaque type Author = { name: String
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

    pub opaque type LockedPackage = { name: String
                                    , version: SemVer.SemVer
                                    // All transitive dependencies, their specific version, and where they were retrieved from.
                                    , transitive: Map[String, SemVer.SemVer]
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
            |> insert((0, 2, 0), empty())
            |> insert((0, 2, 1), empty() |> insert("package_two", (0, 2, 0)));

        let package_two_versions = empty()
            |> insert((0, 1, 0), empty())
            |> insert((0, 2, 0), empty());

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
        use Map.toList;
        use List.{map, find, filterMap};

        let dependencies: Option[(SemVer.SemVer,  Map[String, SemVer.SemVer])] = packageRepository(package, bound)
            |> Map.reduceWithKey(newer);

        match dependencies {
            case None => {
                // TODO: No packages with that name and version combo
                ???
            }
            case Some((version, dependencies)) => {
                DepNode(package, version, toList(dependencies) |> map(match (p, b) -> lockPackageHelper(p, b)))
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
            |> List.map(match (package, version, dependencies) -> LockedPackage({ name = package
                                                                                , version = version
                                                                                , transitive = List.toMap(dependencies)
                                                                                }))
    }
}