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

    def newer(k: SemVer.SemVer, v: a, kx: SemVer.SemVer, vx: a): (SemVer.SemVer,  a) = {
        use SemVer.{cmpSemVer, Ord};
        let cmp = cmpSemVer(k, kx);
        if (cmp == Ord.Lt) (kx, vx) else (k, v)
    }

    def findNewest(versions:  Map[SemVer.SemVer, a]): Option[(SemVer.SemVer,  a)] = {
        use Map.reduceWithKey;

        reduceWithKey(newer, versions)
    }

    def lockPackageHelper(_name: String, dependencies: Map[String, SemVer.SemVer]): List[(String, Map[String, SemVer.SemVer])] = {
        use Map.toList;
        use List.{map, find, filterMap};

        let not = b -> !b;

        // Re-implementation without manually re-implementing Map methods
        // TODO: This silently ignores not-present dependencies in the filtering step
        let packageDependencies: Map[String, Option[(SemVer.SemVer, Map[String, SemVer.SemVer])]] = dependencies
            |> Map.mapWithKey(packageRepository)
            |> Map.map(findNewest)
            |> Map.filter(Option.isEmpty >> not); // <-- here

        let possibilitiesMap = Map.mapWithKey(packageRepository, dependencies);
        let newestMap = Map.map(findNewest, possibilitiesMap);
        let filteredMap = Map.filter(v -> !Option.isEmpty(v));

        let list: List[(String, SemVer.SemVer)] = toList(dependencies);
        let possibilities: List[(String, Map[SemVer.SemVer, Map[String, SemVer.SemVer]])] = map(match (k, v) -> (k, packageRepository(k, v)), list);
        let newest: List[(String, Option[(SemVer.SemVer,  Map[String, SemVer.SemVer])])] = map(match (k, v) -> (k, findNewest(v)), possibilities);

        // TODO: Report errors silently ignored by `filterMap`
        let filtered: List[(String, Map[String, SemVer.SemVer])] = filterMap(match (k, v) -> Option.map(vx -> (k, snd(vx)), v), newest);
        let package_dependencies: List[Map[String, SemVer.SemVer]] = map(match (k, v) -> v, filtered);

        let flattened = map(kv -> {
            let k = fst(kv);
            let v = fst(kv);
            ???
        }, filtered);

        ???
    }

    // Resolve dependency tree and create a list of all dependencies and their transitive dependencies
    pub def lockPackage(self: Package): List[LockedPackage] = {
        let dependencies = lockPackageHelper(self.name, self.depends);
        //let added = () :: dependencies;

        ???
    }
}