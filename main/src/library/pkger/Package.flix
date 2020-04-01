namespace Package {
    use SemVer.SemVer

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
                             , version: SemVer
                             , authors: List[Author]
                             , description: String
                             // This could be mandatory
                             , license: Option[License]
                             // Mapping package dependency names to the semver pattern
                             , depends: Map[String, SemVer]
                             , home: Option[String] // Url?
                             }

    pub opaque type LockedPackage = { name: String
                                    // All transitive dependencies, their specific version, and where they were retrieved from.
                                    , transitive: Map[String, SemVer]
                                    }

    // Add a new package to the package. Handle superseding old versions
    pub def addDependency(self: Package, package: String, version: SemVer): Package = {
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

    def packageRepository(package: String, version: SemVer): Map[SemVer, Map[String, SemVer]] = {
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

        let compat_versions = filterWithKey((k, _) -> isWithinBounds(k, version), versions);

        compat_versions
    }

    // Resolve dependency tree and create a list of all dependencies and their transitive dependencies
    pub def lockPackage(self: Package): List[LockedPackage] = {
        use Map.toList;
        use List.map;

        let x = toList(self.depends)
            |> map(pair -> (fst(pair), packageRepository(fst(pair), snd(pair))));

        ???
    }
}