pub enum SemVerComponent {
    case Integer(Int),
    case Wildcard,
    case NotPresent
}

opaque type SemVer = { major: SemVerComponent
                     , minor: SemVerComponent
                     , patch: SemVerComponent
                     }

// Temporary
opaque type SpecificSemVer = SemVer

opaque type Package = { name: String
                      , authors: List[String]
                      , description: String
                      // This could be an enum. Would only support predefined licenses, or require an `Other` variant
                      , license: String
                      // Mapping package names to the semver pattern
                      , depends: List[(String, SemVer)]
                      , homepage: String // Url?
                      }

opaque type LockedPackage = { name: String
                            // All transitive dependencies, their specific version, and where they were retrieved from.
                            , transitive: List[(String, List[SpecificSemVer])]
                            }

// Add a new package to the package. Handle superseding old versions
pub def addDependency(self: Package, package: String, version: SemVer): Package = ???

// Remove a package from the list of dependencies
pub def removeDependency(self: Package, package: String): Package = ???

// Resolve dependency tree and create a list of all dependencies and their transitive dependencies
pub def lockPackage(self: Package): List[LockedPackage] = ???
