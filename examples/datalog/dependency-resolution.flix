///
/// Package dependency resolution using Datalog.
///
/// Demonstrates transitive dependency computation with version constraints,
/// detection of unsatisfied dependencies, and provenance queries to trace
/// why a problematic dependency was needed.
///

///
/// We introduce type aliases to model the package domain.
///
type alias Package = String    // e.g. "react"
type alias Version = Int32     // e.g. 3 (simplified semver)

///
/// We detect unsatisfied dependencies for a root package and use
/// provenance queries to trace why each was needed.
///
def main(): Unit \ IO =
    let root = "my-app";
    let rootVersion = 1;

    let db = database();

    // A Datalog program that computes transitive dependency resolution.
    let pr = #{
        // Root package is resolved.
        Resolved(root, rootVersion).

        // Transitive: if pkg@pkgVer is resolved and depends on dep with minVer,
        // resolve all available versions of dep that satisfy the constraint.
        Resolved(dep, depVer) :-
            Resolved(pkg, pkgVer),
            Dependency(pkg, pkgVer, dep, minVer),
            Published(dep, depVer),
            if (depVer >= minVer).

        // A dependency has at least one satisfying version.
        Satisfied(pkg, pkgVer, dep, minVer) :-
            Dependency(pkg, pkgVer, dep, minVer),
            Published(dep, depVer),
            if (depVer >= minVer).

        // Missing = resolved package has a dependency that can't be met.
        Missing(pkg, pkgVer, dep, minVer) :-
            Resolved(pkg, pkgVer),
            Dependency(pkg, pkgVer, dep, minVer),
            not Satisfied(pkg, pkgVer, dep, minVer).
    };

    // Query all missing dependencies.
    let missing = query db, pr select (pkg, pkgVer, dep, minVer) from Missing(pkg, pkgVer, dep, minVer);
    println("Missing dependencies:");
    foreach((pkg, pkgVer, dep, minVer) <- missing) {
        println("  ${pkg}@${pkgVer} requires ${dep} >= ${minVer} (MISSING)");

        // Use pquery to trace why pkg@pkgVer was needed.
        let chain = pquery db, pr select Resolved(pkg, pkgVer) with {Dependency};
        println("  Dependency chain:");
        foreach(fact <- chain) {
            ematch fact {
                case Dependency(fromPkg, fromVer, toPkg, toMinVer) => println("    ${fromPkg}@${fromVer} requires ${toPkg} >= ${toMinVer}")
            }
        }
    }

///
/// Returns the database of available packages and their dependencies.
///
/// The database contains two kinds of facts:
/// - `Published(pkg, version)`: Declares that a package exists at a given version.
/// - `Dependency(pkg, version, depPkg, minVersion)`: Declares that `pkg@version`
///    requires `depPkg` with at least `minVersion`.
///
def database(): #{ Published(Package, Version), Dependency(Package, Version, Package, Version) | r } = #{
    // Package registry - available packages and versions
    Published("my-app", 1).
    Published("react", 17).
    Published("react", 18).
    Published("react-dom", 17).
    Published("react-dom", 18).
    Published("next", 13).
    Published("next", 14).
    Published("next-themes", 1).
    Published("postcss", 7).
    Published("postcss", 8).
    Published("autoprefixer", 9).
    Published("autoprefixer", 10).
    Published("string-formatter", 1).
    // No left-pad available - it was unpublished!

    // Dependency declarations: Dependency(pkg, version, depPkg, minVersion)
    Dependency("my-app", 1, "react-dom", 18).
    Dependency("my-app", 1, "next", 14).
    Dependency("react-dom", 18, "react", 18).
    Dependency("next", 14, "react", 18).
    Dependency("next", 14, "postcss", 8).
    Dependency("next", 14, "next-themes", 1).
    Dependency("postcss", 8, "autoprefixer", 9).
    Dependency("next-themes", 1, "string-formatter", 1).
    Dependency("string-formatter", 1, "left-pad", 1).  // unpublished!
}
