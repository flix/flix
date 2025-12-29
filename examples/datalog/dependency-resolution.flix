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
/// We detect missing dependencies for a root package and use
/// provenance queries to trace why each was needed.
///
def main(): Unit \ IO =
    let root = "my-app";
    let rootVer = 1;

    let db = database();

    // A Datalog program that computes transitive dependency resolution.
    let pr = #{
        // Base case: the root package seeds the resolution.
        Resolved(root, rootVer).

        // Inductive case: if a package is resolved and declares a dependency,
        // resolve every published version satisfying the constraint.
        // (Datalog computes all valid candidates, not a single "best" version.)
        Resolved(dep, depVer) :-
            Resolved(pkg, pkgVer),
            Dependency(pkg, pkgVer, dep, minVer),
            Published(dep, depVer),
            if (depVer >= minVer).

        // Auxiliary predicate: true if at least one published version
        // satisfies the dependency. Used to detect unsatisfiable constraints.
        Satisfied(pkg, pkgVer, dep, minVer) :-
            Dependency(pkg, pkgVer, dep, minVer),
            Published(dep, depVer),
            if (depVer >= minVer).

        // A dependency is missing when a resolved package requires something
        // that has no satisfying version (negation-as-failure over Satisfied).
        Missing(pkg, pkgVer, dep, minVer) :-
            Resolved(pkg, pkgVer),
            Dependency(pkg, pkgVer, dep, minVer),
            not Satisfied(pkg, pkgVer, dep, minVer).
    };

    // Query all missing dependencies.
    println("Missing dependencies:");
    let missing = query db, pr select (pkg, pkgVer, dep, minVer) from Missing(pkg, pkgVer, dep, minVer);
    foreach((pkg, pkgVer, dep, minVer) <- missing) {
        println("  ${pkg}@${pkgVer} requires ${dep} >= ${minVer} (MISSING)");

        // Use pquery to trace why pkg@pkgVer was needed.
        let chain = pquery db, pr select Resolved(pkg, pkgVer) with {Dependency};
        println("  Dependency chain:");
        foreach(fact <- chain) {
            ematch fact {
                case Dependency(fromPkg, fromVer, toPkg, toMinVer) =>
                    println("    ${fromPkg}@${fromVer} requires ${toPkg} >= ${toMinVer}")
            }
        };
        println("")
    }

///
/// Returns the database of published packages and their dependencies.
///
/// The database contains two kinds of facts:
/// - `Published(pkg, version)`: Declares that a package exists at a given version.
/// - `Dependency(pkg, version, depPkg, minVersion)`: Declares that `pkg@version`
///    requires `depPkg` with at least `minVersion`.
///
def database(): #{ Published(Package, Version), Dependency(Package, Version, Package, Version) | r } = #{
    // Package registry - published packages and their versions
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
    Dependency("postcss", 8, "autoprefixer", 11).      // only @9, @10 exist
    Dependency("next-themes", 1, "string-formatter", 1).
    Dependency("string-formatter", 1, "left-pad", 1).  // left-pad unpublished!
}
