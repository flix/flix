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

        // Transitive: if pkg@v is resolved and depends on dep with minVer,
        // resolve all available versions of dep that satisfy the constraint.
        Resolved(dep, depV) :-
            Resolved(pkg, v),
            Dependency(pkg, v, dep, minVer),
            Published(dep, depV),
            if (depV >= minVer).

        // A dependency has at least one satisfying version.
        Satisfied(pkg, v, dep, minVer) :-
            Dependency(pkg, v, dep, minVer),
            Published(dep, depV),
            if (depV >= minVer).

        // Missing = resolved package has a dependency that can't be met.
        Missing(pkg, v, dep, minVer) :-
            Resolved(pkg, v),
            Dependency(pkg, v, dep, minVer),
            not Satisfied(pkg, v, dep, minVer).
    };

    // Query all unsatisfied dependencies.
    let missing = query db, pr select (pkg, v, dep, minVer) from Missing(pkg, v, dep, minVer);
    println("Missing dependencies:");
    foreach((pkg, v, dep, minVer) <- missing) {
        println("  ${pkg}@${v} requires ${dep} >= ${minVer} (UNSATISFIED)");

        // Use pquery to trace why pkg@v was needed.
        let chain = pquery db, pr select Resolved(pkg, v) with {Dependency};
        println("  Dependency chain:");
        foreach(fact <- chain) {
            ematch fact {
                case Dependency(p, pv, d, mv) => println("    ${p}@${pv} requires ${d} >= ${mv}")
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
