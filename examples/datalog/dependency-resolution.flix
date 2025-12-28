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
type alias Package = String    // e.g. "json-parser"
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
            Depends(pkg, v, dep, minVer),
            Available(dep, depV),
            if (depV >= minVer).

        // A dependency has at least one satisfying version.
        Satisfiable(pkg, v, dep, minVer) :-
            Depends(pkg, v, dep, minVer),
            Available(dep, depV),
            if (depV >= minVer).

        // Unsatisfied = resolved package has a dependency that can't be met.
        Unsatisfied(pkg, v, dep, minVer) :-
            Resolved(pkg, v),
            Depends(pkg, v, dep, minVer),
            not Satisfiable(pkg, v, dep, minVer).
    };

    // Query all unsatisfied dependencies.
    let unsatisfied = query db, pr select (pkg, v, dep, minVer) from Unsatisfied(pkg, v, dep, minVer);
    println("Unsatisfied dependencies:");
    foreach((pkg, v, dep, minVer) <- unsatisfied) {
        println("  ${pkg}@${v} requires ${dep} >= ${minVer} (UNSATISFIED)");

        // Use pquery to trace why pkg@v was needed.
        let chain = pquery db, pr select Resolved(pkg, v) with {Depends};
        println("  Dependency chain:");
        foreach(fact <- chain) {
            ematch fact {
                case Depends(p, pv, d, mv) => println("    ${p}@${pv} requires ${d} >= ${mv}")
            }
        }
    }

///
/// Returns the database of available packages and their dependencies.
///
/// The database contains two kinds of facts:
/// - `Available(pkg, version)`: Declares that a package exists at a given version.
/// - `Depends(pkg, version, depPkg, minVersion)`: Declares that `pkg@version`
///    requires `depPkg` with at least `minVersion`.
///
def database(): #{ Available(Package, Version), Depends(Package, Version, Package, Version) | r } = #{
    // Package registry - available packages and versions
    Available("my-app", 1).
    Available("web-server", 1).
    Available("web-server", 2).
    Available("json-parser", 1).
    Available("json-parser", 2).
    Available("logger", 1).
    Available("string-utils", 1).
    Available("string-utils", 2).
    Available("string-utils", 3).
    Available("zlib", 1).

    // Dependency declarations: Depends(pkg, version, depPkg, minVersion)
    Depends("my-app", 1, "web-server", 2).
    Depends("web-server", 2, "json-parser", 1).
    Depends("web-server", 2, "logger", 1).
    Depends("json-parser", 1, "string-utils", 1).
    Depends("logger", 1, "string-utils", 2).
    Depends("logger", 1, "zlib", 2).
}
