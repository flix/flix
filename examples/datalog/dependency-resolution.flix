///
/// Package dependency resolution using Datalog.
///
/// Demonstrates transitive dependency computation with version constraints
/// and provenance queries to trace dependency chains.
///

///
/// We introduce type aliases to model the package domain.
///
type alias Package = String    // e.g. "json-parser"
type alias Version = Int32     // e.g. 3 (simplified semver)

///
/// We compute all resolved packages for a root package and use
/// provenance queries to trace the dependency chain.
///
def main(): Unit \ IO =
    let root = "my-app";
    let rootVersion = 1;

    let db = #{
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

        // Dependency declarations: Depends(pkg, version, depPkg, minVersion)
        Depends("my-app", 1, "web-server", 2).
        Depends("web-server", 2, "json-parser", 1).
        Depends("web-server", 2, "logger", 1).
        Depends("json-parser", 1, "string-utils", 1).
        Depends("logger", 1, "string-utils", 2).
    };

    // A Datalog program that computes transitive dependency resolution.
    let rules = #{
        // Root package is resolved.
        Resolved(root, rootVersion).

        // Transitive: if pkg@v is resolved and depends on dep with minVer,
        // resolve all available versions of dep that satisfy the constraint.
        Resolved(dep, depV) :-
            Resolved(pkg, v),
            Depends(pkg, v, dep, minVer),
            Available(dep, depV),
            if (depV >= minVer).
    };

    // Query all resolved packages.
    let resolved = query db, rules select (pkg, v) from Resolved(pkg, v);
    println("Resolved dependencies for ${root}@${rootVersion}:");
    foreach((pkg, v) <- resolved) {
        println("  ${pkg}@${v}")
    };

    // Use pquery to show which Depends facts led to resolving string-utils@3.
    // pquery requires a concrete ground atom, so we trace a specific package version.
    println("");
    println("Dependency chain for string-utils@3 (via pquery):");
    let p = pquery db, rules select Resolved("string-utils", 3) with {Depends};
    let deps = Vector.map(fact -> ematch fact { case Depends(pkg, v, dep, minV) => (pkg, v, dep, minV) }, p);
    foreach((pkg, v, dep, minV) <- deps) {
        println("  ${pkg}@${v} requires ${dep} >= ${minV}")
    }
