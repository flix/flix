///
/// We introduce a few types and aliases to model a taint analysis.
///

/// Types of untrusted data sources.
pub enum SourceType with Eq, Order, ToString {
    case UserInput      // Form fields, query parameters
    case Network        // HTTP requests, socket data
}

/// Types of sensitive sinks.
pub enum SinkType with Eq, Order, ToString {
    case SqlQuery       // SQL injection risk
}

type alias Variable = String      // e.g. "username"
type alias Location = Int32       // e.g. 42

///
/// We compute all SQL injection vulnerabilities by tracking tainted data
/// from untrusted sources to sensitive sinks.
///
/// We then use provenance queries to show the complete data flow path.
///
pub def main(): Unit \ IO =
    let db = database();

    // A Datalog program that computes taint propagation.
    let pr = #{
        // Variables from untrusted sources are tainted.
        Tainted(var, loc) :- Source(var, _, loc).

        // Taint propagates through assignments.
        Tainted(target, loc) :- Tainted(source, _), Assign(target, source, loc).

        // A vulnerability exists when tainted data reaches a sensitive sink.
        Vulnerability(var, sink, loc) :- Tainted(var, _), Sink(var, sink, loc).
    };

    // Query for all vulnerabilities.
    let vulnerabilities = query db, pr select (var, sink, loc) from Vulnerability(var, sink, loc);

    if (Vector.isEmpty(vulnerabilities)) {
        println("No vulnerabilities found. Code is safe!")
    } else {
        println("WARNING: SQL injection vulnerabilities detected!\n");
        println("The following variables contain tainted data at sinks:\n");

        foreach ((var, sink, loc) <- vulnerabilities) {
            println("Vulnerability: '${var}' flows to ${sink} at line ${loc}");

            // Use a provenance query to find the data flow path.
            let p = pquery db, pr select Vulnerability(var, sink, loc) with {Source, Assign};

            println("  Data flow path:");
            foreach (edge <- p) {
                ematch edge {
                    case Source(v, srcType, l) => {
                        let fmtVar = String.padRight(15, ' ', v);
                        println("    [${l}] ${fmtVar} <- ${srcType} (source)")
                    }
                    case Assign(target, source, l) => {
                        let fmtTarget = String.padRight(15, ' ', target);
                        println("    [${l}] ${fmtTarget} <- ${source}")
                    }
                }
            };
            println("")
        }
    }

///
/// Returns the database of data flow facts.
///
/// This models a simple web application with SQL injection vulnerabilities.
///
def database(): #{ Source(Variable, SourceType, Location), Assign(Variable, Variable, Location), Sink(Variable, SinkType, Location) | r } = #{
    // Scenario 1: Direct SQL injection
    Source("username", SourceType.UserInput, 10).
    Assign("query1", "username", 15).
    Sink("query1", SinkType.SqlQuery, 20).

    // Scenario 2: Multi-hop taint propagation
    Source("email", SourceType.UserInput, 30).
    Assign("sanitized", "email", 32).
    Assign("emailQuery", "sanitized", 35).
    Sink("emailQuery", SinkType.SqlQuery, 40).

    // Scenario 3: Multiple assignments before sink
    Source("password", SourceType.UserInput, 50).
    Assign("temp1", "password", 52).
    Assign("temp2", "temp1", 54).
    Assign("authQuery", "temp2", 56).
    Sink("authQuery", SinkType.SqlQuery, 60).

    // Scenario 4: Network input vulnerability
    Source("requestBody", SourceType.Network, 70).
    Assign("jsonData", "requestBody", 72).
    Assign("updateQuery", "jsonData", 75).
    Sink("updateQuery", SinkType.SqlQuery, 80).

    // Safe scenario: no path to sink
    Source("safeInput", SourceType.UserInput, 90).
    Assign("safeVar", "safeInput", 92).
    // No sink for safeVar
}
