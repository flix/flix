///
/// We introduce type aliases to model information flow security.
///
type alias Variable = String         // e.g. "secretKey"
type alias SecurityLevel = String    // e.g. "TopSecret"
type alias Location = Int32          // e.g. 42

///
/// We compute all confidentiality violations by tracking information flows
/// between variables with different security classifications.
///
/// We then use provenance queries to show the complete flow path from
/// high-security variables to low-security variables.
///
pub def main(): Unit \ IO =
    let db = database();

    // A Datalog program that computes transitive information flow and violations.
    let pr = #{
        // Compute transitive closure of information flows.
        Reaches(src, dst) :- FlowsTo(src, dst, _).
        Reaches(src, dst) :- Reaches(src, mid), FlowsTo(mid, dst, _).

        // A violation occurs when information flows between variables
        // with incompatible security levels.
        Violation(src, dst) :-
            Reaches(src, dst),
            Classification(src, srcLevel),
            Classification(dst, dstLevel),
            not CanFlowTo(srcLevel, dstLevel).
    };

    // Query for all confidentiality violations.
    let violations = query db, pr select (src, dst) from Violation(src, dst);

    if (Vector.isEmpty(violations)) {
        println("No confidentiality violations found. Information flows are secure!")
    } else {
        println("WARNING: Confidentiality violations detected!\n");
        println("The following illegal information flows were found:\n");

        foreach ((src, dst) <- violations) {
            // Get security levels
            let srcLevelVec = query db, pr select level from Classification(src, level);
            let dstLevelVec = query db, pr select level from Classification(dst, level);
            let srcLevel = Vector.get(0, srcLevelVec);
            let dstLevel = Vector.get(0, dstLevelVec);

            println("Violation: '${src}' (${srcLevel}) ~> '${dst}' (${dstLevel})");

            // Use a provenance query to find the information flow path.
            let p = pquery db, pr select Reaches(src, dst) with {FlowsTo};

            println("  Information flow path:");
            foreach (edge <- p) {
                ematch edge {
                    case FlowsTo(source, target, loc) => {
                        let fmtSource = String.padRight(15, ' ', source);
                        let arrow = String.padRight(3, ' ', "~>");
                        println("    [${loc}] ${fmtSource} ${arrow} ${target}")
                    }
                }
            };
            println("")
        }
    }

///
/// Returns the database of information flow facts and security classifications.
///
/// This models a system with four security levels forming a lattice:
/// Public < Confidential < Secret < TopSecret
///
def database(): #{ FlowsTo(Variable, Variable, Location),
                   Classification(Variable, SecurityLevel),
                   CanFlowTo(SecurityLevel, SecurityLevel) | r } = #{

    // Define the security lattice: allowed flows between levels.
    // Information can flow UP in security (Public -> Secret) but not DOWN.
    CanFlowTo("Public", "Public").
    CanFlowTo("Public", "Confidential").
    CanFlowTo("Public", "Secret").
    CanFlowTo("Public", "TopSecret").

    CanFlowTo("Confidential", "Confidential").
    CanFlowTo("Confidential", "Secret").
    CanFlowTo("Confidential", "TopSecret").

    CanFlowTo("Secret", "Secret").
    CanFlowTo("Secret", "TopSecret").

    CanFlowTo("TopSecret", "TopSecret").

    // Scenario 1: Direct violation - secret key logged to public output
    Classification("secretKey", "TopSecret").
    Classification("logMessage", "Public").
    FlowsTo("secretKey", "logMessage", 100).

    // Scenario 2: Multi-hop violation - API token flows through intermediates
    Classification("apiToken", "Secret").
    Classification("debugInfo", "Confidential").
    Classification("consoleOutput", "Public").
    FlowsTo("apiToken", "debugInfo", 200).
    FlowsTo("debugInfo", "consoleOutput", 205).

    // Scenario 3: Complex chain - password through multiple variables
    Classification("userPassword", "Secret").
    Classification("temp1", "Secret").
    Classification("temp2", "Confidential").
    Classification("auditLog", "Public").
    FlowsTo("userPassword", "temp1", 300).
    FlowsTo("temp1", "temp2", 302).
    FlowsTo("temp2", "auditLog", 305).

    // Scenario 4: Legal flows (no violation)
    Classification("userName", "Public").
    Classification("userReport", "Confidential").
    Classification("secureReport", "Secret").
    FlowsTo("userName", "userReport", 400).
    FlowsTo("userReport", "secureReport", 405).

    // Scenario 5: Another violation - confidential to public
    Classification("creditCard", "Confidential").
    Classification("errorMessage", "Public").
    FlowsTo("creditCard", "errorMessage", 500).
}
