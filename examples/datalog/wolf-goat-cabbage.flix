//
// The wolf, goat, and cabbage problem is a river crossing puzzle that dates
// back to at least the 9th century:
//
//     A farmer with a wolf, a goat, and a cabbage must cross a river by boat.
//     The boat can carry only the farmer and a single item. If left unattended
//     together, the wolf would eat the goat, or the goat would eat the cabbage.
//     How can they cross the river without anything being eaten?
//

///
/// A type alias for a bank of the river: "west" or "east".
///
type alias Bank = String

///
/// A type alias for what the farmer brings along: "wolf", "goat", "cabbage", or "nothing".
///
type alias Cargo = String

///
/// Solves the wolf, goat, and cabbage problem.
///
/// We model a configuration as the four banks that the farmer, wolf, goat, and
/// cabbage are currently on. A single crossing is a `Step`: the fact
/// `Step(f, w, g, c, cargo, f2, w2, g2, c2)` says that from the configuration
/// `(f, w, g, c)` the farmer can cross with `cargo` to the configuration
/// `(f2, w2, g2, c2)`. A `Step` may be unsafe; `Safe` holds for the
/// configurations where nothing gets eaten. `Reachable` is the set of
/// configurations reachable from the starting configuration by safe steps.
///
/// Datalog can tell us *whether* the puzzle is solvable, because that is a
/// membership question about the minimal model. It cannot return the sequence of
/// crossings, because a sequence is a term and Datalog has no function symbols.
///
/// But the sequence of crossings *is* the derivation of the goal configuration,
/// so `pquery` recovers it: the `Step` facts along the provenance path are the
/// crossings, in order.
///
def main(): Unit \ IO =
    let pr = #{
        // The two banks of the river and the crossings between them.
        Bank("west").
        Bank("east").

        Travel("west", "east").
        Travel("east", "west").

        // The farmer crosses alone: from `f` to `f2`; `w`, `g`, and `c` stay put.
        Step(f, w, g, c, "nothing", f2, w,  g,  c ) :- Travel(f, f2), Bank(w), Bank(g), Bank(c).

        // The farmer crosses with the wolf: both go from `f` to `f2`; `g` and `c` stay put.
        Step(f, f, g, c, "wolf",    f2, f2, g,  c ) :- Travel(f, f2), Bank(g), Bank(c).

        // The farmer crosses with the goat: both go from `f` to `f2`; `w` and `c` stay put.
        Step(f, w, f, c, "goat",    f2, w,  f2, c ) :- Travel(f, f2), Bank(w), Bank(c).

        // The farmer crosses with the cabbage: both go from `f` to `f2`; `w` and `g` stay put.
        Step(f, w, g, f, "cabbage", f2, w,  g,  f2) :- Travel(f, f2), Bank(w), Bank(g).

        // Nothing is eaten if the goat is with the farmer: the wolf and the
        // cabbage may then be on either bank.
        Safe(f, w, f, c) :- Bank(f), Bank(w), Bank(c).

        // Nothing is eaten if the wolf and the cabbage are with the farmer: the
        // goat is then either with everyone else or alone on the other bank.
        Safe(f, f, g, f) :- Bank(f), Bank(g).

        // Everyone starts on the west bank.
        Reachable("west", "west", "west", "west").

        // We may cross into any configuration that is safe.
        //
        // Note that `Reachable` occurs *before* `Step` in the body. A provenance
        // query lists the facts of a proof in pre-order, so the sub-proof of the
        // earlier crossings comes before the `Step` of the latest one. That is
        // what puts the crossings in chronological order.
        Reachable(f2, w2, g2, c2) :-
            Reachable(f, w, g, c),
            Step(f, w, g, c, _, f2, w2, g2, c2),
            Safe(f2, w2, g2, c2).
    };

    // The configurations reachable without anything being eaten. There are
    // 2^4 = 16 configurations in total: each of the four participants is on one
    // of two banks.
    let reachable = query pr select (f, w, g, c) from Reachable(f, w, g, c);
    println("Reachable configurations: ${Vector.length(reachable)} (of 16).");

    // The puzzle is solvable if everyone can end up on the east bank.
    if (not Vector.memberOf(("east", "east", "east", "east"), reachable)) {
        println("The puzzle has no solution.")
    } else {
        // It is solvable, so we ask *how*. A provenance query returns the `Step`
        // facts used to derive the goal configuration -- i.e. the crossings.
        //
        // Flix computes a proof tree of minimal height. Every crossing adds one
        // level to the tree, so we get a shortest solution for free.
        let plan = pquery pr select Reachable("east", "east", "east", "east") with {Step};
        println("A shortest solution takes ${Vector.length(plan)} crossings:");
        foreach (crossing <- plan) {
            ematch crossing {
                case Step(_, _, _, _, cargo, dst, _, _, _) => println("  ${describe(dst, cargo)}")
            }
        }
    }

///
/// Returns a human-readable description of the farmer crossing to `dst` with `cargo`.
///
/// For example, `describe("east", "goat")` is "The farmer crosses to the east bank with the goat.".
///
def describe(dst: Bank, cargo: Cargo): String =
    if (cargo == "nothing")
        "The farmer crosses to the ${dst} bank alone."
    else
        "The farmer crosses to the ${dst} bank with the ${cargo}."
