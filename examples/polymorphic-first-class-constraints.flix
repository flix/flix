/// Returns a set of edge facts labelled with numbers.
/// Note that the return type is `closed` which means that the
/// facts can *only* be used within a constraint system that
/// has labelled edges and paths of ints.
def getEdgesWithNumbers(): #{ Edge(String, Int, String), Path(String, Int, String) } = #{
    Edge("a", 1, "b").
    Edge("b", 1, "c").
    Edge("c", 2, "d").
}

/// Returns a set of edge facts labelled with colors (strings).
/// Note that the return type is `open` (polymorphic) which
/// means that the facts can be used within any constraint
/// as long as the edges are labelled with strings.
def getEdgesWithColor[r](): #{ Edge(String, String, String) | r } = #{
    Edge("a", "red", "b").
    Edge("b", "red", "c").
    Edge("c", "blu", "d").
}

/// Returns a set of polymorphic rules to compute the transitive
/// closure of edges with the *same* label.
def getRules(): #{ Edge(String, l, String), Path(String, l, String) } = #{
    Path(x, l, y) :- Edge(x, l, y).
    Path(x, l, z) :- Path(x, l, y), Path(y, l, z).
}

/// Computes the fixpoint of the two sets of facts with the rules.
/// Note that polymorphism allow us to use `getRules`
/// with both types of facts.
def main(): Unit =
    let r1 = solve getEdgesWithColor() <+> getRules();
    let r2 = solve getEdgesWithNumbers() <+> getRules();
    ()

/// However, the type system ensures that we do not mix facts of
/// different type:
def main2(): Unit =
    /// Uncomment to see that the composition does not type check:
    /// let r1 = solve getEdgesWithColor() <+> getEdgesWithNumbers();
    ()
