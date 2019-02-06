/// Declare two polymorphic predicate symbols.
/// Here an edge and a path are labelled with some type `l`.
rel LabelledEdge[l](x: Str, l: l, y: Str)
rel LabelledPath[l](x: Str, l: l, y: Str)

/// Returns a set of edge facts labelled with numbers.
/// Note that the return type is `closed` which means the facts can *only*
/// be used within a constraint system that has labelled edges and paths.
def getEdgesWithNumbers(): Schema { LabelledEdge[Int], LabelledPath[Int] } = {
    LabelledEdge("a", 1, "b").
    LabelledEdge("b", 1, "c").
    LabelledEdge("c", 2, "d").
}

/// Returns a set of edge facts labelled with colors.
/// Note that the return type is `open` (polymorphic) which means the facts
/// can be used within any constraint as the types of labelled edges match.
def getEdgesWithColor[r](): Schema { LabelledEdge[Str] | r } = {
    LabelledEdge("a", "red", "b").
    LabelledEdge("b", "red", "c").
    LabelledEdge("c", "blu", "d").
}

/// Returns a set of polymorphic rules to compute the transitive closure of
/// edges with the same weight.
def getRules[l](): Schema { LabelledEdge[l], LabelledPath[l] } = {
    LabelledPath(x, w, y) :- LabelledEdge(x, w, y).
    LabelledPath(x, w, z) :- LabelledPath(x, w, y), LabelledPath(y, w, z).
}

/// Composes the two sets of facts with the rules and solves them individually.
/// Note that polymorphism allow us to use `getRules` with both types of facts.
def main(): Unit =
    let r1 = solve getEdgesWithColor() <+> getRules();
    let r2 = solve getEdgesWithNumbers() <+> getRules();
    ()

/// The type system prevents us from mixing facts of different type:
def main2(): Unit =
    /// Uncomment to see that the composition does not type check:
    /// let r1 = solve getEdgesWithColor() <+> getEdgesWithNumbers();
    ()
