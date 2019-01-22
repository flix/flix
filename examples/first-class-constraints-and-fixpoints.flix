/// Declare two predicate symbols.
rel ParentOf(x: Str, y: Str)
rel AncestorOf(x: Str, y: Str)

/// Returns a collection of facts.
def getFacts(): Schema { ParentOf, AncestorOf } = {
    ParentOf("Pompey", "Strabo").
    ParentOf("Gnaeus", "Pompey").
    ParentOf("Pompeia", "Pompey").
    ParentOf("Sextus", "Pompey").
}

/// Returns a collection of rules to compute ancestors.
def getRules(): Schema { ParentOf, AncestorOf } = {
    AncestorOf(x, y) :- ParentOf(x, y).
    AncestorOf(x, z) :- AncestorOf(x, y), AncestorOf(y, z).
}

/// Composes the facts and rules, and computes the lfp.
def main(): Schema { ParentOf, AncestorOf } =
    solve getFacts() <+> getRules()
