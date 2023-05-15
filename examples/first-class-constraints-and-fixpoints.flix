/// Returns a collection of facts.
def getFacts(): #{ ParentOf(String, String), AncestorOf(String, String) } = #{
    ParentOf("Pompey", "Strabo").
    ParentOf("Gnaeus", "Pompey").
    ParentOf("Pompeia", "Pompey").
    ParentOf("Sextus", "Pompey").
}

/// Returns a collection of rules to compute ancestors.
def getRules(): #{ ParentOf(String, String), AncestorOf(String, String) } = #{
    AncestorOf(x, y) :- ParentOf(x, y).
    AncestorOf(x, z) :- AncestorOf(x, y), AncestorOf(y, z).
}

/// Composes the facts and rules, and computes the lfp.
def main(): Unit \ IO =
    query getFacts(), getRules()
        select (x, y) from AncestorOf(x, y) |> println
