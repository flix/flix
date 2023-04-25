/// Declare two predicate symbols.
//rel ParentOf(x: String, y: String)
//rel AncestorOf(x: String, y: String)

/// Returns a collection of facts.
///def getFacts(): #{ ParentOf, AncestorOf } = #{
   // ParentOf("Pompey", "Strabo").
  //  ParentOf("Gnaeus", "Pompey").
 //   ParentOf("Pompeia", "Pompey").
  //  ParentOf("Sextus", "Pompey").
//}

/// Returns a collection of rules to compute ancestors.
//def getRules(): #{ ParentOf, AncestorOf } = #{
  //  AncestorOf(x, y) :- ParentOf(x, y).
    //AncestorOf(x, z) :- AncestorOf(x, y), AncestorOf(y, z).
//}

/// Composes the facts and rules, and computes the lfp.
//def main(): Unit \ IO =
  //  query getFacts(), getRules()
    //    select (x, y) from AncestorOf(x, y) |> println
