
    val ParentSymbol = Symbol.PredicateSymbol("Parent")
    val AgeSymbol = Symbol.PredicateSymbol("Age")
    val ParentChildSymbol = Symbol.PredicateSymbol("P")

    val facts = List(
      HornClause(Predicate(ParentSymbol, List(Term.Str("Caroline"), Term.Str("Inger")))),
      HornClause(Predicate(ParentSymbol, List(Term.Str("Caroline"), Term.Str("Frits")))),

      HornClause(Predicate(ParentSymbol, List(Term.Str("Bjarke"), Term.Str("Inger")))),
      HornClause(Predicate(ParentSymbol, List(Term.Str("Bjarke"), Term.Str("Frits")))),

      HornClause(Predicate(ParentSymbol, List(Term.Str("Magnus"), Term.Str("Inger")))),
      HornClause(Predicate(ParentSymbol, List(Term.Str("Magnus"), Term.Str("Frits")))),

      HornClause(Predicate(AgeSymbol, List(Term.Str("Caroline"), Term.Str("17")))),
      HornClause(Predicate(AgeSymbol, List(Term.Str("Bjarke"), Term.Str("23")))),
      HornClause(Predicate(AgeSymbol, List(Term.Str("Magnus"), Term.Str("28"))))
    )

    val clauses = List(
      HornClause(Predicate(ParentChildSymbol, List(Term.Variable("parent"), Term.Constructor2("NameAndAge", Term.Variable("child"), Term.Variable("age")))), List(
        Predicate(ParentSymbol, List(Term.Variable("child"), Term.Variable("parent"))),
        Predicate(AgeSymbol, List(Term.Variable("child"), Term.Variable("age")))
      ))
    )