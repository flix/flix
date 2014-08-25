(def-type Sign (STop Neg Zer Pos SBot))

(def-leq Sign (e1 Sign e2 Sign)
    (match (e1 e2)
        (case (SBot x)      true)
        (case (Neg Neg)     true)
        (case (Zer Zer)     true)
        (case (Pos Pos)     true)
        (case (_ STop)      true)
        (case _             false)))

(def-lub Sign (e1 Sign e2 Sign)
    (match (e1 e2)
        (case (SBot x)      x)
        (case (x SBot)      x)
        (case (Neg Neg)     Neg)
        (case (Zer Zer)     Zer)
        (case (Pos Pos)     Pos)
        (case _             STop)))

(def-height Sign (e Sign)
    (match e
        (case SBot  3)
        (case Neg   2)
        (case Zer   2)
        (case Pos   2)
        (case STop  1)))

(def-fun sum (e1 Sign e2 Sign)
    (match (e1 e2)
        (case

  val Sum = List(
    HornClause(Predicate(SumSymbol, List(Bot, Term.Variable("_"), Bot))),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Bot, Bot))),

    HornClause(Predicate(SumSymbol, List(Neg, Neg, Neg))),
    HornClause(Predicate(SumSymbol, List(Neg, Zer, Neg))),
    HornClause(Predicate(SumSymbol, List(Neg, Pos, Top))),

    HornClause(Predicate(SumSymbol, List(Zer, Neg, Neg))),
    HornClause(Predicate(SumSymbol, List(Zer, Zer, Zer))),
    HornClause(Predicate(SumSymbol, List(Zer, Pos, Pos))),

    HornClause(Predicate(SumSymbol, List(Pos, Neg, Top))),
    HornClause(Predicate(SumSymbol, List(Pos, Zer, Pos))),
    HornClause(Predicate(SumSymbol, List(Pos, Pos, Pos))),

    HornClause(Predicate(SumSymbol, List(Top, Neg, Top))),
    HornClause(Predicate(SumSymbol, List(Top, Zer, Top))),
    HornClause(Predicate(SumSymbol, List(Top, Pos, Top))),

    HornClause(Predicate(SumSymbol, List(Neg, Top, Top))),
    HornClause(Predicate(SumSymbol, List(Zer, Top, Top))),
    HornClause(Predicate(SumSymbol, List(Pos, Top, Top))),

    HornClause(Predicate(SumSymbol, List(Top, Top, Top)))
  )

      val facts = List(
        HornClause(Predicate(X, List(Term.Constructor0("Sign.Pos")))),
        HornClause(Predicate(Y, List(Term.Constructor0("Sign.Neg"))))
      )

      val clauses = List(
        HornClause(head = Predicate(R, List(Term.Variable("x"))), body = List(Predicate(X, List(Term.Variable("x"))))),
        HornClause(head = Predicate(R, List(Term.Variable("x"))), body = List(Predicate(Y, List(Term.Variable("x")))))
      )