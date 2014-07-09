package domains

import impl.logic._
import syntax.Symbols._

object Interval {

  val Elements = Type.Variant(IndexedSeq(
    Type.Constructor0("Interval.Top"),
    Type.Constructor2("Interval.Range", Type.Int, Type.Int), // TODO: Need additional constraint...
    Type.Constructor0("Interval.Bot")
  ))

  val Top = Term.Constructor0("Interval.Top")
  val Bot = Term.Constructor0("Interval.Bot")

  val LeqSymbol = Symbol.PredicateSymbol("Interval.Leq")
  val JoinSymbol = Symbol.PredicateSymbol("Interval.Join")
  val SumSymbol = Symbol.PredicateSymbol("Interval.Sum")

  val Leq = List(
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_")))),

    HornClause(
      head = Predicate(LeqSymbol, List(
        Term.Constructor2(Symbol.NamedSymbol("Interval.Range"), Term.Variable("b1"), Term.Variable("e1")),
        Term.Constructor2(Symbol.NamedSymbol("Interval.Range"), Term.Variable("b2"), Term.Variable("e2"))
      )),
      body = List(
        Predicate("Int.Leq", List(Term.Variable("b2"), Term.Variable("b1"))),
        Predicate("Int.Leq", List(Term.Variable("e1"), Term.Variable("e2")))
      )),

    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)))
  )

  val Join = List(
    HornClause(Predicate(JoinSymbol, List(Bot, Term.Variable("x"), Term.Variable("x")))),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("x"), Bot, Term.Variable("x")))),

    // Interval.Join(Range(b1, e1), Range(b2, e2), Range(min(b1, b2), max(e1, e2))) :- (max(e1, e2) - min(b1, b2)) <= 10.
    HornClause(
      head = Predicate(JoinSymbol, List(
        Term.Constructor2("Interval.Range", Term.Variable("b1"), Term.Variable("e1")),
        Term.Constructor2("Interval.Range", Term.Variable("b2"), Term.Variable("e2")),
        Term.Constructor2("Interval.Range",
          Term.Apply("Int.min", List(Term.Variable("b1"), Term.Variable("b2"))),
          Term.Apply("Int.max", List(Term.Variable("e1"), Term.Variable("e2"))))
      )),
      body = List(
        Predicate("Int.LessEq", List(
          Term.Apply("Int.minus", List(
            Term.Apply("Int.max", List(Term.Variable("e1"), Term.Variable("e2"))),
            Term.Apply("Int.min", List(Term.Variable("b1"), Term.Variable("b2")))
          )),
          Term.Constant(Value.Int(10))
        ))
      )),

    // Interval.Join(Range(b1, e1), Range(b2, e2), Top) :- (max(e1, e2) - min(b1, b2)) > 10.
    HornClause(
      head = Predicate(JoinSymbol, List(
        Term.Constructor2("Interval.Range", Term.Variable("b1"), Term.Variable("e1")),
        Term.Constructor2("Interval.Range", Term.Variable("b2"), Term.Variable("e2")),
        Top
      )),
      body = List(
        Predicate("Int.Greater", List(
          Term.Apply("Int.-", List(
            Term.Apply("Int.max", List(Term.Variable("e1"), Term.Variable("e2"))),
            Term.Apply("Int.min", List(Term.Variable("b1"), Term.Variable("b2")))
          )),
          Term.Constant(Value.Int(10))
        ))
      )),

    HornClause(Predicate(JoinSymbol, List(Top, Term.Variable("_"), Top))),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("_"), Top, Top)))
  )

  val Sum = List(
    HornClause(Predicate(SumSymbol, List(Bot, Term.Variable("_"), Bot))),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Bot, Bot))),

    HornClause(Predicate(SumSymbol, List(
      Term.Constructor2("Interval.Range", Term.Variable("b1"), Term.Variable("e1")),
      Term.Constructor2("Interval.Range", Term.Variable("b2"), Term.Variable("e2")),
      Term.Constructor2("Interval.Range",
        Term.Apply("Int.+", List(Term.Variable("b1"), Term.Variable("b2"))),
        Term.Apply("Int.+", List(Term.Variable("e1"), Term.Variable("e2"))))
    ))),

    HornClause(Predicate(SumSymbol, List(Top, Term.Variable("_"), Top))),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Top, Top)))
  )

  val Interpretations = Map(
    LeqSymbol -> Interpretation.Leq,
    JoinSymbol -> Interpretation.Join,
    SumSymbol -> Interpretation.Function(Representation.Code)
  )

  val lattice = Lattice(Elements, Bot.toValue, LeqSymbol, JoinSymbol, Leq ::: Join ::: Sum, Interpretations)

}
