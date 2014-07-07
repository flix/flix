package domains

import impl.logic._
import syntax.Symbols._

object Interval {

  val Elements = Type.Variant(IndexedSeq(
    Type.Constructor0("Top"),
    Type.Constructor2("Range", Type.Int, Type.Int), // TODO: Need additional constraint...
    Type.Constructor0("Bot")
  ))

  val Top = Term.Constant(Value.Constructor0("Top"))
  val Bot = Term.Constant(Value.Constructor0("Bot"))

  val LeqSymbol = Symbol.PredicateSymbol("Interval.Leq")
  val JoinSymbol = Symbol.PredicateSymbol("Interval.Join")
  val SumSymbol = Symbol.PredicateSymbol("Interval.Sum")

  val Leq = Set(
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_"))), Set.empty),

    HornClause(
      head = Predicate(LeqSymbol, List(
        Term.Constructor2(Symbol.NamedSymbol("Range"), Term.Variable("b1"), Term.Variable("e1")),
        Term.Constructor2(Symbol.NamedSymbol("Range"), Term.Variable("b2"), Term.Variable("e2"))
      )),
      body = Set(
        Predicate("Int.Leq", List(Term.Variable("b2"), Term.Variable("b1"))),
        Predicate("Int.Leq", List(Term.Variable("e1"), Term.Variable("e2")))
      )),

    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)), Set.empty)
  )

  val Join = Set(
    HornClause(Predicate(JoinSymbol, List(Bot, Term.Variable("x"), Term.Variable("x"))), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("x"), Bot, Term.Variable("x"))), Set.empty),

    // Interval.Join(Range(b1, e1), Range(b2, e2), Range(min(b1, b2), max(e1, e2))) :- (max(e1, e2) - min(b1, b2)) <= 10.
    HornClause(
      head = Predicate(JoinSymbol, List(
        Term.Constructor2("Range", Term.Variable("b1"), Term.Variable("e1")),
        Term.Constructor2("Range", Term.Variable("b2"), Term.Variable("e2")),
        Term.Constructor2("Range",
          Term.Apply("Int.min", List(Term.Variable("b1"), Term.Variable("b2"))),
          Term.Apply("Int.max", List(Term.Variable("e1"), Term.Variable("e2"))))
      )),
      body = Set(
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
        Term.Constructor2("Range", Term.Variable("b1"), Term.Variable("e1")),
        Term.Constructor2("Range", Term.Variable("b2"), Term.Variable("e2")),
        Top
      )),
      body = Set(
        Predicate("Int.Greater", List(
          Term.Apply("Int.-", List(
            Term.Apply("Int.max", List(Term.Variable("e1"), Term.Variable("e2"))),
            Term.Apply("Int.min", List(Term.Variable("b1"), Term.Variable("b2")))
          )),
          Term.Constant(Value.Int(10))
        ))
      )),

    HornClause(Predicate(JoinSymbol, List(Top, Term.Variable("_"), Top)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("_"), Top, Top)), Set.empty)
  )

  val Sum = Set(
    HornClause(Predicate(SumSymbol, List(Bot, Term.Variable("_"), Bot)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Bot, Bot)), Set.empty),

    HornClause(Predicate(SumSymbol, List(
      Term.Constructor2("Range", Term.Variable("b1"), Term.Variable("e1")),
      Term.Constructor2("Range", Term.Variable("b2"), Term.Variable("e2")),
      Term.Constructor2("Range",
        Term.Apply("Int.+", List(Term.Variable("b1"), Term.Variable("b2"))),
        Term.Apply("Int.+", List(Term.Variable("e1"), Term.Variable("e2"))))
    )), Set.empty),

    HornClause(Predicate(SumSymbol, List(Top, Term.Variable("_"), Top)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Top, Top)), Set.empty)
  )

}
