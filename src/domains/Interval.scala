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



  // Interval.Join(Bot, x, x).
  // Interval.Join(x, Bot, x).
  // Interval.Join(Range(b1, e1), Range(b2, e2), Range(b3, e3)) :- (max(e1, e2) - min(b1, b2)) <= 10.
  // Interval.Join(Range(b1, e1), Range(b2, e2), Top) :- (max(e1, e2) - min(b1, b2)) > 10.
  // Interval.Join(Top, _, Top).
  // Interval.Join(_, Top, Top).

  // Interval.Sum(Bot, _, Bot).
  // Interval.Sum(_, Bot, Bot).
  // Interval.Sum(Range(b1, e1), Range(b2, e2), Range(b1 + b2, e1 + e2)).
  // Interval.Sum(Top, _, Top).
  // Interval.Sum(_, Top, Top).

}
