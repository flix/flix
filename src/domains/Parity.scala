package domains

import impl.logic._
import syntax.Symbols._

object Parity {

  val Lattice = Type.Variant(IndexedSeq(
    Type.Constructor0("Top"),
    Type.Constructor0("Odd"), Type.Constructor0("Zero"), Type.Constructor0("Even"),
    Type.Constructor0("Bot")
  ))

  val Top = Term.Constant(Value.Constructor0("Parity.Top"))
  val Odd = Term.Constant(Value.Constructor0("Parity.Odd"))
  val Zero = Term.Constant(Value.Constructor0("Parity.Zero"))
  val Even = Term.Constant(Value.Constructor0("Parity.Even"))
  val Bot = Term.Constant(Value.Constructor0("Parity.Bot"))

  val LeqSymbol = Symbol.PredicateSymbol("Parity.Leq")
  val JoinSymbol = Symbol.PredicateSymbol("Parity.Join")
  val SumSymbol = Symbol.PredicateSymbol("Parity.Sum")

  val Leq = Set(
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_"))), Set.empty),
    HornClause(Predicate(LeqSymbol, List(Odd, Odd)), Set.empty),
    HornClause(Predicate(LeqSymbol, List(Zero, Zero)), Set.empty),
    HornClause(Predicate(LeqSymbol, List(Even, Even)), Set.empty),
    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)), Set.empty)
  )

  val Join = Set(
    HornClause(Predicate(JoinSymbol, List(Bot, Term.Variable("x"), Term.Variable("x"))), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("x"), Bot, Term.Variable("x"))), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Odd, Odd, Odd)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Zero, Zero, Zero)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Even, Even, Even)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Top, Term.Variable("_"), Top)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("_"), Top, Top)), Set.empty)
  )

  val Sum = Set(
    HornClause(Predicate(SumSymbol, List(Bot, Term.Variable("_"), Bot)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Bot, Bot)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Odd, Odd, Even)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Odd, Zero, Odd)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Odd, Even, Odd)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Zero, Odd, Odd)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Zero, Zero, Zero)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Zero, Even, Even)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Even, Odd, Odd)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Even, Zero, Even)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Even, Even, Even)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Top, Term.Variable("_"), Top)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Top, Top)), Set.empty)
  )

}
