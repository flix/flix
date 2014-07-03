package domains

import impl.logic._
import syntax.Symbols._

object Polarity {

  val Lattice = Type.Variant(IndexedSeq(
    Type.Constructor0("Top"),
    Type.Constructor0("Odd"), Type.Constructor0("Zero"), Type.Constructor0("Even"),
    Type.Constructor0("Bot")
  ))

  val Top = Term.Constant(Value.Constructor0("Polarity.Top"))
  val Odd = Term.Constant(Value.Constructor0("Polarity.Odd"))
  val Zero = Term.Constant(Value.Constructor0("Polarity.Zero"))
  val Even = Term.Constant(Value.Constructor0("Polarity.Even"))
  val Bot = Term.Constant(Value.Constructor0("Polarity.Bot"))

  val Leq = Set(
    HornClause(Predicate("Polarity.Leq", List(Bot, Term.Variable("_"))), Set.empty),
    HornClause(Predicate("Polarity.Leq", List(Odd, Odd)), Set.empty),
    HornClause(Predicate("Polarity.Leq", List(Zero, Zero)), Set.empty),
    HornClause(Predicate("Polarity.Leq", List(Even, Even)), Set.empty),
    HornClause(Predicate("Polarity.Leq", List(Term.Variable("_"), Top)), Set.empty)
  )

  val Join = Set(
    HornClause(Predicate("Polarity.Join", List(Bot, Term.Variable("x"), Term.Variable("x"))), Set.empty),
    HornClause(Predicate("Polarity.Join", List(Term.Variable("x"), Bot, Term.Variable("x"))), Set.empty),
    HornClause(Predicate("Polarity.Join", List(Odd, Odd, Odd)), Set.empty),
    HornClause(Predicate("Polarity.Join", List(Zero, Zero, Zero)), Set.empty),
    HornClause(Predicate("Polarity.Join", List(Even, Even, Even)), Set.empty),
    HornClause(Predicate("Polarity.Join", List(Top, Term.Variable("_"), Top)), Set.empty),
    HornClause(Predicate("Polarity.Join", List(Term.Variable("_"), Top, Top)), Set.empty)
  )

  val Sum = Set(
    HornClause(Predicate("Polarity.Sum", List(Bot, Term.Variable("_"), Bot)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Term.Variable("_"), Bot, Bot)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Odd, Odd, Even)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Odd, Zero, Odd)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Odd, Even, Odd)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Zero, Odd, Odd)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Zero, Zero, Zero)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Zero, Even, Even)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Even, Odd, Odd)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Even, Zero, Even)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Even, Even, Even)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Top, Term.Variable("_"), Top)), Set.empty),
    HornClause(Predicate("Polarity.Sum", List(Term.Variable("_"), Top, Top)), Set.empty)
  )

}
