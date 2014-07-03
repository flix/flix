package examples

import impl.logic._
import syntax.Symbols._

object Parity {
  def main(args: Array[String]): Unit = {

    val LatticeType = Type.Variant(IndexedSeq(
      Type.Constructor0("Top"),
      Type.Constructor0("Odd"), Type.Constructor0("Zero"), Type.Constructor0("Even"),
      Type.Constructor0("Bot")
    ))

    val Top = Term.Constant(Value.Constructor0("Sign.Top"))
    val Odd = Term.Constant(Value.Constructor0("Sign.Odd"))
    val Zero = Term.Constant(Value.Constructor0("Sign.Zero"))
    val Even = Term.Constant(Value.Constructor0("Sign.Even"))
    val Bot = Term.Constant(Value.Constructor0("Sign.Bot"))

    val Leq = Set(
      HornClause(Predicate("Parity.Leq", List(Bot, Term.Variable("_"))), Set.empty),
      HornClause(Predicate("Parity.Leq", List(Odd, Odd)), Set.empty),
      HornClause(Predicate("Parity.Leq", List(Zero, Zero)), Set.empty),
      HornClause(Predicate("Parity.Leq", List(Even, Even)), Set.empty),
      HornClause(Predicate("Parity.Leq", List(Term.Variable("_"), Top)), Set.empty)
    )

    val Join = Set(
      HornClause(Predicate("Parity.Join", List(Bot, Term.Variable("x"), Term.Variable("x"))), Set.empty),
      HornClause(Predicate("Parity.Join", List(Term.Variable("x"), Bot, Term.Variable("x"))), Set.empty),
      HornClause(Predicate("Parity.Join", List(Odd, Odd, Odd)), Set.empty),
      HornClause(Predicate("Parity.Join", List(Zero, Zero, Zero)), Set.empty),
      HornClause(Predicate("Parity.Join", List(Even, Even, Even)), Set.empty),
      HornClause(Predicate("Parity.Join", List(Top, Term.Variable("_"), Top)), Set.empty),
      HornClause(Predicate("Parity.Join", List(Term.Variable("_"), Top, Top)), Set.empty)
    )

    val Sum = Set(
      HornClause(Predicate("Parity.Sum", List(Bot, Term.Variable("_"), Bot)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Term.Variable("_"), Bot, Bot)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Odd, Odd, Even)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Odd, Zero, Odd)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Odd, Even, Odd)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Zero, Odd, Odd)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Zero, Zero, Zero)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Zero, Even, Even)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Even, Odd, Odd)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Even, Zero, Even)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Even, Even, Even)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Top, Term.Variable("_"), Top)), Set.empty),
      HornClause(Predicate("Parity.Sum", List(Term.Variable("_"), Top, Top)), Set.empty)
    )

  }
}
