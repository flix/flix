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



  }
}
