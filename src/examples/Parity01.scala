package examples

import impl.logic.{Predicate, Term, Formula, Type}
import syntax.Symbols._

object Parity01 {
  def main(args: Array[String]): Unit = {

    val LatticeType = Type.Variant(IndexedSeq(
      Type.Constructor0("Top"),
      Type.Constructor0("Odd"), Type.Constructor0("Zero"), Type.Constructor0("Even"),
      Type.Constructor0("Bot")
    ))

    val Join = Formula.Disjunction(Set(
      Formula.Atom(Predicate("Leq", List()))

    ))

  }
}
