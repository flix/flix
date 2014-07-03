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


    /**
     * Sign
     */
    // Lattice Sign = Bot | Top | Neg | Zero | Pos.

    // Sign.Leq(Bot, _).
    // Sign.Leq(Neg, Neg).
    // Sign.Leq(Zero, Zero).
    // Sign.Leq(Pos, Pos).
    // Sign.Leq(_, Top).

    // Sign.Join(Bot, x, x).
    // Sign.Join(x, Bot, x).
    // Sign.Join(Neg, Neg, Neg).
    // Sign.Join(Zero, Zero, Zero).
    // Sign.Join(Pos, Pos, Pos).
    // Sign.Join(x, y, Top) :- x != y.
    // Sign.Join(Top, _, Top).
    // Sign.Join(_, Top, Top).

    // Sign.Lift(i, Neg) :- i < 0.
    // Sign.Lift(i, Zero) :- i == 0.
    // Sign.Lift(i, Pos) :- i > 0.

    // Notice: Strictness
    // Sign.Sum(Bot, _, Bot).
    // Sign.Sum(_, Bot, Bot).
    // Sign.Sum(Neg, Neg, Neg).
    // Sign.Sum(Neg, Zero, Neg).
    // Sign.Sum(Neg, Pos, Top).
    // Sign.Sum(Zero, Neg, Neg).
    // Sign.sum(Zero, Zero, Zero).
    // Sign.Sum(Zero, Pos, Pos).
    // Sign.Sum(Pos, Neg, Top).
    // Sign.Sum(Pos, Zero, Pos).
    // Sign.Sum(Pos, Pos, Pos).
    // Sign.Sum(Top, _, Top).
    // Sign.Sum(_, Top, Top).

    val Leq = ???

    val Join = Formula.Disjunction(Set(
      Formula.Atom(Predicate("Leq", List()))

    ))

    val Sum = ???


  }
}
