package examples

import impl.logic._
import syntax.Symbols._

object Sign {
  def main(args: Array[String]): Unit = {



    // Sign.Leq(Bot, _).
    // Sign.Leq(Neg, Neg).
    // Sign.Leq(Zero, Zero).
    // Sign.Leq(Pos, Pos).
    // Sign.Leq(_, Top).

    val Leq = Set(
      HornClause(Predicate("Sign.Leq", List(???, Term.Variable("_"))), Set.empty)
    )


    // Sign.Join(Bot, x, x).
    // Sign.Join(x, Bot, x).
    // Sign.Join(Neg, Neg, Neg).
    // Sign.Join(Zero, Zero, Zero).
    // Sign.Join(Pos, Pos, Pos).
    // Sign.Join(x, y, Top) :- x != y.
    // Sign.Join(Top, _, Top).
    // Sign.Join(_, Top, Top).
    val Join = Formula.Disjunction(Set(
      Formula.Atom(Predicate("Leq", List()))

    ))




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

    val Sum = ???


  }
}
