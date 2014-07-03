package examples

import impl.logic._
import syntax.Symbols._

object Sign {

  val Lattice = Type.Variant(IndexedSeq(
    Type.Constructor0("Top"),
    Type.Constructor0("Neg"), Type.Constructor0("Zero"), Type.Constructor0("Pos"),
    Type.Constructor0("Bot")
  ))

  val Top = Term.Constant(Value.Constructor0("Top"))
  val Neg = Term.Constant(Value.Constructor0("Neg"))
  val Zero = Term.Constant(Value.Constructor0("Zero"))
  val Pos = Term.Constant(Value.Constructor0("Pos"))
  val Bot = Term.Constant(Value.Constructor0("Bot"))

  val Leq = Set(
    HornClause(Predicate("Sign.Leq", List(Bot, Term.Variable("_"))), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Neg, Neg)), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Zero, Zero)), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Pos, Pos)), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Term.Variable("_"), Top)), Set.empty)
  )

  val Join = Set(
    HornClause(Predicate("Sign.Leq", List(Bot, Term.Variable("x"), Term.Variable("x"))), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Term.Variable("x"), Bot, Term.Variable("x"))), Set.empty),

    HornClause(Predicate("Sign.Leq", List(Neg, Neg, Neg)), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Neg, Zero, Top)), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Neg, Pos, Top)), Set.empty),

    HornClause(Predicate("Sign.Leq", List(Zero, Neg, Top)), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Zero, Zero, Zero)), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Zero, Pos, Top)), Set.empty),

    HornClause(Predicate("Sign.Leq", List(Pos, Neg, Top)), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Pos, Zero, Top)), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Pos, Pos, Pos)), Set.empty),

    HornClause(Predicate("Sign.Leq", List(Term.Variable("_"), Top, Top)), Set.empty),
    HornClause(Predicate("Sign.Leq", List(Top, Term.Variable("_"), Top)), Set.empty)
  )

  val Sum = Set(
    HornClause(Predicate("Sign.Sum", List(Bot, Term.Variable("_"), Bot)), Set.empty),
    HornClause(Predicate("Sign.Sum", List(Term.Variable("_"), Bot, Bot)), Set.empty),

    HornClause(Predicate("Sign.Sum", List(Neg, Neg, Neg)), Set.empty),
    HornClause(Predicate("Sign.Sum", List(Neg, Zero, Neg)), Set.empty),
    HornClause(Predicate("Sign.Sum", List(Neg, Pos, Top)), Set.empty),

    HornClause(Predicate("Sign.Sum", List(Zero, Neg, Neg)), Set.empty),
    HornClause(Predicate("Sign.Sum", List(Zero, Zero, Zero)), Set.empty),
    HornClause(Predicate("Sign.Sum", List(Zero, Pos, Pos)), Set.empty),

    HornClause(Predicate("Sign.Sum", List(Pos, Neg, Top)), Set.empty),
    HornClause(Predicate("Sign.Sum", List(Pos, Zero, Pos)), Set.empty),
    HornClause(Predicate("Sign.Sum", List(Pos, Pos, Pos)), Set.empty),

    HornClause(Predicate("Sign.Sum", List(Top, Term.Variable("_"), Top)), Set.empty),
    HornClause(Predicate("Sign.Sum", List(Term.Variable("_"), Top, Top)), Set.empty)
  )

}
