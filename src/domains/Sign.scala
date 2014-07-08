package domains

import impl.logic._
import syntax.Symbols._

object Sign {

  val Elements = Type.Variant(IndexedSeq(
    Type.Constructor0("Top"),
    Type.Constructor0("Neg"), Type.Constructor0("Zero"), Type.Constructor0("Pos"),
    Type.Constructor0("Bot")
  ))

  val Top = Term.Constant(Value.Constructor0("Top"))
  val Neg = Term.Constant(Value.Constructor0("Neg"))
  val Zero = Term.Constant(Value.Constructor0("Zero"))
  val Pos = Term.Constant(Value.Constructor0("Pos"))
  val Bot = Term.Constant(Value.Constructor0("Bot"))

  val LeqSymbol = Symbol.PredicateSymbol("Sign.Leq")
  val JoinSymbol = Symbol.PredicateSymbol("Sign.Join")
  val SumSymbol = Symbol.PredicateSymbol("Sign.Sum")

  val Leq = Set(
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_")))),
    HornClause(Predicate(LeqSymbol, List(Neg, Neg))),
    HornClause(Predicate(LeqSymbol, List(Zero, Zero))),
    HornClause(Predicate(LeqSymbol, List(Pos, Pos))),
    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)))
  )

  val Join = Set(
    HornClause(Predicate(JoinSymbol, List(Bot, Term.Variable("x"), Term.Variable("x")))),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("x"), Bot, Term.Variable("x")))),

    HornClause(Predicate(JoinSymbol, List(Neg, Neg, Neg))),
    HornClause(Predicate(JoinSymbol, List(Neg, Zero, Top))),
    HornClause(Predicate(JoinSymbol, List(Neg, Pos, Top))),

    HornClause(Predicate(JoinSymbol, List(Zero, Neg, Top))),
    HornClause(Predicate(JoinSymbol, List(Zero, Zero, Zero))),
    HornClause(Predicate(JoinSymbol, List(Zero, Pos, Top))),

    HornClause(Predicate(JoinSymbol, List(Pos, Neg, Top))),
    HornClause(Predicate(JoinSymbol, List(Pos, Zero, Top))),
    HornClause(Predicate(JoinSymbol, List(Pos, Pos, Pos))),

    HornClause(Predicate(JoinSymbol, List(Term.Variable("_"), Top, Top))),
    HornClause(Predicate(JoinSymbol, List(Top, Term.Variable("_"), Top)))
  )

  val Sum = Set(
    HornClause(Predicate(SumSymbol, List(Bot, Term.Variable("_"), Bot))),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Bot, Bot))),

    HornClause(Predicate(SumSymbol, List(Neg, Neg, Neg))),
    HornClause(Predicate(SumSymbol, List(Neg, Zero, Neg))),
    HornClause(Predicate(SumSymbol, List(Neg, Pos, Top))),

    HornClause(Predicate(SumSymbol, List(Zero, Neg, Neg))),
    HornClause(Predicate(SumSymbol, List(Zero, Zero, Zero))),
    HornClause(Predicate(SumSymbol, List(Zero, Pos, Pos))),

    HornClause(Predicate(SumSymbol, List(Pos, Neg, Top))),
    HornClause(Predicate(SumSymbol, List(Pos, Zero, Pos))),
    HornClause(Predicate(SumSymbol, List(Pos, Pos, Pos))),

    HornClause(Predicate(SumSymbol, List(Top, Term.Variable("_"), Top))),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Top, Top)))
  )

  val Interpretations = Map(
    LeqSymbol -> Interpretation.Leq,
    JoinSymbol -> Interpretation.Join,
    SumSymbol -> Interpretation.Function(Representation.Code)
  )

  val lattice = Lattice(Elements, Bot.v, LeqSymbol, JoinSymbol, Leq ++ Join ++ Sum, Interpretations)

}
