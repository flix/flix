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
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_"))), Set.empty),
    HornClause(Predicate(LeqSymbol, List(Neg, Neg)), Set.empty),
    HornClause(Predicate(LeqSymbol, List(Zero, Zero)), Set.empty),
    HornClause(Predicate(LeqSymbol, List(Pos, Pos)), Set.empty),
    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)), Set.empty)
  )

  val Join = Set(
    HornClause(Predicate(JoinSymbol, List(Bot, Term.Variable("x"), Term.Variable("x"))), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("x"), Bot, Term.Variable("x"))), Set.empty),

    HornClause(Predicate(JoinSymbol, List(Neg, Neg, Neg)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Neg, Zero, Top)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Neg, Pos, Top)), Set.empty),

    HornClause(Predicate(JoinSymbol, List(Zero, Neg, Top)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Zero, Zero, Zero)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Zero, Pos, Top)), Set.empty),

    HornClause(Predicate(JoinSymbol, List(Pos, Neg, Top)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Pos, Zero, Top)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Pos, Pos, Pos)), Set.empty),

    HornClause(Predicate(JoinSymbol, List(Term.Variable("_"), Top, Top)), Set.empty),
    HornClause(Predicate(JoinSymbol, List(Top, Term.Variable("_"), Top)), Set.empty)
  )

  val Sum = Set(
    HornClause(Predicate(SumSymbol, List(Bot, Term.Variable("_"), Bot)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Bot, Bot)), Set.empty),

    HornClause(Predicate(SumSymbol, List(Neg, Neg, Neg)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Neg, Zero, Neg)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Neg, Pos, Top)), Set.empty),

    HornClause(Predicate(SumSymbol, List(Zero, Neg, Neg)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Zero, Zero, Zero)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Zero, Pos, Pos)), Set.empty),

    HornClause(Predicate(SumSymbol, List(Pos, Neg, Top)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Pos, Zero, Pos)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Pos, Pos, Pos)), Set.empty),

    HornClause(Predicate(SumSymbol, List(Top, Term.Variable("_"), Top)), Set.empty),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Top, Top)), Set.empty)
  )

  val Interpretations = Map(
    LeqSymbol -> Interpretation.Leq,
    JoinSymbol -> Interpretation.Join,
    SumSymbol -> Interpretation.Function(Representation.Code)
  )

  val lattice = Lattice(Elements, Bot.v, LeqSymbol, JoinSymbol, Leq ++ Join ++ Sum, Interpretations)

}
