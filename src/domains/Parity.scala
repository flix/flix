package domains

import impl.logic._
import syntax.Symbols._

object Parity {

  val Elements = Type.Variant(IndexedSeq(
    Type.Constructor0("Parity.Top"),
    Type.Constructor0("Parity.Odd"), Type.Constructor0("Parity.Zero"), Type.Constructor0("Parity.Even"),
    Type.Constructor0("Parity.Bot")
  ))

  val Top = Term.Constant(Value.Constructor0("Parity.Top"))
  val Odd = Term.Constant(Value.Constructor0("Parity.Odd"))
  val Zero = Term.Constant(Value.Constructor0("Parity.Zero"))
  val Even = Term.Constant(Value.Constructor0("Parity.Even"))
  val Bot = Term.Constant(Value.Constructor0("Parity.Bot"))

  val LeqSymbol = Symbol.PredicateSymbol("Parity.Leq")
  val JoinSymbol = Symbol.PredicateSymbol("Parity.Join")
  val SumSymbol = Symbol.PredicateSymbol("Parity.Sum")

  val Leq = List(
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_")))),
    HornClause(Predicate(LeqSymbol, List(Odd, Odd))),
    HornClause(Predicate(LeqSymbol, List(Zero, Zero))),
    HornClause(Predicate(LeqSymbol, List(Even, Even))),
    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)))
  )

  val Join = List(
    HornClause(Predicate(JoinSymbol, List(Bot, Term.Variable("x"), Term.Variable("x")))),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("x"), Bot, Term.Variable("x")))),
    HornClause(Predicate(JoinSymbol, List(Odd, Odd, Odd))),
    HornClause(Predicate(JoinSymbol, List(Zero, Zero, Zero))),
    HornClause(Predicate(JoinSymbol, List(Even, Even, Even))),
    HornClause(Predicate(JoinSymbol, List(Top, Term.Variable("_"), Top))),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("_"), Top, Top)))
  )

  val Sum = List(
    HornClause(Predicate(SumSymbol, List(Bot, Term.Variable("_"), Bot))),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Bot, Bot))),
    HornClause(Predicate(SumSymbol, List(Odd, Odd, Even))),
    HornClause(Predicate(SumSymbol, List(Odd, Zero, Odd))),
    HornClause(Predicate(SumSymbol, List(Odd, Even, Odd))),
    HornClause(Predicate(SumSymbol, List(Zero, Odd, Odd))),
    HornClause(Predicate(SumSymbol, List(Zero, Zero, Zero))),
    HornClause(Predicate(SumSymbol, List(Zero, Even, Even))),
    HornClause(Predicate(SumSymbol, List(Even, Odd, Odd))),
    HornClause(Predicate(SumSymbol, List(Even, Zero, Even))),
    HornClause(Predicate(SumSymbol, List(Even, Even, Even))),
    HornClause(Predicate(SumSymbol, List(Top, Term.Variable("_"), Top))),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Top, Top)))
  )

  val Interpretations = Map(
    LeqSymbol -> Interpretation.Leq,
    JoinSymbol -> Interpretation.Join,
    SumSymbol -> Interpretation.Function(Representation.Code)
  )

  val lattice = Lattice(Elements, Bot.v, LeqSymbol, JoinSymbol, Leq ::: Join ::: Sum, Interpretations)

}
