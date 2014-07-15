package domains

import impl.logic._
import impl.runtime.{Hint, Representation}
import syntax.Symbols._

object Parity {

  val Elements = Type.Variant(IndexedSeq(
    Type.Constructor0("Parity.Top"),
    Type.Constructor0("Parity.Odd"), Type.Constructor0("Parity.Even"),
    Type.Constructor0("Parity.Bot")
  ))

  val Top = Term.Constructor0("Parity.Top")
  val Odd = Term.Constructor0("Parity.Odd")
  val Even = Term.Constructor0("Parity.Even")
  val Bot = Term.Constructor0("Parity.Bot")

  val LeqSymbol = Symbol.PredicateSymbol("Parity.Leq")
  val JoinSymbol = Symbol.PredicateSymbol("Parity.Join")
  val SumSymbol = Symbol.PredicateSymbol("Parity.Sum")

  val Leq = List(
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_")))),
    HornClause(Predicate(LeqSymbol, List(Odd, Odd))),
    HornClause(Predicate(LeqSymbol, List(Even, Even))),
    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)))
  )

  val Join = List(
    HornClause(Predicate(JoinSymbol, List(Bot, Term.Variable("y"), Term.Variable("y")))),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("y"), Bot, Term.Variable("y")))),

    HornClause(Predicate(JoinSymbol, List(Odd, Odd, Odd))),
    HornClause(Predicate(JoinSymbol, List(Odd, Even, Top))),

    HornClause(Predicate(JoinSymbol, List(Even, Even, Even))),
    HornClause(Predicate(JoinSymbol, List(Even, Odd, Top))),

    HornClause(Predicate(JoinSymbol, List(Top, Term.Variable("_"), Top))),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("_"), Top, Top)))
  )

  val Sum = List(
    HornClause(Predicate(SumSymbol, List(Bot, Term.Variable("_"), Bot))),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Bot, Bot))),

    HornClause(Predicate(SumSymbol, List(Odd, Odd, Even))),
    HornClause(Predicate(SumSymbol, List(Odd, Even, Odd))),

    HornClause(Predicate(SumSymbol, List(Even, Odd, Odd))),
    HornClause(Predicate(SumSymbol, List(Even, Even, Even))),

    HornClause(Predicate(SumSymbol, List(Top, Term.Variable("_"), Top))),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Top, Top)))
  )

  val Interpretations = Map(
    LeqSymbol -> Interpretation.Leq,
    JoinSymbol -> Interpretation.Join
  )

  val Hints = Map(
    SumSymbol -> Hint(Representation.Code)
  )

  val lattice = Lattice(Elements, Bot.toValue, LeqSymbol, JoinSymbol, Leq ::: Join ::: Sum, Interpretations)

}
