package domains

import impl.logic._
import impl.runtime.{Hint, Representation}
import syntax.Symbols._

object Constant {

  val Elements = Type.Variant(IndexedSeq(
    Type.Constructor0("Constant.Top"),
    Type.Constructor1("Constant.Singleton", Type.Int),
    Type.Constructor0("Constant.Bot")
  ))

  val Top = Term.Constructor0("Constant.Top")
  val Singleton = Term.Constructor1("Constant.Singleton", Term.Variable("x"))
  val Bot =  Term.Constructor0("Constant.Bot")

  val LeqSymbol = Symbol.PredicateSymbol("Constant.Leq")
  val JoinSymbol = Symbol.PredicateSymbol("Constant.Join")
  val HeightSymbol = Symbol.PredicateSymbol("Constant.Height")

  val Leq = List(
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_")))),
    HornClause(Predicate(LeqSymbol, List(Term.Variable("c"), Term.Variable("c")))),
    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)))
  )

  val Join = List(

  )


}
