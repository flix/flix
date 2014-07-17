package domains

import impl.logic._
import impl.runtime.{Hint, Representation}
import syntax.Symbols._

object Bool {

  val Elements = Type.Variant(IndexedSeq(
    Type.Constructor0("Bool.Top"),
    Type.Constructor0("Bool.True"), Type.Constructor0("Bool.False"),
    Type.Constructor0("Bool.Bot")
  ))

  val Top = Value.Constructor0("Bool.Top").asTerm
  val True = Value.Constructor0("Bool.True").asTerm
  val False = Value.Constructor0("Bool.False").asTerm
  val Bot = Value.Constructor0("Bool.Bot").asTerm

  val LeqSymbol = Symbol.PredicateSymbol("Bool.Leq")
  val JoinSymbol = Symbol.PredicateSymbol("Bool.Join")
  val HeightSymbol = Symbol.PredicateSymbol("Bool.Height")
  val AndSymbol = Symbol.PredicateSymbol("Bool.And")

  val Leq = List(
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_")))),
    HornClause(Predicate(LeqSymbol, List(True, True))),
    HornClause(Predicate(LeqSymbol, List(False, False))),
    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)))
  )

  val Join = List(
    HornClause(Predicate(JoinSymbol, List(Term.Variable("x"), Bot, Term.Variable("x")))),
    HornClause(Predicate(JoinSymbol, List(Bot, Term.Variable("x"), Term.Variable("x")))),

    HornClause(Predicate(JoinSymbol, List(True, True, True))),
    HornClause(Predicate(JoinSymbol, List(False, False, False))),

    HornClause(Predicate(JoinSymbol, List(True, False, Top))),
    HornClause(Predicate(JoinSymbol, List(False, True, Top))),

    HornClause(Predicate(JoinSymbol, List(Top, Top, Top)))
  )

  val Height = List(
    HornClause(Predicate(HeightSymbol, List(Bot, Term.Int(3)))),
    HornClause(Predicate(HeightSymbol, List(True, Term.Int(2)))),
    HornClause(Predicate(HeightSymbol, List(False, Term.Int(17)))),
    HornClause(Predicate(HeightSymbol, List(Top, Term.Int(1))))
  )

  val And = List(
    HornClause(Predicate(AndSymbol, List(Bot, Term.Variable("_"), Bot))),
    HornClause(Predicate(AndSymbol, List(Term.Variable("_"), Bot, Bot)))
  )

  val Hints = Map(
    AndSymbol -> Hint(Representation.Code)
  )

  val lattice = Lattice("MyBool", Elements, Bot.toValue, LeqSymbol, JoinSymbol, HeightSymbol, List(AndSymbol), Leq ::: Join ::: Height ::: And)

}
