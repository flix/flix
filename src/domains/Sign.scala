package domains

import impl.logic._
import impl.runtime.{Hint, Representation}
import syntax.Symbols._

object Sign {

  val Elements = Type.Variant(IndexedSeq(
    Type.Constructor0("Sign.Top"),
    Type.Constructor0("Sign.Neg"), Type.Constructor0("Sign.Zer"), Type.Constructor0("Sign.Pos"),
    Type.Constructor0("Sign.Bot")
  ))

  val Top = Term.Constructor0("Sign.Top")
  val Neg = Term.Constructor0("Sign.Neg")
  val Zer = Term.Constructor0("Sign.Zer")
  val Pos = Term.Constructor0("Sign.Pos")
  val Bot = Term.Constructor0("Sign.Bot")

  val LeqSymbol = Symbol.PredicateSymbol("Sign.Leq")
  val JoinSymbol = Symbol.PredicateSymbol("Sign.Join")
  val HeightSymbol = Symbol.PredicateSymbol("Sign.Height")
  val SumSymbol = Symbol.PredicateSymbol("Sign.Sum")

  val Leq = List(
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_")))),
    HornClause(Predicate(LeqSymbol, List(Neg, Neg))),
    HornClause(Predicate(LeqSymbol, List(Zer, Zer))),
    HornClause(Predicate(LeqSymbol, List(Pos, Pos))),
    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)))
  )

  val Join = List(
    HornClause(Predicate(JoinSymbol, List(Bot, Term.Variable("x"), Term.Variable("x")))),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("x"), Bot, Term.Variable("x")))),

    HornClause(Predicate(JoinSymbol, List(Neg, Neg, Neg))),
    HornClause(Predicate(JoinSymbol, List(Neg, Zer, Top))),
    HornClause(Predicate(JoinSymbol, List(Neg, Pos, Top))),

    HornClause(Predicate(JoinSymbol, List(Zer, Neg, Top))),
    HornClause(Predicate(JoinSymbol, List(Zer, Zer, Zer))),
    HornClause(Predicate(JoinSymbol, List(Zer, Pos, Top))),

    HornClause(Predicate(JoinSymbol, List(Pos, Neg, Top))),
    HornClause(Predicate(JoinSymbol, List(Pos, Zer, Top))),
    HornClause(Predicate(JoinSymbol, List(Pos, Pos, Pos))),

    HornClause(Predicate(JoinSymbol, List(Term.Variable("_"), Top, Top))),
    HornClause(Predicate(JoinSymbol, List(Top, Term.Variable("_"), Top)))
  )

  val Height = List(
    HornClause(Predicate(HeightSymbol, List(Bot, Term.Int(3)))),
    HornClause(Predicate(HeightSymbol, List(Neg, Term.Int(2)))),
    HornClause(Predicate(HeightSymbol, List(Zer, Term.Int(2)))),
    HornClause(Predicate(HeightSymbol, List(Pos, Term.Int(2)))),
    HornClause(Predicate(HeightSymbol, List(Top, Term.Int(1))))
  )

  val Sum = List(
    HornClause(Predicate(SumSymbol, List(Bot, Term.Variable("_"), Bot))),
    HornClause(Predicate(SumSymbol, List(Term.Variable("_"), Bot, Bot))),

    HornClause(Predicate(SumSymbol, List(Neg, Neg, Neg))),
    HornClause(Predicate(SumSymbol, List(Neg, Zer, Neg))),
    HornClause(Predicate(SumSymbol, List(Neg, Pos, Top))),

    HornClause(Predicate(SumSymbol, List(Zer, Neg, Neg))),
    HornClause(Predicate(SumSymbol, List(Zer, Zer, Zer))),
    HornClause(Predicate(SumSymbol, List(Zer, Pos, Pos))),

    HornClause(Predicate(SumSymbol, List(Pos, Neg, Top))),
    HornClause(Predicate(SumSymbol, List(Pos, Zer, Pos))),
    HornClause(Predicate(SumSymbol, List(Pos, Pos, Pos))),

    HornClause(Predicate(SumSymbol, List(Top, Neg, Top))),
    HornClause(Predicate(SumSymbol, List(Top, Zer, Top))),
    HornClause(Predicate(SumSymbol, List(Top, Pos, Top))),

    HornClause(Predicate(SumSymbol, List(Neg, Top, Top))),
    HornClause(Predicate(SumSymbol, List(Zer, Top, Top))),
    HornClause(Predicate(SumSymbol, List(Pos, Top, Top))),

    HornClause(Predicate(SumSymbol, List(Top, Top, Top)))
  )

  val Hints = Map(
    SumSymbol -> Hint(Representation.Code)
  )

  val lattice = Lattice("Sign", Elements, Bot.toValue, LeqSymbol, JoinSymbol, HeightSymbol,  List(SumSymbol),Leq ::: Join ::: Height ::: Sum)

}
