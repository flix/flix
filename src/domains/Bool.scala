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

  val Leq2 = Term.Abs("x", Term.Abs("y",
    Term.Ite(
      Term.BinaryOp(BinaryOperator.Eq, Bot, Term.Variable("x")),
      Term.Bool(b = true),
      Term.Ite(
        Term.BinaryOp(BinaryOperator.Eq, Term.Constructor2("2-tuple", True, True), Term.Constructor2("2-tuple", Term.Variable("x"), Term.Variable("y"))),
        Term.Bool(b = true),
        Term.Ite(
          Term.BinaryOp(BinaryOperator.Eq, Term.Constructor2("2-tuple", False, False), Term.Constructor2("2-tuple", Term.Variable("x"), Term.Variable("y"))),
          Term.Bool(b = true),
          Term.Ite(
            Term.BinaryOp(BinaryOperator.Eq, Top, Term.Variable("y")),
            Term.Bool(b = true),
            Term.Bool(b = false)
          ))))))

  val Leq = List(
    HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_")))),
    HornClause(Predicate(LeqSymbol, List(True, True))),
    HornClause(Predicate(LeqSymbol, List(False, False))),
    HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)))
  )

  val Join2 = Term.Abs("x", Term.Abs("y",
    Term.Ite(
      Term.BinaryOp(BinaryOperator.Eq, Term.Variable("x"), Bot),
      Term.Variable("y"),
      Term.Ite(
        Term.BinaryOp(BinaryOperator.Eq, Term.Variable("y"), Bot),
        Term.Variable("y"),
        Term.Ite(
          Term.BinaryOp(BinaryOperator.Eq, Term.Constructor2("2-tuple", Term.Variable("x"), Term.Variable("y")), Term.Constructor2("2-tuple", True, True)),
          True,
          Term.Ite(
            Term.BinaryOp(BinaryOperator.Eq, Term.Constructor2("2-tuple", Term.Variable("x"), Term.Variable("y")), Term.Constructor2("2-tuple", False, False)),
            False,
            Top
          )
        )
      )
    )
  ))

  println("----")
  println(Interpreter.reduce(Term.App(Term.App(Join2, True), True), Map.empty))
  println(Interpreter.reduce(Term.App(Term.App(Join2, True), False), Map.empty))

  val Join = List(
    HornClause(Predicate(JoinSymbol, List(Term.Variable("x"), Bot, Term.Variable("x")))),
    HornClause(Predicate(JoinSymbol, List(Bot, Term.Variable("x"), Term.Variable("x")))),

    HornClause(Predicate(JoinSymbol, List(True, True, True))),
    HornClause(Predicate(JoinSymbol, List(False, False, False))),

    HornClause(Predicate(JoinSymbol, List(True, False, Top))),
    HornClause(Predicate(JoinSymbol, List(False, True, Top))),

    HornClause(Predicate(JoinSymbol, List(Top, Term.Variable("_"), Top))),
    HornClause(Predicate(JoinSymbol, List(Term.Variable("_"), Top, Top)))
  )

  val Height2 = Term.Abs("x",
    Term.Ite(
      Term.BinaryOp(BinaryOperator.Eq, Term.Variable("x"), Bot),
      Term.Int(3),
      Term.Ite(
        Term.BinaryOp(BinaryOperator.Eq, Term.Variable("x"), True),
        Term.Int(2),
        Term.Ite(
          Term.BinaryOp(BinaryOperator.Eq, Term.Variable("x"), False),
          Term.Int(2),
          Term.Int(0)))))

  val Height = List(
    HornClause(Predicate(HeightSymbol, List(Bot, Term.Int(3)))),
    HornClause(Predicate(HeightSymbol, List(True, Term.Int(2)))),
    HornClause(Predicate(HeightSymbol, List(False, Term.Int(2)))),
    HornClause(Predicate(HeightSymbol, List(Top, Term.Int(1))))
  )

  val And = List(
    HornClause(Predicate(AndSymbol, List(Bot, Term.Variable("_"), Bot))),
    HornClause(Predicate(AndSymbol, List(Term.Variable("_"), Bot, Bot))),

    HornClause(Predicate(AndSymbol, List(True, True, True))),
    HornClause(Predicate(AndSymbol, List(False, False, False))),
    HornClause(Predicate(AndSymbol, List(True, False, Top))),
    HornClause(Predicate(AndSymbol, List(False, True, Top))),

    HornClause(Predicate(AndSymbol, List(Top, True, Top))),
    HornClause(Predicate(AndSymbol, List(Top, False, Top))),

    HornClause(Predicate(AndSymbol, List(True, Top, Top))),
    HornClause(Predicate(AndSymbol, List(False, Top, Top))),
    HornClause(Predicate(AndSymbol, List(Top, Top, Top)))
  )

  val Hints = Map(
    AndSymbol -> Hint(Representation.Code)
  )

  val lattice = Lattice("MyBool", Elements, Bot.toValue, LeqSymbol, JoinSymbol, HeightSymbol, List(AndSymbol), Leq ::: Join ::: Height ::: And)

}
