package domains

import impl.logic._
import syntax.Symbols._

object SignAndParity {

  val Elements = Type.Constructor2("SP", Sign.Elements, Parity.Elements)

  val LeqSymbol = Symbol.PredicateSymbol("SP.Leq")
  val JoinSymbol = Symbol.PredicateSymbol("SP.Join")
  val SumSymbol = Symbol.PredicateSymbol("SP.Sum")

  val Bot = Term.Constructor2("SP", Sign.Bot, Parity.Bot)

  val Leq = List(
    HornClause(
      head = Predicate(LeqSymbol, List(
        Term.Constructor2("SP", Term.Variable("s1"), Term.Variable("p1")),
        Term.Constructor2("SP", Term.Variable("s2"), Term.Variable("p2"))
      )),
      body = List(
        Predicate("Sign.Leq", List(Term.Variable("s1"), Term.Variable("s2"))),
        Predicate("Parity.Leq", List(Term.Variable("p1"), Term.Variable("p2")))
      ))
  )

  val Join = List(
    HornClause(
      head = Predicate(JoinSymbol, List(
        Term.Constructor2("SP", Term.Variable("s1"), Term.Variable("p1")),
        Term.Constructor2("SP", Term.Variable("s2"), Term.Variable("p2")),
        Term.Constructor2("SP", Term.Variable("s3"), Term.Variable("p3"))
      )),
      body = List(
        Predicate("Sign.Join", List(Term.Variable("s1"), Term.Variable("s2"), Term.Variable("s3"))),
        Predicate("Parity.Join", List(Term.Variable("p1"), Term.Variable("p2"), Term.Variable("p3")))
      )))

  val Sum = List(
    HornClause(
      head = Predicate(SumSymbol, List(
        Term.Constructor2("SP", Term.Variable("s1"), Term.Variable("p1")),
        Term.Constructor2("SP", Term.Variable("s2"), Term.Variable("p2")),
        Term.Constructor2("SP", Term.Variable("s3"), Term.Variable("p3"))
      )),
      body = List(
        Predicate("Sign.Sum", List(Term.Variable("s1"), Term.Variable("s2"), Term.Variable("s3"))),
        Predicate("Parity.Sum", List(Term.Variable("p1"), Term.Variable("p2"), Term.Variable("p3")))
      )
    )
  )

  val Interpretations = Map(
    LeqSymbol -> Interpretation.Leq,
    JoinSymbol -> Interpretation.Join,
    SumSymbol -> Interpretation.Function(Representation.Code)
  ) ++ Sign.Interpretations ++ Parity.Interpretations

  val lattice = Lattice(Elements, Bot.toValue, LeqSymbol, JoinSymbol, Leq ::: Join ::: Sum ::: Sign.lattice.clauses ::: Parity.lattice.clauses, Interpretations)

}
