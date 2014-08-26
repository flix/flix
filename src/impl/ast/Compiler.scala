package impl.ast

import impl.logic._

import scala.collection.mutable

// TODO: Rename: def-type to def lattice?
// TODO: Figure out ordering.
// TODO: Introduce fact and rule, not just rule.
// TODO 10. Introduce types in horn clauses.
// TODO 12. Rewrite solver to use lambdas and types.
// TODO 15. Desugar Terms and Types to SMT Lib

object Compiler {

  val types = mutable.Map.empty[Symbol.TypeSymbol, Type]
  val funcs = mutable.Map.empty[SExp.Name, Term]

  def compile(es: List[SExp]): Program = {
    for (e <- es) {
      e match {
        case SExp.Lst(List(SExp.Keyword("def-type"), SExp.Name(n), typ)) =>
          types += ((Symbol.TypeSymbol(n), compileType(typ)))

        case SExp.Lst(List(SExp.Keyword("def-bot"), SExp.Name(n), exp)) => compileTerm(exp)

        case SExp.Lst(List(SExp.Keyword("def-leq"), SExp.Name(n), args, body)) => compileFunction(args, body)

        case SExp.Lst(List(SExp.Keyword("def-lub"), SExp.Name(n), args, body)) => compileFunction(args, body)

        case SExp.Lst(List(SExp.Keyword("def-height"), SExp.Name(n), args, body)) => compileFunction(args, body)

        case SExp.Lst(List(SExp.Keyword("def-fun"), SExp.Str(n), args, body)) => compileFunction(args, body)

        case SExp.Lst(SExp.Keyword("rule") :: head :: tail) => compileRule(head, tail)
      }
    }
    ???
  }

  def compileRule(head: SExp, body: List[SExp]): Unit = {

  }

  def compilePredicate(e: SExp): Unit = ???

  def compileFunction(args: SExp, body: SExp): Term = Term.Unit // TODO

  def compileTerm(e: SExp): Term = e match {
    case SExp.Lst(SExp.Keyword("match") :: exp :: cases) => compileTerm(exp); Term.Unit // TODO

    case SExp.Unit(token) => Term.Unit
    case SExp.Bool(token) => Term.Bool(token.toBoolean)
    case SExp.Int(token) => Term.Int(token.toInt)
    case SExp.Str(token) => Term.Str(token)
    case SExp.Name(n) => Term.Tagged(Symbol.NamedSymbol(n), Term.Unit, Type.Sum(List.empty)) // TODO: Type
    case SExp.Lst(List(SExp.Name(s), e1)) => Term.Tagged(Symbol.NamedSymbol(s), compileTerm(e1), ???) // TODO: Type
    case SExp.Lst(List(e1, e2)) => Term.Tuple2(compileTerm(e1), compileTerm(e2))
    case SExp.Lst(List(e1, e2, e3)) => Term.Tuple3(compileTerm(e1), compileTerm(e2), compileTerm(e3))
    case SExp.Lst(List(e1, e2, e3, e4)) => Term.Tuple4(compileTerm(e1), compileTerm(e2), compileTerm(e3), compileTerm(e4))
    case SExp.Lst(List(e1, e2, e3, e4, e5)) => Term.Tuple5(compileTerm(e1), compileTerm(e2), compileTerm(e3), compileTerm(e4), compileTerm(e5))

    case _ => throw new RuntimeException(s"Unexpected term: $e")
  }

  def compilePattern(e: SExp): Pattern = e match {
    case SExp.Var(x) => Pattern.Var(Symbol.VariableSymbol(x))
    case SExp.Lst(List(e1, e2)) => Pattern.Tuple2(compilePattern(e1), compilePattern(e2))
    case SExp.Lst(List(e1, e2, e3)) => Pattern.Tuple3(compilePattern(e1), compilePattern(e2), compilePattern(e3))
    case SExp.Lst(List(e1, e2, e3, e4)) => Pattern.Tuple4(compilePattern(e1), compilePattern(e2), compilePattern(e3), compilePattern(e4))
    case SExp.Lst(List(e1, e2, e3, e4, e5)) => Pattern.Tuple5(compilePattern(e1), compilePattern(e2), compilePattern(e3), compilePattern(e4), compilePattern(e5))
    case _ => throw new RuntimeException(s"Unexpected pattern: $e")
  }

  def compileType(e: SExp): Type = e match {
    case SExp.Name(s) => Type.Tagged(Symbol.NamedSymbol(s), Type.Unit)
    case SExp.Lst(types) => Type.Sum(types.map(compileType))
    case _ => throw new RuntimeException(s"Unexpected type: $e")
  }

}
