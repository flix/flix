package impl.ast

import impl.logic._
import impl.runtime.{Interpreter, Error}
import impl.verifier.TypeChecker

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

// TODO: Solve type dilema by having uppercase labels and lowercase type names?
// TODO: Figure out ordering or leq, lub etc.
// TODO typechecker
// TODO 15. Desugar Terms and Types to SMT Lib

object Compiler {

  val types = mutable.Map.empty[String, Type]
  val labels = mutable.Map.empty[String, Type.Sum]
  val funcs = mutable.Map.empty[String, Term]

  def lookupType(name: String): Type = types.get(name) match {
    case None => throw new RuntimeException(s"No type defined for name: $name")
    case Some(typ) => typ
  }

  /**
   * Compiles a list of top-level declarations to a logic program.
   */
  def compile(es: List[SExp]): Program = {
    val declarations = ListBuffer.empty[Declaration]
    val constraints = ListBuffer.empty[Constraint]
    for (e <- es) {
      e match {
        case SExp.Lst(List(SExp.Keyword("def-type"), SExp.Name(n), typ)) =>
          types += (n -> compileType(typ))

        case SExp.Lst(List(SExp.Keyword("def-fun"), SExp.Str(n), SExp.Lst(args), body)) =>
          val t = compileAbs(args, body)
          val typ = TypeChecker.typecheck(t)
          funcs += (n -> t)

        case SExp.Lst(List(SExp.Keyword("def-bot"), SExp.Name(n), exp)) =>
          val t = compileTerm(exp)
          val typ = TypeChecker.typecheck(t)
          val v = Interpreter.evaluate(t)
          declarations += Declaration.DeclareBot(v, typ)

        case SExp.Lst(List(SExp.Keyword("def-leq"), SExp.Name(n), SExp.Lst(args), body)) =>
          val t = compileAbs(args, body)
          val typ = TypeChecker.typecheck(t)
          declarations += Declaration.DeclareLeq(t, typ)

        case SExp.Lst(List(SExp.Keyword("def-lub"), SExp.Name(n), SExp.Lst(args), body)) =>
          val t = compileAbs(args, body)
          val typ = TypeChecker.typecheck(t)
          declarations += Declaration.DeclareLub(t, typ)

        case SExp.Lst(List(SExp.Keyword("def-height"), SExp.Name(n), SExp.Lst(args), body)) =>
          val t = compileAbs(args, body)
          val typ = TypeChecker.typecheck(t)
          declarations += Declaration.DeclareHeight(t, typ)

        case SExp.Lst(List(SExp.Keyword("fact"), head)) =>
          constraints += Constraint.Fact(compilePredicate(head))

        case SExp.Lst(List(SExp.Keyword("rule"), head, SExp.Lst(body))) =>
          constraints += Constraint.Rule(compilePredicate(head), body map compilePredicate)
      }
    }
    Program(declarations.toList, constraints.toList)
  }

  /**
   * Compiles the given s-expression `e` to a predicate.
   */
  def compilePredicate(e: SExp): Predicate = e match {
    case SExp.Lst(SExp.Name(s) :: terms) => Predicate(Symbol.PredicateSymbol(s), terms map compileTerm, lookupType(s))
    case _ => throw Error.UnableToParsePredicate(e)
  }

  /**
   * Compiles the given function to a term.
   */
  def compileAbs(args: List[SExp], body: SExp): Term.Abs = {
    def visit(xs: List[SExp]): Term = xs match {
      case SExp.Var(x) :: t :: rest => Term.Abs(Symbol.VariableSymbol(x), compileType(t), visit(rest))
      case Nil => compileTerm(body)
      case _ => ???
    }
    visit(args).asInstanceOf[Term.Abs]
  }

  /**
   * Compiles the given s-expression `e` to a term.
   */
  def compileTerm(e: SExp): Term = e match {
    case SExp.Var(x) => Term.Var(Symbol.VariableSymbol(x))
    case SExp.Unit(token) => Term.Unit
    case SExp.Bool(token) => Term.Bool(token.toBoolean)
    case SExp.Int(token) => Term.Int(token.toInt)
    case SExp.Str(token) => Term.Str(token)

    case SExp.Label(s) => Term.Tagged(Symbol.NamedSymbol(s), Term.Unit, labels(s))
    case SExp.Lst(List(SExp.Label(s), es)) => Term.Tagged(Symbol.NamedSymbol(s), compileTerm(es), labels(s))

    case SExp.Lst(List(e1, e2)) => Term.Tuple2(compileTerm(e1), compileTerm(e2))
    case SExp.Lst(List(e1, e2, e3)) => Term.Tuple3(compileTerm(e1), compileTerm(e2), compileTerm(e3))
    case SExp.Lst(List(e1, e2, e3, e4)) => Term.Tuple4(compileTerm(e1), compileTerm(e2), compileTerm(e3), compileTerm(e4))
    case SExp.Lst(List(e1, e2, e3, e4, e5)) => Term.Tuple5(compileTerm(e1), compileTerm(e2), compileTerm(e3), compileTerm(e4), compileTerm(e5))

    case _ => throw Error.UnableToParseTerm(e)
  }

  /**
   * Compiles the given s-expression `e` to a pattern.
   */
  def compilePattern(e: SExp): Pattern = e match {
    case SExp.Var(x) => Pattern.Var(Symbol.VariableSymbol(x))
    case SExp.Lst(List(e1, e2)) => Pattern.Tuple2(compilePattern(e1), compilePattern(e2))
    case SExp.Lst(List(e1, e2, e3)) => Pattern.Tuple3(compilePattern(e1), compilePattern(e2), compilePattern(e3))
    case SExp.Lst(List(e1, e2, e3, e4)) => Pattern.Tuple4(compilePattern(e1), compilePattern(e2), compilePattern(e3), compilePattern(e4))
    case SExp.Lst(List(e1, e2, e3, e4, e5)) => Pattern.Tuple5(compilePattern(e1), compilePattern(e2), compilePattern(e3), compilePattern(e4), compilePattern(e5))
    case _ => throw Error.UnableToParsePattern(e)
  }

  /**
   * Compiles the given s-expression `e` to a type.
   */
  def compileType(e: SExp): Type = e match {
    case SExp.Name("Bool") => Type.Bool
    case SExp.Name("Int") => Type.Int
    case SExp.Name("Str") => Type.Str
    case SExp.Name(s) => types(s)
    case SExp.Lst(List(SExp.Label(s))) => Type.Tagged(Symbol.NamedSymbol(s), Type.Unit)
    case SExp.Lst(List(SExp.Label(s), e1)) => Type.Tagged(Symbol.NamedSymbol(s), compileType(e))
    case SExp.Lst(List(SExp.Name("Set"), e1)) => Type.Set(compileType(e1))
    case SExp.Lst(List(SExp.Name("Lat"), e1)) => Type.Lat(compileType(e1))
    case SExp.Lst(List(SExp.Keyword("variant"), SExp.Lst(variants))) =>
      val typ = Type.Sum(variants map compileType)
      typ.ts.foreach {
        case Type.Tagged(Symbol.NamedSymbol(s), _) => labels += s -> typ
        case _ => // nop
      }
      typ

    case SExp.Lst(List(e1, e2)) => Type.Tuple2(compileType(e1), compileType(e2))
    case SExp.Lst(List(e1, e2, e3)) => Type.Tuple3(compileType(e1), compileType(e2), compileType(e3))
    case SExp.Lst(List(e1, e2, e3, e4)) => Type.Tuple4(compileType(e1), compileType(e2), compileType(e3), compileType(e4))
    case SExp.Lst(List(e1, e2, e3, e4, e5)) => Type.Tuple5(compileType(e1), compileType(e2), compileType(e3), compileType(e4), compileType(e5))
    case _ => throw Error.UnableToParseType(e)
  }

}
