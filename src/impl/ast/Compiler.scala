package impl.ast

import impl.logic.Declaration.{DeclareBot, DeclareLeq, DeclareLub}
import impl.logic._
import impl.runtime.{Error, Interpreter}
import impl.verifier.Typer

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

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
        case SExp.Lst(List(SExp.Keyword("def-type"), SExp.Name(n), e1)) =>
          val typ = compileType(e1)
          types += (n -> typ)
          typ.resultType match {
            case x: Type.Set =>
              declarations += synthesizeBot(x)
              declarations += synthesizeLeq(x)
              declarations += synthesizeLub(x)
            case _ => // nop
          }

        case SExp.Lst(List(SExp.Keyword("def-fun"), SExp.Var(n), SExp.Lst(args), body)) =>
          val t = compileAbs(args, body)
          val typ = Typer.typecheck(t)
          funcs += (n -> t)

        case SExp.Lst(List(SExp.Keyword("def-bot"), SExp.Name(n), exp)) =>
          val t = compileTerm(exp)
          val typ = Typer.typecheck(t)
          val v = Interpreter.evaluate(t)
          declarations += Declaration.DeclareBot(v, typ)

        case SExp.Lst(List(SExp.Keyword("def-leq"), SExp.Name(n), SExp.Lst(args), body)) =>
          val t = compileAbs(args, body)
          val typ = Typer.typecheck(t)
          declarations += Declaration.DeclareLeq(t, typ)

        case SExp.Lst(List(SExp.Keyword("def-lub"), SExp.Name(n), SExp.Lst(args), body)) =>
          val t = compileAbs(args, body)
          val typ = Typer.typecheck(t)
          declarations += Declaration.DeclareLub(t, typ)

        case SExp.Lst(List(SExp.Keyword("def-height"), SExp.Name(n), SExp.Lst(args), body)) =>
          val t = compileAbs(args, body)
          val typ = Typer.typecheck(t)
          declarations += Declaration.DeclareHeight(t, typ)

        case SExp.Lst(List(SExp.Keyword("fact"), head)) =>
          constraints += Constraint.Fact(compilePredicate(head).asInstanceOf[Predicate.GroundPredicate])

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
    case SExp.Lst(SExp.Name(s) :: xs) =>
      val terms = xs map compileTerm
      val values = terms map (t => Interpreter.evaluateOpt(t))

      val declaredType = lookupType(s)

      if (values.forall(_.nonEmpty))
        Predicate.GroundPredicate(Symbol.PredicateSymbol(s), values map (_.get), declaredType)
      else
        Predicate.NonGroundPredicate(Symbol.PredicateSymbol(s), terms, declaredType)

    case _ => throw Error.ParseError(e)
  }

  /**
   * Compiles the given function to a term.
   */
  def compileAbs(args: List[SExp], body: SExp): Term.Abs = {
    def visit(xs: List[SExp]): Term = xs match {
      case SExp.Var(x) :: t :: rest => Term.Abs(Symbol.VariableSymbol(x), compileType(t), visit(rest))
      case Nil => compileTerm(body)
      case _ => throw Error.ParseError(SExp.Lst(args))
    }
    visit(args).asInstanceOf[Term.Abs]
  }

  /**
   * Compiles the given s-expression `e` to a term.
   */
  def compileTerm(e: SExp): Term = e match {
    case SExp.Var(x) if x == "_" => Term.Var(Symbol.freshVariableSymbol("_"))
    case SExp.Var(x) => Term.Var(Symbol.VariableSymbol(x))

    case SExp.Unit(token) => Term.Unit
    case SExp.Bool(token) => Term.Bool(token.toBoolean)
    case SExp.Int(token) => Term.Int(token.toInt)
    case SExp.Str(token) => Term.Str(token)

    case SExp.Lst(SExp.Keyword("set") :: rest) => Term.Set(rest.map(compileTerm).toSet)
    case SExp.Lst(List(SExp.Keyword("if"), e1, e2, e3)) => Term.IfThenElse(compileTerm(e1), compileTerm(e2), compileTerm(e3))
    case SExp.Lst(SExp.Keyword("match") :: exp :: rules) => Term.Match(compileTerm(exp), rules.map(compileRule))
    case SExp.Lst(List(SExp.Operator(op), left, right)) => Term.BinaryOp(BinaryOperator.valueOf(op), compileTerm(left), compileTerm(right))

    case SExp.Name(s) => Term.Tag(Symbol.NamedSymbol(s), Term.Unit, labels(s))
    case SExp.Lst(List(SExp.Name(s), es)) => Term.Tag(Symbol.NamedSymbol(s), compileTerm(es), labels(s))

    case SExp.Lst(SExp.Var(x) :: args) => args.reverse.foldLeft(funcs(x)) {
      case (t, a: SExp.Var) => Term.App(t, Term.Var(Symbol.VariableSymbol(a.token)))
    }

    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2)) => Term.Tuple2(compileTerm(e1), compileTerm(e2))
    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2, e3)) => Term.Tuple3(compileTerm(e1), compileTerm(e2), compileTerm(e3))
    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2, e3, e4)) => Term.Tuple4(compileTerm(e1), compileTerm(e2), compileTerm(e3), compileTerm(e4))
    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2, e3, e4, e5)) => Term.Tuple5(compileTerm(e1), compileTerm(e2), compileTerm(e3), compileTerm(e4), compileTerm(e5))

    case _ => throw Error.ParseError(e)
  }

  /**
   * Compiles the given s-expression `e` to a rule.
   */
  def compileRule(e: SExp): (Pattern, Term) = e match {
    case SExp.Lst(List(SExp.Keyword("case"), pattern, body)) => (compilePattern(pattern), compileTerm(body))
    case _ => throw Error.ParseError(e)
  }

  /**
   * Compiles the given s-expression `e` to a pattern.
   */
  def compilePattern(e: SExp): Pattern = e match {
    case SExp.Var(x) =>
      if (x == "_")
        Pattern.Wildcard
      else
        Pattern.Var(Symbol.VariableSymbol(x))
    case SExp.Name(s) => Pattern.Tag(Symbol.NamedSymbol(s), Pattern.Unit)

    case SExp.Lst(List(SExp.Name(s), e1)) => Pattern.Tag(Symbol.NamedSymbol(s), compilePattern(e1))

    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2)) => Pattern.Tuple2(compilePattern(e1), compilePattern(e2))
    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2, e3)) => Pattern.Tuple3(compilePattern(e1), compilePattern(e2), compilePattern(e3))
    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2, e3, e4)) => Pattern.Tuple4(compilePattern(e1), compilePattern(e2), compilePattern(e3), compilePattern(e4))
    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2, e3, e4, e5)) => Pattern.Tuple5(compilePattern(e1), compilePattern(e2), compilePattern(e3), compilePattern(e4), compilePattern(e5))
    case _ => throw Error.ParseError(e)
  }

  /**
   * Compiles the given s-expression `e` to a type.
   */
  def compileType(e: SExp): Type = e match {
    case SExp.Name("Bool") => Type.Bool
    case SExp.Name("Int") => Type.Int
    case SExp.Name("Str") => Type.Str
    case SExp.Name(s) => types(s)
    case SExp.Lst(List(SExp.Name("Set"), e1)) => Type.Set(compileType(e1))
    case SExp.Lst(List(SExp.Keyword("->"), e1, e2)) => Type.Function(compileType(e1), compileType(e2))
    case SExp.Lst(List(SExp.Keyword("variant"), SExp.Lst(variants))) =>
      val typ = Type.Sum(variants map compileType)
      typ.ts.foreach {
        case Type.Tag(Symbol.NamedSymbol(s), _) => labels += s -> typ
        case _ => // nop
      }
      typ

    case SExp.Lst(List(SExp.Name(s))) => Type.Tag(Symbol.NamedSymbol(s), Type.Unit)
    case SExp.Lst(List(SExp.Name(s), e1)) => Type.Tag(Symbol.NamedSymbol(s), compileType(e1))

    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2)) => Type.Tuple2(compileType(e1), compileType(e2))
    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2, e3)) => Type.Tuple3(compileType(e1), compileType(e2), compileType(e3))
    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2, e3, e4)) => Type.Tuple4(compileType(e1), compileType(e2), compileType(e3), compileType(e4))
    case SExp.Lst(List(SExp.Keyword("vec"), e1, e2, e3, e4, e5)) => Type.Tuple5(compileType(e1), compileType(e2), compileType(e3), compileType(e4), compileType(e5))
    case _ => throw Error.ParseError(e)
  }

  /**
   * Synthesizes a declaration of the bottom element for the given set type `typ`.
   */
  private def synthesizeBot(typ: Type.Set): DeclareBot = {
    Declaration.DeclareBot(Value.Set(Set.empty), typ)
  }

  /**
   * Synthesizes a declaration of the less-than-equal function for the given set type `typ`.
   */
  private def synthesizeLeq(typ: Type.Set): DeclareLeq = {
    val x = Symbol.freshVariableSymbol("x")
    val y = Symbol.freshVariableSymbol("y")
    val abs = Term.Abs(x, typ,
      Term.Abs(y, typ,
        Term.BinaryOp(
          BinaryOperator.Subset,
          Term.Var(x),
          Term.Var(y))))
    Declaration.DeclareLeq(abs, Type.Function(typ, Type.Function(typ, Type.Bool)))
  }

  /**
   * Synthesizes a declaration of the least-upper-bound function for the given set type `typ`.
   */
  private def synthesizeLub(typ: Type.Set): DeclareLub = {
    val x = Symbol.freshVariableSymbol("x")
    val y = Symbol.freshVariableSymbol("y")
    val abs = Term.Abs(x, typ,
      Term.Abs(y, typ,
        Term.BinaryOp(
          BinaryOperator.Union,
          Term.Var(x),
          Term.Var(y))))
    Declaration.DeclareLub(abs, Type.Function(typ, Type.Function(typ, typ)))
  }

}
