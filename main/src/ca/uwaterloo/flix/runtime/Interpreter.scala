package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Literal, Pattern, Type, FormalArg, Root}
import ca.uwaterloo.flix.language.ast.{TypedAst, Name, BinaryOperator, UnaryOperator}

// TODO: Consider an EvaluationContext
object Interpreter {

  type Env = Map[String, Value]

  // TODO: Use this exception:

  /**
   * An exception thrown to indicate an internal runtime error.
   *
   * This exception should never be thrown if the compiler and runtime is implemented correctly.
   *
   * @param message the error message.
   */
  case class InternalRuntimeError(message: String) extends RuntimeException(message)


  def eval(expr: Expression, root: Root, env: Env = Map()): Value = {
    expr match {
      case Expression.Lit(literal, _, _) => evalLit(literal)
      case Expression.Var(ident, _, loc) => env.get(ident.name) match {
        case None => throw InternalRuntimeError(s"Unbound variable ${ident.name} at ${loc.format}.")
        case Some(value) => value
      }
      case Expression.Ref(name, _, _) =>
        assert(root.constants.contains(name), s"Expected constant $name to be defined.")
        eval(root.constants(name).exp, root, env)
      case Expression.Lambda(formals, body, _, _) => Value.Closure(formals, body, env)
      case Expression.Apply(exp, args, _, _) => eval(exp, root, env) match {
        case Value.Closure(formals, body, closureEnv) =>
          val evalArgs = args.map(x => eval(x, root, env))
          val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
          eval(body, root, newEnv)
        case _ => assert(false, "Expected a function."); Value.Unit
      }
      case Expression.Unary(op, exp, _, _) => evalUnary(op, eval(exp, root, env))
      case Expression.Binary(op, exp1, exp2, _, _) => evalBinary(op, eval(exp1, root, env), eval(exp2, root, env))
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
        val cond = eval(exp1, root, env).toBool
        if (cond) eval(exp2, root, env) else eval(exp3, root, env)
      case Expression.Let(ident, value, body, tpe, loc) =>
        // TODO: Right now Let only supports a single binding. Does it make sense to allow a list of bindings?
        val func = Expression.Lambda(List(FormalArg(ident, value.tpe)), body, Type.Lambda(List(value.tpe), tpe), loc)
        val desugared = Expression.Apply(func, List(value), tpe, loc)
        eval(desugared, root, env)
      case Expression.Match(exp, rules, _, _) =>
        val value = eval(exp, root, env)
        matchRule(rules, value) match {
          case Some((matchExp, matchEnv)) => eval(matchExp, root, env ++ matchEnv)
          case None => throw new RuntimeException(s"Unmatched value $value.")
        }
      case Expression.Tag(name, ident, exp, _, _) => Value.Tag(name, ident.name, eval(exp, root, env))
      case Expression.Tuple(elms, _, _) => Value.Tuple(elms.map(e => eval(e, root, env)))
      case Expression.Error(tpe, loc) => throw new RuntimeException(s"Error at ${loc.format}.")
    }
  }

  def evalLit(lit: Literal): Value = lit match {
    case Literal.Unit(_) => Value.Unit
    case Literal.Bool(b, _) => Value.Bool(b)
    case Literal.Int(i, _) => Value.Int(i)
    case Literal.Str(s, _) => Value.Str(s)
    case Literal.Tag(name, ident, innerLit, _, _) => Value.Tag(name, ident.name, evalLit(innerLit))
    case Literal.Tuple(elms, _, _) => Value.Tuple(elms.map(evalLit))
  }

  private def evalUnary(op: UnaryOperator, v: Value): Value = op match {
    case UnaryOperator.Not => Value.Bool(!v.toBool)
    case UnaryOperator.UnaryPlus => Value.Int(+v.toInt)
    case UnaryOperator.UnaryMinus => Value.Int(-v.toInt)
  }

  private def evalBinary(op: BinaryOperator, v1: Value, v2: Value): Value = op match {
    case BinaryOperator.Plus => Value.Int(v1.toInt + v2.toInt)
    case BinaryOperator.Minus => Value.Int(v1.toInt - v2.toInt)
    case BinaryOperator.Times => Value.Int(v1.toInt * v2.toInt)
    case BinaryOperator.Divide => Value.Int(v1.toInt / v2.toInt)
    case BinaryOperator.Modulo => Value.Int(v1.toInt % v2.toInt) // TODO: Document semantics of modulo on negative operands
    case BinaryOperator.Less => Value.Bool(v1.toInt < v2.toInt)
    case BinaryOperator.LessEqual => Value.Bool(v1.toInt <= v2.toInt)
    case BinaryOperator.Greater => Value.Bool(v1.toInt > v2.toInt)
    case BinaryOperator.GreaterEqual => Value.Bool(v1.toInt >= v2.toInt)
    case BinaryOperator.Equal => Value.Bool(v1 == v2)
    case BinaryOperator.NotEqual => Value.Bool(v1 != v2)
    case BinaryOperator.And => Value.Bool(v1.toBool && v2.toBool)
    case BinaryOperator.Or => Value.Bool(v1.toBool || v2.toBool)
  }

  private def matchRule(rules: List[(Pattern, Expression)], value: Value): Option[(Expression, Env)] = rules match {
    case (pattern, exp) :: rest => unify(pattern, value) match {
      case Some(env) => Some((exp, env))
      case None => matchRule(rest, value)
    }
    case Nil => None
  }

  private def unify(pattern: Pattern, value: Value): Option[Env] = (pattern, value) match {
    case (Pattern.Wildcard(_, _), _) => Some(Map())
    case (Pattern.Var(ident, _, _), _) => Some(Map(ident.name -> value))
    case (Pattern.Lit(lit, _, _), _) if evalLit(lit) == value => Some(Map())
    case (Pattern.Tag(name1, ident1, innerPat, _, _), Value.Tag(name2, ident2, innerVal))
      if name1 == name2 && ident1.name == ident2 => unify(innerPat, innerVal)
    case (Pattern.Tuple(pats, _, _), Value.Tuple(vals)) =>
      val envs = pats.zip(vals).map { case (p, v) => unify(p, v) }.collect { case Some(e) => e }
      if (pats.size == envs.size)
        Some(envs.foldLeft(Map[String, Value]()) { case (acc, newEnv) => acc ++ newEnv })
      else None
    case _ => None
  }

  // TODO: Need to come up with some more clean interfaces
  // TODO: Everything below here is really bad and should just be replaced at will.

  /**
   * Evaluates the given head term `t` under the given environment `env0`
   */
  def evalHeadTerm(t: TypedAst.Term.Head, root: TypedAst.Root, env: Map[String, Value]): Value = t match {
    case TypedAst.Term.Head.Var(x, tpe, loc) => env.get(x.name) match {
      case None => throw InternalRuntimeError("Unbound variable in head term!")
      case Some(value) => value
    }
    case TypedAst.Term.Head.Lit(lit, tpe, loc) => Interpreter.evalLit(lit)
    case TypedAst.Term.Head.Apply(name, terms, tpe, loc) =>
      val f = root.constants(name)
      val Value.Closure(formals, body, closureEnv) = eval(f.exp, root, env)
      val evalArgs = terms.map(t => evalHeadTerm(t, root, env))
      val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
      eval(body, root, newEnv)

  }

  def evalBodyTerm(t: TypedAst.Term.Body, env: Map[String, Value]): Value = t match {
    case TypedAst.Term.Body.Var(x, _, _) => env(x.name)
    // TODO ...
  }

  def eval2(lambda: Expression, v1: Value, v2: Value, root: TypedAst.Root): Value =
    ???

  def evalCall(d: TypedAst.Definition.Constant, terms: List[TypedAst.Term.Body], root: TypedAst.Root, env: Map[String, Value]): Value = {
    eval(d.exp, root, env) match {
      case Value.Closure(formals, body, closureEnv) =>
        val evalArgs = terms.map(t => evalBodyTerm(t, env))
        val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
        eval(body, root, newEnv)
      case _ => ???
    }
  }

}
