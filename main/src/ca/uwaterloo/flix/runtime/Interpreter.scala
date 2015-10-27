package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.TypedAst.{Definition, Expression, Literal, Pattern, Type, Term, Root}
import ca.uwaterloo.flix.language.ast.{TypedAst, BinaryOperator, UnaryOperator}

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

  /*
   * Evaluates an `Expression`. Based on the type of `expr`, call either the
   * specialized `evalInt` or `evalBool`, or the general `evalGeneral`
   * evaluator.
   *
   * Assumes all input has been type-checked.
   */
  def eval(expr: Expression, root: Root, env: Env = Map()): Value = expr.tpe match {
    case Type.Int => Value.mkInt(evalInt(expr, root, env))
    case Type.Bool => if (evalBool(expr, root, env)) Value.True else Value.False
    case Type.Var(_) | Type.Unit | Type.Str | Type.Tag(_, _, _) | Type.Enum(_) | Type.Tuple(_) |
         Type.Lambda(_, _) | Type.Predicate(_) | Type.Native(_) =>
      evalGeneral(expr, root, env)
  }

  /*
   * Evaluates expressions of type `Type.Int`, returning an unwrapped
   * `scala.Int`. Performs casting as necessary.
   *
   * Subexpressions are evaluated by calling specialized eval whenever
   * possible. For example, subexpressions of int binary expressions must
   * themselves be int expressions, so we can call `evalInt`. On the other
   * hand, the condition of an IfThenElse expression must have type bool, so we
   * call `evalBool`. And if the expression cannot have int or bool type (for
   * example, the `exp` of Apply(exp, args, _, _), we directly call
   * `evalGeneral`.
   */
  def evalInt(expr: Expression, root: Root, env: Env = Map()): Int = {
    def evalUnary(op: UnaryOperator, e: Expression): Int = op match {
      case UnaryOperator.UnaryPlus => +evalInt(e, root, env)
      case UnaryOperator.UnaryMinus => -evalInt(e, root, env)
      case UnaryOperator.Not =>
        throw new InternalRuntimeError(s"Expression $expr has type ${expr.tpe} instead of Type.Int.")
    }

    // TODO: Document semantics of modulo on negative operands
    def evalBinary(op: BinaryOperator, e1: Expression, e2: Expression): Int = op match {
      case BinaryOperator.Plus => evalInt(e1, root, env) + evalInt(e2, root, env)
      case BinaryOperator.Minus => evalInt(e1, root, env) - evalInt(e2, root, env)
      case BinaryOperator.Times => evalInt(e1, root, env) * evalInt(e2, root, env)
      case BinaryOperator.Divide => evalInt(e1, root, env) / evalInt(e2, root, env)
      case BinaryOperator.Modulo => evalInt(e1, root, env) % evalInt(e2, root, env)
      case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual |
           BinaryOperator.Equal | BinaryOperator.NotEqual | BinaryOperator.And | BinaryOperator.Or =>
        throw new InternalRuntimeError(s"Binary expression $expr does not return an int.")
    }

    expr match {
      case Expression.Lit(literal, _, _) => evalLit(literal).toInt
      case Expression.Var(ident, _, loc) => env(ident.name).toInt
      case Expression.Ref(name, _, _) => evalInt(root.constants(name).exp, root, env)
      case Expression.Apply(exp, args, _, _) =>
        val Value.Closure(formals, body, closureEnv) = evalGeneral(exp, root, env)
        val evalArgs = args.map(x => eval(x, root, env))
        val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
        evalInt(body, root, newEnv)
      case Expression.Unary(op, exp, _, _) => evalUnary(op, exp)
      case Expression.Binary(op, exp1, exp2, _, _) => evalBinary(op, exp1, exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
        val cond = evalBool(exp1, root, env)
        if (cond) evalInt(exp2, root, env) else evalInt(exp3, root, env)
      case Expression.Let(ident, exp1, exp2, _, _) =>
        // TODO: Right now Let only supports a single binding. Does it make sense to allow a list of bindings?
        val newEnv = env + (ident.name -> eval(exp1, root, env))
        evalInt(exp2, root, newEnv)
      case Expression.Match(exp, rules, _, _) =>
        val value = eval(exp, root, env)
        matchRule(rules, value) match {
          case Some((matchExp, matchEnv)) => evalInt(matchExp, root, env ++ matchEnv)
          case None => throw new RuntimeException(s"Unmatched value $value.")
        }
      case Expression.Lambda(_, _, _, _) | Expression.Tag(_, _, _, _, _) | Expression.Tuple(_, _, _) |
           Expression.NativeField(_, _, _, _, _) | Expression.NativeMethod(_, _, _, _, _) =>
        throw new InternalRuntimeError(s"Expression $expr has type ${expr.tpe} instead of Type.Int.")
      case Expression.Error(tpe, loc) => throw new RuntimeException(s"Error at ${loc.format}.")
    }
  }

  /*
   * Evaluates expressions of type `Type.Bool`, returning an unwrapped
   * `scala.Boolean`. Performs casting as necessary.
   *
   * Subexpressions are evaluated by calling specialized eval whenever
   * possible.
   */
  def evalBool(expr: Expression, root: Root, env: Env = Map()): Boolean = {
    def evalUnary(op: UnaryOperator, e: Expression): Boolean = op match {
      case UnaryOperator.Not => !evalBool(e, root, env)
      case UnaryOperator.UnaryPlus | UnaryOperator.UnaryMinus =>
        throw new InternalRuntimeError(s"Expression $expr has type ${expr.tpe} instead of Type.Bool.")
    }

    def evalBinary(op: BinaryOperator, e1: Expression, e2: Expression): Boolean = op match {
      case BinaryOperator.Plus | BinaryOperator.Minus | BinaryOperator.Times | BinaryOperator.Divide |
           BinaryOperator.Modulo =>
        throw new InternalRuntimeError(s"Binary expression $expr does not return a boolean.")
      case BinaryOperator.Less => evalInt(e1, root, env) < evalInt(e2, root, env)
      case BinaryOperator.LessEqual => evalInt(e1, root, env) <= evalInt(e2, root, env)
      case BinaryOperator.Greater => evalInt(e1, root, env) > evalInt(e2, root, env)
      case BinaryOperator.GreaterEqual => evalInt(e1, root, env) >= evalInt(e2, root, env)
      case BinaryOperator.Equal => eval(e1, root, env) == eval(e2, root, env)
      case BinaryOperator.NotEqual => eval(e1, root, env) != eval(e2, root, env)
      case BinaryOperator.And => evalBool(e1, root, env) && evalBool(e2, root, env)
      case BinaryOperator.Or => evalBool(e1, root, env) || evalBool(e2, root, env)
    }

    expr match {
      case Expression.Lit(literal, _, _) => evalLit(literal).toBool
      case Expression.Var(ident, _, loc) => env(ident.name).toBool
      case Expression.Ref(name, _, _) => evalBool(root.constants(name).exp, root, env)
      case Expression.Apply(exp, args, _, _) =>
        val Value.Closure(formals, body, closureEnv) = evalGeneral(exp, root, env)
        val evalArgs = args.map(x => eval(x, root, env))
        val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
        evalBool(body, root, newEnv)
      case Expression.Unary(op, exp, _, _) => evalUnary(op, exp)
      case Expression.Binary(op, exp1, exp2, _, _) => evalBinary(op, exp1, exp2)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
        val cond = evalBool(exp1, root, env)
        if (cond) evalBool(exp2, root, env) else evalBool(exp3, root, env)
      case Expression.Let(ident, exp1, exp2, _, _) =>
        // TODO: Right now Let only supports a single binding. Does it make sense to allow a list of bindings?
        val newEnv = env + (ident.name -> eval(exp1, root, env))
        evalBool(exp2, root, newEnv)
      case Expression.Match(exp, rules, _, _) =>
        val value = eval(exp, root, env)
        matchRule(rules, value) match {
          case Some((matchExp, matchEnv)) => evalBool(matchExp, root, env ++ matchEnv)
          case None => throw new RuntimeException(s"Unmatched value $value.")
        }
      case Expression.Lambda(_, _, _, _) | Expression.Tag(_, _, _, _, _) | Expression.Tuple(_, _, _) |
           Expression.NativeField(_, _, _, _, _) | Expression.NativeMethod(_, _, _, _, _) =>
        throw new InternalRuntimeError(s"Expression $expr has type ${expr.tpe} instead of Type.Bool.")
      case Expression.Error(tpe, loc) => throw new RuntimeException(s"Error at ${loc.format}.")
    }
  }

  /*
   * A general evaluator of `Expression`s.
   *
   * Subexpressions are always evaluated by calling `eval`, which will call the
   * specialized eval whenever possible.
   */
  def evalGeneral(expr: Expression, root: Root, env: Env = Map()): Value = {
    def evalUnary(op: UnaryOperator, v: Value): Value = op match {
      case UnaryOperator.Not => if (v.toBool) Value.False else Value.True
      case UnaryOperator.UnaryPlus => Value.mkInt(+v.toInt)
      case UnaryOperator.UnaryMinus => Value.mkInt(-v.toInt)
    }

    def evalBinary(op: BinaryOperator, v1: Value, v2: Value): Value = op match {
      case BinaryOperator.Plus => Value.mkInt(v1.toInt + v2.toInt)
      case BinaryOperator.Minus => Value.mkInt(v1.toInt - v2.toInt)
      case BinaryOperator.Times => Value.mkInt(v1.toInt * v2.toInt)
      case BinaryOperator.Divide => Value.mkInt(v1.toInt / v2.toInt)
      case BinaryOperator.Modulo => Value.mkInt(v1.toInt % v2.toInt)
      case BinaryOperator.Less => if (v1.toInt < v2.toInt) Value.True else Value.False
      case BinaryOperator.LessEqual => if (v1.toInt <= v2.toInt) Value.True else Value.False
      case BinaryOperator.Greater => if (v1.toInt > v2.toInt) Value.True else Value.False
      case BinaryOperator.GreaterEqual => if (v1.toInt >= v2.toInt) Value.True else Value.False
      case BinaryOperator.Equal => if (v1 == v2) Value.True else Value.False
      case BinaryOperator.NotEqual => if (v1 != v2) Value.True else Value.False
      case BinaryOperator.And => if (v1.toBool && v2.toBool) Value.True else Value.False
      case BinaryOperator.Or => if (v1.toBool || v2.toBool) Value.True else Value.False
    }

    expr match {
      case Expression.Lit(literal, _, _) => evalLit(literal)
      case Expression.Var(ident, _, loc) => env(ident.name)
      case Expression.Ref(name, _, _) => eval(root.constants(name).exp, root, env)
      case Expression.Lambda(formals, body, _, _) => Value.Closure(formals, body, env)
      case Expression.Apply(exp, args, _, _) =>
        val Value.Closure(formals, body, closureEnv) = evalGeneral(exp, root, env)
        val evalArgs = args.map(x => eval(x, root, env))
        val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
        eval(body, root, newEnv)
      case Expression.Unary(op, exp, _, _) => evalUnary(op, eval(exp, root, env))
      case Expression.Binary(op, exp1, exp2, _, _) => evalBinary(op, eval(exp1, root, env), eval(exp2, root, env))
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
        val cond = evalBool(exp1, root, env)
        if (cond) eval(exp2, root, env) else eval(exp3, root, env)
      case Expression.Let(ident, exp1, exp2, _, _) =>
        // TODO: Right now Let only supports a single binding. Does it make sense to allow a list of bindings?
        val newEnv = env + (ident.name -> eval(exp1, root, env))
        eval(exp2, root, newEnv)
      case Expression.Match(exp, rules, _, _) =>
        val value = eval(exp, root, env)
        matchRule(rules, value) match {
          case Some((matchExp, matchEnv)) => eval(matchExp, root, env ++ matchEnv)
          case None => throw new RuntimeException(s"Unmatched value $value.")
        }
      case Expression.NativeField(className, memberName, field, tpe, loc) => Value.NativeField(field.get())
      case Expression.NativeMethod(classname, memberName, method, tpe, loc) => ??? // TODO
      case Expression.Tag(name, ident, exp, _, _) => Value.mkTag(name, ident.name, eval(exp, root, env))
      case Expression.Tuple(elms, _, _) => Value.Tuple(elms.map(e => eval(e, root, env)))
      case Expression.Error(tpe, loc) => throw new RuntimeException(s"Error at ${loc.format}.")
    }
  }

  def evalLit(lit: Literal): Value = lit match {
    case Literal.Unit(_) => Value.Unit
    case Literal.Bool(b, _) => if (b) Value.True else Value.False
    case Literal.Int(i, _) => Value.mkInt(i)
    case Literal.Str(s, _) => Value.mkStr(s)
    case Literal.Tag(name, ident, innerLit, _, _) => Value.mkTag(name, ident.name, evalLit(innerLit))
    case Literal.Tuple(elms, _, _) => Value.Tuple(elms.map(evalLit))
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
  def evalHeadTerm(t: Term.Head, root: Root, env: Env): Value = t match {
    case Term.Head.Var(x, _, _) => env(x.name)
    case Term.Head.Lit(lit, _, _) => evalLit(lit)
    case Term.Head.Apply(name, terms, _, _) =>
      val function = root.constants(name)
      val Value.Closure(formals, body, closureEnv) = evalGeneral(function.exp, root, env)
      val evalArgs = terms.map(t => evalHeadTerm(t, root, env))
      val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
      eval(body, root, newEnv)
  }

  def evalBodyTerm(t: Term.Body, env: Env): Value = t match {
    case Term.Body.Wildcard(_, _) => ???
    case Term.Body.Var(x, _, _) => env(x.name)
    case Term.Body.Lit(lit, _, _) => evalLit(lit)
  }

  def eval2(lambda: Expression, v1: Value, v2: Value, root: Root): Value = {
    val Value.Closure(formals, body, closureEnv) = evalGeneral(lambda, root)
    val evalArgs = List(v1, v2)
    val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
    eval(body, root, newEnv)
  }

  def evalCall(definition: Definition.Constant, terms: List[Term.Body], root: Root, env: Env): Value = {
    val Value.Closure(formals, body, closureEnv) = evalGeneral(definition.exp, root, env)
    val evalArgs = terms.map(t => evalBodyTerm(t, env))
    val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
    eval(body, root, newEnv)
  }
}
