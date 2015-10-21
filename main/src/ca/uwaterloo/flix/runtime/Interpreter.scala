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

  /*
   * Evaluates an `Expression`. Whenever possible, it calls specialized methods
   * `evalInt` and `evalBool` which avoid creating intermediate `Value`s.
   *
   * Specifically, this means Unary, Binary, and IfThenElse expressions are
   * specialized. All other cases are handled by calling `evalGeneral` (which,
   * in turn, may call `eval`).
   *
   * Subexpressions are evaluated by recursively calling `eval`.
   */
  def eval(expr: Expression, root: Root, env: Env = Map()): Value = {
    /*
     * If the type of the expression is an Int or Bool, we can use the
     * specialized Int and Bool evaluators. Otherwise, we use `evalGeneral`.
     */
    def doEval(exp: Expression): Value = exp.tpe match {
      case Type.Int => Value.mkInt(evalInt(exp, root, env))
      case Type.Bool => if (evalBool(exp, root, env)) Value.True else Value.False
      case _ => evalGeneral(exp, root, env)
    }

    def evalInt(expr: Expression, root: Root, env: Env = Map()): Int = expr match {
      case Expression.Unary(UnaryOperator.UnaryPlus, e, _, _) => +evalInt(e, root, env)
      case Expression.Unary(UnaryOperator.UnaryMinus, e, _, _) => -evalInt(e, root, env)
      case Expression.Binary(BinaryOperator.Plus, e1, e2, _, _) => evalInt(e1, root, env) + evalInt(e2, root, env)
      case Expression.Binary(BinaryOperator.Minus, e1, e2, _, _) => evalInt(e1, root, env) - evalInt(e2, root, env)
      case Expression.Binary(BinaryOperator.Times, e1, e2, _, _) => evalInt(e1, root, env) * evalInt(e2, root, env)
      case Expression.Binary(BinaryOperator.Divide, e1, e2, _, _) => evalInt(e1, root, env) / evalInt(e2, root, env)
      case Expression.Binary(BinaryOperator.Modulo, e1, e2, _, _) =>
        // TODO: Document semantics of modulo on negative operands
        evalInt(e1, root, env) % evalInt(e2, root, env)
      case _ => evalGeneral(expr, root, env).asInstanceOf[Value.Int].i
    }

    def evalBool(expr: Expression, root: Root, env: Env = Map()): Boolean = expr match {
      case Expression.Unary(UnaryOperator.Not, e, _, _) => !evalBool(e, root, env)
      case Expression.Binary(BinaryOperator.Less, e1, e2, _, _) => evalInt(e1, root, env) < evalInt(e2, root, env)
      case Expression.Binary(BinaryOperator.LessEqual, e1, e2, _, _) => evalInt(e1, root, env) <= evalInt(e2, root, env)
      case Expression.Binary(BinaryOperator.Greater, e1, e2, _, _) => evalInt(e1, root, env) > evalInt(e2, root, env)
      case Expression.Binary(BinaryOperator.GreaterEqual, e1, e2, _, _) => evalInt(e1, root, env) >= evalInt(e2, root, env)
      case Expression.Binary(BinaryOperator.Equal, e1, e2, _, _) => eval(e1, root, env) == eval(e2, root, env)
      case Expression.Binary(BinaryOperator.NotEqual, e1, e2, _, _) => eval(e1, root, env) != eval(e2, root, env)
      case Expression.Binary(BinaryOperator.And, e1, e2, _, _) => evalBool(e1, root, env) && evalBool(e2, root, env)
      case Expression.Binary(BinaryOperator.Or, e1, e2, _, _) => evalBool(e1, root, env) || evalBool(e2, root, env)
      case _ => evalGeneral(expr, root, env).asInstanceOf[Value.Bool].b
    }

    expr match {
      case Expression.Unary(_, _, _, _) => doEval(expr)
      case Expression.Binary(_, _, _, _, _) => doEval(expr)
      case Expression.IfThenElse(exp1, exp2, exp3, _, _) =>
        if (evalBool(exp1, root, env)) eval(exp2, root, env) else eval(exp3, root, env)
      case _ => evalGeneral(expr, root, env)
    }
  }

  /*
   * A general evaluator of `Expression`s. It is always safe to call
   * `evalGeneral`, though it may be slower than calling the specialized
   * `eval`.
   *
   * Note that `eval` and `evalGeneral` can be mutually recursive.
   *
   * Subexpressions are always evaluated by calling the faster `eval`, with
   * two exceptions: evaluating the exp of Apply(exp, args, _, _) and
   * evaluating the Apply of a desugared Let expression. `eval` is not
   * specialized for those cases and falls back to calling `evalGeneral`.
   * Therefore, in those cases, we call `evalGeneral` directly.
   */
  private def evalGeneral(expr: Expression, root: Root, env: Env = Map()): Value = {
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
        val cond = eval(exp1, root, env).toBool
        if (cond) eval(exp2, root, env) else eval(exp3, root, env)
      case Expression.Let(ident, value, body, tpe, loc) =>
        // TODO: Right now Let only supports a single binding. Does it make sense to allow a list of bindings?
        val func = Expression.Lambda(List(FormalArg(ident, value.tpe)), body, Type.Lambda(List(value.tpe), tpe), loc)
        val desugared = Expression.Apply(func, List(value), tpe, loc)
        evalGeneral(desugared, root, env)
      case Expression.Match(exp, rules, _, _) =>
        val value = eval(exp, root, env)
        matchRule(rules, value) match {
          case Some((matchExp, matchEnv)) => eval(matchExp, root, env ++ matchEnv)
          case None => throw new RuntimeException(s"Unmatched value $value.")
        }
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
    case TypedAst.Term.Body.Wildcard(_, _) => ???
    case TypedAst.Term.Body.Var(x, _, _) => env(x.name)
    case TypedAst.Term.Body.Lit(lit, _, _) => ???
  }

  def eval2(lambda: Expression, v1: Value, v2: Value, root: TypedAst.Root): Value =
    eval(lambda, root, Map.empty) match {
      case Value.Closure(formals, body, closureEnv) =>
        val evalArgs = List(v1, v2)
        val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
        eval(body, root, newEnv)
      case _ => ??? // TODO: Just omit this
    }

  def evalCall(d: TypedAst.Definition.Constant, terms: List[TypedAst.Term.Body], root: TypedAst.Root, env: Map[String, Value]): Value = {
    eval(d.exp, root, env) match {
      case Value.Closure(formals, body, closureEnv) =>
        val evalArgs = terms.map(t => evalBodyTerm(t, env))
        val newEnv = closureEnv ++ formals.map(_.ident.name).zip(evalArgs).toMap
        eval(body, root, newEnv)
      case _ => ??? // TODO: Just omit this
    }
  }

}
