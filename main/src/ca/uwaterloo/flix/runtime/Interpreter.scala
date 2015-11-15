package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Literal, Pattern, Type, Term, Root}
import ca.uwaterloo.flix.language.ast.{BinaryOperator, UnaryOperator}

import scala.annotation.tailrec
import scala.collection.mutable

// TODO: Consider an EvaluationContext
object Interpreter {

  type Env = mutable.Map[String, Value]

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
  def eval(expr: Expression, root: Root, env: Env = mutable.Map.empty): Value = expr.tpe match {
    case Type.Int => Value.mkInt(evalInt(expr, root, env))
    case Type.Bool => if (evalBool(expr, root, env)) Value.True else Value.False
    case Type.Str => evalGeneral(expr, root, env)
    case Type.Var(_) | Type.Unit | Type.Tag(_, _, _) | Type.Enum(_) | Type.Tuple(_) |
         Type.Set(_) | Type.Lambda(_, _) | Type.Predicate(_) | Type.Native(_) =>
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
  @tailrec private def evalInt(expr: Expression, root: Root, env: Env = mutable.Map.empty): Int =  expr match {
    case Expression.Lit(literal, _, _) => evalLit(literal).toInt
    case Expression.Var(ident, _, loc) => env(ident.name).toInt
    case Expression.Ref(name, _, _) => evalInt(root.constants(name).exp, root, env)
    case apply: Expression.Apply =>
      val evalArgs = new Array[Value](apply.argsAsArray.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(apply.argsAsArray(i), root, env)
        i = i + 1
      }
      evalCall(apply.exp, evalArgs, root, env).toInt
    case Expression.Unary(op, exp, _, _) => evalIntUnary(op, exp, root, env)
    case Expression.Binary(op, exp1, exp2, _, _) => evalIntBinary(op, exp1, exp2, root, env)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = evalBool(exp1, root, env)
      if (cond) evalInt(exp2, root, env) else evalInt(exp3, root, env)
    case Expression.Let(ident, exp1, exp2, _, _) =>
      // TODO: Right now Let only supports a single binding. Does it make sense to allow a list of bindings?
      val newEnv = env + (ident.name -> eval(exp1, root, env))
      evalInt(exp2, root, newEnv)
    case m: Expression.Match =>
      val value = eval(m.exp, root, env)
      val newEnv = env.clone()
      val result = matchRule(m.rulesAsArray, value, newEnv)
      if (result != null)
        evalInt(result, root, newEnv)
      else
        throw new RuntimeException(s"Unmatched value $value.")
    case Expression.NativeField(field, _, _) =>
      field.get().asInstanceOf[java.lang.Integer].intValue()
    case Expression.Lambda(_, _, _, _) | Expression.Tag(_, _, _, _, _) | Expression.Tuple(_, _, _) |
         Expression.Set(_, _, _) | Expression.NativeMethod(_, _, _) =>
      throw new InternalRuntimeError(s"Expression $expr has type ${expr.tpe} instead of Type.Int.")
    case Expression.Error(tpe, loc) => throw new RuntimeException(s"Error at ${loc.format}.")
  }

  private def evalIntUnary(op: UnaryOperator, e: Expression, root: Root, env: Env): Int = op match {
    case UnaryOperator.UnaryPlus => +evalInt(e, root, env)
    case UnaryOperator.UnaryMinus => -evalInt(e, root, env)
    case UnaryOperator.Set.Size => eval(e, root, env).toSet.size
    case UnaryOperator.Not | UnaryOperator.Set.IsEmpty | UnaryOperator.Set.NonEmpty | UnaryOperator.Set.Singleton =>
      throw new InternalRuntimeError(s"Type of unary expression is not Type.Int.")
  }

  // TODO: Document semantics of modulo on negative operands
  private def evalIntBinary(op: BinaryOperator, e1: Expression, e2: Expression, root: Root, env: Env): Int = op match {
    case BinaryOperator.Plus => evalInt(e1, root, env) + evalInt(e2, root, env)
    case BinaryOperator.Minus => evalInt(e1, root, env) - evalInt(e2, root, env)
    case BinaryOperator.Times => evalInt(e1, root, env) * evalInt(e2, root, env)
    case BinaryOperator.Divide => evalInt(e1, root, env) / evalInt(e2, root, env)
    case BinaryOperator.Modulo => evalInt(e1, root, env) % evalInt(e2, root, env)
    case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual |
         BinaryOperator.Equal | BinaryOperator.NotEqual | BinaryOperator.And | BinaryOperator.Or |
         BinaryOperator.Set.Member | BinaryOperator.Set.SubsetOf | BinaryOperator.Set.ProperSubsetOf |
         BinaryOperator.Set.Insert | BinaryOperator.Set.Remove | BinaryOperator.Set.Union |
         BinaryOperator.Set.Intersection | BinaryOperator.Set.Difference =>
      throw new InternalRuntimeError(s"Type of binary expression is not Type.Int.")
  }

  /*
   * Evaluates expressions of type `Type.Bool`, returning an unwrapped
   * `scala.Boolean`. Performs casting as necessary.
   *
   * Subexpressions are evaluated by calling specialized eval whenever
   * possible.
   */
  @tailrec private def evalBool(expr: Expression, root: Root, env: Env = mutable.Map.empty): Boolean = expr match {
    case Expression.Lit(literal, _, _) => evalLit(literal).toBool
    case Expression.Var(ident, _, loc) => env(ident.name).toBool
    case Expression.Ref(name, _, _) => evalBool(root.constants(name).exp, root, env)
    case apply: Expression.Apply =>
      val evalArgs = new Array[Value](apply.argsAsArray.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(apply.argsAsArray(i), root, env)
        i = i + 1
      }
      evalCall(apply.exp, evalArgs, root, env).toBool
    case Expression.Unary(op, exp, _, _) => evalBoolUnary(op, exp, root, env)
    case Expression.Binary(op, exp1, exp2, _, _) => evalBoolBinary(op, exp1, exp2, root, env)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = eval(exp1, root, env).toBool
      if (cond) evalBool(exp2, root, env) else evalBool(exp3, root, env)
    case Expression.Let(ident, exp1, exp2, _, _) =>
      // TODO: Right now Let only supports a single binding. Does it make sense to allow a list of bindings?
      val newEnv = env + (ident.name -> eval(exp1, root, env))
      evalBool(exp2, root, newEnv)
    case m: Expression.Match =>
      val value = eval(m.exp, root, env)
      val newEnv = env.clone()
      val result = matchRule(m.rulesAsArray, value, newEnv)
      if (result != null)
        evalBool(result, root, newEnv)
      else
        throw new RuntimeException(s"Unmatched value $value.")
    case Expression.NativeField(field, _, _) =>
      field.get().asInstanceOf[java.lang.Boolean].booleanValue()
    case Expression.Lambda(_, _, _, _) | Expression.Tag(_, _, _, _, _) | Expression.Tuple(_, _, _) |
         Expression.Set(_, _, _) | Expression.NativeMethod(_, _, _) =>
      throw new InternalRuntimeError(s"Expression $expr has type ${expr.tpe} instead of Type.Bool.")
    case Expression.Error(tpe, loc) => throw new RuntimeException(s"Error at ${loc.format}.")
    }

  private def evalBoolUnary(op: UnaryOperator, e: Expression, root: Root, env: Env): Boolean = op match {
    case UnaryOperator.Not => !evalBool(e, root, env)
    case UnaryOperator.Set.IsEmpty => eval(e, root, env).toSet.isEmpty
    case UnaryOperator.Set.NonEmpty => eval(e, root, env).toSet.nonEmpty
    case UnaryOperator.Set.Singleton => eval(e, root, env).toSet.size == 1
    case UnaryOperator.UnaryPlus | UnaryOperator.UnaryMinus | UnaryOperator.Set.Size =>
      throw new InternalRuntimeError(s"Type of unary expression is not Type.Bool.")
  }

  private def evalBoolBinary(op: BinaryOperator, e1: Expression, e2: Expression, root: Root, env: Env): Boolean = op match {
    case BinaryOperator.Less => evalInt(e1, root, env) < evalInt(e2, root, env)
    case BinaryOperator.LessEqual => evalInt(e1, root, env) <= evalInt(e2, root, env)
    case BinaryOperator.Greater => evalInt(e1, root, env) > evalInt(e2, root, env)
    case BinaryOperator.GreaterEqual => evalInt(e1, root, env) >= evalInt(e2, root, env)
    case BinaryOperator.Equal => eval(e1, root, env) == eval(e2, root, env)
    case BinaryOperator.NotEqual => eval(e1, root, env) != eval(e2, root, env)
    case BinaryOperator.And => evalBool(e1, root, env) && evalBool(e2, root, env)
    case BinaryOperator.Or => evalBool(e1, root, env) || evalBool(e2, root, env)
    case BinaryOperator.Set.Member => eval(e2, root, env).toSet contains eval(e1, root, env)
    case BinaryOperator.Set.SubsetOf => eval(e1, root, env).toSet subsetOf eval(e2, root, env).toSet
    case BinaryOperator.Set.ProperSubsetOf =>
      val s1 = eval(e1, root, env).toSet
      val s2 = eval(e2, root, env).toSet
      s1.subsetOf(s2) && s1.size < s2.size
    case BinaryOperator.Plus | BinaryOperator.Minus | BinaryOperator.Times | BinaryOperator.Divide |
         BinaryOperator.Modulo | BinaryOperator.Set.Insert | BinaryOperator.Set.Remove | BinaryOperator.Set.Union |
         BinaryOperator.Set.Intersection | BinaryOperator.Set.Difference =>
      throw new InternalRuntimeError(s"Type of binary expression is not Type.Bool.")
  }

  /*
   * A general evaluator of `Expression`s.
   *
   * Subexpressions are always evaluated by calling `eval`, which will call the
   * specialized eval whenever possible.
   */
  def evalGeneral(expr: Expression, root: Root, env: Env = mutable.Map.empty): Value = expr match {
    case Expression.Lit(literal, _, _) => evalLit(literal)
    case Expression.Var(ident, _, loc) => env(ident.name)
    case Expression.Ref(name, _, _) => eval(root.constants(name).exp, root, env)
    case exp: Expression.Lambda =>
      val formals = new Array[String](exp.argsAsArray.length)
      var i = 0
      while (i < formals.length) {
        formals(i) = exp.argsAsArray(i).ident.name
        i = i + 1
      }
      Value.Closure(formals, exp.body, env.clone())
    case apply: Expression.Apply =>
      val evalArgs = new Array[Value](apply.argsAsArray.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(apply.argsAsArray(i), root, env)
        i = i + 1
      }
      evalCall(apply.exp, evalArgs, root, env)
    case Expression.Unary(op, exp, _, _) => evalGeneralUnary(op, exp, root, env)
    case Expression.Binary(op, exp1, exp2, _, _) => evalBinary(op, exp1, exp2, root, env)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = evalBool(exp1, root, env)
      if (cond) eval(exp2, root, env) else eval(exp3, root, env)
    case Expression.Let(ident, exp1, exp2, _, _) =>
      // TODO: Right now Let only supports a single binding. Does it make sense to allow a list of bindings?
      val newEnv = env + (ident.name -> eval(exp1, root, env))
      eval(exp2, root, newEnv)
    case m: Expression.Match =>
      val value = eval(m.exp, root, env)
      val newEnv = env.clone()
      val result = matchRule(m.rulesAsArray, value, newEnv)
      if (result != null)
        eval(result, root, newEnv)
      else
        throw new RuntimeException(s"Unmatched value $value.")
    case Expression.NativeField(field, tpe, _) => Value.java2flix(field.get(), tpe)
    case Expression.NativeMethod(method, _, _) => Value.NativeMethod(method)
    case Expression.Tag(name, ident, exp, _, _) => Value.mkTag(name, ident.name, eval(exp, root, env))
    case exp: Expression.Tuple =>
      val elms = new Array[Value](exp.asArray.length)
      var i = 0
      while (i < elms.length) {
        elms(i) = eval(exp.asArray(i), root, env)
        i = i + 1
      }
      Value.Tuple(elms)
    case Expression.Set(elms, _, _) => Value.Set(elms.map(e => eval(e, root, env)).toSet)
    case Expression.Error(tpe, loc) => throw new RuntimeException(s"Error at ${loc.format}.")
  }

  private def evalGeneralUnary(op: UnaryOperator, e: Expression, root: Root, env: Env): Value = {
    val v = eval(e, root, env)
    op match {
      case UnaryOperator.Not => if (v.toBool) Value.False else Value.True
      case UnaryOperator.UnaryPlus => Value.mkInt(+v.toInt)
      case UnaryOperator.UnaryMinus => Value.mkInt(-v.toInt)
      case UnaryOperator.Set.IsEmpty => if (v.toSet.isEmpty) Value.True else Value.False
      case UnaryOperator.Set.NonEmpty => if (v.toSet.nonEmpty) Value.True else Value.False
      case UnaryOperator.Set.Singleton => if (v.toSet.size == 1) Value.True else Value.False
      case UnaryOperator.Set.Size => Value.mkInt(v.toSet.size)
    }
  }

  private def evalBinary(op: BinaryOperator, e1: Expression, e2: Expression, root: Root, env: Env): Value = {
    val v1 = eval(e1, root, env)
    val v2 = eval(e2, root, env)
    op match {
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
      case BinaryOperator.Set.Member => if (v2.toSet.contains(v1)) Value.True else Value.False
      case BinaryOperator.Set.SubsetOf => if (v1.toSet.subsetOf(v2.toSet)) Value.True else Value.False
      case BinaryOperator.Set.ProperSubsetOf =>
        if (v1.toSet.subsetOf(v2.toSet) && v1.toSet.size < v2.toSet.size) Value.True else Value.False
      case BinaryOperator.Set.Insert => Value.Set(v1.toSet + v2)
      case BinaryOperator.Set.Remove => Value.Set(v1.toSet - v2)
      case BinaryOperator.Set.Union => Value.Set(v1.toSet | v2.toSet)
      case BinaryOperator.Set.Intersection => Value.Set(v1.toSet & v2.toSet)
      case BinaryOperator.Set.Difference => Value.Set(v1.toSet &~ v2.toSet)
    }
  }

  def evalLit(lit: Literal): Value = lit match {
    case Literal.Unit(_) => Value.Unit
    case Literal.Bool(b, _) => if (b) Value.True else Value.False
    case Literal.Int(i, _) => Value.mkInt(i)
    case Literal.Str(s, _) => Value.mkStr(s)
    case Literal.Tag(name, ident, innerLit, _, _) => Value.mkTag(name, ident.name, evalLit(innerLit))
    case tpl: Literal.Tuple =>
      val lits = new Array[Value](tpl.asArray.length)
      var i = 0
      while (i < lits.length) {
        lits(i) = evalLit(tpl.asArray(i))
        i = i + 1
      }
      Value.Tuple(lits)
    case Literal.Set(elms, _, _) => Value.Set(elms.map(evalLit).toSet)
  }

  private def matchRule(rules: Array[(Pattern, Expression)], value: Value, env: Env): Expression = {
    var i = 0
    while (i < rules.length) {
      val rule = rules(i)
      if (canUnify(rule._1, value)) {
        unify(rule._1, value, env)
        return rule._2
      }
      i = i + 1
    }
    null
  }

  // Assuming `pattern` can unify with `value`, updates the given `env`.
  // Bad things will happen if `pattern` does not unify with `value`.
  private def unify(pattern: Pattern, value: Value, env: Env): Unit = pattern match {
    case Pattern.Var(ident, _, _) => env.update(ident.name, value)
    case Pattern.Tag(name, ident, innerPat, _, _) => unify(innerPat, value.asInstanceOf[Value.Tag].value, env)
    case p: Pattern.Tuple =>
      val elms = value.asInstanceOf[Value.Tuple].elms
      var i = 0
      while (i < p.asArray.length) {
        unify(p.asArray(i), elms(i), env)
        i = i + 1
      }
    case Pattern.Wildcard(_, _) | Pattern.Lit(_, _, _) => Unit
  }

  // Returns `true` if `pattern` can unify with `value`. Returns `false` otherwise.
  private def canUnify(pattern: Pattern, value: Value): Boolean = pattern match {
    case Pattern.Wildcard(_, _) |
         Pattern.Var(_, _, _) => true
    case Pattern.Lit(lit, _, _) => evalLit(lit) == value
    case Pattern.Tag(name, ident, innerPat, _, _) => value match {
      case v: Value.Tag if name == v.enum && ident.name == v.tag => canUnify(innerPat, v.value)
      case _ => false
    }
    case p: Pattern.Tuple => value match {
      case Value.Tuple(elms) =>
        var i = 0
        while (i < p.asArray.length) {
          val result = canUnify(p.asArray(i), elms(i))
          if (!result) return false
          i = i + 1
        }
        true
      case _ => false
    }
  }

  // TODO: Need to come up with some more clean interfaces
  // TODO: Everything below here is really bad and should just be replaced at will.

  /**
   * Evaluates the given head term `t` under the given environment `env0`
   */
  def evalHeadTerm(t: Term.Head, root: Root, env: mutable.Map[String, Value]): Value = t match {
    case Term.Head.Var(x, _, _) => env(x.name)
    case Term.Head.Lit(lit, _, _) => evalLit(lit)
    case term: Term.Head.Apply =>
      val function = root.constants(term.name).exp
      val evalArgs = new Array[Value](term.argsAsArray.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = evalHeadTerm(term.argsAsArray(i), root, env)
        i = i + 1
      }
      evalCall(function, evalArgs, root, env)
    case Term.Head.NativeField(field, tpe, _) => Value.java2flix(field.get(), tpe)
  }

  def evalBodyTerm(t: Term.Body, env: Env): Value = t match {
    case Term.Body.Wildcard(_, _) => ???
    case Term.Body.Var(x, _, _) => env(x.name)
    case Term.Body.Lit(lit, _, _) => evalLit(lit)
  }

  def evalCall(function: Expression, args: Array[Value], root: Root, env: Env = mutable.Map.empty): Value =
    (evalGeneral(function, root, env): @unchecked) match {
      case Value.Closure(formals, body, closureEnv) =>
        var i = 0
        while (i < formals.length) {
          closureEnv.update(formals(i), args(i))
          i = i + 1
        }
        eval(body, root, closureEnv)
      case Value.NativeMethod(method) =>
        val nativeArgs = new Array[java.lang.Object](args.length)
        var i = 0
        while (i < args.length) {
          nativeArgs(i) = args(i).toJava
          i = i + 1
        }
        val tpe = function.tpe.asInstanceOf[Type.Lambda].retTpe
        Value.java2flix(method.invoke(null, nativeArgs: _*), tpe)
    }

  def eval2(closure: Value.Closure, arg1: Value, arg2: Value, root: Root): Value = {
    val env = closure.env
    env.update(closure.formals(0), arg1)
    env.update(closure.formals(1), arg2)
    eval(closure.body, root, env)
  }

}
