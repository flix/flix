package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{IValue, WrappedValue}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Literal, Pattern, Term, Root}
import ca.uwaterloo.flix.language.ast.{Ast, Type, BinaryOperator, UnaryOperator}

import scala.annotation.tailrec
import scala.collection.mutable

object Interpreter {

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
  def eval(expr: Expression, root: Root, env: mutable.Map[String, AnyRef] = mutable.Map.empty): AnyRef = expr.tpe match {
    case Type.Int => Value.mkInt32(evalInt(expr, root, env))
    case Type.Bool => if (evalBool(expr, root, env)) Value.True else Value.False
    case Type.Str => evalGeneral(expr, root, env)
    case Type.Var(_) | Type.Unit | Type.Tag(_, _, _) | Type.Enum(_) | Type.Tuple(_) |
         Type.Set(_) | Type.Lambda(_, _) | Type.Predicate(_) | Type.Native(_) | Type.Any =>
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
  private def evalInt(expr: Expression, root: Root, env: mutable.Map[String, AnyRef] = mutable.Map.empty): Int = expr match {
    case Expression.Lit(literal, _, _) => Value.cast2int32(evalLit(literal))
    case Expression.Var(ident, _, loc) => Value.cast2int32(env(ident.name))
    case Expression.Ref(name, _, _) => evalInt(root.constants(name).exp, root, env)
    case apply: Expression.Apply =>
      val evalArgs = new Array[AnyRef](apply.argsAsArray.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(apply.argsAsArray(i), root, env)
        i = i + 1
      }
      Value.cast2int32(evalCall(apply.exp, evalArgs, root, env))
    case Expression.Unary(op, exp, _, _) => evalIntUnary(op, exp, root, env)
    case Expression.Binary(op, exp1, exp2, _, _) => evalIntBinary(op, exp1, exp2, root, env)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = evalBool(exp1, root, env)
      if (cond) evalInt(exp2, root, env) else evalInt(exp3, root, env)
    case Expression.Switch(rules, _, _) => evalIntSwitch(rules, root, env)
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
    case Expression.Lambda(_, _, _, _, _) | Expression.Tag(_, _, _, _, _) | Expression.Tuple(_, _, _) |
         Expression.Set(_, _, _) | Expression.Hook(_, _, _) =>
      throw new InternalRuntimeError(s"Expression $expr has type ${expr.tpe} instead of Type.Int.")
    case Expression.Error(tpe, loc) => throw new RuntimeException(s"Error at ${loc.format}.")
  }

  private def evalIntUnary(op: UnaryOperator, e: Expression, root: Root, env: mutable.Map[String, AnyRef]): Int = op match {
    case UnaryOperator.Plus => +evalInt(e, root, env)
    case UnaryOperator.Minus => -evalInt(e, root, env)
    case UnaryOperator.BitwiseNegate => ~evalInt(e, root, env)
    case UnaryOperator.LogicalNot =>
      throw new InternalRuntimeError(s"Type of unary expression is not Type.Int.")
  }

  // TODO: Document semantics of modulo on negative operands
  private def evalIntBinary(op: BinaryOperator, e1: Expression, e2: Expression, root: Root, env: mutable.Map[String, AnyRef]): Int = op match {
    case BinaryOperator.Plus => evalInt(e1, root, env) + evalInt(e2, root, env)
    case BinaryOperator.Minus => evalInt(e1, root, env) - evalInt(e2, root, env)
    case BinaryOperator.Times => evalInt(e1, root, env) * evalInt(e2, root, env)
    case BinaryOperator.Divide => evalInt(e1, root, env) / evalInt(e2, root, env)
    case BinaryOperator.Modulo => evalInt(e1, root, env) % evalInt(e2, root, env)
    case BinaryOperator.BitwiseAnd => evalInt(e1, root, env) & evalInt(e2, root, env)
    case BinaryOperator.BitwiseOr => evalInt(e1, root, env) | evalInt(e2, root, env)
    case BinaryOperator.BitwiseXor => evalInt(e1, root, env) ^ evalInt(e2, root, env)
    case BinaryOperator.BitwiseLeftShift => evalInt(e1, root, env) << evalInt(e2, root, env)
    case BinaryOperator.BitwiseRightShift => evalInt(e1, root, env) >> evalInt(e2, root, env)
    case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual |
         BinaryOperator.Equal | BinaryOperator.NotEqual | BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr =>
      throw new InternalRuntimeError(s"Type of binary expression is not Type.Int.")
  }

  @tailrec private def evalIntSwitch(rules: List[(Expression, Expression)], root: Root, env: mutable.Map[String, AnyRef]): Int = {
    val ((test, exp) :: rest) = rules
    val cond = evalBool(test, root, env)
    if (cond) evalInt(exp, root, env) else evalIntSwitch(rest, root, env)
  }

  /*
   * Evaluates expressions of type `Type.Bool`, returning an unwrapped
   * `scala.Boolean`. Performs casting as necessary.
   *
   * Subexpressions are evaluated by calling specialized eval whenever
   * possible.
   */
  @tailrec private def evalBool(expr: Expression, root: Root, env: mutable.Map[String, AnyRef] = mutable.Map.empty): Boolean = expr match {
    case Expression.Lit(literal, _, _) => Value.cast2bool(evalLit(literal))
    case Expression.Var(ident, _, loc) => Value.cast2bool(env(ident.name))
    case Expression.Ref(name, _, _) => evalBool(root.constants(name).exp, root, env)
    case apply: Expression.Apply =>
      val evalArgs = new Array[AnyRef](apply.argsAsArray.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(apply.argsAsArray(i), root, env)
        i = i + 1
      }
      Value.cast2bool(evalCall(apply.exp, evalArgs, root, env))
    case Expression.Unary(op, exp, _, _) => evalBoolUnary(op, exp, root, env)
    case Expression.Binary(op, exp1, exp2, _, _) => evalBoolBinary(op, exp1, exp2, root, env)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = Value.cast2bool(eval(exp1, root, env))
      if (cond) evalBool(exp2, root, env) else evalBool(exp3, root, env)
    case Expression.Switch(rules, _, _) => evalBoolSwitch(rules, root, env)
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
    case Expression.Lambda(_, _, _, _, _) | Expression.Tag(_, _, _, _, _) | Expression.Tuple(_, _, _) |
         Expression.Set(_, _, _) | Expression.Hook(_, _, _) =>
      throw new InternalRuntimeError(s"Expression $expr has type ${expr.tpe} instead of Type.Bool.")
    case Expression.Error(tpe, loc) => throw new RuntimeException(s"Error at ${loc.format}.")
  }

  private def evalBoolUnary(op: UnaryOperator, e: Expression, root: Root, env: mutable.Map[String, AnyRef]): Boolean = op match {
    case UnaryOperator.LogicalNot => !evalBool(e, root, env)
    case UnaryOperator.Plus | UnaryOperator.Minus | UnaryOperator.BitwiseNegate =>
      throw new InternalRuntimeError(s"Type of unary expression is not Type.Bool.")
  }

  private def evalBoolBinary(op: BinaryOperator, e1: Expression, e2: Expression, root: Root, env: mutable.Map[String, AnyRef]): Boolean = op match {
    case BinaryOperator.Less => evalInt(e1, root, env) < evalInt(e2, root, env)
    case BinaryOperator.LessEqual => evalInt(e1, root, env) <= evalInt(e2, root, env)
    case BinaryOperator.Greater => evalInt(e1, root, env) > evalInt(e2, root, env)
    case BinaryOperator.GreaterEqual => evalInt(e1, root, env) >= evalInt(e2, root, env)
    case BinaryOperator.Equal => eval(e1, root, env) == eval(e2, root, env)
    case BinaryOperator.NotEqual => eval(e1, root, env) != eval(e2, root, env)
    case BinaryOperator.LogicalAnd => evalBool(e1, root, env) && evalBool(e2, root, env)
    case BinaryOperator.LogicalOr => evalBool(e1, root, env) || evalBool(e2, root, env)
    case BinaryOperator.Plus | BinaryOperator.Minus | BinaryOperator.Times | BinaryOperator.Divide |
         BinaryOperator.Modulo | BinaryOperator.BitwiseAnd | BinaryOperator.BitwiseOr | BinaryOperator.BitwiseXor |
         BinaryOperator.BitwiseLeftShift | BinaryOperator.BitwiseRightShift =>
      throw new InternalRuntimeError(s"Type of binary expression is not Type.Bool.")
  }

  @tailrec private def evalBoolSwitch(rules: List[(Expression, Expression)], root: Root, env: mutable.Map[String, AnyRef]): Boolean = {
    val ((test, exp) :: rest) = rules
    val cond = evalBool(test, root, env)
    if (cond) evalBool(exp, root, env) else evalBoolSwitch(rest, root, env)
  }

  /*
   * A general evaluator of `Expression`s.
   *
   * Subexpressions are always evaluated by calling `eval`, which will call the
   * specialized eval whenever possible.
   */
  def evalGeneral(expr: Expression, root: Root, env: mutable.Map[String, AnyRef] = mutable.Map.empty): AnyRef = expr match {
    case Expression.Lit(literal, _, _) => evalLit(literal)
    case Expression.Var(ident, _, loc) => env(ident.name)
    case Expression.Ref(name, _, _) => eval(root.constants(name).exp, root, env)
    case Expression.Hook(hook, _, _) => Value.HookClosure(hook)
    case exp: Expression.Lambda =>
      val formals = new Array[String](exp.argsAsArray.length)
      var i = 0
      while (i < formals.length) {
        formals(i) = exp.argsAsArray(i).ident.name
        i = i + 1
      }
      Value.Closure(formals, exp.body, env.clone())
    case apply: Expression.Apply =>
      val evalArgs = new Array[AnyRef](apply.argsAsArray.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(apply.argsAsArray(i), root, env)
        i = i + 1
      }
      evalCall(apply.exp, evalArgs, root, env)
    case Expression.Unary(op, exp, _, _) => evalGeneralUnary(op, exp, root, env)
    case Expression.Binary(op, exp1, exp2, _, _) => evalGeneralBinary(op, exp1, exp2, root, env)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = evalBool(exp1, root, env)
      if (cond) eval(exp2, root, env) else eval(exp3, root, env)
    case Expression.Switch(rules, _, _) => evalGeneralSwitch(rules, root, env)
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
    case Expression.Tag(name, ident, exp, _, _) => Value.mkTag(name, ident.name, eval(exp, root, env))
    case exp: Expression.Tuple =>
      val elms = new Array[AnyRef](exp.asArray.length)
      var i = 0
      while (i < elms.length) {
        elms(i) = eval(exp.asArray(i), root, env)
        i = i + 1
      }
      Value.Tuple(elms)
    case Expression.Set(elms, _, _) => Value.mkSet(elms.map(e => eval(e, root, env)).toSet)
    case Expression.Error(tpe, loc) => throw new RuntimeException(s"Error at ${loc.format}.")
  }

  private def evalGeneralUnary(op: UnaryOperator, e: Expression, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = {
    val v = eval(e, root, env)
    op match {
      case UnaryOperator.LogicalNot => if (Value.cast2bool(v)) Value.False else Value.True
      case UnaryOperator.Plus => Value.mkInt32(+Value.cast2int32(v))
      case UnaryOperator.Minus => Value.mkInt32(-Value.cast2int32(v))
      case UnaryOperator.BitwiseNegate => Value.mkInt32(~Value.cast2int32(v))
    }
  }

  private def evalGeneralBinary(op: BinaryOperator, e1: Expression, e2: Expression, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = {
    val v1 = eval(e1, root, env)
    val v2 = eval(e2, root, env)
    op match {
      case BinaryOperator.Plus => Value.mkInt32(Value.cast2int32(v1) + Value.cast2int32(v2))
      case BinaryOperator.Minus => Value.mkInt32(Value.cast2int32(v1) - Value.cast2int32(v2))
      case BinaryOperator.Times => Value.mkInt32(Value.cast2int32(v1) * Value.cast2int32(v2))
      case BinaryOperator.Divide => Value.mkInt32(Value.cast2int32(v1) / Value.cast2int32(v2))
      case BinaryOperator.Modulo => Value.mkInt32(Value.cast2int32(v1) % Value.cast2int32(v2))
      case BinaryOperator.Less => if (Value.cast2int32(v1) < Value.cast2int32(v2)) Value.True else Value.False
      case BinaryOperator.LessEqual => if (Value.cast2int32(v1) <= Value.cast2int32(v2)) Value.True else Value.False
      case BinaryOperator.Greater => if (Value.cast2int32(v1) > Value.cast2int32(v2)) Value.True else Value.False
      case BinaryOperator.GreaterEqual => if (Value.cast2int32(v1) >= Value.cast2int32(v2)) Value.True else Value.False
      case BinaryOperator.Equal => if (v1 == v2) Value.True else Value.False
      case BinaryOperator.NotEqual => if (v1 != v2) Value.True else Value.False
      case BinaryOperator.LogicalAnd => if (Value.cast2bool(v1) && Value.cast2bool(v2)) Value.True else Value.False
      case BinaryOperator.LogicalOr => if (Value.cast2bool(v1) || Value.cast2bool(v2)) Value.True else Value.False
      case BinaryOperator.BitwiseAnd => Value.mkInt32(Value.cast2int32(v1) & Value.cast2int32(v2))
      case BinaryOperator.BitwiseOr => Value.mkInt32(Value.cast2int32(v1) | Value.cast2int32(v2))
      case BinaryOperator.BitwiseXor => Value.mkInt32(Value.cast2int32(v1) ^ Value.cast2int32(v2))
      case BinaryOperator.BitwiseLeftShift => Value.mkInt32(Value.cast2int32(v1) << Value.cast2int32(v2))
      case BinaryOperator.BitwiseRightShift => Value.mkInt32(Value.cast2int32(v1) >> Value.cast2int32(v2))
    }
  }

  @tailrec private def evalGeneralSwitch(rules: List[(Expression, Expression)], root: Root, env: mutable.Map[String, AnyRef]): AnyRef = {
    val ((test, exp) :: rest) = rules
    val cond = evalBool(test, root, env)
    if (cond) eval(exp, root, env) else evalGeneralSwitch(rest, root, env)
  }

  def evalLit(lit: Literal): AnyRef = lit match {
    case Literal.Unit(_) => Value.Unit
    case Literal.Bool(b, _) => if (b) Value.True else Value.False
    case Literal.Int(i, _) => Value.mkInt32(i)
    case Literal.Str(s, _) => Value.mkStr(s)
    case Literal.Tag(name, ident, innerLit, _, _) => Value.mkTag(name, ident.name, evalLit(innerLit))
    case tpl: Literal.Tuple =>
      val lits = new Array[AnyRef](tpl.asArray.length)
      var i = 0
      while (i < lits.length) {
        lits(i) = evalLit(tpl.asArray(i))
        i = i + 1
      }
      Value.Tuple(lits)
    case Literal.Set(elms, _, _) => Value.mkSet(elms.map(evalLit).toSet)
  }

  private def matchRule(rules: Array[(Pattern, Expression)], value: AnyRef, env: mutable.Map[String, AnyRef]): Expression = {
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
  private def unify(pattern: Pattern, value: AnyRef, env: mutable.Map[String, AnyRef]): Unit = pattern match {
    case Pattern.Var(ident, _, _) => env.update(ident.name, value)
    case Pattern.Tag(_, _, innerPat, _, _) => unify(innerPat, Value.cast2tag(value).value, env)
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
  private def canUnify(pattern: Pattern, value: AnyRef): Boolean = pattern match {
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
    case Pattern.Wildcard(_, _) | Pattern.Var(_, _, _) => true
  }

  // TODO: Need to come up with some more clean interfaces
  // TODO: Everything below here is really bad and should just be replaced at will.

  /**
    * Evaluates the given head term `t` under the given environment `env0`
    */
  def evalHeadTerm(t: Term.Head, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = t match {
    case Term.Head.Var(x, _, _) => env(x.name)
    case Term.Head.Lit(lit, _, _) => evalLit(lit)
    case term: Term.Head.Apply =>
      val function = root.constants(term.name).exp
      val evalArgs = new Array[AnyRef](term.argsAsArray.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = evalHeadTerm(term.argsAsArray(i), root, env)
        i = i + 1
      }
      evalCall(function, evalArgs, root, env)
    case Term.Head.ApplyHook(hook, args, _, _) =>
      val evalArgs = args.map(a => evalHeadTerm(a, root, env))

      hook match {
        case Ast.Hook.Safe(name, inv, tpe) =>
          val wargs = evalArgs map {
            case arg => new WrappedValue(arg): IValue
          }
          inv(wargs.toArray).getUnsafeRef

        case Ast.Hook.Unsafe(name, inv, tpe) =>
          inv(evalArgs.toArray)
      }
  }

  def evalBodyTerm(t: Term.Body, env: mutable.Map[String, AnyRef]): AnyRef = t match {
    case Term.Body.Wildcard(_, _) => ???
    case Term.Body.Var(x, _, _) => env(x.name)
    case Term.Body.Lit(lit, _, _) => evalLit(lit)
  }

  def evalCall(function: Expression, args: Array[AnyRef], root: Root, env: mutable.Map[String, AnyRef] = mutable.Map.empty): AnyRef =
    (evalGeneral(function, root, env): @unchecked) match {
      case Value.Closure(formals, body, closureEnv) =>
        var i = 0
        while (i < formals.length) {
          closureEnv.update(formals(i), args(i))
          i = i + 1
        }
        eval(body, root, closureEnv)
      case Value.HookClosure(hook) => hook match {
        case Ast.Hook.Safe(name, inv, tpe) =>
          val wargs = args map {
            case arg => new WrappedValue(arg): IValue
          }
          inv(wargs).getUnsafeRef

        case Ast.Hook.Unsafe(name, inv, tpe) =>
          inv(args)
      }
    }

  def eval2(closure: AnyRef, arg1: AnyRef, arg2: AnyRef, root: Root): AnyRef = {
    val clo = Value.cast2closure(closure)
    val env = clo.env
    env.update(clo.formals(0), arg1)
    env.update(clo.formals(1), arg2)
    eval(closure.asInstanceOf[Value.Closure].body, root, env)
  }

}
