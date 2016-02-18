package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{IValue, WrappedValue}
import ca.uwaterloo.flix.language.ast.ExecutableAst._
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
    case Type.Bool => if (evalBool(expr, root, env)) Value.True else Value.False
    case Type.Int32 => Value.mkInt32(evalInt(expr, root, env))
    case Type.Str => evalGeneral(expr, root, env)
    case Type.Var(_) | Type.Unit | Type.Char | Type.Int8 | Type.Int16 | Type.Int64 | Type.Str | Type.Native |
         Type.Proposition | Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) |
         Type.Lambda(_, _) | Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) |
         Type.Predicate(_) | Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
      evalGeneral(expr, root, env)
  }

  /*
   * Evaluates expressions of type `Type.Int32`, returning an unwrapped
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
  @tailrec
  private def evalInt(expr: Expression, root: Root, env: mutable.Map[String, AnyRef] = mutable.Map.empty): Int = expr match {
    case Expression.Int32(lit) => lit
    case load: Expression.LoadInt32 =>
      val bitvec = Value.cast2int64(evalGeneral(load.e, root, env))
      val shifted = bitvec >> load.offset
      (shifted & load.mask).toInt
    case Expression.Var(ident, _, _, loc) => Value.cast2int32(env(ident.name))
    case Expression.Ref(name, _, _) => evalInt(root.constants(name).exp, root, env)
    case Expression.Apply(name, args, _, _) => ???
    case Expression.Apply3(exp, args, _, _) =>
      val evalArgs = new Array[AnyRef](args.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(args(i), root, env)
        i = i + 1
      }
      Value.cast2int32(evalCall(exp, evalArgs, root, env))
    case Expression.Unary(op, exp, _, _) => evalIntUnary(op, exp, root, env)
    case Expression.Binary(op, exp1, exp2, _, _) => evalIntBinary(op, exp1, exp2, root, env)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = evalBool(exp1, root, env)
      if (cond) evalInt(exp2, root, env) else evalInt(exp3, root, env)
    case Expression.Let(ident, _, exp1, exp2, _, _) =>
      val newEnv = env + (ident.name -> eval(exp1, root, env))
      evalInt(exp2, root, newEnv)
    case Expression.GetTagValue(exp, _, _) =>
      val tag = Value.cast2tag(evalGeneral(exp, root, env))
      Value.cast2int32(tag.value)
    case Expression.GetTupleIndex(base, offset, _, _) =>
      val tuple = Value.cast2tuple(evalGeneral(base, root, env))
      Value.cast2int32(tuple(offset))
    case Expression.Unit | Expression.True | Expression.False | Expression.Int8(_) | Expression.Int16(_) |
         Expression.Int64(_) | Expression.Str(_) | Expression.LoadBool(_, _) | Expression.LoadInt8(_, _) |
         Expression.LoadInt16(_, _) | Expression.LoadInt32(_, _) | Expression.StoreBool(_, _, _) |
         Expression.StoreInt8(_, _, _) | Expression.StoreInt16(_, _, _) | Expression.StoreInt32(_, _, _) |
         Expression.Lambda(_, _, _, _, _) | Expression.Hook(_, _, _) | Expression.Closure(_, _, _, _, _) |
         Expression.CheckTag(_, _, _) | Expression.Tag(_, _, _, _, _) | Expression.Tuple(_, _, _) |
         Expression.CheckNil(_, _) | Expression.CheckCons(_, _) | Expression.Set(_, _, _) =>
      throw new InternalRuntimeError(s"Expression $expr has type ${expr.tpe} instead of Type.Int.")
    case Expression.Error(tpe, loc) => throw new RuntimeException(s"Runtime error at ${loc.format}.")
    case Expression.MatchError(tpe, loc) => throw new RuntimeException(s"Match error at ${loc.format}.")
    case Expression.SwitchError(tpe, loc) => throw new RuntimeException(s"Switch error at ${loc.format}.")
  }

  private def evalIntUnary(op: UnaryOperator, e: Expression, root: Root, env: mutable.Map[String, AnyRef]): Int = op match {
    case UnaryOperator.Plus => +evalInt(e, root, env)
    case UnaryOperator.Minus => -evalInt(e, root, env)
    case UnaryOperator.BitwiseNegate => ~evalInt(e, root, env)
    case UnaryOperator.LogicalNot =>
      throw new InternalRuntimeError(s"Type of unary expression is not Type.Int.")
  }

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
         BinaryOperator.Equal | BinaryOperator.NotEqual | BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr |
         BinaryOperator.Implication | BinaryOperator.Biconditional =>
      throw new InternalRuntimeError(s"Type of binary expression is not Type.Int.")
  }

  /*
   * Evaluates expressions of type `Type.Bool`, returning an unwrapped
   * `scala.Boolean`. Performs casting as necessary.
   *
   * Subexpressions are evaluated by calling specialized eval whenever
   * possible.
   */
  @tailrec
  private def evalBool(expr: Expression, root: Root, env: mutable.Map[String, AnyRef] = mutable.Map.empty): Boolean = expr match {
    case Expression.True => true
    case Expression.False => false
    case load: Expression.LoadBool =>
      val bitvec = Value.cast2int64(evalGeneral(load.e, root, env))
      val shifted = bitvec >> load.offset
      (shifted & load.mask) != 0
    case Expression.Var(ident, _, _, loc) => Value.cast2bool(env(ident.name))
    case Expression.Ref(name, _, _) => evalBool(root.constants(name).exp, root, env)
    case Expression.Apply(name, args, _, _) => ???
    case Expression.Apply3(exp, args, _, _) =>
      val evalArgs = new Array[AnyRef](args.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(args(i), root, env)
        i = i + 1
      }
      Value.cast2bool(evalCall(exp, evalArgs, root, env))
    case Expression.Unary(op, exp, _, _) => evalBoolUnary(op, exp, root, env)
    case Expression.Binary(op, exp1, exp2, _, _) => evalBoolBinary(op, exp1, exp2, root, env)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = Value.cast2bool(eval(exp1, root, env))
      if (cond) evalBool(exp2, root, env) else evalBool(exp3, root, env)
    case Expression.Let(ident, _, exp1, exp2, _, _) =>
      val newEnv = env + (ident.name -> eval(exp1, root, env))
      evalBool(exp2, root, newEnv)
    case Expression.CheckTag(tag, exp, _) => Value.cast2tag(evalGeneral(exp, root, env)).tag == tag.name
    case Expression.GetTagValue(exp, _, _) =>
      val tag = Value.cast2tag(evalGeneral(exp, root, env))
      Value.cast2bool(tag.value)
    case Expression.GetTupleIndex(base, offset, _, _) =>
      val tuple = Value.cast2tuple(evalGeneral(base, root, env))
      Value.cast2bool(tuple(offset))
    case Expression.CheckNil(exp, _) => ???
    case Expression.CheckCons(exp, _) => ???
    case Expression.Unit | Expression.Int8(_) | Expression.Int16(_) | Expression.Int32(_) | Expression.Int64(_) |
         Expression.Str(_) | Expression.LoadInt8(_, _) | Expression.LoadInt16(_, _) | Expression.LoadInt32(_, _) |
         Expression.StoreBool(_, _, _) | Expression.StoreInt8(_, _, _) | Expression.StoreInt16(_, _, _) |
         Expression.StoreInt32(_, _, _) | Expression.Lambda(_, _, _, _, _) | Expression.Hook(_, _, _) |
         Expression.Closure(_, _, _, _, _) | Expression.Tag(_, _, _, _, _) | Expression.Tuple(_, _, _) |
         Expression.Set(_, _, _) =>
      throw new InternalRuntimeError(s"Expression $expr has type ${expr.tpe} instead of Type.Bool.")
    case Expression.Error(tpe, loc) => throw new RuntimeException(s"Runtime error at ${loc.format}.")
    case Expression.MatchError(tpe, loc) => throw new RuntimeException(s"Match error at ${loc.format}.")
    case Expression.SwitchError(tpe, loc) => throw new RuntimeException(s"Switch error at ${loc.format}.")
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
    case BinaryOperator.Implication => !evalBool(e1, root, env) || evalBool(e2, root, env)
    case BinaryOperator.Biconditional => evalBool(e1, root, env) == evalBool(e2, root, env)
    case BinaryOperator.Plus | BinaryOperator.Minus | BinaryOperator.Times | BinaryOperator.Divide |
         BinaryOperator.Modulo | BinaryOperator.BitwiseAnd | BinaryOperator.BitwiseOr | BinaryOperator.BitwiseXor |
         BinaryOperator.BitwiseLeftShift | BinaryOperator.BitwiseRightShift =>
      throw new InternalRuntimeError(s"Type of binary expression is not Type.Bool.")
  }

  /*
   * A general evaluator of `Expression`s.
   *
   * Subexpressions are always evaluated by calling `eval`, which will call the
   * specialized eval whenever possible.
   */
  def evalGeneral(expr: Expression, root: Root, env: mutable.Map[String, AnyRef] = mutable.Map.empty): AnyRef = expr match {
    case Expression.Unit => Value.Unit
    case Expression.True => Value.True
    case Expression.False => Value.False
    case Expression.Int8(lit) => Value.mkInt8(lit)
    case Expression.Int16(lit) => Value.mkInt16(lit)
    case Expression.Int32(lit) => Value.mkInt32(lit)
    case Expression.Int64(lit) => Value.mkInt64(lit)
    case Expression.Str(lit) => Value.mkStr(lit)
    case load: LoadExpression =>
      val e = Value.cast2int64(evalGeneral(load.e, root, env))
      val result = (e >> load.offset).toInt & load.mask
      load match {
        case _: Expression.LoadBool => if (result != 0) Value.True else Value.False
        case _: Expression.LoadInt8 => Value.mkInt8(result)
        case _: Expression.LoadInt16 => Value.mkInt16(result)
        case _: Expression.LoadInt32 => Value.mkInt32(result)
      }
    case store: StoreExpression =>
      val e = Value.cast2int64(evalGeneral(store.e, root, env))
      val v = Value.cast2int64(evalGeneral(store.v, root, env))
      val result = (e & store.targetMask) | ((v & store.mask) << store.offset)
      Value.mkInt64(result)
    case Expression.Var(ident, _, _, loc) => env(ident.name)
    case Expression.Ref(name, _, _) => eval(root.constants(name).exp, root, env)
    case Expression.Lambda(annotations, args, body, _, _) =>
      val formals = new Array[String](args.length)
      var i = 0
      while (i < formals.length) {
        formals(i) = args(i).ident.name
        i = i + 1
      }
      Value.Closure(formals, body, env.clone())
    case Expression.Hook(hook, _, _) => Value.HookClosure(hook)
    case Expression.Closure(args, body, clEnv, _, _) => ???
    case Expression.Apply(name, args, _, _) => ???
    case Expression.Apply3(exp, args, _, _) =>
      val evalArgs = new Array[AnyRef](args.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(args(i), root, env)
        i = i + 1
      }
      evalCall(exp, evalArgs, root, env)
    case Expression.Unary(op, exp, _, _) => evalGeneralUnary(op, exp, root, env)
    case Expression.Binary(op, exp1, exp2, _, _) => evalGeneralBinary(op, exp1, exp2, root, env)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = evalBool(exp1, root, env)
      if (cond) eval(exp2, root, env) else eval(exp3, root, env)
    case Expression.Let(ident, _, exp1, exp2, _, _) =>
      val newEnv = env + (ident.name -> eval(exp1, root, env))
      eval(exp2, root, newEnv)
    case Expression.CheckTag(tag, exp, _) =>
      if (Value.cast2tag(evalGeneral(exp, root, env)).tag == tag.name) Value.True else Value.False
    case Expression.GetTagValue(exp, _, _) => Value.cast2tag(evalGeneral(exp, root, env)).value
    case Expression.Tag(name, ident, exp, _, _) => Value.mkTag(name, ident.name, eval(exp, root, env))
    case Expression.GetTupleIndex(base, offset, _, _) => Value.cast2tuple(evalGeneral(base, root, env))(offset)
    case Expression.Tuple(elms, _, _) =>
      val evalElms = new Array[AnyRef](elms.length)
      var i = 0
      while (i < evalElms.length) {
        evalElms(i) = eval(elms(i), root, env)
        i = i + 1
      }
      Value.Tuple(evalElms)
    case Expression.CheckNil(exp, _) => ???
    case Expression.CheckCons(exp, _) => ???
    case Expression.Set(elms, _, _) => Value.mkSet(elms.map(e => eval(e, root, env)).toSet)
    case Expression.Error(tpe, loc) => throw new RuntimeException(s"Runtime error at ${loc.format}.")
    case Expression.MatchError(tpe, loc) => throw new RuntimeException(s"Match error at ${loc.format}.")
    case Expression.SwitchError(tpe, loc) => throw new RuntimeException(s"Switch error at ${loc.format}.")
  }

  // TODO: Can this be cleaned up?
  private def evalGeneralUnary(op: UnaryOperator, e: Expression, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = {
    val v = eval(e, root, env)
    op match {
      case UnaryOperator.LogicalNot => if (Value.cast2bool(v)) Value.False else Value.True
      case UnaryOperator.Plus => v // nop
      case UnaryOperator.Minus => e.tpe match {
        case Type.Int8 => Value.mkInt8(-Value.cast2int8(v))
        case Type.Int16 => Value.mkInt16(-Value.cast2int16(v))
        case Type.Int32 => Value.mkInt32(-Value.cast2int32(v))
        case Type.Int64 => Value.mkInt64(-Value.cast2int64(v))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply UnaryOperator.$op to type ${e.tpe}.")
      }
      case UnaryOperator.BitwiseNegate => e.tpe match {
        case Type.Int8 => Value.mkInt8(~Value.cast2int8(v))
        case Type.Int16 => Value.mkInt16(~Value.cast2int16(v))
        case Type.Int32 => Value.mkInt32(~Value.cast2int32(v))
        case Type.Int64 => Value.mkInt64(~Value.cast2int64(v))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply UnaryOperator.$op to type ${e.tpe}.")
      }
    }
  }

  // TODO: Can this be cleaned up?
  private def evalGeneralBinary(op: BinaryOperator, e1: Expression, e2: Expression, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = {
    val v1 = eval(e1, root, env)
    val v2 = eval(e2, root, env)
    op match {
      case BinaryOperator.Plus => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) + Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) + Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) + Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) + Value.cast2int64(v2))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.Minus => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) - Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) - Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) - Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) - Value.cast2int64(v2))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.Times => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) * Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) * Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) * Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) * Value.cast2int64(v2))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.Divide => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) / Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) / Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) / Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) / Value.cast2int64(v2))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.Modulo => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) % Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) % Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) % Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) % Value.cast2int64(v2))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }

      case BinaryOperator.Less => e1.tpe match {
        case Type.Int8 => if (Value.cast2int8(v1) < Value.cast2int8(v2)) Value.True else Value.False
        case Type.Int16 => if (Value.cast2int16(v1) < Value.cast2int16(v2)) Value.True else Value.False
        case Type.Int32 => if (Value.cast2int32(v1) < Value.cast2int32(v2)) Value.True else Value.False
        case Type.Int64 => if (Value.cast2int64(v1) < Value.cast2int64(v2)) Value.True else Value.False
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.LessEqual => e1.tpe match {
        case Type.Int8 => if (Value.cast2int8(v1) <= Value.cast2int8(v2)) Value.True else Value.False
        case Type.Int16 => if (Value.cast2int16(v1) <= Value.cast2int16(v2)) Value.True else Value.False
        case Type.Int32 => if (Value.cast2int32(v1) <= Value.cast2int32(v2)) Value.True else Value.False
        case Type.Int64 => if (Value.cast2int64(v1) <= Value.cast2int64(v2)) Value.True else Value.False
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.Greater => e1.tpe match {
        case Type.Int8 => if (Value.cast2int8(v1) > Value.cast2int8(v2)) Value.True else Value.False
        case Type.Int16 => if (Value.cast2int16(v1) > Value.cast2int16(v2)) Value.True else Value.False
        case Type.Int32 => if (Value.cast2int32(v1) > Value.cast2int32(v2)) Value.True else Value.False
        case Type.Int64 => if (Value.cast2int64(v1) > Value.cast2int64(v2)) Value.True else Value.False
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.GreaterEqual => e1.tpe match {
        case Type.Int8 => if (Value.cast2int8(v1) >= Value.cast2int8(v2)) Value.True else Value.False
        case Type.Int16 => if (Value.cast2int16(v1) >= Value.cast2int16(v2)) Value.True else Value.False
        case Type.Int32 => if (Value.cast2int32(v1) >= Value.cast2int32(v2)) Value.True else Value.False
        case Type.Int64 => if (Value.cast2int64(v1) >= Value.cast2int64(v2)) Value.True else Value.False
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }

      case BinaryOperator.Equal => if (v1 == v2) Value.True else Value.False
      case BinaryOperator.NotEqual => if (v1 != v2) Value.True else Value.False

      case BinaryOperator.LogicalAnd => if (Value.cast2bool(v1) && Value.cast2bool(v2)) Value.True else Value.False
      case BinaryOperator.LogicalOr => if (Value.cast2bool(v1) || Value.cast2bool(v2)) Value.True else Value.False
      case BinaryOperator.Implication => if (!Value.cast2bool(v1) || Value.cast2bool(v2)) Value.True else Value.False
      case BinaryOperator.Biconditional => if (Value.cast2bool(v1) == Value.cast2bool(v2)) Value.True else Value.False

      case BinaryOperator.BitwiseAnd => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) & Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) & Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) & Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) & Value.cast2int64(v2))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseOr => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) | Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) | Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) | Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) | Value.cast2int64(v2))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseXor => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) ^ Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) ^ Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) ^ Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) ^ Value.cast2int64(v2))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseLeftShift => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) << Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) << Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) << Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) << Value.cast2int64(v2))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseRightShift => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) >> Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) >> Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) >> Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) >> Value.cast2int64(v2))
        case Type.Var(_) | Type.Unit | Type.Bool | Type.Char | Type.Str | Type.Native | Type.Proposition |
             Type.Tag(_, _, _) | Type.UnresolvedTag(_, _, _) | Type.Enum(_, _) | Type.Tuple(_) | Type.Lambda(_, _) |
             Type.Parametric(_, _) | Type.Opt(_) | Type.Lst(_) | Type.Set(_) | Type.Map(_, _) | Type.Predicate(_) |
             Type.Unresolved(_) | Type.Abs(_, _) | Type.Any =>
          throw new InternalRuntimeError(s"Can't apply BinaryOperator.$op to type ${e1.tpe}.")
      }
    }
  }

  // TODO: Need to come up with some more clean interfaces
  // TODO: Everything below here is really bad and should just be replaced at will.

  /**
    * Evaluates the given head term `t` under the given environment `env0`
    */
  def evalHeadTerm(t: Term.Head, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = t match {
    case Term.Head.Var(x, _, _) => env(x.name)
    case Term.Head.Exp(e, _, _) => eval(e, root, env)
    case Term.Head.Apply(name, args, _, _) =>
      val function = root.constants(name).exp
      val evalArgs = new Array[AnyRef](args.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = evalHeadTerm(args(i), root, env)
        i = i + 1
      }
      evalCall(function, evalArgs, root, env)
    case Term.Head.ApplyHook(hook, args, _, _) =>
      val evalArgs = new Array[AnyRef](args.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = evalHeadTerm(args(i), root, env)
        i = i + 1
      }
      hook match {
        case Ast.Hook.Safe(name, inv, _) =>
          val wargs = evalArgs map {
            case arg => new WrappedValue(arg): IValue
          }
          inv(wargs).getUnsafeRef
        case Ast.Hook.Unsafe(name, inv, _) =>
          inv(evalArgs)
      }
  }

  def evalBodyTerm(t: Term.Body, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = t match {
    case Term.Body.Wildcard(_, _) => ???
    case Term.Body.Var(x, _, _, _) => env(x.name)
    case Term.Body.Exp(e, _, _) => eval(e, root, env)
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
        case Ast.Hook.Safe(name, inv, _) =>
          val wargs = args map {
            case arg => new WrappedValue(arg): IValue
          }
          inv(wargs).getUnsafeRef
        case Ast.Hook.Unsafe(name, inv, _) =>
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
