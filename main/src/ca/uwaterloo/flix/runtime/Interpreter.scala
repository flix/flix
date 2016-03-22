package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{IValue, MatchException, SwitchException, UserException, WrappedValue}
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalRuntimeException

import scala.collection.mutable

object Interpreter {

  /*
   * Evaluates an `Expression`. Assumes all input has been type-checked.
   */
  def eval(expr: Expression, root: Root, env: mutable.Map[String, AnyRef] = mutable.Map.empty): AnyRef = expr match {
    case Expression.Unit => Value.Unit
    case Expression.True => Value.True
    case Expression.False => Value.False
    case Expression.Char(lit) => Value.mkChar(lit)
    case Expression.Float32(lit) => Value.mkFloat32(lit)
    case Expression.Float64(lit) => Value.mkFloat64(lit)
    case Expression.Int8(lit) => Value.mkInt8(lit)
    case Expression.Int16(lit) => Value.mkInt16(lit)
    case Expression.Int32(lit) => Value.mkInt32(lit)
    case Expression.Int64(lit) => Value.mkInt64(lit)
    case Expression.Str(lit) => Value.mkStr(lit)
    case load: LoadExpression =>
      val e = Value.cast2int64(eval(load.e, root, env))
      val result = (e >> load.offset).toInt & load.mask
      load match {
        case _: Expression.LoadBool => Value.mkBool(result != 0)
        case _: Expression.LoadInt8 => Value.mkInt8(result)
        case _: Expression.LoadInt16 => Value.mkInt16(result)
        case _: Expression.LoadInt32 => Value.mkInt32(result)
      }
    case store: StoreExpression =>
      val e = Value.cast2int64(eval(store.e, root, env))
      val v = Value.cast2int64(eval(store.v, root, env))
      val result = (e & store.targetMask) | ((v & store.mask) << store.offset)
      Value.mkInt64(result)
    case Expression.Var(ident, _, _, loc) => env.get(ident.name) match {
      case None => throw InternalRuntimeException(s"Key '${ident.name}' not found in environment: '${env.mkString(",")}'.")
      case Some(v) => v
    }
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
    case Expression.Apply3(exp, args, _, _) =>
      val evalArgs = new Array[AnyRef](args.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(args(i), root, env)
        i = i + 1
      }
      evalCall(exp, evalArgs, root, env)
    case Expression.ApplyClosure(exp, args, tpe, loc) =>
      val evalArgs = new Array[AnyRef](args.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = eval(args(i), root, env)
        i = i + 1
      }
      evalCall(exp, evalArgs, root, env)

    case Expression.Unary(op, exp, _, _) => evalUnary(op, exp, root, env)
    case Expression.Binary(op, exp1, exp2, _, _) => op match {
      case o: ArithmeticOperator => evalArithmetic(o, exp1, exp2, root, env)
      case o: ComparisonOperator => evalComparison(o, exp1, exp2, root, env)
      case o: LogicalOperator => evalLogical(o, exp1, exp2, root, env)
      case o: BitwiseOperator => evalBitwise(o, exp1, exp2, root, env)
    }
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = Value.cast2bool(eval(exp1, root, env))
      if (cond) eval(exp2, root, env) else eval(exp3, root, env)
    case Expression.Let(ident, _, exp1, exp2, _, _) =>
      val newEnv = env + (ident.name -> eval(exp1, root, env))
      eval(exp2, root, newEnv)
    case Expression.CheckTag(tag, exp, _) => Value.mkBool(Value.cast2tag(eval(exp, root, env)).tag == tag.name)
    case Expression.GetTagValue(tag, exp, _, _) => Value.cast2tag(eval(exp, root, env)).value
    case Expression.Tag(name, ident, exp, _, _) => Value.mkTag(name, ident.name, eval(exp, root, env))
    case Expression.GetTupleIndex(base, offset, _, _) => Value.cast2tuple(eval(base, root, env))(offset)
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
    case Expression.FSet(elms, _, _) => Value.mkSet(elms.map(e => eval(e, root, env)).toSet)
    case Expression.Error(_, loc) => throw UserException("User exception.", loc)
    case Expression.MatchError(_, loc) => throw MatchException("Non-exhaustive match expression.", loc)
    case Expression.SwitchError(_, loc) => throw SwitchException("Non-exhaustive switch expression.", loc)
  }

  private def evalUnary(op: UnaryOperator, e: Expression, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = {
    val v = eval(e, root, env)
    op match {
      case UnaryOperator.LogicalNot => Value.mkBool(!Value.cast2bool(v))
      case UnaryOperator.Plus => v // nop
      case UnaryOperator.Minus => e.tpe match {
        case Type.Float32 => Value.mkFloat32(-Value.cast2float32(v))
        case Type.Float64 => Value.mkFloat64(-Value.cast2float64(v))
        case Type.Int8 => Value.mkInt8(-Value.cast2int8(v))
        case Type.Int16 => Value.mkInt16(-Value.cast2int16(v))
        case Type.Int32 => Value.mkInt32(-Value.cast2int32(v))
        case Type.Int64 => Value.mkInt64(-Value.cast2int64(v))
        case _ => throw new InternalRuntimeException(s"Can't apply UnaryOperator.$op to type ${e.tpe}.")
      }
      case UnaryOperator.BitwiseNegate => e.tpe match {
        case Type.Int8 => Value.mkInt8(~Value.cast2int8(v))
        case Type.Int16 => Value.mkInt16(~Value.cast2int16(v))
        case Type.Int32 => Value.mkInt32(~Value.cast2int32(v))
        case Type.Int64 => Value.mkInt64(~Value.cast2int64(v))
        case _ => throw new InternalRuntimeException(s"Can't apply UnaryOperator.$op to type ${e.tpe}.")
      }
    }
  }

  private def evalArithmetic(o: ArithmeticOperator, e1: Expression, e2: Expression, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = {
    val v1 = eval(e1, root, env)
    val v2 = eval(e2, root, env)
    o match {
      case BinaryOperator.Plus => e1.tpe match {
        case Type.Float32 => Value.mkFloat32(Value.cast2float32(v1) + Value.cast2float32(v2))
        case Type.Float64 => Value.mkFloat64(Value.cast2float64(v1) + Value.cast2float64(v2))
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) + Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) + Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) + Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) + Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Minus => e1.tpe match {
        case Type.Float32 => Value.mkFloat32(Value.cast2float32(v1) - Value.cast2float32(v2))
        case Type.Float64 => Value.mkFloat64(Value.cast2float64(v1) - Value.cast2float64(v2))
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) - Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) - Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) - Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) - Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Times => e1.tpe match {
        case Type.Float32 => Value.mkFloat32(Value.cast2float32(v1) * Value.cast2float32(v2))
        case Type.Float64 => Value.mkFloat64(Value.cast2float64(v1) * Value.cast2float64(v2))
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) * Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) * Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) * Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) * Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Divide => e1.tpe match {
        case Type.Float32 => Value.mkFloat32(Value.cast2float32(v1) / Value.cast2float32(v2))
        case Type.Float64 => Value.mkFloat64(Value.cast2float64(v1) / Value.cast2float64(v2))
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) / Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) / Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) / Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) / Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Modulo => e1.tpe match {
        case Type.Float32 => Value.mkFloat32(Value.cast2float32(v1) % Value.cast2float32(v2))
        case Type.Float64 => Value.mkFloat64(Value.cast2float64(v1) % Value.cast2float64(v2))
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) % Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) % Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) % Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) % Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Exponentiate => e1.tpe match {
        case Type.Float32 => Value.mkFloat32(math.pow(Value.cast2float32(v1), Value.cast2float32(v2)).toFloat)
        case Type.Float64 => Value.mkFloat64(math.pow(Value.cast2float64(v1), Value.cast2float64(v2)).toDouble)
        case Type.Int8 => Value.mkInt8(math.pow(Value.cast2int8(v1), Value.cast2int8(v2)).toByte)
        case Type.Int16 => Value.mkInt16(math.pow(Value.cast2int16(v1), Value.cast2int16(v2)).toShort)
        case Type.Int32 => Value.mkInt32(math.pow(Value.cast2int32(v1), Value.cast2int32(v2)).toInt)
        case Type.Int64 => Value.mkInt64(math.pow(Value.cast2int64(v1), Value.cast2int64(v2)).toLong)
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
    }
  }

  private def evalComparison(o: ComparisonOperator, e1: Expression, e2: Expression, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = {
    val v1 = eval(e1, root, env)
    val v2 = eval(e2, root, env)
    o match {
      case BinaryOperator.Less => e1.tpe match {
        case Type.Char => Value.mkBool(Value.cast2char(v1) < Value.cast2char(v2))
        case Type.Float32 => Value.mkBool(Value.cast2float32(v1) < Value.cast2float32(v2))
        case Type.Float64 => Value.mkBool(Value.cast2float64(v1) < Value.cast2float64(v2))
        case Type.Int8 => Value.mkBool(Value.cast2int8(v1) < Value.cast2int8(v2))
        case Type.Int16 => Value.mkBool(Value.cast2int16(v1) < Value.cast2int16(v2))
        case Type.Int32 => Value.mkBool(Value.cast2int32(v1) < Value.cast2int32(v2))
        case Type.Int64 => Value.mkBool(Value.cast2int64(v1) < Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.LessEqual => e1.tpe match {
        case Type.Char => Value.mkBool(Value.cast2char(v1) <= Value.cast2char(v2))
        case Type.Float32 => Value.mkBool(Value.cast2float32(v1) <= Value.cast2float32(v2))
        case Type.Float64 => Value.mkBool(Value.cast2float64(v1) <= Value.cast2float64(v2))
        case Type.Int8 => Value.mkBool(Value.cast2int8(v1) <= Value.cast2int8(v2))
        case Type.Int16 => Value.mkBool(Value.cast2int16(v1) <= Value.cast2int16(v2))
        case Type.Int32 => Value.mkBool(Value.cast2int32(v1) <= Value.cast2int32(v2))
        case Type.Int64 => Value.mkBool(Value.cast2int64(v1) <= Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Greater => e1.tpe match {
        case Type.Char => Value.mkBool(Value.cast2char(v1) > Value.cast2char(v2))
        case Type.Float32 => Value.mkBool(Value.cast2float32(v1) > Value.cast2float32(v2))
        case Type.Float64 => Value.mkBool(Value.cast2float64(v1) > Value.cast2float64(v2))
        case Type.Int8 => Value.mkBool(Value.cast2int8(v1) > Value.cast2int8(v2))
        case Type.Int16 => Value.mkBool(Value.cast2int16(v1) > Value.cast2int16(v2))
        case Type.Int32 => Value.mkBool(Value.cast2int32(v1) > Value.cast2int32(v2))
        case Type.Int64 => Value.mkBool(Value.cast2int64(v1) > Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.GreaterEqual => e1.tpe match {
        case Type.Char => Value.mkBool(Value.cast2char(v1) >= Value.cast2char(v2))
        case Type.Float32 => Value.mkBool(Value.cast2float32(v1) >= Value.cast2float32(v2))
        case Type.Float64 => Value.mkBool(Value.cast2float64(v1) >= Value.cast2float64(v2))
        case Type.Int8 => Value.mkBool(Value.cast2int8(v1) >= Value.cast2int8(v2))
        case Type.Int16 => Value.mkBool(Value.cast2int16(v1) >= Value.cast2int16(v2))
        case Type.Int32 => Value.mkBool(Value.cast2int32(v1) >= Value.cast2int32(v2))
        case Type.Int64 => Value.mkBool(Value.cast2int64(v1) >= Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Equal => Value.mkBool(v1 == v2)
      case BinaryOperator.NotEqual => Value.mkBool(v1 != v2)
    }
  }

  private def evalLogical(o: LogicalOperator, e1: Expression, e2: Expression, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = o match {
    case BinaryOperator.LogicalAnd =>
      if (Value.cast2bool(eval(e1, root, env))) eval(e2, root, env) else Value.False
    case BinaryOperator.LogicalOr =>
      if (Value.cast2bool(eval(e1, root, env))) Value.True else eval(e2, root, env)
    case BinaryOperator.Implication =>
      // (e1 ==> e2) === (!e1 || e2)
      val notExp = Expression.Unary(UnaryOperator.LogicalNot, e1, Type.Bool, e1.loc)
      evalLogical(BinaryOperator.LogicalOr, notExp, e2, root, env)
    case BinaryOperator.Biconditional =>
      // (e1 <==> e2) === (e1 == e2)
      evalComparison(BinaryOperator.Equal, e1, e2, root, env)
  }

  private def evalBitwise(o: BitwiseOperator, e1: Expression, e2: Expression, root: Root, env: mutable.Map[String, AnyRef]): AnyRef = {
    val v1 = eval(e1, root, env)
    val v2 = eval(e2, root, env)
    o match {
      case BinaryOperator.BitwiseAnd => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) & Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) & Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) & Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) & Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseOr => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) | Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) | Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) | Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) | Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseXor => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) ^ Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) ^ Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) ^ Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) ^ Value.cast2int64(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseLeftShift => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) << Value.cast2int32(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) << Value.cast2int32(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) << Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) << Value.cast2int32(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseRightShift => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) >> Value.cast2int32(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) >> Value.cast2int32(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) >> Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) >> Value.cast2int32(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
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
          val wargs: Array[IValue] = evalArgs.map(new WrappedValue(_))
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
    eval(function, root, env) match {
      case Value.Closure(formals, body, closureEnv) =>
        var i = 0
        while (i < formals.length) {
          closureEnv.update(formals(i), args(i))
          i = i + 1
        }
        eval(body, root, closureEnv)
      case Value.HookClosure(hook) => hook match {
        case Ast.Hook.Safe(name, inv, _) =>
          val wargs: Array[IValue] = args.map(new WrappedValue(_))
          inv(wargs).getUnsafeRef
        case Ast.Hook.Unsafe(name, inv, _) =>
          inv(args)
      }
      case _ => throw new InternalRuntimeException(s"Trying to call a non-function: $function.")
    }

  def eval2(closure: AnyRef, arg1: AnyRef, arg2: AnyRef, root: Root): AnyRef = {
    val clo = Value.cast2closure(closure)
    val env = clo.env
    env.update(clo.formals(0), arg1)
    env.update(clo.formals(1), arg2)
    eval(closure.asInstanceOf[Value.Closure].body, root, env)
  }

}
