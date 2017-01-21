/*
 * Copyright 2015-2016 Ming-Ho Yee
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.runtime

import java.lang.reflect.InvocationTargetException

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.language.ast.ExecutableAst.Definition.Constant
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalRuntimeException

object Interpreter {

  // TODO: Change environments to use

  /**
    * Evaluates the given expression `exp0` under the given environment `env0`.
    */
  def eval(exp0: Expression, root: Root, env0: Map[String, AnyRef] = Map.empty): AnyRef = exp0 match {
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
    case Expression.BigInt(lit) => Value.mkBigInt(lit)
    case Expression.Str(lit) => Value.mkStr(lit)
    case load: LoadExpression =>
      val e = Value.cast2int64(eval(load.e, root, env0))
      val result = (e >> load.offset).toInt & load.mask
      load match {
        case _: Expression.LoadBool => Value.mkBool(result != 0)
        case _: Expression.LoadInt8 => Value.mkInt8(result)
        case _: Expression.LoadInt16 => Value.mkInt16(result)
        case _: Expression.LoadInt32 => Value.mkInt32(result)
      }
    case store: StoreExpression =>
      val e = Value.cast2int64(eval(store.e, root, env0))
      val v = Value.cast2int64(eval(store.v, root, env0))
      val result = (e & store.targetMask) | ((v & store.mask) << store.offset)
      Value.mkInt64(result)
    case Expression.Var(sym, _, loc) => env0.get(sym.toString) match {
      case None => throw InternalRuntimeException(s"Key '${sym.toString}' not found in environment: '${env0.mkString(",")}'.")
      case Some(v) => v
    }
    case Expression.Ref(name, _, _) => eval(root.definitions(name).exp, root, env0)
    case Expression.MkClosureRef(ref, freeVars, _, _) =>
      // Save the values of the free variables in the Value.Closure structure.
      // When the closure is called, these values will be provided at the beginning of the argument list.
      val bindings = new Array[AnyRef](freeVars.length)
      var i = 0
      while (i < bindings.length) {
        bindings(i) = env0(freeVars(i).sym.toString)
        i = i + 1
      }
      Value.Closure(ref.sym, bindings)
    case Expression.ApplyRef(name, args0, _, _) =>
      val args = evalArgs(args0, root, env0)
      evalCall(root.definitions(name), args, root, env0)
    case Expression.ApplyTail(name, _, args0, _, _) =>
      val args = evalArgs(args0.toArray, root, env0)
      evalCall(root.definitions(name), args, root, env0)
    case Expression.ApplyHook(hook, args0, _, _) =>
      val args = evalArgs(args0, root, env0)
      evalHook(hook, args, root, env0)
    case Expression.ApplyClosure(exp, args0, tpe, loc) =>
      val func = eval(exp, root, env0).asInstanceOf[Value.Closure]
      val args = evalArgs(args0, root, env0)
      evalClosure(func, args, root, env0)
    case Expression.Unary(op, exp, _, _) => evalUnary(op, exp, root, env0)
    case Expression.Binary(op, exp1, exp2, _, _) => op match {
      case o: ArithmeticOperator => evalArithmetic(o, exp1, exp2, root, env0)
      case o: ComparisonOperator => evalComparison(o, exp1, exp2, root, env0)
      case o: LogicalOperator => evalLogical(o, exp1, exp2, root, env0)
      case o: BitwiseOperator => evalBitwise(o, exp1, exp2, root, env0)
    }
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = Value.cast2bool(eval(exp1, root, env0))
      if (cond) eval(exp2, root, env0) else eval(exp3, root, env0)
    case Expression.Let(sym, exp1, exp2, _, _) =>
      val newEnv = env0 + (sym.toString -> eval(exp1, root, env0))
      eval(exp2, root, newEnv)
    case Expression.Is(exp, tag, _) => Value.mkBool(Value.cast2tag(eval(exp, root, env0)).tag == tag)
    case Expression.Tag(name, tag, exp, _, _) => Value.mkTag(tag, eval(exp, root, env0))
    case Expression.Untag(tag, exp, _, _) => Value.cast2tag(eval(exp, root, env0)).value
    case Expression.GetTupleIndex(base, offset, _, _) => Value.cast2tuple(eval(base, root, env0))(offset)
    case Expression.Tuple(elms, _, _) =>
      val evalElms = new Array[AnyRef](elms.length)
      var i = 0
      while (i < evalElms.length) {
        evalElms(i) = eval(elms(i), root, env0)
        i = i + 1
      }
      Value.Tuple(evalElms)
    case Expression.Existential(params, exp, loc) => InternalRuntimeException(s"Unexpected expression: '$exp' at ${loc.source.format}.")
    case Expression.Universal(params, exp, loc) => InternalRuntimeException(s"Unexpected expression: '$exp' at ${loc.source.format}.")
    case Expression.UserError(_, loc) => throw UserException("User exception.", loc)
    case Expression.MatchError(_, loc) => throw MatchException("Non-exhaustive match expression.", loc)
    case Expression.SwitchError(_, loc) => throw SwitchException("Non-exhaustive switch expression.", loc)
  }

  private def evalUnary(op: UnaryOperator, e: Expression, root: Root, env: Map[String, AnyRef]): AnyRef = {
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
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v).negate)
        case _ => throw new InternalRuntimeException(s"Can't apply UnaryOperator.$op to type ${e.tpe}.")
      }
      case UnaryOperator.BitwiseNegate => e.tpe match {
        case Type.Int8 => Value.mkInt8(~Value.cast2int8(v))
        case Type.Int16 => Value.mkInt16(~Value.cast2int16(v))
        case Type.Int32 => Value.mkInt32(~Value.cast2int32(v))
        case Type.Int64 => Value.mkInt64(~Value.cast2int64(v))
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v).not)
        case _ => throw new InternalRuntimeException(s"Can't apply UnaryOperator.$op to type ${e.tpe}.")
      }
    }
  }

  private def evalArithmetic(o: ArithmeticOperator, e1: Expression, e2: Expression, root: Root, env: Map[String, AnyRef]): AnyRef = {
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
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v1) add Value.cast2bigInt(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Minus => e1.tpe match {
        case Type.Float32 => Value.mkFloat32(Value.cast2float32(v1) - Value.cast2float32(v2))
        case Type.Float64 => Value.mkFloat64(Value.cast2float64(v1) - Value.cast2float64(v2))
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) - Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) - Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) - Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) - Value.cast2int64(v2))
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v1) subtract Value.cast2bigInt(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Times => e1.tpe match {
        case Type.Float32 => Value.mkFloat32(Value.cast2float32(v1) * Value.cast2float32(v2))
        case Type.Float64 => Value.mkFloat64(Value.cast2float64(v1) * Value.cast2float64(v2))
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) * Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) * Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) * Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) * Value.cast2int64(v2))
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v1) multiply Value.cast2bigInt(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Divide => e1.tpe match {
        case Type.Float32 => Value.mkFloat32(Value.cast2float32(v1) / Value.cast2float32(v2))
        case Type.Float64 => Value.mkFloat64(Value.cast2float64(v1) / Value.cast2float64(v2))
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) / Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) / Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) / Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) / Value.cast2int64(v2))
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v1) divide Value.cast2bigInt(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Modulo => e1.tpe match {
        case Type.Float32 => Value.mkFloat32(Value.cast2float32(v1) % Value.cast2float32(v2))
        case Type.Float64 => Value.mkFloat64(Value.cast2float64(v1) % Value.cast2float64(v2))
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) % Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) % Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) % Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) % Value.cast2int64(v2))
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v1) remainder Value.cast2bigInt(v2))
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

  private def evalComparison(o: ComparisonOperator, e1: Expression, e2: Expression, root: Root, env: Map[String, AnyRef]): AnyRef = {
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
        case Type.BigInt => Value.mkBool((Value.cast2bigInt(v1) compareTo Value.cast2bigInt(v2)) < 0)
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
        case Type.BigInt => Value.mkBool((Value.cast2bigInt(v1) compareTo Value.cast2bigInt(v2)) <= 0)
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
        case Type.BigInt => Value.mkBool((Value.cast2bigInt(v1) compareTo Value.cast2bigInt(v2)) > 0)
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
        case Type.BigInt => Value.mkBool((Value.cast2bigInt(v1) compareTo Value.cast2bigInt(v2)) >= 0)
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.Equal => Value.mkBool(v1 == v2)
      case BinaryOperator.NotEqual => Value.mkBool(v1 != v2)
    }
  }

  private def evalLogical(o: LogicalOperator, e1: Expression, e2: Expression, root: Root, env: Map[String, AnyRef]): AnyRef = o match {
    case BinaryOperator.LogicalAnd =>
      if (Value.cast2bool(eval(e1, root, env))) eval(e2, root, env) else Value.False
    case BinaryOperator.LogicalOr =>
      if (Value.cast2bool(eval(e1, root, env))) Value.True else eval(e2, root, env)
  }

  private def evalBitwise(o: BitwiseOperator, e1: Expression, e2: Expression, root: Root, env: Map[String, AnyRef]): AnyRef = {
    val v1 = eval(e1, root, env)
    val v2 = eval(e2, root, env)
    o match {
      case BinaryOperator.BitwiseAnd => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) & Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) & Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) & Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) & Value.cast2int64(v2))
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v1) and Value.cast2bigInt(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseOr => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) | Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) | Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) | Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) | Value.cast2int64(v2))
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v1) or Value.cast2bigInt(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseXor => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) ^ Value.cast2int8(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) ^ Value.cast2int16(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) ^ Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) ^ Value.cast2int64(v2))
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v1) xor Value.cast2bigInt(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseLeftShift => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) << Value.cast2int32(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) << Value.cast2int32(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) << Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) << Value.cast2int32(v2))
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v1) shiftLeft Value.cast2int32(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
      case BinaryOperator.BitwiseRightShift => e1.tpe match {
        case Type.Int8 => Value.mkInt8(Value.cast2int8(v1) >> Value.cast2int32(v2))
        case Type.Int16 => Value.mkInt16(Value.cast2int16(v1) >> Value.cast2int32(v2))
        case Type.Int32 => Value.mkInt32(Value.cast2int32(v1) >> Value.cast2int32(v2))
        case Type.Int64 => Value.mkInt64(Value.cast2int64(v1) >> Value.cast2int32(v2))
        case Type.BigInt => Value.mkBigInt(Value.cast2bigInt(v1) shiftRight Value.cast2int32(v2))
        case _ => throw new InternalRuntimeException(s"Can't apply BinaryOperator.$o to type ${e1.tpe}.")
      }
    }
  }

  // TODO: Need to come up with some more clean interfaces
  // TODO: Everything below here is really bad and should just be replaced at will.

  /**
    * Evaluates the given head term `t` under the given environment `env0`
    */
  def evalHeadTerm(t: Term.Head, root: Root, env: Map[String, AnyRef]): AnyRef = t match {
    case Term.Head.Var(x, _, _) => env(x.toString)
    case Term.Head.Exp(e, _, _) => eval(e, root, env)
    case Term.Head.Apply(name, args, _, _) =>
      val defn = root.definitions(name)
      val evalArgs = new Array[AnyRef](args.length)
      var i = 0
      while (i < evalArgs.length) {
        evalArgs(i) = evalHeadTerm(args(i), root, env)
        i = i + 1
      }
      evalCall(defn, evalArgs, root, env)
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

  def evalBodyTerm(t: Term.Body, root: Root, env: Map[String, AnyRef]): AnyRef = t match {
    case Term.Body.Wildcard(_, _) => ???
    case Term.Body.Var(x, _, _, _) => env(x.toString)
    case Term.Body.Exp(e, _, _) => eval(e, root, env)
  }

  def evalCall(defn: Constant, args: Array[AnyRef], root: Root, env0: Map[String, AnyRef] = Map.empty): AnyRef = {
    if (defn.method == null) {
      val env = defn.formals.zip(args).foldLeft(env0) {
        case (macc, (ExecutableAst.FormalParam(name, tpe), actual)) => macc + (name.toString -> actual)
      }
      eval(defn.exp, root, env)
    } else {
      // TODO: Should the reflection call be here, or moved elsewhere?
      try {
        defn.method.invoke(null, args: _*)
      } catch {
        // Rethrow the real exception
        case e: InvocationTargetException => throw e.getTargetException
      }
    }
  }

  private def evalHook(hook: Ast.Hook, args: Array[AnyRef], root: Root, env: Map[String, AnyRef]): AnyRef =
    hook match {
      case Ast.Hook.Safe(name, inv, _) =>
        val wargs: Array[IValue] = args.map(new WrappedValue(_))
        inv(wargs).getUnsafeRef
      case Ast.Hook.Unsafe(name, inv, _) =>
        inv(args)
    }

  private def evalClosure(function: Value.Closure, args: Array[AnyRef], root: Root, env: Map[String, AnyRef]): AnyRef = {
    val Value.Closure(name, bindings) = function
    val constant = root.definitions(name)

    // Bindings for the capture variables are passed as arguments.
    val env1 = constant.formals.take(bindings.length).zip(bindings).foldLeft(env) {
      case (macc, (formal, actual)) => macc + (formal.sym.toString -> actual)
    }

    // Now pass the actual arguments supplied by the caller.
    val env2 = constant.formals.drop(bindings.length).zip(args).foldLeft(env1) {
      case (macc, (formal, actual)) => macc + (formal.sym.toString -> actual)
    }

    eval(constant.exp, root, env2)
  }

  private def evalArgs(args: Array[Expression], root: Root, env: Map[String, AnyRef]): Array[AnyRef] = {
    val evalArgs = new Array[AnyRef](args.length)
    var i = 0
    while (i < evalArgs.length) {
      evalArgs(i) = eval(args(i), root, env)
      i = i + 1
    }
    evalArgs
  }

}
