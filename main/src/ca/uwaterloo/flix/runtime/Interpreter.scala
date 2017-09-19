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

import java.lang.reflect.Modifier

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalRuntimeException

object Interpreter {

  /**
    * Evaluates the given expression `exp0` under the given environment `env0`.
    */
  def eval(exp0: Expression, env0: Map[String, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root): AnyRef = exp0 match {
    //
    // Literal expressions.
    //
    case Expression.Unit => Value.Unit
    case Expression.True => Value.True
    case Expression.False => Value.False
    case Expression.Char(lit) => Value.Char(lit)
    case Expression.Float32(lit) => Value.Float32(lit)
    case Expression.Float64(lit) => Value.Float64(lit)
    case Expression.Int8(lit) => Value.Int8(lit)
    case Expression.Int16(lit) => Value.Int16(lit)
    case Expression.Int32(lit) => Value.Int32(lit)
    case Expression.Int64(lit) => Value.Int64(lit)
    case Expression.BigInt(lit) => Value.BigInt(lit)
    case Expression.Str(lit) => Value.Str(lit)

    //
    // Variable expressions.
    //
    case Expression.Var(sym, _, loc) => env0.get(sym.toString) match {
      case None => throw InternalRuntimeException(s"Key '${sym.toString}' not found in environment: '${env0.mkString(",")}'.")
      case Some(v) => v
    }

    //
    // Closure expressions.
    //
    case Expression.Closure(sym, freeVars, _, _, _) =>
      allocateClosure(sym, freeVars.toArray, env0)

    //
    // Apply* expressions.
    //
    case Expression.ApplyClo(exp, args, _, _) => invokeClo(exp, args, env0, lenv0, root)
    case Expression.ApplyDef(sym, args, _, _) => invokeDef(sym, args, env0, lenv0, root)
    case Expression.ApplyCloTail(exp, args, _, _) => invokeClo(exp, args, env0, lenv0, root)
    case Expression.ApplyDefTail(sym, args, _, _) => invokeDef(sym, args, env0, lenv0, root)
    case Expression.ApplySelfTail(sym, _, args, _, _) => invokeDef(sym, args, env0, lenv0, root)

    case Expression.ApplyHook(hook, args0, _, _) =>
      val args = evalArgs(args0, env0, lenv0, root)
      hook match {
        case Ast.Hook.Unsafe(name, inv, _) =>
          inv(args.toArray)
      }

    //
    // Unary expressions.
    //
    case Expression.Unary(sop, op, exp, _, _) =>
      evalUnary(sop, exp, env0, lenv0, root)

    //
    // Binary expressions.
    //
    case Expression.Binary(sop, op, exp1, exp2, _, _) => evalBinary(sop, exp1, exp2, env0, lenv0, root)

    //
    // If-then-else expressions.
    //
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = cast2bool(eval(exp1, env0, lenv0, root))
      if (cond) eval(exp2, env0, lenv0, root) else eval(exp3, env0, lenv0, root)

    //
    // Block expressions.
    //
    case Expression.Branch(exp, branches, tpe, loc) => eval(exp, env0, branches, root)

    //
    // Jump expressions.
    //
    case Expression.JumpTo(sym, tpe, loc) =>
      lenv0.get(sym) match {
        case None => throw InternalRuntimeException(s"Unknown label: '$sym' in label environment ${lenv0.mkString(" ,")}.")
        case Some(e) => eval(e, env0, lenv0, root)
      }

    //
    // Let expressions.
    //
    case Expression.Let(sym, exp1, exp2, _, _) =>
      val newEnv = env0 + (sym.toString -> eval(exp1, env0, lenv0, root))
      eval(exp2, newEnv, lenv0, root)

    //
    // Let-rec expressions.
    //
    case Expression.LetRec(sym, exp1, exp2, _, _) => exp1 match {
      case Expression.Closure(ref, freeVars, _, _, _) =>
        // Allocate a circular closure.
        val closure = allocateClosure(ref, freeVars.toArray, env0)
        closure.bindings(sym.getStackOffset) = closure

        // Evaluate the body expression under the extended environment.
        val newEnv = env0 + (sym.toString -> closure)
        eval(exp2, newEnv, lenv0, root)
      case _ => throw InternalRuntimeException(s"Non-closure letrec value: ${exp1.getClass.getCanonicalName}.")
    }

    //
    // Is, Tag, and Untag expressions.
    //
    case Expression.Is(sym, tag, exp, _) => mkBool(cast2tag(eval(exp, env0, lenv0, root)).tag == tag)
    case Expression.Tag(sym, tag, exp, _, _) => Value.Tag(sym, tag, eval(exp, env0, lenv0, root))
    case Expression.Untag(sym, tag, exp, _, _) => cast2tag(eval(exp, env0, lenv0, root)).value

    //
    // Index expressions.
    //
    case Expression.Index(base, offset, _, _) =>
      val tuple = cast2tuple(eval(base, env0, lenv0, root))
      tuple.elms(offset)

    //
    // Tuple expressions.
    //
    case Expression.Tuple(elms, _, _) =>
      Value.Tuple(elms.map(e => eval(e, env0, lenv0, root)).toList)

    //
    // Reference expressions.
    //
    case Expression.Ref(exp, tpe, loc) =>
      val box = new Value.Box()
      val value = eval(exp, env0, lenv0, root)
      box.setValue(value)
      box

    //
    // Dereference expressions.
    //
    case Expression.Deref(exp, tpe, loc) =>
      val box = cast2box(eval(exp, env0, lenv0, root))
      box.getValue

    //
    // Assign expressions.
    //
    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val box = cast2box(eval(exp1, env0, lenv0, root))
      val value = eval(exp2, env0, lenv0, root)
      box.setValue(value)
      Value.Unit

    //
    // NativeConstructor expressions.
    //
    case Expression.NativeConstructor(constructor, args, tpe, loc) =>
      val values = evalArgs(args, env0, lenv0, root).map(toJava)
      val arguments = values.toArray
      fromJava(constructor.newInstance(arguments: _*).asInstanceOf[AnyRef])

    //
    // NativeField expressions.
    //
    case Expression.NativeField(field, tpe, loc) =>
      val clazz = field.getDeclaringClass
      fromJava(field.get(clazz))

    //
    // NativeMethod expressions.
    //
    case Expression.NativeMethod(method, args, tpe, loc) =>
      val values = evalArgs(args, env0, lenv0, root).map(toJava)
      if (Modifier.isStatic(method.getModifiers)) {
        val arguments = values.toArray
        fromJava(method.invoke(null, arguments: _*))
      } else {
        val thisObj = values.head
        val arguments = values.tail.toArray
        fromJava(method.invoke(thisObj, arguments: _*))
      }

    //
    // Error expressions.
    //
    case Expression.UserError(_, loc) => throw UserException("User exception.", loc)
    case Expression.MatchError(_, loc) => throw MatchException("Non-exhaustive match expression.", loc)
    case Expression.SwitchError(_, loc) => throw SwitchException("Non-exhaustive switch expression.", loc)

    //
    // Unexpected expressions.
    //
    case Expression.Existential(params, exp, loc) => throw InternalRuntimeException(s"Unexpected expression: '$exp' at ${loc.source.format}.")
    case Expression.Universal(params, exp, loc) => throw InternalRuntimeException(s"Unexpected expression: '$exp' at ${loc.source.format}.")
  }

  /**
    * Applies the given unary semantic operator `sop` to the value of the expression `exp0` under the environment `env0`
    */
  private def evalUnary(sop: SemanticOperator, exp0: Expression, env0: Map[String, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root): AnyRef = {
    // Evaluate the operand.
    val v = eval(exp0, env0, lenv0, root)

    // Apply the operator.
    sop match {
      case SemanticOperator.BoolOp.Not => mkBool(!cast2bool(v))

      case SemanticOperator.Float32Op.Neg => Value.Float32(-cast2float32(v))

      case SemanticOperator.Float64Op.Neg => Value.Float64(-cast2float64(v))

      case SemanticOperator.Int8Op.Neg => Value.Int8((-cast2int8(v)).toByte)
      case SemanticOperator.Int8Op.Not => Value.Int8((~cast2int8(v)).toByte)

      case SemanticOperator.Int16Op.Neg => Value.Int16((-cast2int16(v)).toShort)
      case SemanticOperator.Int16Op.Not => Value.Int16((~cast2int16(v)).toShort)

      case SemanticOperator.Int32Op.Neg => Value.Int32(-cast2int32(v))
      case SemanticOperator.Int32Op.Not => Value.Int32(~cast2int32(v))

      case SemanticOperator.Int64Op.Neg => Value.Int64(-cast2int64(v))
      case SemanticOperator.Int64Op.Not => Value.Int64(~cast2int64(v))

      case SemanticOperator.BigIntOp.Neg => Value.BigInt(cast2bigInt(v).negate)
      case SemanticOperator.BigIntOp.Not => Value.BigInt(cast2bigInt(v).not)

      case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
    }
  }

  /**
    * Applies the given binary semantic operator `sop`  to the values of the two expressions `exp1` and `exp2` under the environment `env0`
    */
  private def evalBinary(sop: SemanticOperator, exp1: Expression, exp2: Expression, env0: Map[String, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root): AnyRef = {

    def evalBoolOp(sop: SemanticOperator.BoolOp): AnyRef = {
      // Evaluate the left operand.
      val v1 = cast2bool(eval(exp1, env0, lenv0, root))

      sop match {
        // Lazy operators.
        case SemanticOperator.BoolOp.And if v1 => mkBool(cast2bool(eval(exp2, env0, lenv0, root)))
        case SemanticOperator.BoolOp.And => mkBool(false)

        // Lazy operators.
        case SemanticOperator.BoolOp.Or if v1 => mkBool(true)
        case SemanticOperator.BoolOp.Or => mkBool(cast2bool(eval(exp2, env0, lenv0, root)))

        case SemanticOperator.BoolOp.Eq => mkBool(v1 == cast2bool(eval(exp2, env0, lenv0, root)))
        case SemanticOperator.BoolOp.Neq => mkBool(v1 != cast2bool(eval(exp2, env0, lenv0, root)))
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    def evalCharOp(sop: SemanticOperator.CharOp): AnyRef = {
      // Evaluate the operands.
      val v1 = eval(exp1, env0, lenv0, root)
      val v2 = eval(exp2, env0, lenv0, root)

      sop match {
        case SemanticOperator.CharOp.Eq => mkBool(cast2char(v1) == cast2char(v2))
        case SemanticOperator.CharOp.Neq => mkBool(cast2char(v1) != cast2char(v2))
        case SemanticOperator.CharOp.Lt => mkBool(cast2char(v1) < cast2char(v2))
        case SemanticOperator.CharOp.Le => mkBool(cast2char(v1) <= cast2char(v2))
        case SemanticOperator.CharOp.Gt => mkBool(cast2char(v1) > cast2char(v2))
        case SemanticOperator.CharOp.Ge => mkBool(cast2char(v1) >= cast2char(v2))
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    def evalFloat32Op(sop: SemanticOperator.Float32Op): AnyRef = {
      // Evaluate the operands.
      val v1 = eval(exp1, env0, lenv0, root)
      val v2 = eval(exp2, env0, lenv0, root)

      sop match {
        case SemanticOperator.Float32Op.Add => Value.Float32(cast2float32(v1) + cast2float32(v2))
        case SemanticOperator.Float32Op.Sub => Value.Float32(cast2float32(v1) - cast2float32(v2))
        case SemanticOperator.Float32Op.Mul => Value.Float32(cast2float32(v1) * cast2float32(v2))
        case SemanticOperator.Float32Op.Div => Value.Float32(cast2float32(v1) / cast2float32(v2))
        case SemanticOperator.Float32Op.Rem => Value.Float32(cast2float32(v1) % cast2float32(v2))
        case SemanticOperator.Float32Op.Exp => Value.Float32(math.pow(cast2float32(v1), cast2float32(v2)).toFloat)
        case SemanticOperator.Float32Op.Eq => mkBool(cast2float32(v1) == cast2float32(v2))
        case SemanticOperator.Float32Op.Neq => mkBool(cast2float32(v1) != cast2float32(v2))
        case SemanticOperator.Float32Op.Lt => mkBool(cast2float32(v1) < cast2float32(v2))
        case SemanticOperator.Float32Op.Le => mkBool(cast2float32(v1) <= cast2float32(v2))
        case SemanticOperator.Float32Op.Gt => mkBool(cast2float32(v1) > cast2float32(v2))
        case SemanticOperator.Float32Op.Ge => mkBool(cast2float32(v1) >= cast2float32(v2))
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    def evalFloat64Op(sop: SemanticOperator.Float64Op): AnyRef = {
      // Evaluate the operands.
      val v1 = eval(exp1, env0, lenv0, root)
      val v2 = eval(exp2, env0, lenv0, root)

      sop match {
        case SemanticOperator.Float64Op.Add => Value.Float64(cast2float64(v1) + cast2float64(v2))
        case SemanticOperator.Float64Op.Sub => Value.Float64(cast2float64(v1) - cast2float64(v2))
        case SemanticOperator.Float64Op.Mul => Value.Float64(cast2float64(v1) * cast2float64(v2))
        case SemanticOperator.Float64Op.Div => Value.Float64(cast2float64(v1) / cast2float64(v2))
        case SemanticOperator.Float64Op.Rem => Value.Float64(cast2float64(v1) % cast2float64(v2))
        case SemanticOperator.Float64Op.Exp => Value.Float64(math.pow(cast2float64(v1), cast2float64(v2)))
        case SemanticOperator.Float64Op.Eq => mkBool(cast2float64(v1) == cast2float64(v2))
        case SemanticOperator.Float64Op.Neq => mkBool(cast2float64(v1) != cast2float64(v2))
        case SemanticOperator.Float64Op.Lt => mkBool(cast2float64(v1) < cast2float64(v2))
        case SemanticOperator.Float64Op.Le => mkBool(cast2float64(v1) <= cast2float64(v2))
        case SemanticOperator.Float64Op.Gt => mkBool(cast2float64(v1) > cast2float64(v2))
        case SemanticOperator.Float64Op.Ge => mkBool(cast2float64(v1) >= cast2float64(v2))
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    def evalInt8Op(sop: SemanticOperator.Int8Op): AnyRef = {
      // Evaluate the operands.
      val v1 = eval(exp1, env0, lenv0, root)
      val v2 = eval(exp2, env0, lenv0, root)

      sop match {
        case SemanticOperator.Int8Op.Add => Value.Int8((cast2int8(v1) + cast2int8(v2)).toByte)
        case SemanticOperator.Int8Op.Sub => Value.Int8((cast2int8(v1) - cast2int8(v2)).toByte)
        case SemanticOperator.Int8Op.Mul => Value.Int8((cast2int8(v1) * cast2int8(v2)).toByte)
        case SemanticOperator.Int8Op.Div => Value.Int8((cast2int8(v1) / cast2int8(v2)).toByte)
        case SemanticOperator.Int8Op.Rem => Value.Int8((cast2int8(v1) % cast2int8(v2)).toByte)
        case SemanticOperator.Int8Op.Exp => Value.Int8(math.pow(cast2int8(v1), cast2int8(v2)).toByte)
        case SemanticOperator.Int8Op.And => Value.Int8((cast2int8(v1) & cast2int8(v2)).toByte)
        case SemanticOperator.Int8Op.Or => Value.Int8((cast2int8(v1) | cast2int8(v2)).toByte)
        case SemanticOperator.Int8Op.Xor => Value.Int8((cast2int8(v1) ^ cast2int8(v2)).toByte)
        case SemanticOperator.Int8Op.Shl => Value.Int8((cast2int8(v1) << cast2int32(v2)).toByte)
        case SemanticOperator.Int8Op.Shr => Value.Int8((cast2int8(v1) >> cast2int32(v2)).toByte)
        case SemanticOperator.Int8Op.Eq => mkBool(cast2int8(v1) == cast2int8(v2))
        case SemanticOperator.Int8Op.Neq => mkBool(cast2int8(v1) != cast2int8(v2))
        case SemanticOperator.Int8Op.Lt => mkBool(cast2int8(v1) < cast2int8(v2))
        case SemanticOperator.Int8Op.Le => mkBool(cast2int8(v1) <= cast2int8(v2))
        case SemanticOperator.Int8Op.Gt => mkBool(cast2int8(v1) > cast2int8(v2))
        case SemanticOperator.Int8Op.Ge => mkBool(cast2int8(v1) >= cast2int8(v2))
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    def evalInt16Op(sop: SemanticOperator.Int16Op): AnyRef = {
      // Evaluate the operands.
      val v1 = eval(exp1, env0, lenv0, root)
      val v2 = eval(exp2, env0, lenv0, root)

      sop match {
        case SemanticOperator.Int16Op.Add => Value.Int16((cast2int16(v1) + cast2int16(v2)).toShort)
        case SemanticOperator.Int16Op.Sub => Value.Int16((cast2int16(v1) - cast2int16(v2)).toShort)
        case SemanticOperator.Int16Op.Mul => Value.Int16((cast2int16(v1) * cast2int16(v2)).toShort)
        case SemanticOperator.Int16Op.Div => Value.Int16((cast2int16(v1) / cast2int16(v2)).toShort)
        case SemanticOperator.Int16Op.Rem => Value.Int16((cast2int16(v1) % cast2int16(v2)).toShort)
        case SemanticOperator.Int16Op.Exp => Value.Int16(math.pow(cast2int16(v1), cast2int16(v2)).toShort)
        case SemanticOperator.Int16Op.And => Value.Int16((cast2int16(v1) & cast2int16(v2)).toShort)
        case SemanticOperator.Int16Op.Or => Value.Int16((cast2int16(v1) | cast2int16(v2)).toShort)
        case SemanticOperator.Int16Op.Xor => Value.Int16((cast2int16(v1) ^ cast2int16(v2)).toShort)
        case SemanticOperator.Int16Op.Shl => Value.Int16((cast2int16(v1) << cast2int32(v2)).toShort)
        case SemanticOperator.Int16Op.Shr => Value.Int16((cast2int16(v1) >> cast2int32(v2)).toShort)
        case SemanticOperator.Int16Op.Eq => mkBool(cast2int16(v1) == cast2int16(v2))
        case SemanticOperator.Int16Op.Neq => mkBool(cast2int16(v1) != cast2int16(v2))
        case SemanticOperator.Int16Op.Lt => mkBool(cast2int16(v1) < cast2int16(v2))
        case SemanticOperator.Int16Op.Le => mkBool(cast2int16(v1) <= cast2int16(v2))
        case SemanticOperator.Int16Op.Gt => mkBool(cast2int16(v1) > cast2int16(v2))
        case SemanticOperator.Int16Op.Ge => mkBool(cast2int16(v1) >= cast2int16(v2))
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    def evalInt32Op(sop: SemanticOperator.Int32Op): AnyRef = {
      // Evaluate the operands.
      val v1 = eval(exp1, env0, lenv0, root)
      val v2 = eval(exp2, env0, lenv0, root)

      sop match {
        case SemanticOperator.Int32Op.Add => Value.Int32(cast2int32(v1) + cast2int32(v2))
        case SemanticOperator.Int32Op.Sub => Value.Int32(cast2int32(v1) - cast2int32(v2))
        case SemanticOperator.Int32Op.Mul => Value.Int32(cast2int32(v1) * cast2int32(v2))
        case SemanticOperator.Int32Op.Div => Value.Int32(cast2int32(v1) / cast2int32(v2))
        case SemanticOperator.Int32Op.Rem => Value.Int32(cast2int32(v1) % cast2int32(v2))
        case SemanticOperator.Int32Op.Exp => Value.Int32(math.pow(cast2int32(v1), cast2int32(v2)).toInt)
        case SemanticOperator.Int32Op.And => Value.Int32(cast2int32(v1) & cast2int32(v2))
        case SemanticOperator.Int32Op.Or => Value.Int32(cast2int32(v1) | cast2int32(v2))
        case SemanticOperator.Int32Op.Xor => Value.Int32(cast2int32(v1) ^ cast2int32(v2))
        case SemanticOperator.Int32Op.Shl => Value.Int32(cast2int32(v1) << cast2int32(v2))
        case SemanticOperator.Int32Op.Shr => Value.Int32(cast2int32(v1) >> cast2int32(v2))
        case SemanticOperator.Int32Op.Eq => mkBool(cast2int32(v1) == cast2int32(v2))
        case SemanticOperator.Int32Op.Neq => mkBool(cast2int32(v1) != cast2int32(v2))
        case SemanticOperator.Int32Op.Lt => mkBool(cast2int32(v1) < cast2int32(v2))
        case SemanticOperator.Int32Op.Le => mkBool(cast2int32(v1) <= cast2int32(v2))
        case SemanticOperator.Int32Op.Gt => mkBool(cast2int32(v1) > cast2int32(v2))
        case SemanticOperator.Int32Op.Ge => mkBool(cast2int32(v1) >= cast2int32(v2))
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    def evalInt64Op(sop: SemanticOperator.Int64Op): AnyRef = {
      // Evaluate the operands.
      val v1 = eval(exp1, env0, lenv0, root)
      val v2 = eval(exp2, env0, lenv0, root)

      sop match {
        case SemanticOperator.Int64Op.Add => Value.Int64(cast2int64(v1) + cast2int64(v2))
        case SemanticOperator.Int64Op.Sub => Value.Int64(cast2int64(v1) - cast2int64(v2))
        case SemanticOperator.Int64Op.Mul => Value.Int64(cast2int64(v1) * cast2int64(v2))
        case SemanticOperator.Int64Op.Div => Value.Int64(cast2int64(v1) / cast2int64(v2))
        case SemanticOperator.Int64Op.Rem => Value.Int64(cast2int64(v1) % cast2int64(v2))
        case SemanticOperator.Int64Op.Exp => Value.Int64(math.pow(cast2int64(v1), cast2int64(v2)).toLong)
        case SemanticOperator.Int64Op.And => Value.Int64(cast2int64(v1) & cast2int64(v2))
        case SemanticOperator.Int64Op.Or => Value.Int64(cast2int64(v1) | cast2int64(v2))
        case SemanticOperator.Int64Op.Xor => Value.Int64(cast2int64(v1) ^ cast2int64(v2))
        case SemanticOperator.Int64Op.Shl => Value.Int64(cast2int64(v1) << cast2int32(v2))
        case SemanticOperator.Int64Op.Shr => Value.Int64(cast2int64(v1) >> cast2int32(v2))
        case SemanticOperator.Int64Op.Eq => mkBool(cast2int64(v1) == cast2int64(v2))
        case SemanticOperator.Int64Op.Neq => mkBool(cast2int64(v1) != cast2int64(v2))
        case SemanticOperator.Int64Op.Lt => mkBool(cast2int64(v1) < cast2int64(v2))
        case SemanticOperator.Int64Op.Le => mkBool(cast2int64(v1) <= cast2int64(v2))
        case SemanticOperator.Int64Op.Gt => mkBool(cast2int64(v1) > cast2int64(v2))
        case SemanticOperator.Int64Op.Ge => mkBool(cast2int64(v1) >= cast2int64(v2))
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    def evalBigIntOp(sop: SemanticOperator.BigIntOp): AnyRef = {
      // Evaluate the operands.
      val v1 = eval(exp1, env0, lenv0, root)
      val v2 = eval(exp2, env0, lenv0, root)

      sop match {
        case SemanticOperator.BigIntOp.Add => Value.BigInt(cast2bigInt(v1) add cast2bigInt(v2))
        case SemanticOperator.BigIntOp.Sub => Value.BigInt(cast2bigInt(v1) subtract cast2bigInt(v2))
        case SemanticOperator.BigIntOp.Mul => Value.BigInt(cast2bigInt(v1) multiply cast2bigInt(v2))
        case SemanticOperator.BigIntOp.Div => Value.BigInt(cast2bigInt(v1) divide cast2bigInt(v2))
        case SemanticOperator.BigIntOp.Rem => Value.BigInt(cast2bigInt(v1) remainder cast2bigInt(v2))
        case SemanticOperator.BigIntOp.Exp => Value.BigInt(cast2bigInt(v1) pow cast2int32(v2))
        case SemanticOperator.BigIntOp.And => Value.BigInt(cast2bigInt(v1) and cast2bigInt(v2))
        case SemanticOperator.BigIntOp.Or => Value.BigInt(cast2bigInt(v1) or cast2bigInt(v2))
        case SemanticOperator.BigIntOp.Xor => Value.BigInt(cast2bigInt(v1) xor cast2bigInt(v2))
        case SemanticOperator.BigIntOp.Shl => Value.BigInt(cast2bigInt(v1) shiftLeft cast2int32(v2))
        case SemanticOperator.BigIntOp.Shr => Value.BigInt(cast2bigInt(v1) shiftRight cast2int32(v2))
        case SemanticOperator.BigIntOp.Eq => mkBool(cast2bigInt(v1) == cast2bigInt(v2))
        case SemanticOperator.BigIntOp.Neq => mkBool(cast2bigInt(v1) != cast2bigInt(v2))
        case SemanticOperator.BigIntOp.Lt => mkBool((cast2bigInt(v1) compareTo cast2bigInt(v2)) < 0)
        case SemanticOperator.BigIntOp.Le => mkBool((cast2bigInt(v1) compareTo cast2bigInt(v2)) <= 0)
        case SemanticOperator.BigIntOp.Gt => mkBool((cast2bigInt(v1) compareTo cast2bigInt(v2)) > 0)
        case SemanticOperator.BigIntOp.Ge => mkBool((cast2bigInt(v1) compareTo cast2bigInt(v2)) >= 0)
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    def evalOtherOp(sop: SemanticOperator): AnyRef = {
      // Evaluate the operands.
      val v1 = eval(exp1, env0, lenv0, root)
      val v2 = eval(exp2, env0, lenv0, root)

      sop match {
        case SemanticOperator.StringOp.Concat => Value.Str(cast2str(v1) + cast2str(v2))
        case SemanticOperator.StringOp.Eq => mkBool(cast2str(v1) == cast2str(v2))
        case SemanticOperator.StringOp.Neq => mkBool(cast2str(v1) != cast2str(v2))
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    // Apply the operator.
    sop match {
      case op: SemanticOperator.BoolOp => evalBoolOp(op)
      case op: SemanticOperator.CharOp => evalCharOp(op)
      case op: SemanticOperator.Float32Op => evalFloat32Op(op)
      case op: SemanticOperator.Float64Op => evalFloat64Op(op)
      case op: SemanticOperator.Int8Op => evalInt8Op(op)
      case op: SemanticOperator.Int16Op => evalInt16Op(op)
      case op: SemanticOperator.Int32Op => evalInt32Op(op)
      case op: SemanticOperator.Int64Op => evalInt64Op(op)
      case op: SemanticOperator.BigIntOp => evalBigIntOp(op)
      case _ => evalOtherOp(sop)
    }
  }

  /**
    * Invokes the given closure expression `exp` with the given arguments `args` under the given environment `env0`..
    */
  private def invokeClo(exp: Expression, args: List[Expression], env0: Map[String, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root): AnyRef = {
    val v = eval(exp, env0, lenv0, root)
    val Value.Closure(name, bindings) = cast2closure(v)
    val as = evalArgs(args, env0, lenv0, root)
    val constant = root.defs(name)
    // Bindings for the capture variables are passed as arguments.
    val env1 = constant.formals.take(bindings.length).zip(bindings).foldLeft(env0) {
      case (macc, (formal, actual)) => macc + (formal.sym.toString -> actual)
    }
    // Now pass the actual arguments supplied by the caller.
    val env2 = constant.formals.drop(bindings.length).zip(as).foldLeft(env1) {
      case (macc, (formal, actual)) => macc + (formal.sym.toString -> actual)
    }
    eval(constant.exp, env2, Map.empty, root)
  }

  /**
    * Invokes the given definition `sym` with the given arguments `args` under the given environment `env0`..
    */
  private def invokeDef(sym: Symbol.DefnSym, args: List[Expression], env0: Map[String, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root): AnyRef = {
    val as = evalArgs(args, env0, lenv0, root)
    // TODO: Using the linker here is quite a hack.
    fromJava(Linker.link(sym, root).invoke(as.toArray))
  }

  /**
    * Evaluates the given list of expressions `exps` under the given environment `env0` to a list of values.
    */
  private def evalArgs(exps: List[Expression], env0: Map[String, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root): List[AnyRef] = {
    exps.map(a => eval(a, env0, lenv0, root))
  }

  /**
    * Allocates a closure for the given definition `sym` with free variables `freeVars` under the given environment `env0`.
    */
  private def allocateClosure(sym: Symbol.DefnSym, freeVars: Array[ExecutableAst.FreeVar], env0: Map[String, AnyRef]): Value.Closure = {
    // Save the values of the free variables in the Value.Closure structure.
    // When the closure is called, these values will be provided at the beginning of the argument list.
    val bindings = new Array[AnyRef](freeVars.length)
    var i = 0
    while (i < bindings.length) {
      // A value might be absent from the the environment if it is recursively bound.
      env0.get(freeVars(i).sym.toString) match {
        case None => // Ok, value probably recursive.
        case Some(v) =>
          bindings(i) = v
      }
      i = i + 1
    }
    Value.Closure(sym, bindings)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Casts                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Casts the given reference `ref` to a primitive boolean.
    */
  private def cast2bool(ref: Any): Boolean = ref match {
    case Value.True => true
    case Value.False => false
    case _ => throw InternalRuntimeException(s"Unexpected non-bool value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to a primitive char.
    */
  private def cast2char(ref: AnyRef): Char = ref match {
    case Value.Char(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-char value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to a Float32.
    */
  private def cast2float32(ref: AnyRef): Float = ref match {
    case Value.Float32(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-float32 value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to a Float64.
    */
  private def cast2float64(ref: AnyRef): Double = ref match {
    case Value.Float64(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-float64 value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to an int8.
    */
  private def cast2int8(ref: AnyRef): Byte = ref match {
    case Value.Int8(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-int8 value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to an int16.
    */
  private def cast2int16(ref: AnyRef): Short = ref match {
    case Value.Int16(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-int16 value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to an int32.
    */
  private def cast2int32(ref: AnyRef): Int = ref match {
    case Value.Int32(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-int32 value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to an int64.
    */
  private def cast2int64(ref: AnyRef): Long = ref match {
    case Value.Int64(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-int64 value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to a java.math.BigInteger.
    */
  private def cast2bigInt(ref: AnyRef): java.math.BigInteger = ref match {
    case Value.BigInt(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-bigint value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to a string.
    */
  private def cast2str(ref: AnyRef): String = ref match {
    case Value.Str(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-str value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to a box.
    */
  private def cast2box(ref: AnyRef): Value.Box = ref match {
    case v: Value.Box => v
    case _ => throw InternalRuntimeException(s"Unexpected non-box value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to a closure.
    */
  private def cast2closure(ref: AnyRef): Value.Closure = ref match {
    case v: Value.Closure => v
    case _ => throw InternalRuntimeException(s"Unexpected non-closure value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to a tag.
    */
  private def cast2tag(ref: AnyRef): Value.Tag = ref match {
    case v: Value.Tag => v
    case _ => throw InternalRuntimeException(s"Unexpected non-tag value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Casts the given reference `ref` to a tuple.
    */
  private def cast2tuple(ref: AnyRef): Value.Tuple = ref match {
    case v: Value.Tuple => v
    case _ => throw InternalRuntimeException(s"Unexpected non-tuple value: ${ref.getClass.getCanonicalName}.")
  }

  /**
    * Constructs a bool from the given boolean `b`.
    */
  private def mkBool(b: Boolean): AnyRef = if (b) Value.True else Value.False


  /**
    * Returns the given reference `ref` as a Java object.
    */
  def toJava(ref: AnyRef): AnyRef = ref match {
    case Value.Unit => scala.Unit
    case Value.True => java.lang.Boolean.TRUE
    case Value.False => java.lang.Boolean.FALSE
    case Value.Char(lit) => new java.lang.Character(lit)
    case Value.Int8(lit) => new java.lang.Byte(lit)
    case Value.Int16(lit) => new java.lang.Short(lit)
    case Value.Int32(lit) => new java.lang.Integer(lit)
    case Value.Int64(lit) => new java.lang.Long(lit)
    case Value.BigInt(lit) => lit
    case Value.Str(lit) => lit
    case _ => ref
  }

  /**
    * Returns the given reference `ref` as a Value object.
    */
  def fromJava(ref: AnyRef): AnyRef = ref match {
    case scala.Unit => Value.Unit
    case o: java.lang.Boolean => if (o.booleanValue()) Value.True else Value.False
    case o: java.lang.Character => Value.Char(o.charValue())
    case o: java.lang.Byte => Value.Int8(o.byteValue())
    case o: java.lang.Short => Value.Int16(o.shortValue())
    case o: java.lang.Integer => Value.Int32(o.intValue())
    case o: java.lang.Long => Value.Int64(o.longValue())
    case o: java.math.BigInteger => Value.BigInt(o)
    case o: java.lang.String => Value.Str(o)
    case _ => ref
  }

}
