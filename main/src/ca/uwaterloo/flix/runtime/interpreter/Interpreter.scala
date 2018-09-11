/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.interpreter

import java.lang.reflect.{InvocationTargetException, Modifier}

import ca.uwaterloo.flix.api._
import ca.uwaterloo.flix.language.ast.FinalAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.runtime.{InvocationTarget, Linker}
import ca.uwaterloo.flix.runtime.solver._
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym
import ca.uwaterloo.flix.runtime.solver.api.{Attribute => _, Constraint => _, Lattice => _, Relation => _, _}
import ca.uwaterloo.flix.util.{InternalRuntimeException, Verbosity}
import ca.uwaterloo.flix.util.tc.Show._
import flix.runtime._

import scala.collection.mutable

object Interpreter {

  /**
    * Evaluates the given expression `exp0` under the given environment `env0`.
    */
  def eval(exp0: Expression, env0: Map[String, AnyRef], henv0: Map[Symbol.EffSym, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root)(implicit flix: Flix): AnyRef = exp0 match {
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

    case Expression.Var(sym, _, loc) => env0.get(sym.toString) match {
      case None => throw InternalRuntimeException(s"Key '${sym.toString}' not found in environment.")
      case Some(v) => v
    }

    case Expression.Closure(sym, freeVars, _, _, _) =>
      allocateClosure(sym, freeVars.toArray, env0)

    case Expression.ApplyClo(exp, args, _, _) =>
      val clo = eval(exp, env0, henv0, lenv0, root)
      invokeClo(clo, args, env0, henv0, lenv0, root)

    case Expression.ApplyDef(sym, args, _, _) => invokeDef(sym, args, env0, henv0, lenv0, root)

    case Expression.ApplyEff(sym, args, tpe, loc) => invokeEff(sym, args, env0, henv0, lenv0, root)

    case Expression.ApplyCloTail(exp, args, _, _) =>
      val clo = eval(exp, env0, henv0, lenv0, root)
      invokeClo(clo, args, env0, henv0, lenv0, root)

    case Expression.ApplyDefTail(sym, args, _, _) => invokeDef(sym, args, env0, henv0, lenv0, root)

    case Expression.ApplyEffTail(sym, args, _, _) => invokeEff(sym, args, env0, henv0, lenv0, root)

    case Expression.ApplySelfTail(sym, _, args, _, _) => invokeDef(sym, args, env0, henv0, lenv0, root)

    case Expression.Unary(sop, op, exp, _, _) =>
      evalUnary(sop, exp, env0, henv0, lenv0, root)

    case Expression.Binary(sop, op, exp1, exp2, _, _) => evalBinary(sop, exp1, exp2, env0, henv0, lenv0, root)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _) =>
      val cond = cast2bool(eval(exp1, env0, henv0, lenv0, root))
      if (cond) eval(exp2, env0, henv0, lenv0, root) else eval(exp3, env0, henv0, lenv0, root)

    case Expression.Branch(exp, branches, tpe, loc) => eval(exp, env0, henv0, branches, root)

    case Expression.JumpTo(sym, tpe, loc) =>
      lenv0.get(sym) match {
        case None => throw InternalRuntimeException(s"Unknown label: '$sym' in label environment ${lenv0.mkString(" ,")}.")
        case Some(e) => eval(e, env0, henv0, lenv0, root)
      }

    case Expression.Let(sym, exp1, exp2, _, _) =>
      val newEnv = env0 + (sym.toString -> eval(exp1, env0, henv0, lenv0, root))
      eval(exp2, newEnv, henv0, lenv0, root)

    case Expression.LetRec(sym, exp1, exp2, _, _) => exp1 match {
      case Expression.Closure(ref, freeVars, _, _, _) =>
        // Allocate a circular closure.
        val closure = allocateClosure(ref, freeVars.toArray, env0)
        closure.bindings(sym.getStackOffset) = closure

        // Evaluate the body expression under the extended environment.
        val newEnv = env0 + (sym.toString -> closure)
        eval(exp2, newEnv, henv0, lenv0, root)
      case _ => throw InternalRuntimeException(s"Non-closure letrec value: ${exp1.getClass.getName}.")
    }

    case Expression.Is(sym, tag, exp, _) => mkBool(cast2tag(eval(exp, env0, henv0, lenv0, root)).tag == tag)

    case Expression.Tag(sym, tag, exp, _, _) => Value.Tag(sym, tag, eval(exp, env0, henv0, lenv0, root))

    case Expression.Untag(sym, tag, exp, _, _) => cast2tag(eval(exp, env0, henv0, lenv0, root)).value

    case Expression.Index(base, offset, _, _) =>
      val tuple = cast2tuple(eval(base, env0, henv0, lenv0, root))
      tuple.elms(offset)

    case Expression.Tuple(elms, _, _) =>
      val es = elms.map(e => eval(e, env0, henv0, lenv0, root))
      Value.Tuple(es)

    case Expression.ArrayLit(elms, tpe, _) =>
      val es = elms.map(e => eval(e, env0, henv0, lenv0, root))
      Value.Arr(es.toArray, tpe.typeArguments.head)

    case Expression.ArrayNew(elm, len, tpe, _) =>
      val e = eval(elm, env0, henv0, lenv0, root)
      val ln = cast2int32(eval(len, env0, henv0, lenv0, root))
      val array = new Array[AnyRef](ln)
      for (i <- 0 until ln) {
        array(i) = e
      }
      Value.Arr(array, tpe.typeArguments.head)

    case Expression.ArrayLoad(base, index, _, _) =>
      val array = cast2array(eval(base, env0, henv0, lenv0, root))
      val indexCasted = cast2int32(eval(index, env0, henv0, lenv0, root))
      if (0 <= indexCasted && indexCasted < array.elms.length)
        array.elms(indexCasted)
      else
        throw InternalRuntimeException(s"Array index out of bounds: $index  Array length: ${array.elms.length}.")

    case Expression.ArrayStore(base, index, elm, _, _) =>
      val array = cast2array(eval(base, env0, henv0, lenv0, root))
      val indexCasted = cast2int32(eval(index, env0, henv0, lenv0, root))
      val obj = eval(elm, env0, henv0, lenv0, root)
      if (0 <= indexCasted && indexCasted < array.elms.length) {
        array.elms(indexCasted) = obj
        Value.Unit
      }
      else
        throw InternalRuntimeException(s"Array index out of bounds: $index  Array length: ${array.elms.length}.")

    case Expression.ArrayLength(base, _, _) =>
      val array = cast2array(eval(base, env0, henv0, lenv0, root))
      Value.Int32(array.elms.length)

    case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
      val array = cast2array(eval(base, env0, henv0, lenv0, root))
      val i1Casted = cast2int32(eval(startIndex, env0, henv0, lenv0, root))
      val i2Casted = cast2int32(eval(endIndex, env0, henv0, lenv0, root))

      if (i1Casted >= i2Casted)
        throw InternalRuntimeException(s"startIndex >= endIndex, startIndex: ${i1Casted}  endIndex: ${i2Casted}.")
      else if (i1Casted < 0)
        throw InternalRuntimeException(s"Invalid startIndex: ${i1Casted}.")
      else if (i2Casted > array.elms.length)
        throw InternalRuntimeException(s"endIndex out of bounds: ${i2Casted}    Array length: ${array.elms.length}.")
      else {
        val resultArray = new Array[AnyRef](i2Casted - i1Casted)

        for (i <- i1Casted until i2Casted) {
          resultArray(i - i1Casted) = array.elms(i)
        }
        Value.Arr(resultArray, tpe.typeArguments.head)
      }

    case Expression.Ref(exp, tpe, loc) =>
      val box = new Value.Box()
      val value = eval(exp, env0, henv0, lenv0, root)
      box.setValue(value)
      box

    case Expression.Deref(exp, tpe, loc) =>
      val box = cast2box(eval(exp, env0, henv0, lenv0, root))
      box.getValue

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val box = cast2box(eval(exp1, env0, henv0, lenv0, root))
      val value = eval(exp2, env0, henv0, lenv0, root)
      box.setValue(value)
      Value.Unit

    case Expression.HandleWith(exp, bindings, tpe, loc) =>
      // Evaluate each handler expression and construct the new handler environment.
      val henv = bindings.foldLeft(henv0) {
        case (macc, HandlerBinding(sym, handler)) => macc + (sym -> eval(handler, env0, henv0, lenv0, root))
      }
      // Evaluate the expression in the new handler environment.
      eval(exp, env0, henv, lenv0, root)

    case Expression.TryCatch(exp, rules, tpe, loc) =>
      try {
        eval(exp, env0, henv0, lenv0, root)
      } catch {
        case ex: Throwable =>
          val exceptionClass = ex.getClass
          for (CatchRule(sym, clazz, body) <- rules) {
            if (clazz.isAssignableFrom(exceptionClass)) {
              val env1 = env0 + (sym.toString -> ex)
              return eval(body, env1, henv0, lenv0, root)
            }
          }
          // Fallthrough, rethrow the exception.
          throw ex
      }

    case Expression.NativeConstructor(constructor, args, tpe, loc) =>
      val values = evalArgs(args, env0, henv0, lenv0, root).map(toJava)
      val arguments = values.toArray
      fromJava(constructor.newInstance(arguments: _*).asInstanceOf[AnyRef])

    case Expression.NativeField(field, tpe, loc) =>
      val clazz = field.getDeclaringClass
      fromJava(field.get(clazz))

    case Expression.NativeMethod(method, args, tpe, loc) => try {
      val values = evalArgs(args, env0, henv0, lenv0, root).map(toJava)
      if (Modifier.isStatic(method.getModifiers)) {
        val arguments = values.toArray
        fromJava(method.invoke(null, arguments: _*))
      } else {
        val thisObj = values.head
        val arguments = values.tail.toArray
        fromJava(method.invoke(thisObj, arguments: _*))
      }
    } catch {
      case ex: InvocationTargetException => throw ex.getTargetException
    }

    case Expression.NewRelation(sym, tpe, loc) =>
      val attr = root.relations(sym).attr.map {
        case Attribute(name, _) => new api.Attribute(name)
      }
      new api.Relation(sym.name, attr.toArray)

    case Expression.NewLattice(sym, tpe, loc) =>
      val attr = root.lattices(sym).attr.map {
        case Attribute(name, _) => new api.Attribute(name)
      }
      new api.Lattice(sym.name, attr.init.toArray, attr.last, /* TODO*/ null)

    case Expression.Constraint(c, tpe, loc) =>
      evalConstraint(c, env0, henv0, lenv0, root)

    case Expression.ConstraintUnion(exp1, exp2, tpe, loc) =>
      val v1 = cast2constraintset(eval(exp1, env0, henv0, lenv0, root))
      val v2 = cast2constraintset(eval(exp2, env0, henv0, lenv0, root))
      v1.union(v2)

    case Expression.FixpointSolve(exp, tpe, loc) =>
      val cs = cast2constraintset(eval(exp, env0, henv0, lenv0, root))
      val fixpoint = solve(cs)
      fixpoint.toString

    case Expression.FixpointCheck(exp, tpe, loc) =>
      // TODO
      val cs = cast2constraintset(eval(exp, env0, henv0, lenv0, root))
      println(cs)
      val solver = mkSolver(cs)
      try {
        val fixpoint = solve(cs)
        println(fixpoint)
        Value.True
      } catch {
        case ex: RuleError => Value.False
      }

    case Expression.FixpointDelta(exp, tpe, loc) =>
      val cs = cast2constraintset(eval(exp, env0, henv0, lenv0, root))
      deltaSolve(cs)

    case Expression.UserError(_, loc) => throw new NotImplementedError(loc.reified)

    case Expression.HoleError(sym, _, loc) => throw new HoleError(sym.toString, loc.reified)

    case Expression.MatchError(_, loc) => throw new MatchError(loc.reified)

    case Expression.SwitchError(_, loc) => throw new SwitchError(loc.reified)

    case Expression.Existential(params, exp, loc) => throw InternalRuntimeException(s"Unexpected expression: '$exp' at ${loc.source.format}.")

    case Expression.Universal(params, exp, loc) => throw InternalRuntimeException(s"Unexpected expression: '$exp' at ${loc.source.format}.")
  }

  /**
    * Applies the given unary semantic operator `sop` to the value of the expression `exp0` under the environment `env0`
    */
  private def evalUnary(sop: SemanticOperator, exp0: Expression, env0: Map[String, AnyRef], henv0: Map[Symbol.EffSym, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root)(implicit flix: Flix): AnyRef = {
    // Evaluate the operand.
    val v = eval(exp0, env0, henv0, lenv0, root)

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
  private def evalBinary(sop: SemanticOperator, exp1: Expression, exp2: Expression, env0: Map[String, AnyRef], henv0: Map[Symbol.EffSym, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root)(implicit flix: Flix): AnyRef = {

    def evalBoolOp(sop: SemanticOperator.BoolOp): AnyRef = {
      // Evaluate the left operand.
      val v1 = cast2bool(eval(exp1, env0, henv0, lenv0, root))

      sop match {
        // Lazy operators.
        case SemanticOperator.BoolOp.And if v1 => mkBool(cast2bool(eval(exp2, env0, henv0, lenv0, root)))
        case SemanticOperator.BoolOp.And => mkBool(false)

        // Lazy operators.
        case SemanticOperator.BoolOp.Or if v1 => mkBool(true)
        case SemanticOperator.BoolOp.Or => mkBool(cast2bool(eval(exp2, env0, henv0, lenv0, root)))

        case SemanticOperator.BoolOp.Eq => mkBool(v1 == cast2bool(eval(exp2, env0, henv0, lenv0, root)))
        case SemanticOperator.BoolOp.Neq => mkBool(v1 != cast2bool(eval(exp2, env0, henv0, lenv0, root)))
        case _ => throw InternalRuntimeException(s"Unexpected Semantic Operator: '$sop'.")
      }
    }

    def evalCharOp(sop: SemanticOperator.CharOp): AnyRef = {
      // Evaluate the operands.
      val v1 = eval(exp1, env0, henv0, lenv0, root)
      val v2 = eval(exp2, env0, henv0, lenv0, root)

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
      val v1 = eval(exp1, env0, henv0, lenv0, root)
      val v2 = eval(exp2, env0, henv0, lenv0, root)

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
      val v1 = eval(exp1, env0, henv0, lenv0, root)
      val v2 = eval(exp2, env0, henv0, lenv0, root)

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
      val v1 = eval(exp1, env0, henv0, lenv0, root)
      val v2 = eval(exp2, env0, henv0, lenv0, root)

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
      val v1 = eval(exp1, env0, henv0, lenv0, root)
      val v2 = eval(exp2, env0, henv0, lenv0, root)

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
      val v1 = eval(exp1, env0, henv0, lenv0, root)
      val v2 = eval(exp2, env0, henv0, lenv0, root)

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
      val v1 = eval(exp1, env0, henv0, lenv0, root)
      val v2 = eval(exp2, env0, henv0, lenv0, root)

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
      val v1 = eval(exp1, env0, henv0, lenv0, root)
      val v2 = eval(exp2, env0, henv0, lenv0, root)

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
      val v1 = eval(exp1, env0, henv0, lenv0, root)
      val v2 = eval(exp2, env0, henv0, lenv0, root)

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
    * Invokes the given closure value `clo` with the given arguments `args` under the given environment `env0`.
    */
  private def invokeClo(clo: AnyRef, args: List[Expression], env0: Map[String, AnyRef], henv0: Map[Symbol.EffSym, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root)(implicit flix: Flix): AnyRef = {
    val Value.Closure(name, bindings) = cast2closure(clo)
    val as = evalArgs(args, env0, henv0, lenv0, root)
    val constant = root.defs(name)
    // Bindings for the capture variables are passed as arguments.
    val env1 = constant.formals.take(bindings.length).zip(bindings).foldLeft(env0) {
      case (macc, (formal, actual)) => macc + (formal.sym.toString -> actual)
    }
    // Now pass the actual arguments supplied by the caller.
    val env2 = constant.formals.drop(bindings.length).zip(as).foldLeft(env1) {
      case (macc, (formal, actual)) => macc + (formal.sym.toString -> actual)
    }
    eval(constant.exp, env2, henv0, Map.empty, root)
  }

  /**
    * Invokes the given definition `sym` with the given arguments `args` under the given environment `env0`.
    */
  private def invokeDef(sym: Symbol.DefnSym, args: List[Expression], env0: Map[String, AnyRef], henv0: Map[Symbol.EffSym, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root)(implicit flix: Flix): AnyRef = {
    // Lookup the definition.
    val defn = root.defs(sym)

    // Evaluate the arguments.
    val as = evalArgs(args, env0, henv0, lenv0, root)

    // Construct the new environment by pairing the formal parameters with the actual arguments.
    val env = defn.formals.zip(as).foldLeft(Map.empty[String, AnyRef]) {
      case (macc, (FormalParam(arg, _), v)) => macc + (arg.toString -> v)
    }

    // Evaluate the body expression under the new local variable environment and an empty label environment.
    eval(defn.exp, env, henv0, Map.empty, root)
  }

  /**
    * Invokes the given definition `sym` with the given arguments `args` under the given environment `env0`.
    */
  private def invokeEff(sym: Symbol.EffSym, args: List[Expression], env0: Map[String, AnyRef], henv0: Map[Symbol.EffSym, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root)(implicit flix: Flix): AnyRef = {
    // Evaluate the arguments.
    val as = evalArgs(args, env0, henv0, lenv0, root)

    // Lookup the effect symbol in the current handler environment.
    henv0.get(sym) match {
      case Some(value) =>
        // Case 1: Handler found.
        val clo = cast2closure(value)
        invokeClo(clo, args, env0, henv0, lenv0, root)
      case None =>
        // Case 2: No handler found. Try the default handler.
        root.handlers.get(sym) match {
          case None => throw InternalRuntimeException(s"No default effect handler for: '$sym'.")
          case Some(handler) =>
            // Bind arguments to formal parameters.
            val env = handler.fparams.zip(as).foldLeft(Map.empty[String, AnyRef]) {
              case (macc, (fparam, value)) => macc + (fparam.sym.toString -> value)
            }
            // Evaluate the body of the handler.
            // TODO: What handler environment should be used here?
            eval(handler.exp, env, henv0, Map.empty, root)
        }
    }
  }

  /**
    * Evaluates the given list of expressions `exps` under the given environment `env0` to a list of values.
    */
  private def evalArgs(exps: List[Expression], env0: Map[String, AnyRef], henv0: Map[Symbol.EffSym, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root)(implicit flix: Flix): List[AnyRef] = {
    exps.map(a => eval(a, env0, henv0, lenv0, root))
  }

  /**
    * Allocates a closure for the given definition `sym` with free variables `freeVars` under the given environment `env0`.
    */
  private def allocateClosure(sym: Symbol.DefnSym, freeVars: Array[FinalAst.FreeVar], env0: Map[String, AnyRef]): Value.Closure = {
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

  /**
    * Evaluates the given constraint `c0` to a constraint value under the given environment `env0`.
    */
  private def evalConstraint(c0: Constraint, env0: Map[String, AnyRef], henv0: Map[Symbol.EffSym, AnyRef], lenv0: Map[Symbol.LabelSym, Expression], root: Root)(implicit flix: Flix): AnyRef = {
    implicit val _ = root
    implicit val cache = new SymbolCache

    val cparams = c0.cparams.map(p => cache.getVarSym(p.sym))
    val head = evalHeadPredicate(c0.head, env0)
    val body = c0.body.map(b => evalBodyPredicate(b, env0))

    val constraint = new api.Constraint(cparams.toArray, head, body.toArray)

    new ConstraintSet(Array(constraint))
  }

  /**
    * Evaluates the given head predicate `h0` under the given environment `env0` to a head predicate value.
    */
  private def evalHeadPredicate(h0: FinalAst.Predicate.Head, env0: Map[String, AnyRef])(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): api.predicate.Predicate = h0 match {
    case FinalAst.Predicate.Head.True(_) => new api.predicate.TruePredicate()
    case FinalAst.Predicate.Head.False(_) => new api.predicate.FalsePredicate()
    case FinalAst.Predicate.Head.RelAtom(baseOpt, sym, terms, _) =>
      // Retrieve the relation.
      val relation = baseOpt match {
        case None => getRelation(sym)
        case Some(baseSym) =>
          cast2relation(env0(baseSym.toString))
      }
      val ts = terms.map(t => evalHeadTerm(t, env0))
      new api.predicate.AtomPredicate(relation, positive = true, ts.toArray, null)
    case FinalAst.Predicate.Head.LatAtom(baseOpt, sym, terms, _) =>
      // Retrieve the lattice.
      val lattice = baseOpt match {
        case None => getLattice(sym)
        case Some(baseSym) =>
          cast2lattice(env0(baseSym.toString))
      }
      val ts = terms.map(t => evalHeadTerm(t, env0))
      new api.predicate.AtomPredicate(lattice, positive = true, ts.toArray, null)
  }

  /**
    * Evaluates the given body predicate `b0` under the given environment `env0` to a body predicate value.
    */
  private def evalBodyPredicate(b0: FinalAst.Predicate.Body, env0: Map[String, AnyRef])(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): api.predicate.Predicate = b0 match {
    case FinalAst.Predicate.Body.RelAtom(baseOpt, sym, polarity, terms, index2sym, loc) =>
      // Retrieve the relation.
      val relation = baseOpt match {
        case None => getRelation(sym)
        case Some(baseSym) =>
          cast2relation(env0(baseSym.toString))
      }

      val p = polarity match {
        case Ast.Polarity.Positive => true
        case Ast.Polarity.Negative => false
      }
      val ts = terms.map(t => evalBodyTerm(t, env0))
      // TODO: Get rid of i2s
      val i2s = index2sym map {
        case x if x != null => cache.getVarSym(x)
        case _ => null
      }
      new api.predicate.AtomPredicate(relation, p, ts.toArray, i2s.toArray)

    case FinalAst.Predicate.Body.LatAtom(baseOpt, sym, polarity, terms, index2sym, loc) =>
      // Retrieve the lattice.
      val lattice = baseOpt match {
        case None => getLattice(sym)
        case Some(baseSym) =>
          cast2lattice(env0(baseSym.toString))
      }
      val p = polarity match {
        case Ast.Polarity.Positive => true
        case Ast.Polarity.Negative => false
      }
      val ts = terms.map(t => evalBodyTerm(t, env0))
      // TODO: Get rid of i2s
      val i2s = index2sym map {
        case x if x != null => cache.getVarSym(x)
        case _ => null
      }
      new api.predicate.AtomPredicate(lattice, p, ts.toArray, i2s.toArray)

    case FinalAst.Predicate.Body.Filter(sym, terms, loc) =>
      val f = (as: Array[AnyRef]) => Linker.link(sym, root).invoke(as).getValue.asInstanceOf[Boolean].booleanValue()
      val ts = terms.map(t => evalBodyTerm(t, env0))
      new api.predicate.FilterPredicate(f, ts.toArray)

    case FinalAst.Predicate.Body.Functional(varSym, defSym, terms, loc) =>
      val s = cache.getVarSym(varSym)
      val f = (as: Array[AnyRef]) => Linker.link(defSym, root).invoke(as).getValue.asInstanceOf[Array[ProxyObject]]
      new api.predicate.FunctionalPredicate(s, f, terms.map(t => cache.getVarSym(t)).toArray)
  }

  /**
    * Evaluates the given head term `t0` under the given environment `env0` to a head term value.
    */
  private def evalHeadTerm(t0: FinalAst.Term.Head, env0: Map[String, AnyRef])(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): api.term.Term = t0 match {
    //
    // Free Variables (i.e. variables that are quantified over in the constraint).
    //
    case FinalAst.Term.Head.QuantVar(sym, _, _) =>
      // Lookup the corresponding symbol in the cache.

      new api.term.VarTerm(cache.getVarSym(sym))

    //
    // Bound Variables (i.e. variables that have a value in the local environment).
    //
    case FinalAst.Term.Head.CapturedVar(sym, tpe, _) =>
      // Retrieve the value from the local environment and wrap it in a proxy object.
      val v = wrapValueInProxyObject(env0(sym.toString), tpe)

      // Construct a literal term with a function that evaluates to the proxy object.
      new api.term.LitTerm(() => v)

    //
    // Literals.
    //
    case FinalAst.Term.Head.Lit(sym, _, _) =>
      // Construct a literal term with a function that invokes another function which returns the literal.
      new api.term.LitTerm(() => Linker.link(sym, root).invoke(Array.emptyObjectArray))

    //
    // Applications.
    //
    case FinalAst.Term.Head.App(sym, args, _, _) =>
      // Construct a function that when invoked applies the underlying function.
      val f = (args: Array[AnyRef]) => Linker.link(sym, root).invoke(args)
      val as = args.map(cache.getVarSym)
      new api.term.AppTerm(f, as.toArray)
  }

  /**
    * Evaluates the given body term `t0` under the given environment `env0` to a body term value.
    */
  private def evalBodyTerm(t0: FinalAst.Term.Body, env0: Map[String, AnyRef])(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): api.term.Term = t0 match {
    //
    // Wildcards.
    //
    case FinalAst.Term.Body.Wild(_, _) => new api.term.WildTerm()

    //
    // Free Variables (i.e. variables that are quantified over in the constraint).
    //
    case FinalAst.Term.Body.QuantVar(sym, _, _) =>
      // Lookup the corresponding symbol in the cache.
      new api.term.VarTerm(cache.getVarSym(sym))

    //
    // Bound Variables (i.e. variables that have a value in the local environment).
    //
    case FinalAst.Term.Body.CapturedVar(sym, tpe, _) =>
      // Retrieve the value from the local environment and wrap it in a proxy object.
      val v = wrapValueInProxyObject(env0(sym.toString), tpe)

      // Construct a literal term with a function that evaluates to the proxy object.
      new api.term.LitTerm(() => v)

    //
    // Literals.
    //
    case FinalAst.Term.Body.Lit(sym, _, _) =>
      // Construct a literal term with a function that invokes another function which returns the literal.
      new api.term.LitTerm(() => Linker.link(sym, root).invoke(Array.emptyObjectArray))
  }

  /**
    * Returns the relation value associated with the given relation symbol `sym`.
    *
    * NB: Allocates a new relation if no such relation exists in the cache.
    */
  private def getRelation(sym: Symbol.RelSym)(implicit root: FinalAst.Root, flix: Flix): api.RelationVar = root.relations(sym) match {
    case FinalAst.Relation(_, _, attr, _) =>
      val name = sym.toString
      val as = attr.map(a => new api.Attribute(a.name)).toArray
      new RelationVar(name, as)
  }

  /**
    * Returns the lattice value associated with the given lattice symbol `sym`.
    *
    * NB: Allocates a new lattice if no such lattice exists in the cache.
    */
  private def getLattice(sym: Symbol.LatSym)(implicit root: FinalAst.Root, flix: Flix): api.LatticeVar = root.lattices(sym) match {
    case FinalAst.Lattice(_, _, attr, _) =>
      val name = sym.toString
      val as = attr.map(a => new api.Attribute(a.name))
      val keys = as.init.toArray
      val value = as.last
      val ops = getLatticeOps(attr.last.tpe)
      new LatticeVar(name, keys, value, ops)
  }

  /**
    * Returns the lattice operations associated with the given type `tpe`.
    */
  private def getLatticeOps(tpe: Type)(implicit root: FinalAst.Root, flix: Flix): LatticeOps = {
    val lattice = root.latticeComponents(tpe)

    new LatticeOps {
      override def bot: ProxyObject = Linker.link(lattice.bot, root).invoke(Array.empty)

      override def equ: InvocationTarget = Linker.link(lattice.equ, root)

      override def leq: InvocationTarget = Linker.link(lattice.leq, root)

      override def lub: InvocationTarget = Linker.link(lattice.lub, root)

      override def glb: InvocationTarget = Linker.link(lattice.glb, root)
    }
  }

  /**
    * Returns the given value `v` of the given type `tpe` wrapped in a proxy object.
    */
  private def wrapValueInProxyObject(v: AnyRef, tpe: Type)(implicit root: FinalAst.Root, flix: Flix): ProxyObject = {
    // Retrieve the operator symbols.
    val eqSym = root.specialOps(SpecialOperator.Equality)(tpe)
    val hashSym = root.specialOps(SpecialOperator.HashCode)(tpe)
    val toStrSym = root.specialOps(SpecialOperator.ToString)(tpe)

    // Construct the operators.
    val eqOp = (x: AnyRef, y: AnyRef) => Linker.link(eqSym, root).invoke(Array(x, y)).getValue.asInstanceOf[Boolean]
    val hashOp = (x: AnyRef) => Linker.link(hashSym, root).invoke(Array(x)).getValue.asInstanceOf[Integer].intValue()
    val toStrOp = (x: AnyRef) => Linker.link(toStrSym, root).invoke(Array(x)).getValue.asInstanceOf[String]

    // Return the proxy object.
    new ProxyObject(v, eqOp, hashOp, toStrOp)
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
    case _ => throw InternalRuntimeException(s"Unexpected non-bool value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a primitive char.
    */
  private def cast2char(ref: AnyRef): Char = ref match {
    case Value.Char(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-char value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a Float32.
    */
  private def cast2float32(ref: AnyRef): Float = ref match {
    case Value.Float32(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-float32 value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a Float64.
    */
  private def cast2float64(ref: AnyRef): Double = ref match {
    case Value.Float64(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-float64 value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to an int8.
    */
  private def cast2int8(ref: AnyRef): Byte = ref match {
    case Value.Int8(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-int8 value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to an int16.
    */
  private def cast2int16(ref: AnyRef): Short = ref match {
    case Value.Int16(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-int16 value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to an int32.
    */
  private def cast2int32(ref: AnyRef): Int = ref match {
    case Value.Int32(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-int32 value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to an int64.
    */
  private def cast2int64(ref: AnyRef): Long = ref match {
    case Value.Int64(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-int64 value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a java.math.BigInteger.
    */
  private def cast2bigInt(ref: AnyRef): java.math.BigInteger = ref match {
    case Value.BigInt(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-bigint value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a string.
    */
  private def cast2str(ref: AnyRef): String = ref match {
    case Value.Str(lit) => lit
    case _ => throw InternalRuntimeException(s"Unexpected non-str value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a box.
    */
  private def cast2box(ref: AnyRef): Value.Box = ref match {
    case v: Value.Box => v
    case _ => throw InternalRuntimeException(s"Unexpected non-box value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a closure.
    */
  private def cast2closure(ref: AnyRef): Value.Closure = ref match {
    case v: Value.Closure => v
    case _ => throw InternalRuntimeException(s"Unexpected non-closure value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a tag.
    */
  private def cast2tag(ref: AnyRef): Value.Tag = ref match {
    case v: Value.Tag => v
    case _ => throw InternalRuntimeException(s"Unexpected non-tag value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a tuple.
    */
  private def cast2tuple(ref: AnyRef): Value.Tuple = ref match {
    case v: Value.Tuple => v
    case _ => throw InternalRuntimeException(s"Unexpected non-tuple value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to an array value.
    */
  private def cast2array(ref: AnyRef): Value.Arr = ref match {
    case v: Value.Arr => v
    case _ => throw InternalRuntimeException(s"Unexpected non-array value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a constraint value.
    */
  private def cast2constraintset(ref: AnyRef): ConstraintSet = ref match {
    case v: ConstraintSet => v
    case _ => throw InternalRuntimeException(s"Unexpected non-constraint value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a relation.
    */
  private def cast2relation(ref: AnyRef): api.Relation = ref match {
    case r: api.Relation => r
    case _ => throw InternalRuntimeException(s"Unexpected non-relation value: ${ref.getClass.getName}.")
  }

  /**
    * Casts the given reference `ref` to a lattice.
    */
  private def cast2lattice(ref: AnyRef): api.Lattice = ref match {
    case r: api.Lattice => r
    case _ => throw InternalRuntimeException(s"Unexpected non-lattice value: ${ref.getClass.getName}.")
  }

  /**
    * Constructs a bool from the given boolean `b`.
    */
  private def mkBool(b: Boolean): AnyRef = if (b) Value.True else Value.False

  /**
    * Computes the fixed point of the given constraint set `cs`.
    */
  private def solve(cs: ConstraintSet)(implicit flix: Flix): Fixpoint = {
    val solver = mkSolver(cs)
    solver.solve()
  }

  /**
    * Returns the minimal set of facts that fails to satisfy the given constraint set.
    */
  private def deltaSolve(cs: ConstraintSet)(implicit flix: Flix): String = {
    // Configure the fixpoint solver based on the Flix options.
    val options = new FixpointOptions
    options.setMonitored(flix.options.monitor)
    options.setThreads(flix.options.threads)
    options.setVerbose(flix.options.verbosity == Verbosity.Verbose)

    // Construct the solver.
    val deltaSolver = new DeltaSolver(cs, options)
    deltaSolver.deltaSolve()
  }

  /**
    * Returns a fixpoint solver for the given constraint set `cs`.
    */
  private def mkSolver(cs: ConstraintSet)(implicit flix: Flix): Solver = {
    // Configure the fixpoint solver based on the Flix options.
    val options = new FixpointOptions
    options.setMonitored(flix.options.monitor)
    options.setThreads(flix.options.threads)
    options.setVerbose(flix.options.verbosity == Verbosity.Verbose)

    // Construct the solver.
    new Solver(cs.complete(), options)
  }

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
    case Value.Arr(elms, tpe) =>
      // TODO: Should cases for all primitive types be added?
      tpe match {
        case Type.Str =>
          // Convert an object array to a string array.
          val result = new Array[String](elms.length)
          for (i <- elms.indices) {
            result(i) = toJava(elms(i)).asInstanceOf[String]
          }
          result
        case Type.Int32 =>
          // Convert an object array to an int array.
          val result = new Array[Int](elms.length)
          for (i <- elms.indices) {
            result(i) = toJava(elms(i)).asInstanceOf[Int]
          }
          result
        case _ if tpe.isTuple =>
          elms
        case _ => throw InternalRuntimeException(s"Unable to construct array of type: '${tpe.show}'.")
      }
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


  // Class used to ensure that the symbols share the same object by identity.
  private class SymbolCache {

    val varSyms = mutable.Map.empty[Symbol.VarSym, VarSym]

    def getVarSym(sym: Symbol.VarSym): VarSym =
      varSyms.get(sym) match {
        case None =>
          val newSym = new VarSym(sym.text)
          varSyms += (sym -> newSym)
          newSym
        case Some(res) => res
      }

  }

}
