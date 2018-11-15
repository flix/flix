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

package ca.uwaterloo.flix.runtime

import java.lang.reflect.InvocationTargetException

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst._
import ca.uwaterloo.flix.language.ast.{SpecialOperator, Symbol, Type}
import ca.uwaterloo.flix.runtime.interpreter.Interpreter
import ca.uwaterloo.flix.runtime.solver.api.ProxyObject
import ca.uwaterloo.flix.util.{Evaluation, InternalRuntimeException}

object Linker {

  /**
    * Returns an invocation target for the Flix function corresponding to the given symbol `sym`.
    */
  def link(sym: Symbol.DefnSym, root: Root)(implicit flix: Flix): InvocationTarget = {
    // Lookup the definition symbol in the program.
    root.defs.get(sym) match {
      case None => throw InternalRuntimeException(s"Undefined symbol: '$sym'.")
      case Some(defn) =>
        // Determine whether to invoke the interpreted or compiled code.
        flix.options.evaluation match {
          case Evaluation.Interpreted => linkInterpreted(defn, root)
          case Evaluation.Compiled =>
            if (defn.method == null) {
              throw InternalRuntimeException(s"Undefined reflective method object for symbol: '$sym'.")
            }
            linkCompiled(defn, root)
        }
    }
  }

  /**
    * Returns an invocation target for the given definition `defn` that is interpreted.
    */
  private def linkInterpreted(defn: Def, root: Root)(implicit flix: Flix): InvocationTarget = new InvocationTarget {
    override def invoke(args: Array[AnyRef]): ProxyObject = {
      // Extend the environment with the values of the actual arguments.
      val env0 = defn.formals.zip(args).foldLeft(Map.empty[String, AnyRef]) {
        case (macc, (FormalParam(name, tpe), actual)) => macc + (name.toString -> Interpreter.fromJava(actual))
      }
      // The initial label environment is empty.
      val lenv0 = Map.empty[Symbol.LabelSym, Expression]

      // Evaluate the function body.
      val result = Interpreter.toJava(Interpreter.eval(defn.exp, env0, Map.empty, Map.empty, root))

      // Immediately return the result if it is already a proxy object.
      if (result.isInstanceOf[ProxyObject]) {
        return result.asInstanceOf[ProxyObject]
      }

      // Retrieve the value type.
      val resultType = defn.tpe.typeArguments.last

      // Check whether the value is an array.
      // NB: This is a hack to get functional predicates to work.
      if (resultType.typeConstructor != Type.Array) {
        // Case 1: Non-array value.

        // Retrieve operations.
        val eq = getEqOp(resultType, root)
        val hash = getHashOp(resultType, root)
        val toString = getToStrOp(resultType, root)

        // Create the proxy object.
        new ProxyObject(result, eq, hash, toString)
      } else {
        // Case 2: Array value.

        // Retrieve the wrapped array.
        val wrappedArray = getWrappedArray(result, resultType, root)

        // Construct the wrapped array object.
        new ProxyObject(wrappedArray, null, null, null)
      }

    }
  }

  /**
    * Returns an invocation target for the given definition `defn` that is compiled.
    */
  private def linkCompiled(defn: Def, root: Root)(implicit flix: Flix): InvocationTarget = new InvocationTarget {
    override def invoke(args: Array[AnyRef]): ProxyObject =
      try {
        // Java Reflective Call.
        val as = if (args.isEmpty) Array(null) else args

        // Check the number of arguments.
        if (defn.method.getParameterCount != as.length) {
          throw InternalRuntimeException(s"Expected ${defn.method.getParameterCount} arguments, but got: ${as.length} for method ${defn.method.getName}.")
        }

        val result = defn.method.invoke(null, as: _*)

        // Eq, Hash, and toString
        val resultType = defn.tpe.typeArguments.last
        val eq = getEqOp(resultType, root)
        val hash = getHashOp(resultType, root)
        val toString = getToStrOp(resultType, root)

        // Create the proxy object.
        new ProxyObject(result, eq, hash, toString)
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
  }

  /**
    * Returns the given array `result` with all its values wrapped in proxy object.
    */
  private def getWrappedArray(result: AnyRef, tpe: Type, root: Root)(implicit flix: Flix): Array[ProxyObject] = {
    // Wrap the array values in proxy objects.
    result match {
      case a: Array[Char] => a map (v => new ProxyObject(Char.box(v), null, null, null))

      case a: Array[Byte] => a map (v => new ProxyObject(Byte.box(v), null, null, null))
      case a: Array[Short] => a map (v => new ProxyObject(Short.box(v), null, null, null))
      case a: Array[Int] => a map (v => new ProxyObject(Int.box(v), null, null, null))
      case a: Array[Long] => a map (v => new ProxyObject(Long.box(v), null, null, null))

      case a: Array[Float] => a map (v => new ProxyObject(Float.box(v), null, null, null))
      case a: Array[Double] => a map (v => new ProxyObject(Double.box(v), null, null, null))

      case a: Array[AnyRef] => a map {
        case v =>
          // The type of the array elements.
          val elmType = tpe.typeArguments.head

          // Construct the wrapped element.
          new ProxyObject(v, getEqOp(elmType, root), getHashOp(elmType, root), getToStrOp(elmType, root))
      }
    }
  }

  /**
    * Returns a Java function that computes equality of two raw Flix values.
    */
  private def getEqOp(tpe: Type, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], java.lang.Boolean] = (a: Array[AnyRef]) => {
    val x = a(0)
    val y = a(1)
    val sym = root.specialOps(SpecialOperator.Equality)(tpe)
    link(sym, root).invoke(Array(x, y)).getValue match {
      case java.lang.Boolean.TRUE => true
      case java.lang.Boolean.FALSE => false
      case v => throw InternalRuntimeException(s"Unexpected value: '$v' of type '${v.getClass.getName}'.")
    }
  }

  /**
    * Returns a Java function that computes the hashCode of a raw Flix value.
    */
  private def getHashOp(tpe: Type, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], Integer] = (a: Array[AnyRef]) => {
    val x = a(0)
    val sym = root.specialOps(SpecialOperator.HashCode)(tpe)
    link(sym, root).invoke(Array(x)).getValue match {
      case i: java.lang.Integer => i.intValue()
      case v => throw InternalRuntimeException(s"Unexpected value: '$v' of type '${v.getClass.getName}'.")
    }
  }

  /**
    * Returns a Java function that computes the string representation of a raw Flix value.
    */
  private def getToStrOp(tpe: Type, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], String] = (a: Array[AnyRef]) => {
    val x = a(0)
    val sym = root.specialOps(SpecialOperator.ToString)(tpe)
    link(sym, root).invoke(Array(x)).getValue match {
      case s: java.lang.String => s
      case v => throw InternalRuntimeException(s"Unexpected value: '$v' of type '${v.getClass.getName}'.")
    }
  }

}
