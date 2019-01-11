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
import ca.uwaterloo.flix.language.ast.{SpecialOperator, Symbol, MonoType}
import ca.uwaterloo.flix.runtime.interpreter.Interpreter
import ca.uwaterloo.flix.util.{Evaluation, InternalRuntimeException}
import flix.runtime.ProxyObject

object Linker {

  // TODO: Completely remove the linker...

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
          case Evaluation.Interpreted => ???
          case Evaluation.Compiled =>
            if (defn.method == null) {
              throw InternalRuntimeException(s"Undefined reflective method object for symbol: '$sym'.")
            }
            linkCompiled(defn, root)
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

        val MonoType.Arrow(targs, tresult) = defn.tpe

        // Eq, Hash, and toString
        val eq = getEqOp(tresult, root)
        val hash = getHashOp(tresult, root)
        val toString = getToStrOp(tresult, root)

        // Create the proxy object.
        ProxyObject.of(result, eq, hash, toString)
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
  }


  /**
    * Returns a Java function that computes equality of two raw Flix values.
    */
  def getEqOp(tpe: MonoType, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], ProxyObject] = (a: Array[AnyRef]) => {
    val x = a(0)
    val y = a(1)
    val sym = root.specialOps(SpecialOperator.Equality)(tpe)
    link(sym, root).invoke(Array(x, y))
  }

  /**
    * Returns a Java function that computes the hashCode of a raw Flix value.
    */
  def getHashOp(tpe: MonoType, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], ProxyObject] = (a: Array[AnyRef]) => {
    val x = a(0)
    val sym = root.specialOps(SpecialOperator.HashCode)(tpe)
    link(sym, root).invoke(Array(x))
  }

  /**
    * Returns a Java function that computes the string representation of a raw Flix value.
    */
  def getToStrOp(tpe: MonoType, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], ProxyObject] = (a: Array[AnyRef]) => {
    val x = a(0)
    val sym = root.specialOps(SpecialOperator.ToString)(tpe)
    link(sym, root).invoke(Array(x))
  }

}
