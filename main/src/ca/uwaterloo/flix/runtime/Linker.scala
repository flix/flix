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
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.language.ast.Symbol
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
            linkCompiled(defn)
        }
    }
  }

  /**
    * Returns an invocation target for the given definition `defn` that is interpreted.
    */
  private def linkInterpreted(defn: Def, root: Root)(implicit flix: Flix): InvocationTarget = new InvocationTarget {
    override def invoke(args: Array[AnyRef]): AnyRef = {
      // Extend the environment with the values of the actual arguments.
      val env0 = defn.formals.zip(args).foldLeft(Map.empty[String, AnyRef]) {
        case (macc, (FormalParam(name, tpe), actual)) => macc + (name.toString -> Interpreter.fromJava(actual))
      }
      // The initial label environment is empty.
      val lenv0 = Map.empty[Symbol.LabelSym, Expression]

      // Evaluate the function body.
      val result = Interpreter.eval(defn.exp, env0, Map.empty, root)

      // Convert the result to its Java representation.
      Interpreter.toJava(result)
    }
  }

  /**
    * Returns an invocation target for the given definition `defn` that is compiled.
    */
  private def linkCompiled(defn: Def)(implicit flix: Flix): InvocationTarget = new InvocationTarget {
    override def invoke(args: Array[AnyRef]): AnyRef =
      try {
        // Java Reflective Call.
        defn.method.invoke(null, args: _*)
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
  }

}
