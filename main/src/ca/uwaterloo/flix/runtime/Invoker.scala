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

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.ExecutableAst
import ca.uwaterloo.flix.util.InternalRuntimeException

object Invoker {

  // TODO: Rename to Linker, and introduce InvocationTarget(s).

  /**
    * Immediately invokes the Flix code corresponding to the given definition symbol `sym`.
    */
  def invoke(sym: Symbol.DefnSym, args: Array[AnyRef], root: ExecutableAst.Root, env0: Map[String, AnyRef] = Map.empty): AnyRef = { // TODO: Remove environment.
    // Lookup the definition symbol in the program.
    root.definitions.get(sym) match {
      case None => throw InternalRuntimeException(s"Undefined symbol: '$sym'.")
      case Some(defn) =>
        // Determine whether to invoke the interpreted or compiled code.
        if (defn.method == null) {
          invokeInterpreted(defn, args, root, env0)
        } else {
          invokeCompiled(defn, args)
        }
    }
  }

  /**
    * Invokes the interpreted Flix code.
    */
  private def invokeInterpreted(defn: ExecutableAst.Definition.Constant, args: Array[AnyRef], root: ExecutableAst.Root, env0: Map[String, AnyRef]): AnyRef = {  // TODO: Remove environment.
    // Extend the environment with the values of the actual arguments.
    val env = defn.formals.zip(args).foldLeft(env0) {
      case (macc, (ExecutableAst.FormalParam(name, tpe), actual)) => macc + (name.toString -> actual)
    }
    Interpreter.eval(defn.exp, root, env)
  }

  /**
    * Invokes the compiled Flix code.
    */
  private def invokeCompiled(defn: ExecutableAst.Definition.Constant, args: Array[AnyRef]): AnyRef = {
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
