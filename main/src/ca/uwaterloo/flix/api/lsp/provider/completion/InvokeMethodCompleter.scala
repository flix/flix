/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.MethodCompletion
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.TypeReduction

import java.lang.reflect.Method

object InvokeMethodCompleter {
  def getCompletions(e: TypeError.MethodNotFound, ctx: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[MethodCompletion] = {
    getClassOrInterface(e.tpe) match {
      case None => Nil
      case Some(clazz) => getMethods(clazz).map {
        case m => MethodCompletion(e.methodName, m)
      }
    }
  }

  /**
   * Returns all relevant methods available on the given `clazz`.
   */
  private def getMethods(clazz: Class[_]): List[Method] = {
    val availableMethods = TypeReduction.getMethods(clazz)
    // TODO: Add more filtering
    availableMethods.sortBy(_.getName)
  }

  /**
   * Optionally returns the Java class object of the given type `tpe`.
   *
   * Returns `None` if `tpe` tpe is not a Java class or interface.
   */
  private def getClassOrInterface(tpe: Type): Option[Class[_]] = tpe match {
    case Type.Cst(TypeConstructor.Str, _) => Some(classOf[String])
    case Type.Cst(TypeConstructor.Native(clazz), _) => Some(clazz)
    case _ => None
  }

}
