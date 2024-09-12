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

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.MethodCompletion
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.language.phase.jvm.JvmOps

import java.lang.reflect.{Method, Modifier}

object InvokeStaticMethodCompleter {
  def getCompletions(e: ResolutionError.UndefinedJvmStaticField): Iterable[MethodCompletion] = {
    getMethods(e.clazz).map {
      case m => MethodCompletion(e.field, m)
    }
  }

  private def getMethods(clazz: Class[_]): List[Method] = {
    val availableMethods = JvmOps.getMethods(clazz)
    val staticMethods = availableMethods.filter(m => Modifier.isStatic(m.getModifiers))
    staticMethods.sortBy(_.getName)
  }
}
