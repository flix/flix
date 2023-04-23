/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ImportFieldCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object ImportFieldCompleter extends Completer {
  /**
    * Returns a List of Completion for importField.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[ImportFieldCompletion] = {
    val static_get = raw"\s*import\s+static\s+get\s+(.*)".r
    val static_set = raw"\s*import\s+static\s+set\s+(.*)".r
    val get = raw"\s*import\s+get\s+(.*)".r
    val set = raw"\s*import\s+set\s+(.*)".r
    context.prefix match {
      case static_get(clazz) => importFieldCompletions(clazz, isStatic = true, isGet = true)
      case static_set(clazz) => importFieldCompletions(clazz, isStatic = true, isGet = false)
      case get(clazz) => importFieldCompletions(clazz, isStatic = false, isGet = true)
      case set(clazz) => importFieldCompletions(clazz, isStatic = false, isGet = false)
      case _ => Nil
    }
  }

  /**
    * Returns completions for a dot seperated class string
    */
  private def importFieldCompletions(clazz: String, isStatic: Boolean, isGet: Boolean): Iterable[ImportFieldCompletion] = {
    CompletionUtils.classFromDotSeperatedString(clazz) match {
      case Some((clazzObject, clazz)) => clazzObject.getFields
        // Filter if the method is static or not.
        .filter(field => java.lang.reflect.Modifier.isStatic(field.getModifiers) == isStatic)
        .map(field => Completion.ImportFieldCompletion(field, clazz, isGet))
      case None => Nil
    }
  }
}
