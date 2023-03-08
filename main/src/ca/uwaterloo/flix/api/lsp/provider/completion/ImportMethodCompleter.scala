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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ImportMethodCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object ImportMethodCompleter extends Completer {
  /**
    * Returns a List of Completion for importMethod (both static and instance methods).
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[ImportMethodCompletion] = {
    val instance = raw"\s*import\s+(.*)".r
    val static = raw"\s*import\s+static\s+(.*)".r
    context.prefix match {
      // We match on static first because import static ... would also match on instance regex.
      case static(clazz) => methodsCompletion(clazz, isStatic = true)
      case instance(clazz) => methodsCompletion(clazz, isStatic = false)
      case _ => Nil
    }
  }

  /**
    * Convert methods of a class into Completion
    */
  private def methodsCompletion(clazz: String, isStatic: Boolean): Iterable[ImportMethodCompletion] = {
    CompletionUtils.classFromDotSeperatedString(clazz) match {
      case Some((clazzObject, clazz)) => clazzObject.getMethods
        // Filter if the method is static or not.
        .filter(method => java.lang.reflect.Modifier.isStatic(method.getModifiers) == isStatic)
        .map(method => Completion.ImportMethodCompletion(method, clazz))
      case None => Nil
    }
  }
}
