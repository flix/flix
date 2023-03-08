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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ImportNewCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object ImportNewCompleter extends Completer {
  /**
    * Returns a List of Completion for importNew (java constructors).
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[ImportNewCompletion] = {
    val regex = raw"\s*import\s+new\s+(.*)".r
    context.prefix match {
      case regex(clazz) => CompletionUtils.classFromString(clazz) match {
        case Some((clazzObject, clazz)) =>
          // Gets the name of the type excluding the package to use as a suggestion for the name of the constructor.
          val className = clazz.split('.').last
          clazzObject.getConstructors.map(constructor =>
            Completion.ImportNewCompletion(constructor, clazz, Some(s"new$className")))
        case None => Nil
      }
      case _ => Nil
    }
  }
}
