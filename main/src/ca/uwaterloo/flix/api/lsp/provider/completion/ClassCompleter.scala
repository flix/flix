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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ClassCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object ClassCompleter extends Completer {
  /**
    * Returns a List of Completion for java packages/classes.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[ClassCompletion] = {
    val regex = raw"\s*import\s+(?:.*\s+)*(.*)".r
    context.prefix match {
      case regex(clazz) =>
        val path = clazz.split('.').toList
        // Get completions for if we are currently typing the next package/class and if we have just finished typing a package
        javaClassCompletionsFromPrefix(path)(root) ++ javaClassCompletionsFromPrefix(path.dropRight(1))(root)
      case _ => Nil
    }
  }

  /**
    * Gets completions from a java path prefix
    */
  private def javaClassCompletionsFromPrefix(prefix: List[String])(implicit root: TypedAst.Root): Iterable[ClassCompletion] = {
    root.names(prefix).map(clazz => {
      val label = prefix match {
        case Nil => clazz
        case v => v.mkString("", ".", s".$clazz")
      }
      Completion.ClassCompletion(label)
    })
  }
}
