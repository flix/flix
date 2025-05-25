/*
 * Copyright 2025 Magnus Madsen
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

import ca.uwaterloo.flix.api.lsp.Range

/**
  * Completer for annotations.
  */
object AnnotationCompleter {

  /**
    * Returns annotation completions that start with the given `prefix`.
    */
  def getAnnotations(prefix: String, range: Range): List[Completion] =
    List(
      Completion.AnnotationCompletion("Deprecated",       range, Priority.Medium),
      Completion.AnnotationCompletion("Lazy",             range, Priority.Medium),
      Completion.AnnotationCompletion("LazyWhenPure",     range, Priority.Medium),
      Completion.AnnotationCompletion("Parallel",         range, Priority.Medium),
      Completion.AnnotationCompletion("ParallelWhenPure", range, Priority.Medium),
      Completion.AnnotationCompletion("Test",             range, Priority.Medium)
    ).filter {
      case c => c.name.startsWith(prefix)
    }

}
