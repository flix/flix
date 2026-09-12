/*
 * Copyright 2025 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
      Completion.AnnotationCompletion("DefaultHandler",   range, Priority.Medium(0)),
      Completion.AnnotationCompletion("Deprecated",       range, Priority.Medium(0)),
      Completion.AnnotationCompletion("Lazy",             range, Priority.Medium(0)),
      Completion.AnnotationCompletion("LazyWhenPure",     range, Priority.Medium(0)),
      Completion.AnnotationCompletion("Parallel",         range, Priority.Medium(0)),
      Completion.AnnotationCompletion("ParallelWhenPure", range, Priority.Medium(0)),
      Completion.AnnotationCompletion("Tailrec",          range, Priority.Medium(0)),
      Completion.AnnotationCompletion("Terminates",       range, Priority.Medium(0)),
      Completion.AnnotationCompletion("Test",             range, Priority.Medium(0))
    ).filter {
      case c => c.name.startsWith(prefix)
    }

}
