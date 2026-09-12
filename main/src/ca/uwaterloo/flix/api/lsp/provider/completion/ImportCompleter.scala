/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.Range
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ImportCompletion
import ca.uwaterloo.flix.language.ast.TypedAst

object ImportCompleter {

  /**
    * Returns a list of completions.
    *
    * @param name  The whole name of the unresolved import. e.g. java.io.Fi
    * @param range The range of the completion.
    */
  def getCompletions(name: String, range: Range)(implicit root: TypedAst.Root): Iterable[ImportCompletion] = {
    val path = name.split('.').toList
    // Get completions for if we are currently typing the next package/class and if we have just finished typing a package
    javaClassCompletionsFromPrefix(path, range)(root) ++ javaClassCompletionsFromPrefix(path.dropRight(1), range)(root)
  }

  /**
    * Gets completions from a java path prefix
    */
  private def javaClassCompletionsFromPrefix(prefix: List[String], range: Range)(implicit root: TypedAst.Root): Iterable[ImportCompletion] = {
    root.availableClasses.byPackage(prefix).map(clazz => {
      val label = prefix match {
        case Nil => clazz
        case v => v.mkString("", ".", s".$clazz")
      }
      Completion.ImportCompletion(label, range, Priority.Highest(0), isPackage = clazz.head.isLower)
    })
  }
}
