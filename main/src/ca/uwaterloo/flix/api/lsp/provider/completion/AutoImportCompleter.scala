/*
 * Copyright 2024 Chenhao Gao
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.AutoImportCompletion
import ca.uwaterloo.flix.api.lsp.{CompletionItemLabelDetails, Range}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope}

object AutoImportCompleter {

  /**
    * Returns a list of import completions to auto complete the class name and import the java class.
    *
    * Example:
    * If we have an undefined name which is the prefix of an existing and unimported java class
    *
    * {{{
    *    let s = Mat // undefined name error
    * }}}
    *
    * We propose to complete the name to `Math` and import the class `java.lang.Math`
    *
    * {{{
    *    import java.lang.Math
    *    ...
    *    let s = Math
    * }}}
    *
    * @param prefix the prefix of the class name, usually from the ident of a qname.
    * @param range  the range of the completion.
    * @param ap     the anchor position of the completion.
    * @param scp    the local scope.
    */
  def getCompletions(prefix: String, range: Range, ap: AnchorPosition, scp: LocalScope)(implicit root: Root): Iterable[AutoImportCompletion] = {
    if (!CompletionUtils.shouldComplete(prefix)) return Nil
    val availableClasses = root.availableClasses.byClass.m.filter(_._1.exists(_.isUpper))
    availableClasses.keys.filter(CompletionUtils.fuzzyMatch(prefix, _)).flatMap { className =>
      availableClasses(className).collect { case namespace if !scp.m.contains(className) =>
        val qualifiedName = namespace.mkString(".") + "." + className
        val priority = mkPriority(qualifiedName)
        val labelDetails = CompletionItemLabelDetails(None, Some(s"import $qualifiedName"))
        AutoImportCompletion(className, qualifiedName, range, ap, labelDetails, priority)
      }
    }
  }

  /**
    * Returns the priority of the completion item based on the qualified name.
    *
    * We give these packages the `Low` priority:
    *  - java.lang
    *  - java.io
    *  - java.nio
    *  - java.util
    *
    * We give these packages the `Lowest` priority:
    * - com.sun
    * - sun.
    *
    * Every other package gets the `Lower` priority.
    */
  private def mkPriority(qualifiedName: String): Priority = {
    val frequentlyUsedPackages = List("java.lang", "java.io", "java.nio", "java.util")
    val rarelyUsedPackages = List("com.sun", "sun.")
    if (frequentlyUsedPackages.exists(qualifiedName.startsWith))
      Priority.Low(0)
    else if (rarelyUsedPackages.exists(qualifiedName.startsWith))
      Priority.Lowest(0)
    else
      Priority.Lower(0)
  }
}
