/*
 * Copyright 2024 Chenhao Gao
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

import ca.uwaterloo.flix.api.lsp.CompletionItemLabelDetails
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.AutoImportCompletion
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope}
import ca.uwaterloo.flix.language.errors.ResolutionError

object AutoImportCompleter {

  /**
   * Returns a list of import completions to auto complete the class name and import the java class.
   *
   * Example:
   *  If we have an undefined name which is the prefix of an existing and unimported java class
   *
   *  {{{
   *    let s = Mat // undefined name error
   *  }}}
   *
   *  We propose to complete the name to `Math` and import the class `java.lang.Math`
   *
   *  {{{
   *    import java.lang.Math
   *    ...
   *    let s = Math
   *  }}}
   */
  def getCompletions(err: ResolutionError.UndefinedName)(implicit root: Root): Iterable[AutoImportCompletion] =
    javaClassCompletionsByClass(err.qn.ident.name, err.ap, err.env)

  def getCompletions(err: ResolutionError.UndefinedType)(implicit root: Root): Iterable[AutoImportCompletion] =
    javaClassCompletionsByClass(err.qn.ident.name, err.ap, err.env)

  /**
   * Gets completions from a java class prefix.
   *
   * Note: we will not propose completions for classes with a lowercase name.
   */
  private def javaClassCompletionsByClass(prefix: String, ap: AnchorPosition, env: LocalScope)(implicit root: Root): Iterable[AutoImportCompletion] = {
    val availableClasses = root.availableClasses.byClass.m.filter(_._1.exists(_.isUpper))
    availableClasses.keys.filter(_.startsWith(prefix)).flatMap { className =>
      availableClasses(className).collect { case namespace if (!env.m.contains(className)) =>
        val qualifiedName = namespace.mkString(".") + "." + className
        val priority = mkPriority(qualifiedName)
        val labelDetails = CompletionItemLabelDetails(None, Some("import" + qualifiedName))
          AutoImportCompletion(className, qualifiedName, ap, labelDetails, priority)
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
      Priority.Low
    else if (rarelyUsedPackages.exists(qualifiedName.startsWith))
      Priority.Lowest
    else
      Priority.Lower
  }
}
