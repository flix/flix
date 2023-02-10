/*
 * Copyright 2022 Paul Butcher
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

import ca.uwaterloo.flix.api.lsp.{CompletionItem, Index, TextEdit}
import ca.uwaterloo.flix.api.lsp.provider.CompletionProvider.Priority
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}

object TypeCompleter {

  /**
    * Completions for types (enums, aliases, and built-in types)
    */
  def getTypeCompletions()(implicit context: CompletionContext, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    def getInternalPriority(loc: SourceLocation, ns: List[String]): String => String = {
      if (loc.source.name == context.uri)
        Priority.boost
      else if (ns.isEmpty)
        Priority.normal
      else
        Priority.low
    }

    // Boost priority if there's a colon immediately before the word the user's typing
    val typePriorityBoost = raw".*:\s*(?:[^\s]|(?:\s*,\s*))*".r
    val typeAliasPriorityBoost = raw"\s*type\s+alias\s+.+\s*=\s*(?:[^\s]|(?:\s*,\s*))*".r
    val priority = if ((typePriorityBoost matches context.prefix) || (typeAliasPriorityBoost matches context.prefix))
      Priority.boost _ else Priority.low _

    val enums = root.enums.collect {
      case (_, t) if !t.ann.isInternal =>
        val name = t.sym.name
        val internalPriority = getInternalPriority(t.loc, t.sym.namespace)
        Completion.TypeCompleter(s"$name${formatTParams(t.tparams)}", priority(internalPriority(name)),
          TextEdit(context.range, s"$name${formatTParamsSnippet(t.tparams)}"), Some(t.doc.text)).toCompletionItem
    }

    val aliases = root.typeAliases.map {
      case (_, t) =>
        val name = t.sym.name
        val internalPriority = getInternalPriority(t.loc, t.sym.namespace)
        Completion.TypeCompleter(s"$name${formatTParams(t.tparams)}", priority(internalPriority(name)),
          TextEdit(context.range, s"$name${formatTParamsSnippet(t.tparams)}"), Some(t.doc.text)).toCompletionItem
    }

    enums ++ aliases ++ BuiltinTypeCompleter.getBuiltinTypeCompletions(priority)
  }

  /**
    * Format type params in the right form to be inserted as a snippet
    * e.g. "[${1:a}, ${2:b}, ${3:c}]"
    */
  private def formatTParamsSnippet(tparams: List[TypedAst.TypeParam]): String = {
    tparams match {
      case Nil => ""
      case _ => tparams.zipWithIndex.map {
        case (tparam, idx) => "$" + s"{${idx + 1}:${tparam.name}}"
      }.mkString("[", ", ", "]")
    }
  }

  /**
    * Format type params in the right form to be displayed in the list of completions
    * e.g. "[a, b, c]"
    */
  private def formatTParams(tparams: List[TypedAst.TypeParam]): String = {
    tparams match {
      case Nil => ""
      case _ => tparams.map(_.name).mkString("[", ", ", "]")
    }
  }
}
