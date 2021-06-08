/*
 * Copyright 2021 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, InsertTextFormat, Position}
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.debug.{Audience, FormatScheme, FormatType}

object CompleteProvider {

  private implicit val audience: Audience = Audience.External

  /**
    * Returns a list of auto-complete suggestions.
    */
  def autoComplete(uri: String, pos: Position, prefix: String, root: TypedAst.Root): List[CompletionItem] = {
    getKeywordCompletionItems() ::: getSnippetCompletionItems() ::: getSuggestions(root)
  }

  /**
    * Returns a list of keyword completion items.
    */
  private def getKeywordCompletionItems(): List[CompletionItem] = List(
    // TODO: Manoj: Add more.
    // NB: Please keep the list alphabetically sorted.

    // Keywords:
    CompletionItem("namespace", "namespace", None, None, CompletionItemKind.Keyword, InsertTextFormat.PlainText, List("{", "}")),

    // Keyword-like (e.g. println):
    CompletionItem("println", "println", None, None, CompletionItemKind.Keyword, InsertTextFormat.PlainText, List("(", ")")),
  )

  /**
    * Returns a list of snippet completion items.
    */
  private def getSnippetCompletionItems(): List[CompletionItem] = List(
    // TODO: Manoj: Add more.
    // NB: Please keep the list alphabetically sorted.

    // Declaration-based:

    // Expressed-based:
    CompletionItem("match", "match ${1:exp} {\n case ${2:pat} => ${3:exp}\n}", None, None, CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    CompletionItem("query", "query ${1:db} select ${2:cols} from ${3:preds} ${4:where ${5:cond}}", None, None, CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
  )

  /**
    * Returns a list of other completion items.
    */
  private def getSuggestions(root: TypedAst.Root): List[CompletionItem] = {
    ///
    /// Return immediately if there is no AST.
    ///
    if (root == null) {
      return Nil
    }

    val includeNamespaces = List(
      List("Option"),
      List("Result"),
      List("List"),
      List("Set"),
      List("Map")
    )

    def includeDef(defn: TypedAst.Def): Boolean = includeNamespaces.contains(defn.sym.namespace) && defn.spec.mod.isPublic

    // TODO: Magnus

    // TODO: Cleanup
    val listDefs = root.defs.filter(kv => includeDef(kv._2))
    listDefs.map {
      case (_, defn) =>
        val label = reconstructSignature(defn)
        val insertText = defInsertText(defn)
        val detail = Some(FormatScheme.formatScheme(defn.spec.declaredScheme))
        val documentation = Some(defn.spec.doc.text)
        CompletionItem(label, insertText, detail, documentation, CompletionItemKind.Function, InsertTextFormat.Snippet, List("(", ")"))
    }.toList

  }

  // TODO: Magnus
  private def reconstructSignature(defn: TypedAst.Def): String = {
    val prefix = defn.sym.toString
    val args = defn.spec.fparams.map {
      case fparam => s"${fparam.sym.text}: ${FormatType.formatType(fparam.tpe)}"
    }
    val tpe = FormatType.formatType(getResultType(defn.spec.declaredScheme.base))
    val eff = FormatType.formatType(defn.spec.eff) // TODO: Dont show pure
    s"${prefix}(${args.mkString(", ")}): $tpe$eff"
  }

  // TODO: Magnus
  private def defInsertText(defn: TypedAst.Def): String = {
    val prefix = defn.sym.toString
    val args = defn.spec.fparams.zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:${fparam.sym.text}}"
    }
    s"${prefix}(${args.mkString(", ")})"
  }

  /**
    * Returns the return type of the given function type `tpe0`.
    */
  private def getResultType(tpe0: Type): Type = tpe0.typeArguments.last

}
