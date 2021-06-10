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

import ca.uwaterloo.flix.api.lsp.{CompletionItem, CompletionItemKind, Index, InsertTextFormat, Position}
import ca.uwaterloo.flix.language.ast.{Scheme, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.debug.{Audience, FormatScheme, FormatType}

object CompleteProvider {

  private implicit val audience: Audience = Audience.External

  /**
    * Returns a list of auto-complete suggestions.
    */
  def autoComplete(uri: String, pos: Position, prefix: Option[String])(implicit index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    getKeywordCompletionItems() ++ getSnippetCompletionItems() ++ getSuggestions(uri, pos, prefix)
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
  private def getSuggestions(uri: String, pos: Position, prefix: Option[String])(implicit index: Index, root: TypedAst.Root): List[CompletionItem] = {
    ///
    /// Return immediately if there is no AST.
    ///
    if (root == null) {
      return Nil
    }

    // TODO: Add support for classes and enums?
    // TODO: Use the current position to determine what to suggest.

    val defSuggestions = root.defs.values.filter(matchesDef(_, prefix, uri)).map(getDefCompletionItem)
    val sigSuggestions = root.sigs.values.filter(matchesSig(_, prefix, uri)).map(getSigCompletionItem)
    (defSuggestions ++ sigSuggestions).toList.sortBy(_.label)
  }

  /**
    * Returns `true` if the given definition `decl` should be included in the suggestions.
    */
  private def matchesDef(decl: TypedAst.Def, prefix: Option[String], uri: String): Boolean = {
    val isPublic = decl.spec.mod.isPublic
    val isMatch = prefix match {
      case None => true
      case Some(s) =>
        val isNamespace = s.nonEmpty && s.head.isUpper
        if (isNamespace)
          decl.sym.toString.startsWith(s)
        else
          decl.sym.text.startsWith(s)
    }
    val isInFile = decl.spec.loc.source.name == uri

    isMatch && (isPublic || isInFile)
  }

  /**
    * Returns `true` if the given signature `sign` matches the given `prefix`.
    */
  private def matchesSig(sign: TypedAst.Sig, prefix: Option[String], uri: String): Boolean = {
    val isPublic = sign.spec.mod.isPublic
    val isMatch = prefix match {
      case None => true
      case Some(s) =>
        val isNamespace = s.nonEmpty && s.head.isUpper
        if (isNamespace)
          sign.sym.toString.startsWith(s)
        else
          sign.sym.name.startsWith(s)
    }
    val isInFile = sign.spec.loc.source.name == uri

    isMatch && (isPublic || isInFile)
  }

  /**
    * Returns a completion item for the given definition `decl`.
    */
  private def getDefCompletionItem(decl: TypedAst.Def): CompletionItem = {
    val name = decl.sym.toString
    val label = getDefLabel(decl)
    val insertText = getApplySnippet(name, decl.spec.fparams)
    val detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme))
    val documentation = Some(decl.spec.doc.text)
    val completionKind = CompletionItemKind.Function
    val textFormat = InsertTextFormat.Snippet
    val commitCharacters = Nil
    CompletionItem(label, insertText, detail, documentation, completionKind, textFormat, commitCharacters)
  }

  /**
    * Returns a completion item for the given signature `decl`.
    */
  private def getSigCompletionItem(decl: TypedAst.Sig): CompletionItem = {
    val name = decl.sym.toString
    val label = getSigLabel(decl)
    val insertText = getApplySnippet(name, decl.spec.fparams)
    val detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme))
    val documentation = Some(decl.spec.doc.text)
    val completionKind = CompletionItemKind.Interface
    val textFormat = InsertTextFormat.Snippet
    val commitCharacters = Nil
    CompletionItem(label, insertText, detail, documentation, completionKind, textFormat, commitCharacters)
  }

  /**
    * Returns the label for the given definition `decl`.
    */
  private def getDefLabel(decl: TypedAst.Def): String =
    getLabel(decl.sym.toString, decl.spec.fparams, decl.spec.declaredScheme)

  /**
    * Returns the label for the given signature `decl`.
    */
  private def getSigLabel(decl: TypedAst.Sig): String =
    getLabel(decl.sym.toString, decl.spec.fparams, decl.spec.declaredScheme)

  /**
    * Returns the label for the given `name`, formal parameters `fparams`, and scheme `sc`.
    */
  private def getLabel(name: String, fparams: List[TypedAst.FormalParam], sc: Scheme): String = {
    val args = fparams.map {
      case fparam => s"${fparam.sym.text}: ${FormatType.formatType(fparam.tpe)}"
    }

    val base = sc.base
    val tpe = FormatType.formatType(getResultType(base))
    val eff = base.typeConstructor match {
      case Some(TypeConstructor.Arrow(_)) => base.typeArguments.head match {
        case Type.Cst(TypeConstructor.True, _) => ""
        case Type.Cst(TypeConstructor.False, _) => " & Impure"
        case e => " & " + FormatType.formatType(e)
      }
      case _ => ""
    }

    s"$name(${args.mkString(", ")}): $tpe$eff"
  }

  /**
    * Returns a snippet for a function with the given `name` and `fparams`.
    *
    * For example, `name(${0:arg1}, ${1:arg2})`.
    */
  private def getApplySnippet(name: String, fparams: List[TypedAst.FormalParam]): String = {
    val args = fparams.zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:${fparam.sym.text}}"
    }
    s"$name(${args.mkString(", ")})"
  }

  /**
    * Returns the return type of the given function type `tpe0`.
    */
  private def getResultType(tpe0: Type): Type = tpe0.typeArguments.last

}
