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
  private def getSuggestions(uri: String, pos: Position, prefix: Option[String])(implicit index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    ///
    /// Return immediately if there is no AST.
    ///
    if (root == null) {
      return Nil
    }

    // TODO: Need to take into account "where" in the document we are. E.g. inside exp/defn or inside decl? or something else?
    // TODO: Need to decide whether to include private defs based on the current file/namespace.
    // TODO: Use uri and pos to determine what should be suggested.
    // TODO: Add support for fields, predicates, type classes.

    println(s"Auto-completing the prefix: $prefix.")

    val defSuggestions = root.defs.values.filter(matchesDef(_, prefix, uri)).map(getDefCompletionItem)
    val enumSuggestions = root.enums.values.filter(matchesEnum(_, prefix, uri)).flatMap(getEnumCompletionItems)
    val sigSuggestions = root.sigs.values.filter(matchesSig(_, prefix)).map(getSigCompletionItem)
    defSuggestions ++ enumSuggestions ++ sigSuggestions
  }

  /**
    * Returns `true` if the given definition `defn` matches the given `prefix`.
    */
  private def matchesDef(defn: TypedAst.Def, prefix: Option[String], uri: String): Boolean = {
    val isPublic = defn.spec.mod.isPublic
    val isMatch = prefix match {
      case None => true
      case Some(s) =>
        val isNamespace = s.contains(".")
        if (isNamespace)
          defn.sym.toString.startsWith(s)
        else
          defn.sym.text.startsWith(s)
    }
    val isInFile = defn.spec.loc.source.name == uri
    (isPublic && isMatch) || isInFile // TODO: Deal with the name?
  }

  /**
    * Returns `true` if the given enum `enum` matches the given `prefix`.
    */
  private def matchesEnum(enum: TypedAst.Enum, prefix: Option[String], uri: String): Boolean = {
    val isPublic = enum.mod.isPublic
    val isInFile = enum.loc.source.name == uri
    val isMatch = prefix match {
      case None => true
      case Some(s) => enum.sym.name.startsWith(s)
    }

    isPublic && isMatch && isInFile
  }

  /**
    * Returns `true` if the given signature `sign` matches the given `prefix`.
    */
  private def matchesSig(sign: TypedAst.Sig, prefix: Option[String]): Boolean = {
    val isPublic = sign.spec.mod.isPublic
    val isMatch = prefix match {
      case None => true
      case Some(s) => sign.sym.toString.startsWith(s)
    }
    isPublic && isMatch
  }

  /**
    * Returns a list of completion items for the given enum `enum`.
    */
  private def getEnumCompletionItems(enum: TypedAst.Enum): List[CompletionItem] = {
    enum.cases.map {
      case (tag, caze) =>
        val name = tag.name
        val label = tag.name
        val insertText = tag.name + "($" + "{1:exp})"
        val detail = Some(FormatScheme.formatScheme(caze.sc))
        val documentation = Some(enum.doc.text)
        val completionKind = CompletionItemKind.EnumMember
        val textFormat = InsertTextFormat.Snippet
        val commitCharacters = Nil // TODO
        CompletionItem(label, insertText, detail, documentation, completionKind, textFormat, commitCharacters)
    }.toList
  }

  /**
    * Returns a completion item for the given definition `defn`.
    */
  private def getDefCompletionItem(defn: TypedAst.Def): CompletionItem = {
    val name = defn.sym.toString
    val label = getDefLabel(defn)
    val insertText = getApplySnippet(name, defn.spec.fparams)
    val detail = Some(FormatScheme.formatScheme(defn.spec.declaredScheme))
    val documentation = Some(defn.spec.doc.text)
    val completionKind = CompletionItemKind.Function
    val textFormat = InsertTextFormat.Snippet
    val commitCharacters = Nil // TODO
    CompletionItem(label, insertText, detail, documentation, completionKind, textFormat, commitCharacters)
  }

  /**
    * Returns a completion item for the given signature `sign`.
    */
  private def getSigCompletionItem(sign: TypedAst.Sig): CompletionItem = {
    val name = sign.sym.toString
    val label = getSigLabel(sign)
    val insertText = getApplySnippet(name, sign.spec.fparams)
    val detail = Some(FormatScheme.formatScheme(sign.spec.declaredScheme))
    val documentation = Some(sign.spec.doc.text)
    val completionKind = CompletionItemKind.Function
    val textFormat = InsertTextFormat.Snippet
    val commitCharacters = Nil // TODO
    CompletionItem(label, insertText, detail, documentation, completionKind, textFormat, commitCharacters)
  }

  /**
    * Returns the label for the given definition `defn`.
    */
  private def getDefLabel(defn: TypedAst.Def): String =
    getLabel(defn.sym.toString, defn.spec.fparams, defn.spec.declaredScheme)

  /**
    * Returns the label for the given signature `sign`.
    */
  private def getSigLabel(sign: TypedAst.Sig): String =
    getLabel(sign.sym.toString, sign.spec.fparams, sign.spec.declaredScheme)

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
