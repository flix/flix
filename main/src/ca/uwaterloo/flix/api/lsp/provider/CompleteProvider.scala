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

import ca.uwaterloo.flix.api.lsp._
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.{Audience, FormatScheme, FormatType}
import ca.uwaterloo.flix.util.InternalCompilerException

object CompleteProvider {

  private implicit val audience: Audience = Audience.External

  /**
    * A list of keywords that block completions.
    */
  private val BlockList: List[String] = List("class", "def", "instance", "namespace")

  /**
    * Returns a list of auto-complete suggestions.
    */
  def autoComplete(uri: String, pos: Position, line: Option[String], word: Option[String])(implicit index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    // Ordered by priority.
    getVarSuggestions(uri, pos, line, word) ++
      getDefAndSigSuggestions(uri, pos, line, word) ++
      getInstanceSuggestions(uri, pos, line, word) ++
      getWithSuggestions(uri, pos, line, word) ++
      getKeywordCompletionItems(line, word) ++
      getSnippetCompletionItems(line, word)
  }

  /**
    * Returns a list of keyword completion items.
    */
  private def getKeywordCompletionItems(line: Option[String], word: Option[String]): List[CompletionItem] = {
    /// Return if there is already a keyword on this line.
    if (matchesOneOf(line, BlockList)) {
      return Nil
    }

    List(
      // NB: Please keep the list alphabetically sorted.

      // Keywords:
      CompletionItem("alias", "alias ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("and", "and ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("as", "as ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("case", "case ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.Snippet, Nil),
      CompletionItem("chan", "chan ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("choose", "choose ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("class", "class ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("def", "def ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("deref", "deref ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("discard", "discard", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("do", "do ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("eff", "eff ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("else", "else ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("enum", "enum ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("false", "false", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("fix", "fix ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("forall", "forall ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("force", "force ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("from", "from ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("get", "get ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("if", "if ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("import", "import ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("instance", "instance ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("into", "into ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("lat", "lat ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("law", "law ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("lazy", "lazy ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("let", "let ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("match", "match ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("namespace", "namespace ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("new", "new ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("not", "not ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("null", "null ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("opaque", "opaque ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("or", "or ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("override", "override ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("project", "project ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("pub", "pub ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("Pure", "Pure ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("query", "query ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("Record", "Record ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("ref", "ref ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("region", "region ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("rel", "rel ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("Schema", "Schema ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("sealed", "sealed ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("select", "select ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("set", "set ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("solve", "solve ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("spawn", "spawn ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("true", "true", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("try", "try ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("type", "type ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("use", "use ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("where", "where ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("with", "with ", None, Some("keyword"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),

      // Types:
      CompletionItem("Bool", "Bool", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("Char", "Char", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("Float32", "Float32", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("Float64", "Float64", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("Int32", "Int32", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("Int64", "Int64", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("String", "String", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil),
      CompletionItem("Unit", "Unit", None, Some("type"), CompletionItemKind.Keyword, InsertTextFormat.PlainText, Nil)
    )
  }

  /**
    * Returns a list of snippet completion items.
    */
  private def getSnippetCompletionItems(line: Option[String], word: Option[String]): List[CompletionItem] = {
    /// Return if there is already a keyword on this line.
    if (matchesOneOf(line, BlockList)) {
      return Nil
    }

    List(
      // NB: Please keep the list alphabetically sorted.

      // Declaration-based:
      CompletionItem("main", "def main(): Unit & Impure = \n    println(\"Hello World!\")", None, Some("snippet for Hello World Program"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),

      // Expressed-based:
      CompletionItem("query", "query ${1:db} select ${2:cols} from ${3:preds} ${4:where ${5:cond}}", None, Some("snippet for query"), CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil),
    )
  }

  /**
    * Returns a list of completion items based on with type class constraints.
    */
  private def getWithSuggestions(uri: String, pos: Position, line: Option[String], word: Option[String])(implicit index: Index, root: TypedAst.Root): List[CompletionItem] = {
    ///
    /// Return immediately if there is no AST or the position is not appropriate.
    ///
    if (root == null || !line.exists(s => s.contains("class") || s.contains("with"))) {
      return Nil
    }

    root.classes.map {
      case (_, clazz) =>
        val hole = "${1:t}"
        val classSym = clazz.sym
        val label = s"$classSym[...]"
        val insertText = s"$classSym[$hole]"
        val detail = Some(fmtClass(clazz))
        val documentation = Some(clazz.doc.text)
        CompletionItem(label, insertText, detail, documentation, CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil)
    }.toList
  }

  /**
    * Returns a list of completion items based on type classes.
    */
  private def getInstanceSuggestions(uri: String, pos: Position, line: Option[String], word: Option[String])(implicit index: Index, root: TypedAst.Root): List[CompletionItem] = {
    ///
    /// Return immediately if there is no AST.
    ///
    if (root == null) {
      return Nil
    }

    /**
      * Replaces the text in the given variable symbol `sym` everywhere in the type `tpe`
      * with an equivalent variable symbol with the given `newText`.
      */
    def replaceText(tvar: Symbol.TypeVarSym, tpe: Type, newText: String): Type = tpe match {
      case Type.KindedVar(sym, loc) if tvar == sym => Type.KindedVar(sym.withText(Ast.VarText.SourceText(newText)), loc)
      case Type.KindedVar(_, _) => tpe
      case Type.Cst(_, _) => tpe

      case Type.Apply(tpe1, tpe2, loc) =>
        val t1 = replaceText(tvar, tpe1, newText)
        val t2 = replaceText(tvar, tpe2, newText)
        Type.Apply(t1, t2, loc)

      case Type.Alias(sym, args0, tpe0, loc) =>
        val args = args0.map(replaceText(tvar, _, newText))
        val t = replaceText(tvar, tpe0, newText)
        Type.Alias(sym, args, t, loc)

      case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type variable.")
      case _: Type.Ascribe => throw InternalCompilerException("Unexpected kind ascription.")
    }

    /**
      * Formats the given type `tpe`.
      */
    def fmtType(clazz: TypedAst.Class, tpe: Type, hole: String): String =
      FormatType.formatWellKindedType(replaceText(clazz.tparam.sym, tpe, hole))

    /**
      * Formats the given formal parameters in `spec`.
      */
    def fmtFormalParams(clazz: TypedAst.Class, spec: TypedAst.Spec, hole: String): String =
      spec.fparams.map {
        case fparam => s"${fparam.sym.text}: ${fmtType(clazz, fparam.tpe, hole)}"
      }.mkString(", ")

    /**
      * Formats the given signature `sig`.
      */
    def fmtSignature(clazz: TypedAst.Class, sig: TypedAst.Sig, hole: String): String = {
      val fparams = fmtFormalParams(clazz, sig.spec, hole)
      val retTpe = fmtType(clazz, sig.spec.retTpe, hole)
      val eff = sig.spec.eff match {
        case Type.Cst(TypeConstructor.True, _) => ""
        case Type.Cst(TypeConstructor.False, _) => " & Impure"
        case e => " & " + FormatType.formatWellKindedType(e)
      }
      s"    pub def ${sig.sym.name}($fparams): $retTpe$eff = ???"
    }

    // Return immediately if the current line does not contain the word "instance".
    if (!line.exists(_.contains("instance"))) {
      return Nil
    }

    root.classes.map {
      case (_, clazz) =>
        val hole = "${1:t}"
        val classSym = clazz.sym
        val signatures = clazz.signatures.filter(_.impl.isEmpty)
        val body = signatures.map(s => fmtSignature(clazz, s, hole)).mkString("\n\n")

        val label = s"$classSym[...]"
        val insertText = s"$classSym[$hole] {\n\n$body\n\n}\n"
        val detail = Some(fmtClass(clazz))
        val documentation = Some(clazz.doc.text)
        CompletionItem(label, insertText, detail, documentation, CompletionItemKind.Snippet, InsertTextFormat.Snippet, Nil)
    }.toList
  }

  /**
    * Formats the given class `clazz`.
    */
  private def fmtClass(clazz: TypedAst.Class): String = {
    s"class ${clazz.sym.name}[${clazz.tparam.name.name}]"
  }

  /**
    * Returns a list of auto-complete suggestions based on defs and sigs.
    */
  private def getDefAndSigSuggestions(uri: String, pos: Position, line: Option[String], word: Option[String])(implicit index: Index, root: TypedAst.Root): List[CompletionItem] = {
    ///
    /// Return immediately if there is no AST or the position is not appropriate.
    ///
    if (root == null || matchesOneOf(line, BlockList)) {
      return Nil
    }

    ///
    /// If the search word is `List.f` we should not include the namespace.
    ///
    /// That is, exclude the namespace if there is a period, but not at the end.
    ///
    val withoutNS = word match {
      case None => false
      case Some(prefix) => prefix.contains(".") && !prefix.endsWith(".")
    }

    val defSuggestions = root.defs.values.filter(matchesDef(_, word, uri)).map(getDefCompletionItem(withoutNS, _))
    val sigSuggestions = root.sigs.values.filter(matchesSig(_, word, uri)).map(getSigCompletionItem(withoutNS, _))
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
    val isInFile = decl.sym.loc.source.name == uri

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
    val isInFile = sign.sym.loc.source.name == uri

    isMatch && (isPublic || isInFile)
  }

  /**
    * Returns a list of auto-complete suggestions based on local variables in the current uri.
    */
  private def getVarSuggestions(uri: String, pos: Position, line: Option[String], word: Option[String])(implicit index: Index, root: TypedAst.Root): List[CompletionItem] = {
    ///
    /// Return immediately if there is no AST or the position is not appropriate.
    ///
    if (root == null || matchesOneOf(line, BlockList)) {
      return Nil
    }

    ///
    /// Find all local variables in the current uri with a given range.
    ///
    val iter = index.queryWithRange(uri, queryLine = pos.line, beforeLine = 20, afterLine = 10).collect {
      case Entity.LocalVar(sym, tpe) => getVarCompletionItem(sym, tpe)
      case Entity.FormalParam(fparam) => getVarCompletionItem(fparam.sym, fparam.tpe)
    }

    iter.toList
  }

  /**
    * Returns a completion item for the given definition `decl`.
    */
  private def getDefCompletionItem(withoutNS: Boolean, decl: TypedAst.Def): CompletionItem = {
    val name = if (withoutNS) decl.sym.text else decl.sym.toString
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
    * Returns a completion item for the given variable symbol `sym` with the given type `tpe`.
    */
  private def getVarCompletionItem(sym: Symbol.VarSym, tpe: Type): CompletionItem = {
    val name = sym.text
    val label = sym.text
    val insertText = sym.text
    val detail = Some(FormatType.formatWellKindedType(tpe))
    val documentation = None
    val completionKind = CompletionItemKind.Variable
    val textFormat = InsertTextFormat.PlainText
    val commitCharacters = Nil
    CompletionItem(label, insertText, detail, documentation, completionKind, textFormat, commitCharacters)
  }

  /**
    * Returns a completion item for the given signature `decl`.
    */
  private def getSigCompletionItem(withoutNS: Boolean, decl: TypedAst.Sig): CompletionItem = {
    val name = if (withoutNS) decl.sym.name else decl.sym.toString
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
    getLabel(decl.sym.toString, decl.spec)

  /**
    * Returns the label for the given signature `decl`.
    */
  private def getSigLabel(decl: TypedAst.Sig): String =
    getLabel(decl.sym.toString, decl.spec)

  /**
    * Returns the label for the given `name`, and `spec`.
    */
  private def getLabel(name: String, spec: TypedAst.Spec): String = spec match {
    case TypedAst.Spec(_, _, _, _, fparams, _, retTpe0, eff0, _) =>
      val args = fparams.map {
        fparam => s"${fparam.sym.text}: ${FormatType.formatWellKindedType(fparam.tpe)}"
      }

      val retTpe = FormatType.formatWellKindedType(retTpe0)
      val eff = eff0 match {
        case Type.Cst(TypeConstructor.True, _) => "Pure"
        case Type.Cst(TypeConstructor.False, _) => "Impure"
        case e => FormatType.formatWellKindedType(e)
      }

      s"$name(${args.mkString(", ")}): $retTpe & $eff"
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
    * Returns `true` if `line` exists and contains one of the given keywords.
    */
  private def matchesOneOf(line: Option[String], keywords: List[String]): Boolean =
    keywords.exists(keyword => line.exists(s => s.contains(keyword)))

}
