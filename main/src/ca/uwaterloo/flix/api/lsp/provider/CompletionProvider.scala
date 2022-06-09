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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp._
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.{Audience, FormatScheme, FormatType}
import ca.uwaterloo.flix.language.phase.Parser.Letters
import ca.uwaterloo.flix.util.InternalCompilerException
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.parboiled2.CharPredicate

object CompletionProvider {
  private implicit val audience: Audience = Audience.External

  def autoComplete(uri: String, pos: Position, source: Option[String])(implicit index: Index, root: TypedAst.Root): JObject = {
    val completions = source.flatMap(getContext(_, uri, pos)) match {
      case None => Nil
      case Some(context) => getCompletions()(context, index, root)
    }

    ("status" -> "success") ~ ("result" -> CompletionList(isIncomplete = true, completions).toJSON)
  }

  private def getCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): List[CompletionItem] = {
    getKeywordCompletions() ++
      getSnippetCompletions() ++
      getVarCompletions() ++
      getDefAndSigCompletions() ++
      getWithCompletions() ++
      getInstanceCompletions()
  }

  private def keywordCompletion(name: String)(implicit context: Context, index: Index, root: TypedAst.Root): CompletionItem = {
    CompletionItem(label = name,
      sortText = "9" + name,
      textEdit = TextEdit(context.range, name),
      kind = CompletionItemKind.Keyword)
  }

  private def getKeywordCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): List[CompletionItem] = {
    // TODO: keyword-specific help text?
    // NB: Please keep the list alphabetically sorted.
    List("@benchmark", "@Deprecated", "@Experimental", "@Parallel", "@ParallelWhenPure", "@Lazy", "@LazyWhenPure", "@Space", "@test", "@Time",
      "and", "as", "case", "chan", "choose", "class", "def", "deref", "discard", "do", "eff", "else", "enum", "false", "fix", "forall",
      "force", "from", "get", "if", "import", "Impure", "instance", "into", "lat", "law", "lazy", "let", "match", "namespace", "new",
      "not", "null", "opaque", "or", "override", "project", "pub", "Pure", "query", "Record", "ref", "region", "rel", "Schema", "sealed",
      "select", "set", "solve", "spawn", "true", "try", "type", "use", "where", "with"
    ) map keywordCompletion
  }

  private def snippetCompletion(name: String, snippet: String, documentation: String)(implicit context: Context, index: Index, root: TypedAst.Root): CompletionItem = {
    CompletionItem(label = name,
      sortText = "8" + name,
      textEdit = TextEdit(context.range, snippet),
      documentation = Some(documentation),
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Snippet)
  }

  private def getSnippetCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): List[CompletionItem] = {
    List(
      // NB: Please keep the list alphabetically sorted.
      snippetCompletion("main", 
        "def main(): Unit & Impure = \n    println(\"Hello World!\")",
        "snippet for Hello World Program"),
      snippetCompletion("query",
        "query ${1:db} select ${2:cols} from ${3:preds} ${4:where ${5:cond}}",
        "snippet for query")
    )
  }

  private def varCompletion(sym: Symbol.VarSym, tpe: Type)(implicit context: Context, index: Index, root: TypedAst.Root): CompletionItem = {
    CompletionItem(label = sym.text,
      sortText = "5" + sym.text,
      textEdit = TextEdit(context.range, sym.text),
      detail = Some(FormatType.formatWellKindedType(tpe)),
      kind = CompletionItemKind.Variable)
  }

  private def getVarCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): List[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    ///
    /// Find all local variables in the current uri with a given range.
    ///
    val iter = index.queryWithRange(context.uri, queryLine = context.range.start.line, beforeLine = 20, afterLine = 10).collect {
      case Entity.LocalVar(sym, tpe) => varCompletion(sym, tpe)
      case Entity.FormalParam(fparam) => varCompletion(fparam.sym, fparam.tpe)
    }

    iter.toList
  }

  private def getLabelForNameAndSpec(name: String, spec: TypedAst.Spec): String = spec match {
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

  private def getApplySnippet(name: String, fparams: List[TypedAst.FormalParam]): String = {
    val args = fparams.zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:${fparam.sym.text}}"
    }
    s"$name(${args.mkString(", ")})"
  }

  private def defCompletion(decl: TypedAst.Def)(implicit context: Context, index: Index, root: TypedAst.Root): CompletionItem = {
    val name = decl.sym.toString
    CompletionItem(label = getLabelForNameAndSpec(decl.sym.toString, decl.spec),
      sortText = "2" + name,
      textEdit = TextEdit(context.range, getApplySnippet(name, decl.spec.fparams)),
      detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
      documentation = Some(decl.spec.doc.text),
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Function)
  }

  private def sigCompletion(decl: TypedAst.Sig)(implicit context: Context, index: Index, root: TypedAst.Root): CompletionItem = {
    val name = decl.sym.toString
    CompletionItem(label = getLabelForNameAndSpec(decl.sym.toString, decl.spec),
      sortText = "2" + name,
      textEdit = TextEdit(context.range, getApplySnippet(name, decl.spec.fparams)),
      detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
      documentation = Some(decl.spec.doc.text),
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Interface)
  }

  /**
    * Returns `true` if the given definition `decl` should be included in the suggestions.
    */
  private def matchesDef(decl: TypedAst.Def, word: String, uri: String): Boolean = {
    val isPublic = decl.spec.mod.isPublic
    val isNamespace = word.nonEmpty && word.head.isUpper
    val isMatch = if (isNamespace)
                    decl.sym.toString.startsWith(word)
                  else
                    decl.sym.text.startsWith(word)
    val isInFile = decl.sym.loc.source.name == uri

    isMatch && (isPublic || isInFile)
  }

  /**
    * Returns `true` if the given signature `sign` should be included in the suggestions.
    */
  private def matchesSig(sign: TypedAst.Sig, word: String, uri: String): Boolean = {
    val isPublic = sign.spec.mod.isPublic
    val isNamespace = word.nonEmpty && word.head.isUpper
    val isMatch = if (isNamespace)
                    sign.sym.toString.startsWith(word)
                  else
                    sign.sym.name.startsWith(word)
    val isInFile = sign.sym.loc.source.name == uri

    isMatch && (isPublic || isInFile)
  }

  private def getDefAndSigCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    val word = context.word
    val uri = context.uri

    val defSuggestions = root.defs.values.filter(matchesDef(_, word, uri)).map(defCompletion)
    val sigSuggestions = root.sigs.values.filter(matchesSig(_, word, uri)).map(sigCompletion)
    defSuggestions ++ sigSuggestions
  }

  /**
    * Returns a list of completion items based on with type class constraints.
    */
  private def getWithCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    val linePattern = raw".*enum.*\sw.*".r
    val wordPattern = "wi?t?h?".r

    if (linePattern matches context.prefix) {
      root.classes.map {
        case(_, clazz) =>
          val name = clazz.sym.toString
          val completion = if (wordPattern matches context.word) s"with $name" else name
          CompletionItem(label = completion,
            sortText = "1" + name,
            textEdit = TextEdit(context.range, completion),
            documentation = Some(clazz.doc.text),
            kind = CompletionItemKind.Class)
      }
    } else {
      Nil
    }
  }

  /**
    * Returns a list of completion items based on type classes.
    */
  private def getInstanceCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null || context.previousWord != "instance") {
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
      case _: Type.UnkindedArrow => throw InternalCompilerException("Unexpected unkinded arrow.")
      case _: Type.ReadWrite => throw InternalCompilerException("Unexpected unkinded type.")
      case _: Type.Ascribe => throw InternalCompilerException("Unexpected kind ascription.")
    }

    /**
      * Formats the given type `tpe`.
      */
    def fmtType(clazz: TypedAst.Class, tpe: Type, hole: String): String =
      FormatType.formatWellKindedType(replaceText(clazz.tparam.sym, tpe, hole))

    /**
      * Formats the given class `clazz`.
      */
    def fmtClass(clazz: TypedAst.Class): String = {
      s"class ${clazz.sym.name}[${clazz.tparam.name.name}]"
    }

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

    root.classes.map {
      case (_, clazz) =>
        val hole = "${1:t}"
        val classSym = clazz.sym
        val signatures = clazz.signatures.filter(_.impl.isEmpty)
        val body = signatures.map(s => fmtSignature(clazz, s, hole)).mkString("\n\n")
        val completion = s"$classSym[$hole] {\n\n$body\n\n}\n"

        CompletionItem(label = s"$classSym[...]",
          sortText = s"1$classSym",
          textEdit = TextEdit(context.range, completion),
          detail = Some(fmtClass(clazz)),
          documentation = Some(clazz.doc.text),
          insertTextFormat = InsertTextFormat.Snippet,
          kind = CompletionItemKind.Snippet)
    }.toList
  }

  private case class Context(uri: String, range: Range, word: String, previousWord: String, prefix: String)

  /**
    * Characters that constitute a word.
    * This is more permissive than the parser, but that's OK.
    */
  private val isWordChar = Letters.LegalLetter ++ Letters.OperatorLetter ++
      Letters.MathLetter ++ Letters.GreekLetter ++ CharPredicate("@/.")

  /**
    * Given the source, and cursor position within it, find:
    * range: The start and end position of the word underneath (or alongside) the cursor
    * word: The word underneath (or alongside) the cursor
    * previousWord: The word before the above
    * prefix: The text from the start of the line up to the cursor
    */
  private def getContext(source: String, uri: String, pos: Position): Option[Context] = {
      val x = pos.character - 1
      val y = pos.line - 1
      for(line <- source.linesWithSeparators.slice(y, y + 1).toList.headOption) yield {
        val (prefix, suffix) = line.splitAt(x)
        val reversedPrefix = prefix.reverse
        val wordStart = reversedPrefix.takeWhile(isWordChar).reverse
        val wordEnd = suffix.takeWhile(isWordChar)
        val word = wordStart + wordEnd
        val start = x - wordStart.length
        val end = x + wordEnd.length
        val previousWord = reversedPrefix.dropWhile(isWordChar).dropWhile(_.isWhitespace).takeWhile(isWordChar).reverse
        val range = Range(Position(y, start), Position(y, end))
        new Context(uri, range, word, previousWord, prefix)
      }
  }
}
