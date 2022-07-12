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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.language.fmt.{Audience, FormatScheme, FormatType}
import ca.uwaterloo.flix.language.phase.Parser.Letters
import ca.uwaterloo.flix.language.phase.Resolver.DerivableSyms
import ca.uwaterloo.flix.util.InternalCompilerException
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.parboiled2.CharPredicate

/**
  * CompletionProvider
  *
  * Takes a source file, along with the position of the cursor within that file, and returns a list of CompletionItems.
  *
  * https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_completion
  *
  * This list is not displayed to the user as-is, the client always both sorts and filters the list (or at least,
  * VSCode does). Therefore we always need to provide both filterText (currently copied from label) and sortText.
  *
  * Note that we use textEdit rather than insertText to avoid relying on VSCode's tokenisation, so we can ensure that
  * we're consistent with Flix's parser.
  */
object CompletionProvider {
  private implicit val audience: Audience = Audience.External

  //
  // This list manually maintained. If a new built-in type is added, it must be extended.
  // Built-in types are typically described in TypeConstructor, Namer and Resolver.
  //
  val builtinTypeNames: List[String] = List(
    "Unit",
    "Bool",
    "Char",
    "Float32",
    "Float64",
    "Int8",
    "Int16",
    "Int32",
    "Int64",
    "BigInt",
    "String",
    "Array",
    "Ref",
    "Channel",
    "Lazy"
  )

  //
  // To ensure that completions are displayed "most useful" first, we precede sortText with a number. Priorities
  // differ depending on the type of completion, and can be boosted depending upon context (e.g. type completions
  // are boosted if the cursor is preceded by a ":")
  //
  // 1: High: completions which are only available within a very specific context
  // 2: Boost: completions which are normally low priority, but the context makes them more likely
  // 4: Snippet: snippets are relatively high priority because they're rare, and to be useful at all they need to be available
  // 5: Local: local variables
  // 7: Normal: completions that are relevant within no particular context
  // 9: Low: completions that are unlikely to be relevant unless within a specific context
  //
  object Priority {
    def high(name: String): String = "1" + name

    def boost(name: String): String = "2" + name

    def snippet(name: String): String = "4" + name

    def local(name: String): String = "5" + name

    def normal(name: String): String = "7" + name

    def low(name: String): String = "9" + name
  }

  /**
    * Process a completion request.
    */
  def autoComplete(uri: String, pos: Position, source: Option[String], currentErrors: List[CompilationMessage])(implicit index: Index, root: TypedAst.Root): JObject = {
    //
    // To the best of my knowledge, completions should never be Nil. It could only happen if source was None
    // (what would having no source even mean?) or if the position represented by pos was invalid with respect
    // to the source (which would imply a bug in VSCode?).
    //
    val completions = source.flatMap(getContext(_, uri, pos)) match {
      case None => Nil
      case Some(context) => getCompletions()(context, index, root) ++ getCompletionsFromErrors(pos, currentErrors)(context, index, root)
    }

    ("status" -> "success") ~ ("result" -> CompletionList(isIncomplete = true, completions).toJSON)
  }

  private def getCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    //
    // The order of this list doesn't matter because suggestions are ordered
    // through sortText
    //
    getKeywordCompletions() ++
      getSnippetCompletions() ++
      getVarCompletions() ++
      getDefAndSigCompletions() ++
      getWithCompletions() ++
      getInstanceCompletions() ++
      getTypeCompletions()
  }

  /**
    * Returns a list of completions based on the given compilation messages.
    */
  private def getCompletionsFromErrors(pos: Position, errors: List[CompilationMessage])(implicit context: Context, index: Index, root: TypedAst.Root): Iterator[CompletionItem] = {
    val undefinedNames = errors.collect {
      case m: ResolutionError.UndefinedName => m
    }
    closest(pos, undefinedNames) match {
      case None => Iterator.empty
      case Some(undefinedNameError) =>
        val suggestions = undefinedNameError.env.map {
          case (name, sym) => CompletionItem(label = name,
            sortText = Priority.high(name),
            textEdit = TextEdit(context.range, name + " "),
            detail = None,
            kind = CompletionItemKind.Variable)
        }
        suggestions.iterator
    }
  }

  private def keywordCompletion(name: String)(implicit context: Context, index: Index, root: TypedAst.Root): CompletionItem = {
    CompletionItem(label = name,
      sortText = Priority.normal(name),
      textEdit = TextEdit(context.range, s"$name "),
      kind = CompletionItemKind.Keyword)
  }

  private def getKeywordCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): List[CompletionItem] = {
    // TODO: keyword-specific help text?
    // NB: Please keep the list alphabetically sorted.
    List(
      "@benchmark",
      "@Deprecated",
      "@Experimental",
      "@Parallel",
      "@ParallelWhenPure",
      "@Lazy",
      "@LazyWhenPure",
      "@Space",
      "@test",
      "@Time",
      "and",
      "as",
      "case",
      "chan",
      "choose",
      "class",
      "def",
      "deref",
      "discard",
      "do",
      "eff",
      "else",
      "enum",
      "false",
      "fix",
      "forall",
      "force",
      "foreach",
      "from",
      "get",
      "if",
      "import",
      "Impure",
      "instance",
      "into",
      "lat",
      "law",
      "lazy",
      "let",
      "match",
      "namespace",
      "new",
      "not",
      "null",
      "opaque",
      "or",
      "override",
      "project",
      "pub",
      "Pure",
      "query",
      "Record",
      "ref",
      "region",
      "rel",
      "Schema",
      "sealed",
      "select",
      "set",
      "solve",
      "spawn",
      "true",
      "try",
      "type",
      "use",
      "where",
      "with",
      "without"
    ) map keywordCompletion
  }

  private def snippetCompletion(name: String, snippet: String, documentation: String)(implicit context: Context, index: Index, root: TypedAst.Root): CompletionItem = {
    CompletionItem(label = name,
      sortText = Priority.snippet(name),
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
      sortText = Priority.local(sym.text),
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
    case TypedAst.Spec(_, _, _, _, fparams, _, retTpe0, pur0, eff0, _) => // TODO use eff
      val args = fparams.map {
        fparam => s"${fparam.sym.text}: ${FormatType.formatWellKindedType(fparam.tpe)}"
      }

      val retTpe = FormatType.formatWellKindedType(retTpe0)
      val eff = pur0 match {
        case Type.Cst(TypeConstructor.True, _) => "Pure"
        case Type.Cst(TypeConstructor.False, _) => "Impure"
        case e => FormatType.formatWellKindedType(e)
      }

      s"$name(${args.mkString(", ")}): $retTpe & $eff"
  }

  /**
    * Generate a snippet which represents calling a function.
    * Drops the last one or two arguments in the event that the function is in a pipeline
    * (i.e. is preceeded by `|>`, `!>`, or `||>`)
    */
  private def getApplySnippet(name: String, fparams: List[TypedAst.FormalParam])(implicit context: Context): String = {
    val paramsToDrop = context.previousWord match {
      case "||>" => 2
      case "|>" | "!>" => 1
      case _ => 0
    }
    val args = fparams.dropRight(paramsToDrop).zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:${fparam.sym.text}}"
    }
    if (args.nonEmpty)
      s"$name(${args.mkString(", ")})"
    else
      name
  }

  /**
    * Under some circumstances, even though we set `isIncomplete`, which is supposed to opt-out
    * of this behaviour, VSCode filters returned completions when the user types more text
    * without calling the language server again (so it has no chance to return different
    * completions).
    *
    * If we use `label` as filter text (which is the default), this can result in many false
    * positives, e.g. if the user types "MyList[t", the "t" will result in many potential Def
    * and Sig completions. If the user then types "]" VSCode will filter this list using the
    * "word" "t]" which will match many of these completions (e.g. "Nec.tail(c: Nec[a]): ...").
    *
    * To avoid this behaviour, we set `filterText` for Def and Sig completions to be just the
    * name. The "(" is there so that they still see completions if they enter the opening
    * bracket of a function call (but not if they start filling in the argument list).
    */
  private def getFilterTextForName(name: String): String = {
    s"${name}("
  }

  private def defCompletion(decl: TypedAst.Def)(implicit context: Context, index: Index, root: TypedAst.Root): CompletionItem = {
    val name = decl.sym.toString
    CompletionItem(label = getLabelForNameAndSpec(decl.sym.toString, decl.spec),
      sortText = Priority.normal(name),
      filterText = Some(getFilterTextForName(name)),
      textEdit = TextEdit(context.range, getApplySnippet(name, decl.spec.fparams)),
      detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
      documentation = Some(decl.spec.doc.text),
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Function)
  }

  private def sigCompletion(decl: TypedAst.Sig)(implicit context: Context, index: Index, root: TypedAst.Root): CompletionItem = {
    val name = decl.sym.toString
    CompletionItem(label = getLabelForNameAndSpec(decl.sym.toString, decl.spec),
      sortText = Priority.normal(name),
      filterText = Some(getFilterTextForName(name)),
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
    def isInternal(decl: TypedAst.Def): Boolean =
      decl.spec.ann.exists(a => a.name match {
        case Ast.Annotation.Internal(_) => true
        case _ => false
      })

    val isPublic = decl.spec.mod.isPublic && !isInternal(decl)
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

    //
    // When used with `enum`, `with` needs to be treated differently: we should only show derivable
    // type classes, and we shouldn't include the type parameter
    //

    val enumPattern = raw"\s*enum\s+(.*\s)wi?t?h?\s?.*".r
    val withPattern = raw"\s*(def|instance|class)\s+(.*\s)wi?t?h?\s?.*".r
    val wordPattern = "wi?t?h?".r

    val currentWordIsWith = wordPattern matches context.word

    if (enumPattern matches context.prefix) {
      for {
        (_, clazz) <- root.classes
        sym = clazz.sym
        if DerivableSyms.contains(sym)
        name = sym.toString
        completion = if (currentWordIsWith) s"with $name" else name
      } yield
        CompletionItem(label = completion,
          sortText = Priority.high(name),
          textEdit = TextEdit(context.range, completion),
          documentation = Some(clazz.doc.text),
          kind = CompletionItemKind.Class)
    } else if (withPattern.matches(context.prefix) || currentWordIsWith) {
      root.classes.map {
        case (_, clazz) =>
          val name = clazz.sym.toString
          val hole = "${1:t}"
          val application = s"$name[$hole]"
          val completion = if (currentWordIsWith) s"with $application" else application
          val label = if (currentWordIsWith) s"with $name[...]" else s"$name[...]"
          CompletionItem(label = label,
            sortText = Priority.high(name),
            textEdit = TextEdit(context.range, completion),
            documentation = Some(clazz.doc.text),
            insertTextFormat = InsertTextFormat.Snippet,
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
      val pur = sig.spec.pur match {
        case Type.Cst(TypeConstructor.True, _) => ""
        case Type.Cst(TypeConstructor.False, _) => " & Impure"
        case e => " & " + FormatType.formatWellKindedType(e)
      }
      s"    pub def ${sig.sym.name}($fparams): $retTpe$pur = ???"
    }

    root.classes.map {
      case (_, clazz) =>
        val hole = "${1:t}"
        val classSym = clazz.sym
        val signatures = clazz.signatures.filter(_.impl.isEmpty)
        val body = signatures.map(s => fmtSignature(clazz, s, hole)).mkString("\n\n")
        val completion = s"$classSym[$hole] {\n\n$body\n\n}\n"

        CompletionItem(label = s"$classSym[...]",
          sortText = Priority.high(classSym.toString),
          textEdit = TextEdit(context.range, completion),
          detail = Some(fmtClass(clazz)),
          documentation = Some(clazz.doc.text),
          insertTextFormat = InsertTextFormat.Snippet,
          kind = CompletionItemKind.Snippet)
    }.toList
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

  /**
    * Completions for types (enums, aliases, and built-in types)
    */
  private def getTypeCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    // Boost priority if there's a colon immediately before the word the user's typing
    val priorityBoost = raw".*:\s*[^\s]*".r
    val priority = if (priorityBoost matches context.prefix) Priority.boost _ else Priority.low _

    val enums = root.enums.map {
      case (_, t) =>
        val name = t.sym.name
        CompletionItem(label = s"$name${formatTParams(t.tparams)}",
          sortText = priority(name),
          textEdit = TextEdit(context.range, s"$name${formatTParamsSnippet(t.tparams)}"),
          documentation = Some(t.doc.text),
          insertTextFormat = InsertTextFormat.Snippet,
          kind = CompletionItemKind.Enum)
    }

    val aliases = root.typeAliases.map {
      case (_, t) =>
        val name = t.sym.name
        CompletionItem(label = s"$name${formatTParams(t.tparams)}",
          sortText = priority(name),
          textEdit = TextEdit(context.range, s"$name${formatTParamsSnippet(t.tparams)}"),
          documentation = Some(t.doc.text),
          insertTextFormat = InsertTextFormat.Snippet,
          kind = CompletionItemKind.Enum)
    }

    val builtinTypes = builtinTypeNames map { name =>
      CompletionItem(label = name,
        sortText = priority(name),
        textEdit = TextEdit(context.range, name),
        kind = CompletionItemKind.Enum)
    }

    enums ++ aliases ++ builtinTypes
  }

  /*
   * @param uri          Source file URI (from client)
   * @param range        Start and end position of the word underneath (or alongside) the cursor
   * @param word         The word underneath (or alongside) the cursor
   * @param previousWord The word before the above (note that this may be on either the current or previous line)
   * @param prefix       The text from the start of the line up to the cursor
   */
  private case class Context(uri: String, range: Range, word: String, previousWord: String, prefix: String)

  /**
    * Characters that constitute a word.
    * This is more permissive than the parser, but that's OK.
    */
  private val isWordChar = Letters.LegalLetter ++ Letters.OperatorLetter ++
    Letters.MathLetter ++ Letters.GreekLetter ++ CharPredicate("@/.")

  /**
    * Returns the word at the end of a string, discarding trailing whitespace first
    */
  private def getLastWord(s: String) = {
    s.reverse.dropWhile(_.isWhitespace).takeWhile(isWordChar).reverse
  }

  /**
    * Returns the second-to-last word at the end of a string, *not* discarding
    * trailing whitespace first.
    */
  private def getSecondLastWord(s: String) = {
    s.reverse.dropWhile(isWordChar).dropWhile(_.isWhitespace).takeWhile(isWordChar).reverse
  }

  /**
    * Find context from the source, and cursor position within it.
    */
  private def getContext(source: String, uri: String, pos: Position): Option[Context] = {
    val x = pos.character - 1
    val y = pos.line - 1
    val lines = source.linesWithSeparators.toList
    for (line <- lines.slice(y, y + 1).headOption) yield {
      val (prefix, suffix) = line.splitAt(x)
      val wordStart = prefix.reverse.takeWhile(isWordChar).reverse
      val wordEnd = suffix.takeWhile(isWordChar)
      val word = wordStart + wordEnd
      val start = x - wordStart.length
      val end = x + wordEnd.length
      val prevWord = getSecondLastWord(prefix)
      val previousWord = if (prevWord.nonEmpty) {
        prevWord
      } else lines.slice(y - 1, y).headOption match {
        case None => ""
        case Some(s) => getLastWord(s)
      }
      val range = Range(Position(y, start), Position(y, end))
      Context(uri, range, word, previousWord, prefix)
    }
  }

  /**
    * Optionally returns the error message in `l` closest to the given position `pos`.
    */
  private def closest[T <: CompilationMessage](pos: Position, l: List[T]): Option[T] = {
    if (l.isEmpty)
      None
    else
      Some(l.minBy(msg => lineDistance(pos, msg.loc)))
  }

  /**
    * Returns the line distance between `pos` and `loc`.
    *
    * Returns `Int.MaxValue` if `loc` is Unknown.
    */
  private def lineDistance(pos: Position, loc: SourceLocation): Int =
    if (loc == SourceLocation.Unknown)
      Int.MaxValue
    else
      Math.abs(pos.line - loc.beginLine)

}
