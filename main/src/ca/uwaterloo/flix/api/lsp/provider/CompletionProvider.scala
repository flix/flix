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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp._
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.language.fmt.{Audience, FormatScheme, FormatType}
import ca.uwaterloo.flix.language.phase.Parser.Letters
import ca.uwaterloo.flix.language.phase.Resolver.DerivableSyms
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._
import org.parboiled2.CharPredicate
import java.lang.reflect.Executable
import java.lang.reflect.Constructor
import java.lang.reflect.Method
import ca.uwaterloo.flix.util.collection.MultiMap
import java.lang.reflect.Field

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
  private val BuiltinTypeNames: List[String] = List(
    "Unit",
    "Bool",
    "Char",
    "Float64",
    "BigDecimal",
    "Int32",
    "Int64",
    "BigInt",
    "String"
  )

  // Built-in types with hardcoded low priority
  private val LowPriorityBuiltinTypeNames: List[String] = List(
    "Int8",
    "Int16",
    "Float32"
  )

  // Built-in types with type parameters
  private val BuiltinTypeNamesWithTypeParameters: List[(String, List[String])] = List(
    ("Array", List("a", "r")),
    ("Ref", List("a", "r")),
    ("Sender", List("t", "r")),
    ("Receiver", List("t", "r")),
    ("Lazy", List("t"))
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
  def autoComplete(uri: String, pos: Position, source: Option[String], currentErrors: List[CompilationMessage])(implicit flix: Flix, index: Index, root: TypedAst.Root): JObject = {
    val holeCompletions = getHoleExpCompletions(pos, uri, index, root)
    // If we are currently on a hole the only useful completion is a hole completion.
    if (holeCompletions.nonEmpty) {
      return ("status" -> "success") ~ ("result" -> CompletionList(isIncomplete = false, holeCompletions).toJSON)
    }

    //
    // To the best of my knowledge, completions should never be Nil. It could only happen if source was None
    // (what would having no source even mean?) or if the position represented by pos was invalid with respect
    // to the source (which would imply a bug in VSCode?).
    //
    val completions = source.flatMap(getContext(_, uri, pos)) match {
      case None => Nil
      case Some(context) => getCompletions()(context, flix, index, root) ++ getCompletionsFromErrors(pos, currentErrors)(context, index, root)
    }

    ("status" -> "success") ~ ("result" -> CompletionList(isIncomplete = true, completions).toJSON)
  }

  /**
    * Gets completions for when the cursor position is on a hole expression with an expression
    */
  private def getHoleExpCompletions(pos: Position, uri: String, index: Index, root: TypedAst.Root)(implicit flix: Flix): Iterable[CompletionItem] = {
    if (root == null) return Nil
    val entity = index.query(uri, pos)
    entity match {
      case Some(Entity.Exp(TypedAst.Expression.HoleWithExp(TypedAst.Expression.Var(sym, sourceType, _), targetType, _, _, loc))) =>
        HoleCompletion.candidates(sourceType, targetType, root)
          .map((root.defs(_)))
          .filter(_.spec.mod.isPublic)
          .zipWithIndex
          .map { case (decl, idx) => holeDefCompletion(f"$idx%09d", uri, loc, sym, decl, root) }
      case _ => Nil
    }
  }

  /**
    * Creates a completion item from a hole with expression and a def.
    */
  private def holeDefCompletion(priority: String, uri: String, loc: SourceLocation, sym: Symbol.VarSym, decl: TypedAst.Def, root: TypedAst.Root)(implicit flix: Flix): CompletionItem = {
    val name = decl.sym.toString
    val args = decl.spec.fparams.dropRight(1).zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.sym.text}}"
    } ::: sym.text :: Nil
    val params = args.mkString(", ")
    val snippet = s"$name($params)"
    CompletionItem(label = getLabelForNameAndSpec(decl.sym.toString, decl.spec),
      filterText = Some(s"${sym.text}?$name"),
      sortText = priority,
      textEdit = TextEdit(Range.from(loc), snippet),
      detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
      documentation = Some(decl.spec.doc.text),
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Function)
  }

  private def getCompletions()(implicit context: Context, flix: Flix, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    // If we match one of the we know what type of completion we need
    val withRegex = raw".*\s*wi?t?h?(?:\s+[^\s]*)?".r
    val typeRegex = raw".*:\s*(?:[^\s]|(?:\s*,\s*))*".r
    val typeAliasRegex = raw"\s*type\s+alias\s+.+\s*=\s*(?:[^\s]|(?:\s*,\s*))*".r
    val effectRegex = raw".*[\\]\s*[^\s]*".r
    val importRegex = raw"\s*import\s+.*".r
    val useRegex = raw"\s*use\s+[^\s]*".r
    val instanceRegex = raw"\s*instance\s+[^s]*".r

    // if any of the following matches we do not want any completions
    val defRegex = raw"\s*def\s+[^=]*".r
    val enumRegex = raw"\s*enum\s+.*".r
    val incompleteTypeAliasRegex = raw"\s*type\s+alias\s+.*".r
    val classRegex = raw"\s*class\s+.*".r
    val letRegex = raw"\s*let\s+[^\s]*".r
    val letStarRegex = raw"\s*let[\*]\s+[^\s]*".r
    val namespaceRegex = raw"\s*namespace\s+.*".r
    val tripleQuestionMarkRegex = raw"\?|.*\s+\?.*".r
    val underscoreRegex = raw"(?:(?:.*\s+)|)_[^s]*".r

    // if any of the following matches we know the next must be an expression
    val channelKeywordRegex = raw".*<-\s*[^\s]*".r
    val doubleColonRegex = raw".*::\s*[^\s]*".r
    val tripleColonRegex = raw".*:::\s*[^\s]*".r

    // We check type and effect first because for example following def we do not want completions other than type and effect if applicable.
    context.prefix match {
      case channelKeywordRegex() | doubleColonRegex() | tripleColonRegex() => getExpCompletions()
      case withRegex() => getWithCompletions()
      case typeRegex() | typeAliasRegex() => getTypeCompletions()
      case effectRegex() => getEffectCompletions()
      case defRegex() | enumRegex() | incompleteTypeAliasRegex() | classRegex() | letRegex() | letStarRegex() | namespaceRegex() | underscoreRegex() | tripleQuestionMarkRegex() => Nil
      case importRegex() => getImportCompletions()
      case useRegex() => getUseCompletions()
      case instanceRegex() => getInstanceCompletions()
      //
      // The order of this list doesn't matter because suggestions are ordered
      // through sortText
      //
      case _ => getExpCompletions() ++
        getPredicateCompletions() ++
        getTypeCompletions() ++
        getEffectCompletions()
    }
  }

  /**
    * Returns a list of completions that may be used in a position where an expression is needed.
    * This should include all completions supported that could be an expression.
    * All of the completions are not neccesarily sound.
    */
  private def getExpCompletions()(implicit context: Context, flix: Flix, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    getKeywordCompletions() ++
      getSnippetCompletions() ++
      getVarCompletions() ++
      getDefAndSigCompletions() ++
      getFieldCompletions() ++
      getOpCompletions() ++
      getMatchCompletitions()
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
    // NB: Please keep the list alphabetically sorted.
    List(
      "@Deprecated",
      "@Parallel",
      "@ParallelWhenPure",
      "@Lazy",
      "@LazyWhenPure",
      "@Test",
      "and",
      "as",
      "case",
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
      "for",
      "forall",
      "force",
      "foreach",
      "from",
      "get",
      "if",
      "inject",
      "import",
      "instance",
      "into",
      "lat",
      "law",
      "lazy",
      "let",
      "match",
      "mod",
      "new",
      "not",
      "null",
      "opaque",
      "or",
      "override",
      "par",
      "pub",
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
      "typematch",
      "use",
      "where",
      "with",
      "without",
      "yield"
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
        "def main(): Unit \\ IO = \n    println(\"Hello World!\")",
        "snippet for Hello World Program"),
      snippetCompletion("query",
        "query ${1:db} select ${2:cols} from ${3:preds} ${4:where ${5:cond}}",
        "snippet for query")
    )
  }

  private def varCompletion(sym: Symbol.VarSym, tpe: Type)(implicit context: Context, index: Index, root: TypedAst.Root, flix: Flix): CompletionItem = {
    CompletionItem(label = sym.text,
      sortText = Priority.local(sym.text),
      textEdit = TextEdit(context.range, sym.text),
      detail = Some(FormatType.formatType(tpe)),
      kind = CompletionItemKind.Variable)
  }

  private def getVarCompletions()(implicit context: Context, index: Index, root: TypedAst.Root, flix: Flix): List[CompletionItem] = {
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

  private def isUnitType(tpe: Type): Boolean = tpe == Type.Unit

  private def isUnitFunction(fparams: List[TypedAst.FormalParam]): Boolean = fparams.length == 1 && isUnitType(fparams(0).tpe)

  private def getLabelForNameAndSpec(name: String, spec: TypedAst.Spec)(implicit flix: Flix): String = spec match {
    case TypedAst.Spec(_, _, _, _, fparams, _, retTpe0, pur0, eff0, _, _) =>
      val args = if (isUnitFunction(fparams))
        Nil
      else
        fparams.map {
          fparam => s"${fparam.sym.text}: ${FormatType.formatType(fparam.tpe)}"
        }

      val retTpe = FormatType.formatType(retTpe0)

      // don't show purity if bool effects are turned off
      val pur = if (flix.options.xnobooleffects) {
        ""
      } else {
        pur0 match {
          case Type.Cst(TypeConstructor.True, _) => ""
          case Type.Cst(TypeConstructor.False, _) => " & Impure"
          case p => " & " + FormatType.formatType(p)
        }
      }

      // don't show effect if set effects are turned off
      val eff = if (flix.options.xnoseteffects) {
        ""
      } else {
        eff0 match {
          case Type.Cst(TypeConstructor.Empty, _) => ""
          case e => " \\ " + FormatType.formatType(e)
        }
      }

      s"$name(${args.mkString(", ")}): $retTpe$pur$eff"
  }

  /**
    * Generate a snippet which represents calling a function.
    * Drops the last one or two arguments in the event that the function is in a pipeline
    * (i.e. is preceeded by `|>`, `!>`, or `||>`)
    * Returns None if there are two few arguments.
    */
  private def getApplySnippet(name: String, fparams: List[TypedAst.FormalParam])(implicit context: Context): Option[String] = {
    val paramsToDrop = context.previousWord match {
      case "||>" => 2
      case "|>" | "!>" => 1
      case _ => 0
    }

    val functionIsUnit = isUnitFunction(fparams)

    if (paramsToDrop > fparams.length || (functionIsUnit && paramsToDrop > 0)) return None

    val args = fparams.dropRight(paramsToDrop).zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.sym.text}}"
    }
    if (functionIsUnit)
      Some(s"$name()")
    else if (args.nonEmpty)
      Some(s"$name(${args.mkString(", ")})")
    else
      Some(name)
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

  private def defCompletion(decl: TypedAst.Def)(implicit context: Context, flix: Flix, index: Index, root: TypedAst.Root): Option[CompletionItem] = {
    val name = decl.sym.toString
    getApplySnippet(name, decl.spec.fparams).map(snippet => {
      CompletionItem(label = getLabelForNameAndSpec(decl.sym.toString, decl.spec),
        sortText = Priority.normal(name),
        filterText = Some(getFilterTextForName(name)),
        textEdit = TextEdit(context.range, snippet),
        detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
        documentation = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Function)
    })
  }

  private def sigCompletion(decl: TypedAst.Sig)(implicit context: Context, flix: Flix, index: Index, root: TypedAst.Root): Option[CompletionItem] = {
    val name = decl.sym.toString
    getApplySnippet(name, decl.spec.fparams).map(snippet =>
      CompletionItem(label = getLabelForNameAndSpec(decl.sym.toString, decl.spec),
        sortText = Priority.normal(name),
        filterText = Some(getFilterTextForName(name)),
        textEdit = TextEdit(context.range, snippet),
        detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
        documentation = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Interface))
  }

  private def opCompletion(decl: TypedAst.Op)(implicit context: Context, flix: Flix, index: Index, root: TypedAst.Root): Option[CompletionItem] = {
    // NB: priority is high because only an op can come after `do`
    val name = decl.sym.toString
    getApplySnippet(name, decl.spec.fparams).map(snippet =>
      CompletionItem(label = getLabelForNameAndSpec(decl.sym.toString, decl.spec),
        sortText = Priority.high(name),
        filterText = Some(getFilterTextForName(name)),
        textEdit = TextEdit(context.range, snippet),
        detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
        documentation = Some(decl.spec.doc.text),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Interface))
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

  /**
    * Returns `true` if the given effect operation `op` should be included in the suggestions.
    */
  private def matchesOp(op: TypedAst.Op, word: String, uri: String): Boolean = {
    val isPublic = op.spec.mod.isPublic
    val isNamespace = word.nonEmpty && word.head.isUpper
    val isMatch = if (isNamespace)
      op.sym.toString.startsWith(word)
    else
      op.sym.name.startsWith(word)
    val isInFile = op.sym.loc.source.name == uri

    isMatch && (isPublic || isInFile)
  }

  private def getDefAndSigCompletions()(implicit context: Context, flix: Flix, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    val word = context.word
    val uri = context.uri

    val defSuggestions = root.defs.values.filter(matchesDef(_, word, uri)).flatMap(defCompletion)
    val sigSuggestions = root.sigs.values.filter(matchesSig(_, word, uri)).flatMap(sigCompletion)
    defSuggestions ++ sigSuggestions
  }

  private def getOpCompletions()(implicit context: Context, flix: Flix, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null || context.previousWord != "do") {
      return Nil
    }

    val word = context.word
    val uri = context.uri

    root.effects.values.flatMap(_.ops).filter(matchesOp(_, word, uri)).flatMap(opCompletion)
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
    * Returns a list of completion items for match type completions
    */
  private def getMatchCompletitions()(implicit context: Context, index: Index, root: TypedAst.Root, flix: Flix): Iterable[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    val matchPattern = raw".*\s*ma?t?c?h?\s?.*".r

    if (!(matchPattern matches context.prefix)) {
      return Nil
    }

    val wordPattern = "ma?t?c?h?".r
    val currentWordIsMatch = wordPattern matches context.word

    root.enums.foldLeft[List[CompletionItem]](Nil)((acc, enm) => {
      if (enm._2.cases.size >= 2) matchCompletion(enm._2, currentWordIsMatch) match {
        case Some(v) => v :: acc
        case None => acc
      }
      else acc
    })
  }

  /**
    * Converts an enum into a exhaustive match completion
    */
  private def matchCompletion(enm: TypedAst.Enum, currentWordIsMatch: Boolean)(implicit context: Context, flix: Flix): Option[CompletionItem] = {
    val includeMatch = if (currentWordIsMatch) "match " else ""
    val priority: String => String = if (enm.loc.source.name == context.uri) {
      Priority.high
    }
    else if (enm.mod.isPublic && enm.sym.namespace.isEmpty) {
      Priority.boost
    }
    else {
      return None
    }
    val (completion, _) = enm.cases.toList.sortBy(_._1.loc).foldLeft(("", 1))({
      case ((acc, z), (sym, cas)) => {
        val name = sym.name
        val (str, k) = cas.tpe.typeConstructor match {
          case Some(TypeConstructor.Unit) => (s"$name => $${${z + 1}:???}", z + 1)
          case Some(TypeConstructor.Tuple(arity)) => (List.range(1, arity + 1)
            .map(elem => s"$${${elem + z}:_elem$elem}")
            .mkString(s"$name(", ", ", s") => $${${arity + z + 1}:???}"), z + arity + 1)
          case _ => (s"$name($${${z + 1}:_elem}) => $${${z + 2}:???}", z + 2)
        }
        (acc + "    case " + str + "\n", k)
      }
    })
    Some(matchCompletion(enm.sym.name, s"$includeMatch$${1:???} {\n$completion}", priority))
  }

  /**
    * Creates a completion item for an exhaustive match
    */
  private def matchCompletion(sym: String, completion: String, priority: String => String)(implicit context: Context): CompletionItem = {
    val label = s"match $sym"
    CompletionItem(label = label,
      sortText = priority(label),
      textEdit = TextEdit(context.range, completion),
      documentation = None,
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Snippet)
  }

  /**
    * Returns a list of completion for predicates
    */
  private def getPredicateCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    index.predDefs.m.concat(index.predUses.m).foldLeft[List[CompletionItem]](Nil)({
      case (acc, (pred, locs)) => {
        val priority: String => String = if (locs.exists(loc => loc.source.name == context.uri)) Priority.boost else Priority.low
        val name = pred.name
        CompletionItem(label = name,
          sortText = priority(name),
          textEdit = TextEdit(context.range, name),
          documentation = None,
          insertTextFormat = InsertTextFormat.PlainText,
          kind = CompletionItemKind.Variable) :: acc
      }
    })
  }

  /**
    * Gets completions for record fields
    */
  private def getFieldCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    // Do not get field completions if we are importing or using.
    if (root == null || context.prefix.contains("import") || context.prefix.contains("use")) {
      return Nil
    }

    val regex = raw"(.*)[.].*".r

    context.word match {
      case regex(prefix) => {
        index.fieldDefs.m.concat(index.fieldUses.m)
          .filter { case (_, locs) => locs.exists(loc => loc.source.name == context.uri) }
          .foldLeft[List[CompletionItem]](Nil) {
            case (acc, (field, locs)) => {
              val name = s"$prefix.${field.name}"
              CompletionItem(label = name,
                sortText = Priority.high(name),
                textEdit = TextEdit(context.range, name),
                documentation = None,
                insertTextFormat = InsertTextFormat.PlainText,
                kind = CompletionItemKind.Variable) :: acc
            }
          }
      }
      case _ => Nil
    }
  }

  /**
    * Returns a list of completion items based on type classes.
    */
  private def getInstanceCompletions()(implicit context: Context, index: Index, root: TypedAst.Root, flix: Flix): Iterable[CompletionItem] = {
    if (root == null || context.previousWord != "instance") {
      return Nil
    }

    /**
      * Replaces the text in the given variable symbol `sym` everywhere in the type `tpe`
      * with an equivalent variable symbol with the given `newText`.
      */
    def replaceText(tvar: Symbol.KindedTypeVarSym, tpe: Type, newText: String): Type = tpe match {
      case Type.Var(sym, loc) if tvar == sym => Type.Var(sym.withText(Ast.VarText.SourceText(newText)), loc)
      case Type.Var(_, _) => tpe
      case Type.Cst(_, _) => tpe

      case Type.Apply(tpe1, tpe2, loc) =>
        val t1 = replaceText(tvar, tpe1, newText)
        val t2 = replaceText(tvar, tpe2, newText)
        Type.Apply(t1, t2, loc)

      case Type.Alias(sym, args0, tpe0, loc) =>
        val args = args0.map(replaceText(tvar, _, newText))
        val t = replaceText(tvar, tpe0, newText)
        Type.Alias(sym, args, t, loc)
    }

    /**
      * Formats the given type `tpe`.
      */
    def fmtType(clazz: TypedAst.Class, tpe: Type, hole: String)(implicit flix: Flix): String =
      FormatType.formatType(replaceText(clazz.tparam.sym, tpe, hole))

    /**
      * Formats the given class `clazz`.
      */
    def fmtClass(clazz: TypedAst.Class): String = {
      s"class ${clazz.sym.name}[${clazz.tparam.name.name}]"
    }

    /**
      * Formats the given formal parameters in `spec`.
      */
    def fmtFormalParams(clazz: TypedAst.Class, spec: TypedAst.Spec, hole: String)(implicit flix: Flix): String =
      spec.fparams.map {
        case fparam => s"${fparam.sym.text}: ${fmtType(clazz, fparam.tpe, hole)}"
      }.mkString(", ")

    /**
      * Formats the given signature `sig`.
      */
    def fmtSignature(clazz: TypedAst.Class, sig: TypedAst.Sig, hole: String)(implicit flix: Flix): String = {
      val fparams = fmtFormalParams(clazz, sig.spec, hole)
      val retTpe = fmtType(clazz, sig.spec.retTpe, hole)
      val pur = sig.spec.pur match {
        case Type.Cst(TypeConstructor.True, _) => ""
        case Type.Cst(TypeConstructor.False, _) => " & Impure"
        case e => " & " + FormatType.formatType(e)
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

    def getInternalPriority(loc: SourceLocation, ns: List[String]): String => String = {
      if (loc.source.name == context.uri)
        Priority.boost _
      else if (ns.isEmpty)
        Priority.normal _
      else
        Priority.low _
    }

    // Boost priority if there's a colon immediately before the word the user's typing
    val typePriorityBoost = raw".*:\s*(?:[^\s]|(?:\s*,\s*))*".r
    val typeAliasPriorityBoost = raw"\s*type\s+alias\s+.+\s*=\s*(?:[^\s]|(?:\s*,\s*))*".r
    val priority = if ((typePriorityBoost matches context.prefix) || (typeAliasPriorityBoost matches context.prefix)) Priority.boost _ else Priority.low _

    val enums = root.enums.map {
      case (_, t) =>
        val name = t.sym.name
        val internalPriority = getInternalPriority(t.loc, t.sym.namespace)
        CompletionItem(label = s"$name${formatTParams(t.tparams)}",
          sortText = priority(internalPriority(name)),
          textEdit = TextEdit(context.range, s"$name${formatTParamsSnippet(t.tparams)}"),
          documentation = Some(t.doc.text),
          insertTextFormat = InsertTextFormat.Snippet,
          kind = CompletionItemKind.Enum)
    }

    val aliases = root.typeAliases.map {
      case (_, t) =>
        val name = t.sym.name
        val internalPriority = getInternalPriority(t.loc, t.sym.namespace)
        CompletionItem(label = s"$name${formatTParams(t.tparams)}",
          sortText = priority(internalPriority(name)),
          textEdit = TextEdit(context.range, s"$name${formatTParamsSnippet(t.tparams)}"),
          documentation = Some(t.doc.text),
          insertTextFormat = InsertTextFormat.Snippet,
          kind = CompletionItemKind.Enum)
    }

    val builtinTypes = BuiltinTypeNames map { name =>
      val internalPriority = Priority.high _
      CompletionItem(label = name,
        sortText = priority(internalPriority(name)),
        textEdit = TextEdit(context.range, name),
        kind = CompletionItemKind.Enum)
    }

    val lowPriorityBuiltinTypes = LowPriorityBuiltinTypeNames map { name =>
      val internalPriority = Priority.low _
      CompletionItem(label = name,
        sortText = priority(internalPriority(name)),
        textEdit = TextEdit(context.range, name),
        kind = CompletionItemKind.Enum)
    }

    val builtinTypesWithParams = BuiltinTypeNamesWithTypeParameters map { case (name, tparams) =>
      val internalPriority = Priority.boost _
      val fmtTparams = tparams.zipWithIndex.map { case (name, idx) => s"$${${idx + 1}:$name}" }.mkString(", ")
      val finalName = s"$name[${tparams.mkString(", ")}]"
      CompletionItem(label = finalName,
        sortText = priority(internalPriority(name)),
        textEdit = TextEdit(context.range, s"$name[$fmtTparams]"),
        insertTextFormat = InsertTextFormat.Snippet,
        kind = CompletionItemKind.Enum)
    }

    enums ++ aliases ++ builtinTypes ++ lowPriorityBuiltinTypes ++ builtinTypesWithParams
  }

  /**
    * Completions for Effects
    */
  private def getEffectCompletions()(implicit context: Context, index: Index, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    // Boost priority if there is `\` or `\ {` immediately before the word the user is typing
    val effSetPrefix = raw".*\\\s*\{?\s*\S*".r

    val priority = if (context.previousWord == "without") {
      // If the last word is `without`, we can be very sure an effect is coming.
      Priority.high _
    } else if (effSetPrefix.matches(context.prefix) || context.previousWord == "with") {
      // If the last word is `with` or looks like `\` or `\ {`, it is likely an effect but could be something else.
      Priority.boost _
    } else {
      // Otherwise it's probably not an effect.
      Priority.low _
    }

    root.effects.map {
      case (_, t) =>
        val name = t.sym.name
        CompletionItem(label = name,
          sortText = priority(name),
          textEdit = TextEdit(context.range, name),
          documentation = Some(t.doc.text),
          insertTextFormat = InsertTextFormat.Snippet,
          kind = CompletionItemKind.Enum)
    }
  }

  /**
    * Gets completions after use keyword
    */
  private def getUseCompletions()(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null) {
      return Nil
    }

    val regex = raw"\s*use\s+(.*)".r

    context.prefix match {
      case regex(ns) => {
        // Six cases
        // 0: We have nothing i.e.
        // 1: a path i.e. A/B
        // 2: an item i.e. Foo (enum/class/def/type alias)
        // 3: path and item i.e. A/B.Foo
        // 4: item and tag/sig i.e Foo.Bar
        // 5: path, item and tag/sig i.e A/B.Foo.Bar

        val segments = ns.split('.');
        segments.toList match {
          // case 0
          case Nil => nsCompletionsAfterPrefix(Nil) ++ getItemUseCompletions(Nil)
          // case 1/2
          case x :: Nil => {
            // We might be done typing the namespace or not. We need to try both cases
            val prefix1 = x.split('/').toList;
            val prefix2 = x.split('/').dropRight(1).toList;
            // case 1
            nsCompletionsAfterPrefix(prefix1) ++
              nsCompletionsAfterPrefix(prefix2) ++
              getItemUseCompletions(prefix1) ++
              getItemUseCompletions(prefix2) ++
              // case 2
              getEnumTagCompletions(Nil, x) ++
              getClassSigCompletions(Nil, x)
          }
          // case 3/4
          case x :: y :: Nil => {
            val ns = x.split('/').toList;
            // case 3
            getItemUseCompletions(ns) ++
              getEnumTagCompletions(ns, y) ++
              getClassSigCompletions(ns, y) ++
              // case 4
              getEnumTagCompletions(Nil, x) ++
              getClassSigCompletions(Nil, x)
          }
          // case 5
          case x :: y :: _ :: Nil => {
            val ns = x.split('/').toList;
            getEnumTagCompletions(ns, y) ++
              getClassSigCompletions(ns, y)
          }
          case _ => Nil
        }
      }
      case _ => Nil
    }
  }

  /**
    * Gets completions for a sub namespace of a prefix namespace
    * I.e if you have namespace A/B/C/D, then if prefix is A/B it will return a completion for A/B/C
    */
  private def nsCompletionsAfterPrefix(prefix: List[String])(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    val nss = root.defs.keySet.map(_.namespace) ++
      root.enums.keySet.map(_.namespace) ++
      root.classes.keySet.map(_.namespace) ++
      root.typeAliases.keySet.map(_.namespace);

    nss.flatMap(ns => getFirstAfterGivenPrefix(ns, prefix))
      .map(nextNs => {
        val name = prefix.appended(nextNs).mkString("/")
        useCompletion(name, CompletionItemKind.Module)
      })
  }

  /**
    * Returns the first namespace after a given prefix if the prefix is a prefix and there is a next
    */
  private def getFirstAfterGivenPrefix(ns: List[String], prefix: List[String]): Option[String] = {
    (ns, prefix) match {
      case (x :: _, Nil) => Some(x)
      case (x :: xs, y :: ys) if x == y => getFirstAfterGivenPrefix(xs, ys)
      case _ => None
    }
  }

  /**
    * Gets completions for all items in a namespace
    */
  private def getItemUseCompletions(ns: List[String])(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    getEnumUseCompletions(ns) ++
      getClassUseCompletions(ns) ++
      getDefUseCompletions(ns) ++
      getTypeUseCompletions(ns)
  }

  /**
    * Gets completions for enums in a given namespace
    */
  private def getEnumUseCompletions(ns: List[String])(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    root.enums.filter { case (sym, emn) => emn.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => useCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Enum))
  }

  /**
    * Gets completions for classes in a given namespace
    */
  private def getClassUseCompletions(ns: List[String])(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    root.classes.filter { case (sym, clazz) => clazz.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => useCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Interface))
  }

  /**
    * Gets completions for functions in a given namespace
    */
  private def getDefUseCompletions(ns: List[String])(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    root.defs.filter { case (sym, df) => df.spec.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => useCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Function))
  }

  /**
    * Gets completion for type aliases in a given namespace
    */
  private def getTypeUseCompletions(ns: List[String])(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    root.typeAliases.filter { case (sym, tpe) => tpe.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => useCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Struct))
  }

  /**
    * Gets completions for enum tags
    */
  private def getEnumTagCompletions(ns: List[String], enmName: String)(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    root.enums.filter { case (sym, _) => sym.name == enmName && sym.namespace == ns }
      .flatMap { case (sym, emn) => emn.cases.map {
        case (casSym, _) => useCompletion(s"${nsToStringDot(ns)}${sym.name}.${casSym.name}", CompletionItemKind.EnumMember)
      }
      }
  }

  /**
    * Gets completions for class sigs
    */
  private def getClassSigCompletions(ns: List[String], className: String)(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    root.classes.filter { case (sym, _) => sym.name == className && sym.namespace == ns }
      .flatMap { case (sym, clazz) => clazz.signatures.map {
        case sig => useCompletion(s"${nsToStringSlash(ns)}${sym.name}.${sig.sym.name}", CompletionItemKind.EnumMember)
      }
      }
  }

  /**
    * Converts a namespace into a /-seperated string with a dot at the end unless it is the root namespace
    */
  private def nsToStringDot(ns: List[String]): String = {
    ns match {
      case Nil => ""
      case _ => s"${ns.mkString("/")}."
    }
  }

  /**
    * Converts a namespace into a /-seperated string with a / at the end unless it is the root namespace
    */
  private def nsToStringSlash(ns: List[String]): String = {
    ns match {
      case Nil => ""
      case _ => s"${ns.mkString("/")}/"
    }
  }

  /**
    * Creates a completion for a use completion.
    */
  private def useCompletion(name: String, kind: CompletionItemKind)(implicit context: Context): CompletionItem = {
    CompletionItem(
      label = name,
      sortText = Priority.high(name),
      textEdit = TextEdit(context.range, name),
      documentation = None,
      kind = kind)
  }

  /**
    * Get completions for java imports.
    */
  private def getImportCompletions()(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    if (root == null) Nil else getImportNewCompletions() ++ getImportMethodCompletions() ++ getJavaClassCompletions() ++ getImportFieldCompletions()
  }

  /**
    * Get completions for importing java constructors.
    */
  private def getImportNewCompletions()(implicit context: Context): Iterable[CompletionItem] = {
    val regex = raw"\s*import\s+new\s+(.*)".r
    context.prefix match {
      case regex(clazz) => classFromString(clazz) match {
        case Some((clazzObject, clazz)) => {
          // Gets the name of the type excluding the package to use as a suggestion for the name of the constructor.
          val className = clazz.split('.').last
          clazzObject.getConstructors().map(constructor => executableCompletion(constructor, clazz, Some(s"new$className")))
        }
        case None => Nil
      }
      case _ => Nil
    }
  }

  /**
    * Get completions for method imports (both static and instance methods)
    */
  private def getImportMethodCompletions()(implicit context: Context): Iterable[CompletionItem] = {
    val instance = raw"\s*import\s+(.*)".r
    val static = raw"\s*import\s+static\s+(.*)".r
    context.prefix match {
      // We match on static first because import static ... would also match on instance regex.
      case static(clazz) => methodsCompletion(clazz, true)
      case instance(clazz) => methodsCompletion(clazz, false)
      case _ => Nil
    }
  }

  /**
    * Convert methods of a class into completionitems
    */
  private def methodsCompletion(clazz: String, isStatic: Boolean)(implicit context: Context): Iterable[CompletionItem] = {
    classFromDotSeperatedString(clazz) match {
      case Some((clazzObject, clazz)) => clazzObject.getMethods()
        // Filter if the method is static or not.
        .filter((method) => java.lang.reflect.Modifier.isStatic(method.getModifiers()) == isStatic)
        .map((method) => executableCompletion(method, clazz, None))
      case None => Nil
    }
  }

  /**
    * returns a completion from a java executable (method/constructor) instace.
    * clazz is the clazz in string form used for the completion.
    * aliasSuggestion is used to suggest a alias for the function if applicable.
    */
  private def executableCompletion(exec: Executable, clazz: String, aliasSuggestion: Option[String])(implicit context: Context): CompletionItem = {
    val typesString = exec.getParameters().map(param => convertJavaClassToFlixType(param.getType())).mkString("(", ", ", ")")
    val finalAliasSuggestion = aliasSuggestion match {
      case Some(aliasSuggestion) => s" as $${0:$aliasSuggestion}"
      case None => ""
    }
    // Get the name of the function if it is not a constructor.
    val name = if (exec.isInstanceOf[Constructor[_ <: Object]]) "" else s".${exec.getName}"
    // So for constructors we do not have a return type method but we know it is the declaring class.
    val returnType = if (exec.isInstanceOf[Method]) exec.asInstanceOf[Method].getReturnType() else exec.getDeclaringClass()
    val label = s"$clazz$name$typesString"
    val replace = s"$clazz$name$typesString: ${convertJavaClassToFlixType(returnType)} \\ IO$finalAliasSuggestion;"
    CompletionItem(
      label = label,
      // Prioritise executables with fewer parameters.
      sortText = Priority.high(s"${exec.getParameterCount()}$label"),
      textEdit = TextEdit(context.range, replace),
      documentation = None,
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Method)
  }

  /**
    * Gets completions for java packages/classes
    */
  private def getJavaClassCompletions()(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    val regex = raw"\s*import\s+(?:.*\s+)*(.*)".r
    context.prefix match {
      case regex(clazz) => {
        val path = clazz.split('.').toList;
        // Get completions for if we are currently typing the next package/class and if we have just finished typing a package
        javaClassCompletionsFromPrefix(path) ++ javaClassCompletionsFromPrefix(path.dropRight(1))
      }
      case _ => Nil
    }
  }

  /**
    * Gets completions from a java path prefix
    */
  private def javaClassCompletionsFromPrefix(prefix: List[String])(implicit context: Context, root: TypedAst.Root): Iterable[CompletionItem] = {
    root.names(prefix).map(clazz => {
      val label = prefix match {
        case Nil => clazz
        case v => v.mkString("", ".", s".$clazz")
      }
      CompletionItem(
        label = label,
        sortText = Priority.high(label),
        textEdit = TextEdit(context.range, label),
        documentation = None,
        insertTextFormat = InsertTextFormat.PlainText,
        kind = CompletionItemKind.Class)
    })
  }

  /**
    * Gets completions for importing fields
    */
  private def getImportFieldCompletions()(implicit context: Context): Iterable[CompletionItem] = {
    val static_get = raw"\s*import\s+static\s+get\s+(.*)".r
    val static_set = raw"\s*import\s+static\s+set\s+(.*)".r
    val get = raw"\s*import\s+get\s+(.*)".r
    val set = raw"\s*import\s+set\s+(.*)".r
    context.prefix match {
      case static_get(clazz) => importFieldCompletions(clazz, true, true)
      case static_set(clazz) => importFieldCompletions(clazz, true, false)
      case get(clazz) => importFieldCompletions(clazz, false, true)
      case set(clazz) => importFieldCompletions(clazz, false, false)
      case _ => Nil
    }
  }

  /**
    * Returns completions for a dot seperated class string
    */
  private def importFieldCompletions(clazz: String, isStatic: Boolean, isGet: Boolean)(implicit context: Context): Iterable[CompletionItem] = {
    classFromDotSeperatedString(clazz) match {
      case Some((clazzObject, clazz)) => clazzObject.getFields()
        // Filter if the method is static or not.
        .filter(field => java.lang.reflect.Modifier.isStatic(field.getModifiers()) == isStatic)
        .map(field => fieldCompletion(clazz, field, isGet))
      case None => Nil
    }
  }

  /**
    * Creates a field completion from a Field
    */
  private def fieldCompletion(clazz: String, field: Field, isGet: Boolean)(implicit context: Context): CompletionItem = {
    val ret = if (isGet) convertJavaClassToFlixType(field.getType()) else "Unit"
    val asSuggestion = if (isGet) s"get${field.getName()}" else s"set${field.getName()}"
    val label = s"${clazz}.${field.getName()}: $ret"
    CompletionItem(
      label = label,
      sortText = Priority.high(label),
      textEdit = TextEdit(context.range, s"$label \\ IO as $${0:$asSuggestion};"),
      documentation = None,
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Field
    )
  }

  /**
    * Returns a class object if the string is a class or removing the last "part" makes it a class
    */
  private def classFromDotSeperatedString(clazz: String): Option[(Class[_ <: Object], String)] = {
    // If the last charachter is . then this drops that
    // I.e if we have java.lang.String. this converts to java.lang.String
    // while if it does not end with . it is unchanged
    val clazz1 = clazz.split('.').mkString(".")
    // If we are typing the method/field to import we drop that
    val clazz2 = clazz.split('.').dropRight(1).mkString(".")
    classFromString(clazz1).orElse(classFromString(clazz2))
  }

  /**
    * Return a class object if the class exists
    */
  private def classFromString(clazz: String): Option[(Class[_ <: Object], String)] = {
    try {
      Some((java.lang.Class.forName(clazz), clazz))
    }
    catch {
      case _: ClassNotFoundException => None
    }
  }

  /**
    * Converts a Java Class Object into a string representing the type in flix syntax.
    * I.e. java.lang.String => String, byte => Int8, java.lang.Object[] => Array[##java.lang.Object, false].
    */
  private def convertJavaClassToFlixType(clazz: Class[_ <: Object]): String = {
    if (clazz.isArray()) {
      s"Array[${convertJavaClassToFlixType(clazz.getComponentType())}, Static]"
    }
    else {
      clazz.getName() match {
        case "byte" => "Int8"
        case "short" => "Int16"
        case "int" => "Int32"
        case "long" => "Int64"
        case "float" => "Float32"
        case "double" => "Float64"
        case "boolean" => "Bool"
        case "char" => "Char"
        case "java.lang.String" => "String"
        case "java.math.BigInteger" => "BigInt"
        case "java.math.BigDecimal" => "BigDecimal"
        case "java.util.function.IntFunction" => "Int32 => ##java.lang.Object"
        case "java.util.function.IntUnaryOperator" => "Int32 => Int32"
        case "void" => "Unit"
        case other => s"##$other"
      }
    }
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
