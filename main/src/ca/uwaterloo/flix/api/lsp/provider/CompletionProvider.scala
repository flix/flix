/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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
import ca.uwaterloo.flix.api.lsp.provider.completion._
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.language.fmt.{FormatScheme, FormatType}
import ca.uwaterloo.flix.language.phase.Parser.Letters
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
  def autoComplete(uri: String, pos: Position, source: Option[String], currentErrors: List[CompilationMessage])(implicit flix: Flix, index: Index, root: Option[TypedAst.Root], deltaContext: DeltaContext): JObject = {
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
      case Some(context) => getCompletions()(context, flix, index, root, deltaContext) ++ getCompletionsFromErrors(pos, currentErrors)(context, index, root)
    }

    ("status" -> "success") ~ ("result" -> CompletionList(isIncomplete = true, completions).toJSON)
  }

  /**
    * Gets completions for when the cursor position is on a hole expression with an expression
    */
  private def getHoleExpCompletions(pos: Position, uri: String, index: Index, root: Option[TypedAst.Root])(implicit flix: Flix): Iterable[CompletionItem] = {
    if (root.isEmpty) return Nil
    val entity = index.query(uri, pos)
    entity match {
      case Some(Entity.Exp(TypedAst.Expression.HoleWithExp(TypedAst.Expression.Var(sym, sourceType, _), targetType, _, _, loc))) =>
        HoleCompletion.candidates(sourceType, targetType, root.get)
          .map((root.get.defs(_)))
          .filter(_.spec.mod.isPublic)
          .zipWithIndex
          .map { case (decl, idx) => holeDefCompletion(f"$idx%09d", uri, loc, sym, decl, root) }
      case _ => Nil
    }
  }

  /**
    * Creates a completion item from a hole with expression and a def.
    */
  private def holeDefCompletion(priority: String, uri: String, loc: SourceLocation, sym: Symbol.VarSym, decl: TypedAst.Def, root: Option[TypedAst.Root])(implicit flix: Flix): CompletionItem = {
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

  private def getCompletions()(implicit context: CompletionContext, flix: Flix, index: Index, root: Option[TypedAst.Root], delta: DeltaContext): Iterable[CompletionItem] = {
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
    val modRegex = raw"\s*mod\s+.*".r
    val tripleQuestionMarkRegex = raw"\?|.*\s+\?.*".r
    val underscoreRegex = raw"(?:(?:.*\s+)|)_[^s]*".r

    // if any of the following matches we know the next must be an expression
    val channelKeywordRegex = raw".*<-\s*[^\s]*".r
    val doubleColonRegex = raw".*::\s*[^\s]*".r
    val tripleColonRegex = raw".*:::\s*[^\s]*".r

    // We check type and effect first because for example following def we do not want completions other than type and effect if applicable.
    context.prefix match {
      case channelKeywordRegex() | doubleColonRegex() | tripleColonRegex() => getExpCompletions()
      case withRegex() => WithCompleter.getCompletions map (withComp => withComp.toCompletionItem)
      case typeRegex() | typeAliasRegex() => TypeCompleter.getCompletions map (typ => typ.toCompletionItem)
      case effectRegex() => EffectCompleter.getCompletions map (effect => effect.toCompletionItem)
      case defRegex() | enumRegex() | incompleteTypeAliasRegex() | classRegex() | letRegex() | letStarRegex() | modRegex() | underscoreRegex() | tripleQuestionMarkRegex() => Nil
      case importRegex() =>
        if (root.isEmpty)
          Nil
        else
          (ImportNewCompleter.getCompletions ++ ImportMethodCompleter.getCompletions ++ ImportFieldCompleter.getCompletions
            ++ ClassCompleter.getCompletions map (comp => comp.toCompletionItem))
      case useRegex() => getUseCompletions()
      case instanceRegex() => getInstanceCompletions()
      //
      // The order of this list doesn't matter because suggestions are ordered
      // through sortText
      //
      case _ => getExpCompletions() ++
        (PredicateCompleter.getCompletions ++
          TypeCompleter.getCompletions ++
          EffectCompleter.getCompletions map (comp => comp.toCompletionItem))
    }
  }

  /**
    * Returns a list of completions that may be used in a position where an expression is needed.
    * This should include all completions supported that could be an expression.
    * All of the completions are not necessarily sound.
    */
  private def getExpCompletions()(implicit context: CompletionContext, flix: Flix, index: Index, root: Option[TypedAst.Root], deltaContext: DeltaContext): Iterable[CompletionItem] = {
    (KeywordCompleter.getCompletions ++
      SnippetCompleter.getCompletions ++
      VarCompleter.getCompletions map (comp => comp.toCompletionItem)) ++
      getDefAndSigCompletions() ++
      (FieldCompleter.getCompletions map (field => field.toCompletionItem)) ++
      getOpCompletions() ++
      getMatchCompletitions()
  }

  /**
    * Returns a list of completions based on the given compilation messages.
    */
  private def getCompletionsFromErrors(pos: Position, errors: List[CompilationMessage])(implicit context: CompletionContext, index: Index, root: Option[TypedAst.Root]): Iterator[CompletionItem] = {
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
  private def getApplySnippet(name: String, fparams: List[TypedAst.FormalParam])(implicit context: CompletionContext): Option[String] = {
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

  private def defCompletion(decl: TypedAst.Def)(implicit context: CompletionContext, flix: Flix, index: Index, root: Option[TypedAst.Root]): Option[CompletionItem] = {
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

  private def sigCompletion(decl: TypedAst.Sig)(implicit context: CompletionContext, flix: Flix, index: Index, root: Option[TypedAst.Root]): Option[CompletionItem] = {
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

  private def opCompletion(decl: TypedAst.Op)(implicit context: CompletionContext, flix: Flix, index: Index, root: Option[TypedAst.Root]): Option[CompletionItem] = {
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
    def isInternal(decl: TypedAst.Def): Boolean = decl.spec.ann.isInternal

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

  private def getDefAndSigCompletions()(implicit context: CompletionContext, flix: Flix, index: Index, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    if (root.isEmpty) {
      return Nil
    }

    val word = context.word
    val uri = context.uri

    val defSuggestions = root.get.defs.values.filter(matchesDef(_, word, uri)).flatMap(defCompletion)
    val sigSuggestions = root.get.sigs.values.filter(matchesSig(_, word, uri)).flatMap(sigCompletion)
    defSuggestions ++ sigSuggestions
  }

  private def getOpCompletions()(implicit context: CompletionContext, flix: Flix, index: Index, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    if (root.isEmpty || context.previousWord != "do") {
      return Nil
    }

    val word = context.word
    val uri = context.uri

    root.get.effects.values.flatMap(_.ops).filter(matchesOp(_, word, uri)).flatMap(opCompletion)
  }

  /**
    * Returns a list of completion items for match type completions
    */
  private def getMatchCompletitions()(implicit context: CompletionContext, index: Index, root: Option[TypedAst.Root], flix: Flix): Iterable[CompletionItem] = {
    if (root.isEmpty) {
      return Nil
    }

    val matchPattern = raw".*\s*ma?t?c?h?\s?.*".r

    if (!(matchPattern matches context.prefix)) {
      return Nil
    }

    val wordPattern = "ma?t?c?h?".r
    val currentWordIsMatch = wordPattern matches context.word

    root.get.enums.foldLeft[List[CompletionItem]](Nil)((acc, enm) => {
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
  private def matchCompletion(enm: TypedAst.Enum, currentWordIsMatch: Boolean)(implicit context: CompletionContext, flix: Flix): Option[CompletionItem] = {
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
  private def matchCompletion(sym: String, completion: String, priority: String => String)(implicit context: CompletionContext): CompletionItem = {
    val label = s"match $sym"
    CompletionItem(label = label,
      sortText = priority(label),
      textEdit = TextEdit(context.range, completion),
      documentation = None,
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Snippet)
  }

  /**
    * Returns a list of completion items based on type classes.
    */
  private def getInstanceCompletions()(implicit context: CompletionContext, index: Index, root: Option[TypedAst.Root], flix: Flix): Iterable[CompletionItem] = {
    if (root.isEmpty || context.previousWord != "instance") {
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

    root.get.classes.map {
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
    * Gets completions after use keyword
    */
  private def getUseCompletions()(implicit context: CompletionContext, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    if (root.isEmpty) {
      return Nil
    }

    val regex = raw"\s*use\s+(.*)".r

    context.prefix match {
      case regex(ns) => {
        // Six cases
        // 0: We have nothing i.e.
        // 1: a path i.e. A.B
        // 2: an item i.e. Foo (enum/class/def/type alias)
        // 3: path and item i.e. A.B.Foo
        // 4: item and tag/sig i.e Foo.Bar
        // 5: path, item and tag/sig i.e A.B.Foo.Bar

        val segments = ns.split('.');
        segments.toList match {
          // case 0
          case Nil => nsCompletionsAfterPrefix(Nil) ++ getItemUseCompletions(Nil)
          // case 1/2
          case x :: Nil => {
            // We might be done typing the namespace or not. We need to try both cases
            val prefix1 = x.split('.').toList;
            val prefix2 = x.split('.').dropRight(1).toList;
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
            val ns = x.split('.').toList;
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
            val ns = x.split('.').toList;
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
    * I.e if you have namespace A.B.C.D, then if prefix is A.B it will return a completion for A.B.C
    */
  private def nsCompletionsAfterPrefix(prefix: List[String])(implicit context: CompletionContext, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    if (root.isEmpty) return Nil
    val nss = root.get.defs.keySet.map(_.namespace) ++
      root.get.enums.keySet.map(_.namespace) ++
      root.get.classes.keySet.map(_.namespace) ++
      root.get.typeAliases.keySet.map(_.namespace);

    nss.flatMap(ns => getFirstAfterGivenPrefix(ns, prefix))
      .map(nextNs => {
        val name = prefix.appended(nextNs).mkString(".")
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
  private def getItemUseCompletions(ns: List[String])(implicit context: CompletionContext, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    getEnumUseCompletions(ns) ++
      getClassUseCompletions(ns) ++
      getDefUseCompletions(ns) ++
      getTypeUseCompletions(ns)
  }

  /**
    * Gets completions for enums in a given namespace
    */
  private def getEnumUseCompletions(ns: List[String])(implicit context: CompletionContext, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    if (root.isEmpty) return Nil
    root.get.enums.filter { case (sym, emn) => emn.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => useCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Enum))
  }

  /**
    * Gets completions for classes in a given namespace
    */
  private def getClassUseCompletions(ns: List[String])(implicit context: CompletionContext, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    if (root.isEmpty) return Nil
    root.get.classes.filter { case (sym, clazz) => clazz.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => useCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Interface))
  }

  /**
    * Gets completions for functions in a given namespace
    */
  private def getDefUseCompletions(ns: List[String])(implicit context: CompletionContext, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    if (root.isEmpty) return Nil
    root.get.defs.filter { case (sym, df) => df.spec.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => useCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Function))
  }

  /**
    * Gets completion for type aliases in a given namespace
    */
  private def getTypeUseCompletions(ns: List[String])(implicit context: CompletionContext, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    if (root.isEmpty) return Nil
    root.get.typeAliases.filter { case (sym, tpe) => tpe.mod.isPublic }
      .keySet.filter(_.namespace == ns)
      .map(sym => useCompletion(s"${nsToStringDot(ns)}${sym.name}", CompletionItemKind.Struct))
  }

  /**
    * Gets completions for enum tags
    */
  private def getEnumTagCompletions(ns: List[String], enmName: String)(implicit context: CompletionContext, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    if (root.isEmpty) return Nil
    root.get.enums.filter { case (sym, _) => sym.name == enmName && sym.namespace == ns }
      .flatMap { case (sym, emn) => emn.cases.map {
        case (casSym, _) => useCompletion(s"${nsToStringDot(ns)}${sym.name}.${casSym.name}", CompletionItemKind.EnumMember)
      }
      }
  }

  /**
    * Gets completions for class sigs
    */
  private def getClassSigCompletions(ns: List[String], className: String)(implicit context: CompletionContext, root: Option[TypedAst.Root]): Iterable[CompletionItem] = {
    if (root.isEmpty) return Nil
    root.get.classes.filter { case (sym, _) => sym.name == className && sym.namespace == ns }
      .flatMap { case (sym, clazz) => clazz.signatures.map {
        case sig => useCompletion(s"${nsToStringDot(ns)}${sym.name}.${sig.sym.name}", CompletionItemKind.EnumMember)
      }
      }
  }

  /**
    * Converts a namespace into a .-seperated string with a dot at the end unless it is the root namespace
    */
  private def nsToStringDot(ns: List[String]): String = {
    ns match {
      case Nil => ""
      case _ => s"${ns.mkString(".")}."
    }
  }

  /**
    * Converts a namespace into a .-seperated string with a / at the end unless it is the root namespace
    */
  private def nsToStringSlash(ns: List[String]): String = {
    ns match {
      case Nil => ""
      case _ => s"${ns.mkString(".")}/"
    }
  }

  /**
    * Creates a completion for a use completion.
    */
  private def useCompletion(name: String, kind: CompletionItemKind)(implicit context: CompletionContext): CompletionItem = {
    CompletionItem(
      label = name,
      sortText = Priority.high(name),
      textEdit = TextEdit(context.range, name),
      documentation = None,
      kind = kind)
  }

  /**
    * Returns a class object if the string is a class or removing the last "part" makes it a class
    */
  def classFromDotSeperatedString(clazz: String): Option[(Class[_], String)] = {
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
  def classFromString(clazz: String): Option[(Class[_], String)] = {
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
  def convertJavaClassToFlixType(clazz: Class[_]): String = {
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
  private def getContext(source: String, uri: String, pos: Position): Option[CompletionContext] = {
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
      CompletionContext(uri, range, word, previousWord, prefix)
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
