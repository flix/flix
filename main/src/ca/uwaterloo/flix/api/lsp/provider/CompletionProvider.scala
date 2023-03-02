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
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
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
      case useRegex() => UseCompleter.getCompletions map (comp => comp.toCompletionItem)
      case instanceRegex() => InstanceCompleter.getCompletions map (comp => comp.toCompletionItem)
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
    KeywordCompleter.getCompletions ++
      SnippetCompleter.getCompletions ++
      VarCompleter.getCompletions ++
      DefCompleter.getCompletions ++
      SignatureCompleter.getCompletions ++
      FieldCompleter.getCompletions ++
      OpCompleter.getCompletions ++
      MatchCompleter.getCompletions map (comp => comp.toCompletionItem)
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

  def getLabelForNameAndSpec(name: String, spec: TypedAst.Spec)(implicit flix: Flix): String = spec match {
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
    */
  def getApplySnippet(name: String, fparams: List[TypedAst.FormalParam])(implicit context: CompletionContext): String = {
    val functionIsUnit = isUnitFunction(fparams)

    val args = fparams.dropRight(paramsToDrop).zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.sym.text}}"
    }
    if (functionIsUnit)
      s"$name()"
    else if (args.nonEmpty)
      s"$name(${args.mkString(", ")})"
    else
      name
  }

  /**
    * Helper function for deciding if a snippet can be generated.
    * Returns false if there are too few arguments.
    */
  def canApplySnippet(fparams: List[TypedAst.FormalParam])(implicit context: CompletionContext): Boolean = {
    val functionIsUnit = isUnitFunction(fparams)

    if (paramsToDrop > fparams.length || (functionIsUnit && paramsToDrop > 0)) false else true
  }

  /**
    * Calculates how many params to drops in the event that the function is in a pipeline
    * (i.e. is preceeded by `|>`, `!>`, or `||>`)
    */
  private def paramsToDrop(implicit context: CompletionContext): Int = {
    context.previousWord match {
      case "||>" => 2
      case "|>" | "!>" => 1
      case _ => 0
    }
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
  def getFilterTextForName(name: String): String = {
    s"$name("
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
