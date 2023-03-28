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
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatScheme
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
    val completions = source.flatMap(getContext(_, uri, pos, currentErrors)) match {
      case None => Nil
      case Some(context) =>
        root match {
          case Some(nonOptionRoot) =>
            (getCompletions()(context, flix, index, nonOptionRoot, deltaContext) ++
            FromErrorsCompleter.getCompletions(context)(flix, index, nonOptionRoot, deltaContext)
              map (comp => comp.toCompletionItem(context)))
          case None => Nil
        }
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
          .map(root.get.defs(_))
          .filter(_.spec.mod.isPublic)
          .zipWithIndex
          .map { case (decl, idx) => holeDefCompletion(f"$idx%09d", loc, sym, decl) }
      case _ => Nil
    }
  }

  /**
    * Creates a completion item from a hole with expression and a def.
    */
  private def holeDefCompletion(priority: String, loc: SourceLocation, sym: Symbol.VarSym, decl: TypedAst.Def)(implicit flix: Flix): CompletionItem = {
    val name = decl.sym.toString
    val args = decl.spec.fparams.dropRight(1).zipWithIndex.map {
      case (fparam, idx) => "$" + s"{${idx + 1}:?${fparam.sym.text}}"
    } ::: sym.text :: Nil
    val params = args.mkString(", ")
    val snippet = s"$name($params)"
    CompletionItem(label = CompletionUtils.getLabelForNameAndSpec(decl.sym.toString, decl.spec),
      filterText = Some(s"${sym.text}?$name"),
      sortText = priority,
      textEdit = TextEdit(Range.from(loc), snippet),
      detail = Some(FormatScheme.formatScheme(decl.spec.declaredScheme)),
      documentation = Some(decl.spec.doc.text),
      insertTextFormat = InsertTextFormat.Snippet,
      kind = CompletionItemKind.Function)
  }

  private def getCompletions()(implicit context: CompletionContext, flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[Completion] = {
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
      case withRegex() => WithCompleter.getCompletions(context)
      case typeRegex() | typeAliasRegex() => TypeCompleter.getCompletions(context)
      case effectRegex() => EffectCompleter.getCompletions(context)
      case defRegex() | enumRegex() | incompleteTypeAliasRegex() | classRegex() | letRegex() | letStarRegex() | modRegex() | underscoreRegex() | tripleQuestionMarkRegex() => Nil
      case importRegex() =>
        ImportNewCompleter.getCompletions(context) ++
          ImportMethodCompleter.getCompletions(context) ++
          ImportFieldCompleter.getCompletions(context) ++
          ClassCompleter.getCompletions(context)
      case useRegex() => UseCompleter.getCompletions(context)
      case instanceRegex() => InstanceCompleter.getCompletions(context)
      //
      // The order of this list doesn't matter because suggestions are ordered
      // through sortText
      //
      case _ => getExpCompletions() ++
        PredicateCompleter.getCompletions(context) ++
          TypeCompleter.getCompletions(context) ++
          EffectCompleter.getCompletions(context)
    }
  }

  /**
    * Returns a list of completions that may be used in a position where an expression is needed.
    * This should include all completions supported that could be an expression.
    * All of the completions are not necessarily sound.
    */
  private def getExpCompletions()(implicit context: CompletionContext, flix: Flix, index: Index, root: TypedAst.Root, deltaContext: DeltaContext): Iterable[Completion] = {
    KeywordCompleter.getCompletions(context) ++
      SnippetCompleter.getCompletions(context) ++
      VarCompleter.getCompletions(context) ++
      DefCompleter.getCompletions(context) ++
      SignatureCompleter.getCompletions(context) ++
      FieldCompleter.getCompletions(context) ++
      OpCompleter.getCompletions(context) ++
      MatchCompleter.getCompletions(context)
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
  private def getLastWord(s: String): String = {
    s.reverse.dropWhile(_.isWhitespace).takeWhile(isWordChar).reverse
  }

  /**
    * Returns the second-to-last word at the end of a string, *not* discarding
    * trailing whitespace first.
    */
  private def getSecondLastWord(s: String): String = {
    s.reverse.dropWhile(isWordChar).dropWhile(_.isWhitespace).takeWhile(isWordChar).reverse
  }

  /**
    * Find context from the source, and cursor position within it.
    */
  private def getContext(source: String, uri: String, pos: Position, errors: List[CompilationMessage]): Option[CompletionContext] = {
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
      CompletionContext(uri, pos, range, word, previousWord, prefix, errors)
    }
  }
}
