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
import ca.uwaterloo.flix.api.lsp.*
import ca.uwaterloo.flix.api.lsp.provider.completion.*
import ca.uwaterloo.flix.api.lsp.provider.completion.semantic.{GetStaticFieldCompleter, InvokeStaticMethodCompleter}
import ca.uwaterloo.flix.api.lsp.provider.completion.syntactic.{ExprSnippetCompleter, KeywordCompleter}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.SyntacticContext
import ca.uwaterloo.flix.language.errors.{ParseError, ResolutionError, TypeError, WeederError}
import ca.uwaterloo.flix.language.phase.Lexer

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

  def autoComplete(uri: String, pos: Position, source: String, currentErrors: List[CompilationMessage])(implicit root: Root, flix: Flix): CompletionList = {
    getCompletionContext(source, pos) match {
      case None =>
        CompletionList(isIncomplete = true, Nil)

      case Some(ctx) =>
        val items = getCompletions(uri, pos, currentErrors)(root, flix).map(_.toCompletionItem(ctx))
        CompletionList(isIncomplete = true, items)
    }
  }

  /**
    * Returns all completions in the given `uri` at the given position `pos`.
    */
  private def getCompletions(uri: String, pos: Position, currentErrors: List[CompilationMessage])(implicit root: Root, flix: Flix): List[Completion] = {
    if (currentErrors.isEmpty)
      HoleCompleter.getHoleCompletion(uri, pos, root).toList
    else
      errorsAt(uri, pos, currentErrors).flatMap {
        case err: WeederError.UndefinedAnnotation => KeywordCompleter.getModKeywords(Range.from(err.loc))

        case err: WeederError.UnqualifiedUse => UseCompleter.getCompletions(uri, err)

        case err: ResolutionError.UndefinedTag =>
          EnumCompleter.getCompletions(err) ++
            EnumTagCompleter.getCompletions(err) ++
            ModuleCompleter.getCompletions(err)

        case err: ResolutionError.UndefinedName =>
          val ap = err.ap
          val env = err.env
          val ident = err.qn.ident.name
          val range = Range.from(err.loc)
          AutoImportCompleter.getCompletions(ident, range, ap, env) ++
            LocalScopeCompleter.getCompletions(err) ++
            KeywordCompleter.getExprKeywords(Range.from(err.loc)) ++
            DefCompleter.getCompletions(err) ++
            EnumCompleter.getCompletions(err) ++
            EffectCompleter.getCompletions(err) ++
            OpCompleter.getCompletions(err) ++
            SignatureCompleter.getCompletions(err) ++
            EnumTagCompleter.getCompletions(err) ++
            TraitCompleter.getCompletions(err) ++
            ModuleCompleter.getCompletions(err)

        case err: ResolutionError.UndefinedType =>
          val ap = err.ap
          val env = err.env
          val ident = err.qn.ident.name
          val range = Range.from(err.loc)
          TypeBuiltinCompleter.getCompletions ++
            AutoImportCompleter.getCompletions(ident, range, ap, env) ++
            LocalScopeCompleter.getCompletions(err) ++
            EnumCompleter.getCompletions(err) ++
            StructCompleter.getCompletions(err) ++
            EffectCompleter.getCompletions(err) ++
            TypeAliasCompleter.getCompletions(err) ++
            ModuleCompleter.getCompletions(err)

        case err: ResolutionError.UndefinedEffect => EffectCompleter.getCompletions(err)
        case err: ResolutionError.UndefinedJvmImport => ImportCompleter.getCompletions(err)
        case err: ResolutionError.UndefinedJvmStaticField => GetStaticFieldCompleter.getCompletions(err) ++ InvokeStaticMethodCompleter.getCompletions(err)
        case err: ResolutionError.UndefinedKind => KindCompleter.getCompletions(err)
        case err: ResolutionError.UndefinedOp => OpCompleter.getCompletions(err)
        case err: ResolutionError.UndefinedStructField => StructFieldCompleter.getCompletions(err, root)
        case err: ResolutionError.UndefinedTrait => TraitCompleter.getCompletions(err)
        case err: ResolutionError.UndefinedUse => UseCompleter.getCompletions(uri, err)

        case err: TypeError.FieldNotFound => MagicMatchCompleter.getCompletions(err) ++ InvokeMethodCompleter.getCompletions(err.tpe, err.fieldName)
        case err: TypeError.MethodNotFound => InvokeMethodCompleter.getCompletions(err.tpe, err.methodName)

        case err: ParseError => getSyntacticCompletions(uri, err)

        case _ => Nil
      }
  }

  /**
    * Returns completions based on the syntactic context.
    */
  private def getSyntacticCompletions(uri: String, e: ParseError)(implicit root: Root, flix: Flix): List[Completion] = {
    val range: Range = Range.from(e.loc)
    e.sctx match {
      // Expressions.
      case SyntacticContext.Expr.Constraint => (PredicateCompleter.getCompletions(uri, range) ++ KeywordCompleter.getConstraintKeywords(range)).toList
      case SyntacticContext.Expr.OtherExpr => KeywordCompleter.getExprKeywords(range)

      // Declarations.
      case SyntacticContext.Decl.Enum => KeywordCompleter.getEnumKeywords(range)
      case SyntacticContext.Decl.Effect => KeywordCompleter.getEffectKeywords(range)
      case SyntacticContext.Decl.Instance => KeywordCompleter.getInstanceKeywords(range)
      case SyntacticContext.Decl.Module => KeywordCompleter.getModKeywords(range) ++ ExprSnippetCompleter.getCompletions(range)
      case SyntacticContext.Decl.Struct => KeywordCompleter.getStructKeywords(range)
      case SyntacticContext.Decl.Trait => KeywordCompleter.getTraitKeywords(range)
      case SyntacticContext.Decl.Type => KeywordCompleter.getTypeKeywords(range)

      case SyntacticContext.Unknown => Nil
    }
  }

  /**
    * Find context from the source, and cursor position within it.
    */
  private def getCompletionContext(source: String, pos: Position): Option[CompletionContext] = {
    // Use zero-indexed lines and characters.
    source.linesWithSeparators.toList.lift(pos.line - 1).map { line =>
      val (prefix, suffix) = line.splitAt(pos.character - 1)
      // Find the word at the cursor position.
      val wordStart = prefix.reverse.takeWhile(isWordChar)
      val wordEnd = suffix.takeWhile(isWordChar)
      val start = pos.character - wordStart.length
      val end = pos.character + wordEnd.length
      val range = Range(pos.copy(character = start), pos.copy(character = end))
      CompletionContext(range)
    }
  }

  /**
    * Characters that constitute a word.
    */
  private def isWordChar(c: Char) = isLetter(c) || Lexer.isMathNameChar(c) || Lexer.isGreekNameChar(c) || Lexer.isUserOp(c).isDefined

  /**
    * Characters that may appear in a word.
    */
  private def isLetter(c: Char) = c match {
    case c if c >= 'a' && c <= 'z' => true
    case c if c >= 'A' && c <= 'Z' => true
    case c if c >= '0' && c <= '9' => true
    // We also include some special symbols. This is more permissive than the lexer, but that's OK.
    case '_' => true
    case '!' => true
    case '@' => true
    case '/' => true
    case '.' => true
    case '#' => true
    case _ => false
  }

  /**
    * Filters the list of errors to only those that occur at the given position.
    */
  private def errorsAt(uri: String, pos: Position, errors: List[CompilationMessage]): List[CompilationMessage] =
    errors.filter(err => uri == err.loc.source.name && pos.line <= err.loc.beginLine)

}
