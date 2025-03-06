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
import ca.uwaterloo.flix.language.ast.TypedAst
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

  def autoComplete(uri: String, pos: Position, source: String, currentErrors: List[CompilationMessage])(implicit flix: Flix, root: TypedAst.Root): CompletionList = {
    val completionItems  =
      getCompletionContext(source, pos).map {ctx =>
        val completions = if (currentErrors.isEmpty)
          HoleCompleter.getHoleCompletion(uri, pos, root)
        else
          errorsAt(uri, pos, currentErrors).flatMap({
            case err: WeederError.UnqualifiedUse => UseCompleter.getCompletions(uri, err)
            case WeederError.UndefinedAnnotation(_, _) => KeywordCompleter.getModKeywords
            case err: ResolutionError.UndefinedEffect => EffectCompleter.getCompletions(err)
            case err: ResolutionError.UndefinedUse => UseCompleter.getCompletions(uri, err)
            case err: ResolutionError.UndefinedTag =>
              EnumCompleter.getCompletions(err) ++
                EnumTagCompleter.getCompletions(err) ++
                ModuleCompleter.getCompletions(err)
            case err: ResolutionError.UndefinedName =>
              AutoImportCompleter.getCompletions(err) ++
                LocalScopeCompleter.getCompletions(err) ++
                KeywordCompleter.getExprKeywords ++
                DefCompleter.getCompletions(err) ++
                EnumCompleter.getCompletions(err) ++
                EffectCompleter.getCompletions(err) ++
                OpCompleter.getCompletions(err) ++
                SignatureCompleter.getCompletions(err) ++
                EnumTagCompleter.getCompletions(err) ++
                TraitCompleter.getCompletions(err) ++
                ModuleCompleter.getCompletions(err)
            case err: ResolutionError.UndefinedType =>
              TypeBuiltinCompleter.getCompletions ++
                AutoImportCompleter.getCompletions(err) ++
                LocalScopeCompleter.getCompletions(err) ++
                EnumCompleter.getCompletions(err) ++
                StructCompleter.getCompletions(err) ++
                EffectCompleter.getCompletions(err) ++
                TypeAliasCompleter.getCompletions(err) ++
                ModuleCompleter.getCompletions(err)
            case err: ResolutionError.UndefinedJvmStaticField => GetStaticFieldCompleter.getCompletions(err) ++ InvokeStaticMethodCompleter.getCompletions(err)
            case err: ResolutionError.UndefinedJvmImport => ImportCompleter.getCompletions(err)
            case err: ResolutionError.UndefinedTrait => TraitCompleter.getCompletions(err)
            case err: ResolutionError.UndefinedStructField => StructFieldCompleter.getCompletions(err, root)
            case err: ResolutionError.UndefinedKind => KindCompleter.getCompletions(err)
            case err: ResolutionError.UndefinedOp => OpCompleter.getCompletions(err)
            case err: TypeError.FieldNotFound => MagicMatchCompleter.getCompletions(err) ++ InvokeMethodCompleter.getCompletions(err.tpe, err.fieldName)
            case err: TypeError.MethodNotFound => InvokeMethodCompleter.getCompletions(err.tpe, err.methodName)
            case err: ParseError => err.sctx match {
              // Expressions.
              case SyntacticContext.Expr.Constraint => PredicateCompleter.getCompletions(uri) ++ KeywordCompleter.getConstraintKeywords
              case SyntacticContext.Expr.OtherExpr => KeywordCompleter.getExprKeywords

                // Declarations.
                case SyntacticContext.Decl.Enum => KeywordCompleter.getEnumKeywords
                case SyntacticContext.Decl.Effect => KeywordCompleter.getEffectKeywords
                case SyntacticContext.Decl.Module => KeywordCompleter.getModKeywords ++ ExprSnippetCompleter.getCompletions()
                case SyntacticContext.Decl.Struct => KeywordCompleter.getStructKeywords
                case SyntacticContext.Decl.Trait => KeywordCompleter.getTraitKeywords
                case SyntacticContext.Decl.Type => KeywordCompleter.getTypeKeywords

              case _ => Nil
            }
            case _ => Nil
          })
        completions.map(comp => comp.toCompletionItem(ctx))
      }.getOrElse(Nil)
    CompletionList(isIncomplete = true, completionItems)
  }

  /**
    * Find context from the source, and cursor position within it.
    */
  private def getCompletionContext(source: String, pos: Position): Option[CompletionContext] = {
    // Use zero-indexed lines and characters.
    source.linesWithSeparators.toList.lift(pos.line - 1).map { line =>
      val (prefix, suffix) = line.splitAt(pos.character -1)
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
