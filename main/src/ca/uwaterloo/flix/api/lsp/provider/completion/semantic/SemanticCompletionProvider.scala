/*
 * Copyright 2024 Chenhao Gao
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
package ca.uwaterloo.flix.api.lsp.provider.completion.semantic

import ca.uwaterloo.flix.api.lsp.Position
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.KindCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.{AutoImportCompleter, AutoUseCompleter, Completion, CompletionContext, EffSymCompleter, ImportCompleter, KindCompleter, LocalScopeCompleter, MagicMatchCompleter}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.errors.{ResolutionError, TypeError}

object SemanticCompletionProvider {
  /**
    * Returns a list of Semantic Completions
    */
  def getCompletions(ctx: CompletionContext, errors: List[CompilationMessage])(implicit root: TypedAst.Root): Iterable[Completion] = {
    errorsAt(ctx.uri, ctx.pos, errors).flatMap({
      case err: ResolutionError.UndefinedJvmClass => ImportCompleter.getCompletions(err)
      case err: ResolutionError.UndefinedName => AutoImportCompleter.getCompletions(err) ++ LocalScopeCompleter.getCompletions(err) ++ AutoUseCompleter.getCompletions(err)
      case err: ResolutionError.UndefinedType =>
        AutoImportCompleter.getCompletions(err) ++ LocalScopeCompleter.getCompletions(err) ++ AutoUseCompleter.getCompletions(err) ++ EffSymCompleter.getCompletions(err)
      case err: ResolutionError.UndefinedKind => KindCompleter.getCompletions(err)
      case err: TypeError.FieldNotFound => MagicMatchCompleter.getCompletions(err)

      case _ => Nil
    })
  }

  /**
    * Filters the list of errors to only those that occur at the given position.
    */
  private def errorsAt(uri: String, pos: Position, errors: List[CompilationMessage]): List[CompilationMessage] =
    errors.filter(err => uri == err.loc.source.name && pos.line <= err.loc.beginLine)
}
