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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.PredicateCompletion
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.language.fmt.FormatType

object PredicateCompleter {

  def getCompletions(ctx: CompletionContext)(implicit index: Index, flix: Flix): Iterable[PredicateCompletion] = {
    //
    // Find all predicates together with their type and source location.
    //
    val predsWithTypeAndLoc = index.predTypes

    //
    // Select all predicate symbols that occur in the same file.
    //
    for (
      (pred, arityAndLocs) <- predsWithTypeAndLoc.m;
      (tpe, loc) <- arityAndLocs;
      if loc.source.name == ctx.uri
    ) yield Completion.PredicateCompletion(pred.name, arityOf(tpe), FormatType.formatType(tpe))
  }

  /**
   * Returns the arity of the given predicate type `tpe`.
   *
   * The arity might not always be known. If so, we return 1.
   */
  private def arityOf(tpe: Type): Int = {
    tpe.typeArguments match {
      case targ :: Nil => targ.typeConstructor match {
        case Some(TypeConstructor.Tuple(l)) => l
        case _ => 1
      }
      case _ => 1
    }
  }

}
