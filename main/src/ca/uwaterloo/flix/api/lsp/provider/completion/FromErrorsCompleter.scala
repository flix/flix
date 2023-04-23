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
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.FromErrorsCompletion
import ca.uwaterloo.flix.api.lsp.{Index, Position}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.language.errors.ResolutionError

object FromErrorsCompleter extends Completer {
  /**
    * Returns a List of Completion based on the given compilation messages.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[FromErrorsCompletion] = {
    val errors = context.errors
    val pos = context.pos
    val undefinedNames = errors.collect {
      case m: ResolutionError.UndefinedName => m
    }
    closest(pos, undefinedNames) match {
      case None => Nil
      case Some(undefinedNameError) =>
        undefinedNameError.env.map {
          case (name, _) => FromErrorsCompletion(name)
        }
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
