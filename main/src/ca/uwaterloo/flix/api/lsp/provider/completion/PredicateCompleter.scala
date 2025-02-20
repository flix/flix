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
import ca.uwaterloo.flix.api.lsp.acceptors.FileAcceptor
import ca.uwaterloo.flix.api.lsp.{Consumer, Visitor}
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.PredicateCompletion
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{Name, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

object PredicateCompleter {

  def getCompletions(uri: String)(implicit root: Root, flix: Flix): Iterable[PredicateCompletion] = {

    //
    // Find all predicates together with their type and source location.
    //
    var predsWithTypeAndLoc: Set[(Name.Pred, Type)] = Set.empty

    object PredConsumer extends Consumer {
      override def consumePredicate(p: TypedAst.Predicate): Unit = p match {
        case TypedAst.Predicate.Head.Atom(name, _, _, tpe, _) => predsWithTypeAndLoc += ((name, tpe))
        case TypedAst.Predicate.Body.Atom(name, _, _, _, _, tpe, _) => predsWithTypeAndLoc += ((name, tpe))
        case _ => ()
      }
    }

    //
    // Select all predicate symbols that occur in the same file.
    //
    Visitor.visitRoot(root, PredConsumer, FileAcceptor(uri))

    predsWithTypeAndLoc.map{case (predName, tpe) => Completion.PredicateCompletion(predName.name, arityOf(tpe), FormatType.formatType(tpe))}
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
