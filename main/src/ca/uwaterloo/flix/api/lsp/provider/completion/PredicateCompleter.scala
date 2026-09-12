/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.acceptors.FileAcceptor
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.PredicateCompletion
import ca.uwaterloo.flix.api.lsp.{Consumer, Range, Visitor}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{Name, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType

object PredicateCompleter {

  def getCompletions(uri: String, range: Range)(implicit root: Root, flix: Flix): Iterable[PredicateCompletion] = {

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

    predsWithTypeAndLoc.map {
      case (predName, tpe) =>
      Completion.PredicateCompletion(predName.name, range, Priority.Lower(0), arityOf(tpe), FormatType.formatType(tpe))
    }
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
