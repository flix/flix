package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.{MatchCompletion, NewObjectCompletion}
import ca.uwaterloo.flix.language.ast.{Ast, TypedAst}

object NewObjectCompleter extends Completer {

  /**
    * Returns a List of Completion for completer.
    */
  override def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[Completion] = {
    val regex = raw"\s*n?e?w?\s+(?:.*\s+)*(.*)".r
    if (!regex.matches(context.prefix)) {
      Nil
    } else {
      val wordPattern = "n?e?w?".r
      val currentWordIsNew = wordPattern.matches(context.word)

      root.uses.foldLeft(List.empty[MatchCompletion]) {
        case (acc, (_, useOrImport)) => newObjectCompletions(useOrImport, currentWordIsNew) ::: acc
      }
    }
  }

  private def newObjectCompletions(useOrImports: List[Ast.UseOrImport], currentWordIsNew: Boolean): List[NewObjectCompletion] = {
    useOrImports.foldLeft(List.empty[NewObjectCompletion]) {
      case (acc, x) => x match {
        case Ast.UseOrImport.Use(_, _, _) => acc
        case i@Ast.UseOrImport.Import(_, _, _) => newObjectCompletion(i, currentWordIsNew) match {
          case Some(v) => v :: acc
          case None => acc
        }
      }
    }
  }

  private def newObjectCompletion(useOrImport: Ast.UseOrImport, currentWordIsNew: Boolean): Option[NewObjectCompletion] = {
    ???
  }
}
