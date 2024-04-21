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

  private def newObjectCompletions(useOrImports: List[Ast.UseOrImport], currentWordIsNew: Boolean)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): List[NewObjectCompletion] = {
    useOrImports.foldLeft(List.empty[NewObjectCompletion]) {
      case (acc, x) => x match {
        case _: Ast.UseOrImport.Use => acc
        case imprt: Ast.UseOrImport.Import => newObjectCompletion(imprt, currentWordIsNew) match {
          case Some(v) => v :: acc
          case None => acc
        }
      }
    }
  }

  private def newObjectCompletion(imprt: Ast.UseOrImport.Import, currentWordIsNew: Boolean)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Option[NewObjectCompletion] = imprt match {
    case Ast.UseOrImport.Import(clazz, alias, loc) =>
      val includeNew = if (currentWordIsNew) "new " else ""
      val (completion, _) = ???
      ???
  }
}
