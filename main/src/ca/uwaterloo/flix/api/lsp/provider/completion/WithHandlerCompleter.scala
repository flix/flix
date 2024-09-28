package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{Index, InsertTextFormat, TextEdit}
import ca.uwaterloo.flix.language.ast.TypedAst

object WithHandlerCompleter {
  def getCompletions(context: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root): Iterable[Completion] = {
    if (context.previousWord != "with") {
      return Nil
    }

    // TODO: infer the effect of the expression
    root.effects.map { case (sym, eff) =>
      val effString = sym.name
      val opStrings = eff.ops.map {
        case TypedAst.Op(sym, spec) =>
        val fparamsString = (spec.fparams.map(p => p.sym.text) :+ "k").mkString(", ")
        s"    def ${sym.name}($fparamsString) = ???"
      }
      val bodyString = s"$effString {\n${opStrings.mkString("\n")}\n}"
      Completion.WithHandlerCompletion(effString, TextEdit(context.range, bodyString))
    }
  }
}
