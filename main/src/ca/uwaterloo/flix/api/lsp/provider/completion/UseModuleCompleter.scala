package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.ModCompletion

object UseModuleCompleter extends Completer {

  def getCompletions(ctx: CompletionContext)(implicit flix: Flix, index: Index, root: TypedAst.Root, delta: DeltaContext): Iterable[Completion] = {
    print("in UMC \n")
    val regex = raw"\s*use\s+(.*)".r
    ctx.prefix match {
      case regex(ns) =>
              val nestedModules = CompletionUtils.getNestedModules(ns)
              nestedModules.map(mod => ModCompletion(mod))
    }
  }
}
