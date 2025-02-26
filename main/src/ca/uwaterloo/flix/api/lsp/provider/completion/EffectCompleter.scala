package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.{EffectCompletion, HandlerCompletion}
import ca.uwaterloo.flix.language.ast.NamedAst.Declaration.Effect
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.{AnchorPosition, LocalScope, Resolution}
import ca.uwaterloo.flix.language.errors.ResolutionError

object EffectCompleter {
  /**
    * Returns a List of Completion for effects.
    * Whether the returned completions are qualified is based on whether the name in the error is qualified.
    * When providing completions for unqualified enums that is not in scope, we will also automatically use the enum.
    */
  def getCompletions(err: ResolutionError.UndefinedType)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.qn.loc.source.name, err.ap, err.env, err.qn, inHandler = false)
  }

  def getCompletions(err: ResolutionError.UndefinedEffect)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.qn.loc.source.name, err.ap, err.env, err.qn, inHandler = true)
  }

  def getCompletions(err: ResolutionError.UndefinedName)(implicit root: TypedAst.Root): Iterable[Completion] = {
    getCompletions(err.qn.loc.source.name, err.ap, err.env, err.qn, inHandler = false)
  }

  private def getCompletions(uri: String, ap: AnchorPosition, env: LocalScope, qn: Name.QName, inHandler: Boolean)(implicit root: TypedAst.Root): Iterable[Completion] = {
    if (qn.namespace.nonEmpty)
      root.effects.values.collect{
        case effect if CompletionUtils.isAvailable(effect) && CompletionUtils.matchesName(effect.sym, qn, qualified = true) =>
          if (inHandler)
            HandlerCompletion(effect, ap, qualified = true, inScope = true)
          else
            EffectCompletion(effect, ap, qualified = true, inScope = true)
      }
    else
      root.effects.values.collect({
        case effect if CompletionUtils.isAvailable(effect) && CompletionUtils.matchesName(effect.sym, qn, qualified = false) =>
          if (inHandler)
            HandlerCompletion(effect, ap, qualified = false, inScope = inScope(effect, env))
          else
          EffectCompletion(effect, ap, qualified = false, inScope = inScope(effect, env))
      })
  }

  /**
    * Checks if the definition is in scope.
    * If we can find the definition in the scope or the definition is in the root namespace, it is in scope.
    */
  private def inScope(effect: TypedAst.Effect, scope: LocalScope): Boolean = {
    val thisName = effect.sym.toString
    val isResolved = scope.m.values.exists(_.exists {
      case Resolution.Declaration(Effect(_, _, _, thatName, _, _)) => thisName == thatName.toString
      case _ => false
    })
    val isRoot = effect.sym.namespace.isEmpty
    isRoot || isResolved
  }
}
