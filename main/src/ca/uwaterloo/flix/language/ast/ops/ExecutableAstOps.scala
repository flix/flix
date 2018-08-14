package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}

object ExecutableAstOps {

  /**
    * Returns the free variables in the given head term `term0`.
    */
  def freeVarsOf(term0: ExecutableAst.Term.Head): Set[Symbol.VarSym] = term0 match {
    case ExecutableAst.Term.Head.Var(sym, tpe, loc) => Set(sym)
    case ExecutableAst.Term.Head.Lit(lit, tpe, loc) => Set.empty
    case ExecutableAst.Term.Head.Cst(sym, tpe, loc) => Set.empty
    case ExecutableAst.Term.Head.App(sym, args, tpe, loc) => args.toSet
  }

  /**
    * Returns the free variables in the given body term `term0`.
    */
  def freeVarsOf(term0: ExecutableAst.Term.Body): Set[Symbol.VarSym] = term0 match {
    case ExecutableAst.Term.Body.Wild(tpe, loc) => Set.empty
    case ExecutableAst.Term.Body.Var(sym, tpe, loc) => Set(sym)
    case ExecutableAst.Term.Body.Lit(lit, tpe, loc) => Set.empty
    case ExecutableAst.Term.Body.Cst(sym, tpe, loc) => Set.empty
  }

}
