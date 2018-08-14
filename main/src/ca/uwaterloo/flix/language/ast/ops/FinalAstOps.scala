package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.{FinalAst, Symbol}

object FinalAstOps {

  /**
    * Returns the free variables in the given head term `term0`.
    */
  def freeVarsOf(term0: FinalAst.Term.Head): Set[Symbol.VarSym] = term0 match {
    case FinalAst.Term.Head.Var(sym, tpe, loc) => Set(sym)
    case FinalAst.Term.Head.Lit(lit, tpe, loc) => Set.empty
    case FinalAst.Term.Head.Cst(sym, tpe, loc) => Set.empty
    case FinalAst.Term.Head.App(sym, args, tpe, loc) => args.toSet
  }

  /**
    * Returns the free variables in the given body term `term0`.
    */
  def freeVarsOf(term0: FinalAst.Term.Body): Set[Symbol.VarSym] = term0 match {
    case FinalAst.Term.Body.Wild(tpe, loc) => Set.empty
    case FinalAst.Term.Body.Var(sym, tpe, loc) => Set(sym)
    case FinalAst.Term.Body.Lit(lit, tpe, loc) => Set.empty
    case FinalAst.Term.Body.Cst(sym, tpe, loc) => Set.empty
  }

}
