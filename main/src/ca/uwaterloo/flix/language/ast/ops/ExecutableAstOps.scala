package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.{ExecutableAst, Symbol}

object ExecutableAstOps {

  /**
    * Returns the free variable symbols in the given pattern `pat0`.
    */
  def freeVarsOf(pat0: ExecutableAst.Pattern): Set[Symbol.VarSym] = pat0 match {
    case ExecutableAst.Pattern.Wild(tpe, loc) => Set.empty
    case ExecutableAst.Pattern.Var(sym, tpe, loc) => Set(sym)
    case ExecutableAst.Pattern.Unit(loc) => Set.empty
    case ExecutableAst.Pattern.True(loc) => Set.empty
    case ExecutableAst.Pattern.False(loc) => Set.empty
    case ExecutableAst.Pattern.Char(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Float32(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Float64(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Int8(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Int16(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Int32(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Int64(lit, loc) => Set.empty
    case ExecutableAst.Pattern.BigInt(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Str(lit, loc) => Set.empty
    case ExecutableAst.Pattern.Tag(sym, tag, pat, tpe, loc) => freeVarsOf(pat)
    case ExecutableAst.Pattern.Tuple(elms, tpe, loc) => (elms flatMap freeVarsOf).toSet
  }

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
    case ExecutableAst.Term.Body.Pat(pat, tpe, loc) => freeVarsOf(pat)
  }


}
