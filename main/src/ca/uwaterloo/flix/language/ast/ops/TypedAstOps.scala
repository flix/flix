package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.ast.TypedAst.{Expression, Pattern, Root}

object TypedAstOps {

  /**
    * Returns the free variables in the given pattern `pat0`.
    */
  def freeVarsOf(pat0: Pattern): Set[Symbol.VarSym] = pat0 match {
    case Pattern.Wild(tpe, loc) => Set.empty
    case Pattern.Var(sym, tpe, loc) => Set(sym)
    case Pattern.Unit(loc) => Set.empty
    case Pattern.True(loc) => Set.empty
    case Pattern.False(loc) => Set.empty
    case Pattern.Char(lit, loc) => Set.empty
    case Pattern.Float32(lit, loc) => Set.empty
    case Pattern.Float64(lit, loc) => Set.empty
    case Pattern.Int8(lit, loc) => Set.empty
    case Pattern.Int16(lit, loc) => Set.empty
    case Pattern.Int32(lit, loc) => Set.empty
    case Pattern.Int64(lit, loc) => Set.empty
    case Pattern.BigInt(lit, loc) => Set.empty
    case Pattern.Str(lit, loc) => Set.empty
    case Pattern.Tag(sym, tag, pat, tpe, loc) => freeVarsOf(pat)
    case Pattern.Tuple(elms, tpe, loc) => (elms flatMap freeVarsOf).toSet
  }

  // TODO: Place where?
  case class HoleContext(sym: Symbol.HoleSym, localVars: Set[(Symbol.VarSym, Type)])

  /**
    * Returns a map of the holes in the given ast `root`.
    */
  def holesOf(root: Root): Map[Symbol.HoleSym, HoleContext] = {
    // TODO: What about similarly named holes?
    def visitExp(exp0: Expression): Map[Symbol.HoleSym, HoleContext] = exp0 match {
      case _ => ???
    }

    // TODO: Need release flag?

    ???
  }

}
